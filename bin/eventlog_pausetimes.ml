open Rresult.R.Infix
open Eventlog
open Bin_common

module Itv = Interval_map.Make(Int)
module B = Itv.Bound
module I = Itv.Interval

type entry = {
  name : phase;
  overhead : int;
  ts : int;
  is_top_level : bool; (* is the event the outermost in the event stack ? *)
}

type state = {
    stacks : ((int * int), entry Stack.t) Hashtbl.t;
    trees : ((int * int), (int * int * entry) list) Hashtbl.t;
}

exception Mismatched_name of (string * string)
let raise_mismatched_name en ex =
  let names = (string_of_phase en, string_of_phase ex) in
  raise (Mismatched_name names)

let encode_event state ev =
  if ev.is_backup_thread then (* bt thread, shouldn't count in pausetimes *)
    state
  else
  match ev.payload with
  | Entry { phase; } -> begin
     let key = ev.pid, ev.pid in
     let ts = ev.timestamp in
     let name = phase in
     match Hashtbl.find_opt state.stacks key with
     | Some stack ->
        let is_top_level = if Stack.length stack = 0 then true else false in
        Stack.push { ts; name; overhead = 0; is_top_level; } stack; state
     | None ->
        let stack = Stack.create () in
        Stack.push { ts; name; overhead = 0; is_top_level = true; } stack;
        Hashtbl.add state.stacks key stack;
        state
    end
  | Exit { phase; } -> begin
     let key = ev.pid, ev.pid in
     let ts = ev.timestamp in
     let name = phase in
     match Hashtbl.find_opt state.stacks key with
     | None -> state
     | Some stack ->
     if Stack.is_empty stack then
       state (* FIXME(engil): exit event but no entry event? sounds wrong. *)
     else
       let entry = Stack.pop stack in

       if entry.name != name then               (* phase mismatch *)
         raise_mismatched_name entry.name name; (* calculations probably skewed,
                                                   raise. *)
       match Hashtbl.find_opt state.trees key with
       | Some tree ->
          Hashtbl.replace state.trees key ((entry.ts, ts, entry)::tree);
          state
       | None ->
          Hashtbl.add state.trees key ([(entry.ts, ts, entry)]);
          state
    end
  | Flush { duration; } ->
     let key = ev.pid, ev.pid in
     let stack = Hashtbl.find_opt state.stacks key in
     (match stack with
     | None -> state
     | Some stack ->
     if Stack.is_empty stack then
       state (* standalone flush, no overhead to propagate *)
     else
       let stack' = Stack.create () in
       Stack.fold (fun acc e -> { e with overhead = e.overhead + duration; }::acc) [] stack
       |> List.iter (fun e -> (Stack.push e stack'));
       Hashtbl.replace state.stacks key stack';
       state)
  | _ -> state

let finish_marking = phase_of_string "major/finish_marking"
let finish_sweeping = phase_of_string "major/finish_sweeping"
let terminate_cond ev = ev.name = finish_marking || ev.name = finish_sweeping
let top_level_cond ev = if ev.is_top_level then true else false
let interval b1 b2 = I.create (B.Included b1) (B.Included b2)

let process_trees state =
  Hashtbl.fold begin fun (_, _) tree (acc_latencies, acc_itvs) ->
  let itv, terminate_itv =
    List.fold_left begin fun (itv, terminate_itv) (entry, ex, ev) ->
      let terminate_itv =
        if terminate_cond ev then
          Itv.add (interval entry ex) (ev.name, entry ,ex, ev.overhead) terminate_itv
        else
          terminate_itv
      in
      let itv =
        if top_level_cond ev then
          Itv.add (interval entry ex) (ev.name, entry, ex, ev.overhead) itv
        else
          itv
      in
      (itv, terminate_itv)
      end (Itv.empty, Itv.empty) tree
  in
  let itv = Itv.fold (fun it _v itv -> Itv.remove_interval it itv) terminate_itv itv in
  let latencies =
    Itv.fold (fun _it v l ->
        if List.length v > 0 then
          let (_, entry, ex, overhead) = List.hd v in ((ex - entry) - overhead) ::l else l
      ) itv acc_latencies
  in
  latencies, itv::acc_itvs
  end state.trees ([], [])

let print_json ~name ~max_latency ~mean_latency distribution =
  Printf.printf
{|{"name": "%s", "mean_latency": %d, "max_latency": %d, "distr_latency": [%s] }
|}
    name
    (int_of_float mean_latency)
    (int_of_float max_latency)
    (List.map int_of_float distribution
     |> List.map string_of_int
     |> String.concat ",")


let process_latencies name state =
  let percentages = [
      10.; 20.; 30.; 40.; 50.; 60.;
      70.; 80.; 90.; 99.; 99.9
  ] in
  let latencies, _ = process_trees state in
  let sorted_latencies =
    List.sort Int.compare (latencies)
    |> Array.of_list
    |> Array.map float_of_int
  in
  let sorted_len = Array.length sorted_latencies in
  let max_latency =
    if sorted_len = 0 then 0. else sorted_latencies.(Array.length sorted_latencies - 1)
  in
  let mean_latency = Owl_base.Stats.mean sorted_latencies in
  let distribution = List.map (Owl_base.Stats.percentile sorted_latencies) percentages in
  print_json ~name ~max_latency ~mean_latency distribution

let traverse state file_in =
  let module P = Eventlog.Parser in
  match Common.load_file file_in with
  | Error `Msg err ->
    Printf.eprintf "[%s] %s\n" (Fpath.to_string file_in) err;
    state
  | Ok data ->
  let decoder = P.decoder () in
  let total_len = Bigstringaf.length data in
  P.src decoder data 0 total_len true;
  let convert state =
    let rec aux state =
      match P.decode decoder with
      | `Ok Event ev ->
        let state = encode_event state ev in
        aux state
      | `Ok _ -> aux state
      | `Error (`Msg msg) ->
        Printf.eprintf "[%s] some events were discarded: %s\n" (Fpath.to_string file_in) msg;
        state
      | `End -> state
      | `Await -> state
    in
    let state = aux state in
    state
  in
  convert state

let main src_in name =
  let aux () =
    let stacks = Hashtbl.create 128 in
    let trees = Hashtbl.create 128 in
    let state = { stacks; trees; } in
    Common.load src_in >>= fun tracedir ->
    let state = List.fold_left traverse state tracedir in
    Ok state
  in
  match aux () with
  | Error err -> Error err
  | Ok state -> process_latencies name state; Ok ()


module Args = struct

  open Cmdliner

  let name =
    let doc = "Name for the compiler switch" in
    Arg.(required & pos ~rev:true 0 (some string) None & info [] ~docv:"NAME" ~doc)
  let srcs =
     let doc = "Source file(s) to copy. You can also pass a directory as an argument and it will process all files within this directory." in
     Arg.(non_empty & pos_left ~rev:true 0 file [] & info [] ~docv:"SOURCE" ~doc)
  let info =
    let doc = "" in
    let man = [
      `S Manpage.s_bugs;
    ]
    in
    Term.info "ocaml-eventlog-pausetimes" ~version:"%%VERSION%%" ~doc ~exits:Term.default_exits ~man

end

let () =
  let open Cmdliner in
  Term.exit @@ Term.eval Term.(const main $ Args.srcs $ Args.name, Args.info)
