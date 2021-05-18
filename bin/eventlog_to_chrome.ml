open Rresult.R.Infix
open Eventlog
open Bin_common

module PSet = Set.Make(Int)

type state = {
  errored : bool;
  encoder : Jsonm.encoder;
  pset : PSet.t; (* set of pids encountered, for metadata *)
}

let enc e l = Jsonm.encode e l |> ignore

let os = `Lexeme `Os
let oe = `Lexeme `Oe
let as_ = `Lexeme `As
let ae = `Lexeme `Ae
let name s = `Lexeme (`Name s)
let string s = `Lexeme (`String s)
let number f = `Lexeme (`Float (float_of_int f))

let enc_name_value e n v =
  enc e n;
  enc e v

let encode_catapult_prelude e =
  enc e os;
  enc e (name "displayTimeUnit");
  enc e (string "ns");
  enc e (name "traceEvents");
  enc e as_

let encode_thread_metadata ~is_backup_thread pid e =
  let process_name =
    if is_backup_thread then
      Printf.sprintf "BackupThread%d" (pid - 1) (* BT is domain's pid + 1 *)
    else
      Printf.sprintf "Domain%d" pid
  in
  enc e os;
  enc_name_value e (name "ph") (string "M");
  enc_name_value e (name "name") (string "thread_name");
  enc_name_value e (name "pid") (number pid);
  enc_name_value e (name "tid") (number pid);
  enc e (name "args");
  enc e os;
  enc_name_value e (name "name") (string process_name);
  enc e oe;
  enc e oe

let encode_event ({ encoder=e; pset; _ } as state) { is_backup_thread; timestamp; pid; payload; } =
  let pid = if is_backup_thread then pid + 1 else pid in
  let state =
    if PSet.exists ((=) pid) pset then
      state
    else
      let pset = PSet.add pid pset in
      encode_thread_metadata ~is_backup_thread pid e;
      { state with pset; }
  in
  let ts = Printf.sprintf "%d.%03d" (timestamp / 1000) (timestamp mod 1000) in
  enc e os;
  enc_name_value e (name "ts") (string ts);
  enc_name_value e (name "pid") (number pid);
  enc_name_value e (name "tid") (number pid);
  enc e (name "ph");
  begin match payload with
  | Entry { phase; } ->
    enc e (string "B");
    enc_name_value e (name "name") (string (string_of_phase phase))
  | Exit { phase; } ->
    enc e (string "E");
    enc_name_value e (name "name") (string (string_of_phase phase))
  | Flush { duration; } ->
    let dur = Printf.sprintf "%d.%03d" (duration / 1000) (duration mod 1000) in
    enc e (string "X");
    enc_name_value e (name "name") (string "eventlog/flush");
    enc_name_value e (name "dur") (string dur);
  | Alloc { bucket; count; } ->
    enc e (string "C");
    enc_name_value e (name "name") (string (string_of_alloc_bucket bucket));
    enc e (name "args");
    enc e os;
    enc_name_value e (name "value") (number count);
    enc e oe
  | Counter { kind; count; } ->
    enc e (string "C");
    enc_name_value e (name "name") (string (string_of_gc_counter kind));
    enc e (name "args");
    enc e os;
    enc_name_value e (name "value") (number count);
    enc e oe
  end;
  enc e oe;
  state

let traverse state file_in =
  let module P = Eventlog.Parser in
  match Common.load_file file_in with
  | Error `Msg err ->
    Printf.eprintf "[%s] %s\n" (Fpath.to_string file_in) err;
    { state with errored = true; }
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
        { state with errored = true; }
      | `End -> state
      | `Await -> state
    in
    aux state
  in
  convert state

let main out_file src_in =
  let aux out () =
    let encoder = Jsonm.encoder (`Channel out) in
    encode_catapult_prelude encoder;
    let state = { pset = PSet.empty; encoder; errored = false; } in
    Common.load src_in >>= fun tracedir ->
    let state = List.fold_left traverse state tracedir in
    enc encoder ae;
    enc encoder oe;
    enc encoder `End;
    Ok state
  in
  let res =
    match out_file with
    | None -> aux stdout ()
    | Some out_file ->
       Fpath.of_string out_file >>= fun out ->
       Printf.printf "Writing to file: %s\n" (Fpath.to_string out); flush stdout;
       Bos.OS.File.with_oc out aux ()
       |> Result.join
  in
  match res with
  | Error err -> Error err
  | Ok { errored = true; _ } -> Error (`Msg "Some errors were encountered during processing.")
  | _ -> Ok ()


module Args = struct

  open Cmdliner

  let tracedir =
    let doc = "source" in
    Arg.(non_empty & pos_all string [] & info [] ~docv:"SOURCE" ~doc)
  let outfile =
    let doc = "output JSON file (default to stdout)" in
    Arg.(value & opt (some string) None & info ["o"; "output"] ~docv:"OUTPUT" ~doc)
  let info =
    let doc = "" in
    let man = [
      `S Manpage.s_bugs;
    ]
    in
    Term.info "ocaml-eventlog-to-chrome" ~version:"%%VERSION%%" ~doc ~exits:Term.default_exits ~man

end

let () =
  let open Cmdliner in
  Term.exit @@ Term.eval Term.(const main $ Args.outfile $ Args.tracedir, Args.info)
