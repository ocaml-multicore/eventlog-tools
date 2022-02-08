open Rresult.R.Infix
open Bin_common

type allocs = (Eventlog.bucket, int) Hashtbl.t
type counters = (Eventlog.counter_kind, int list) Hashtbl.t

module Events = struct

  type t = {
    h : (Eventlog.phase, int) Hashtbl.t; (* temporary table holding the entry event to track an event's lifetime *)
    mutable last_flush : (int * int); (* timestamp * duration *)
    events : (Eventlog.phase, int list) Hashtbl.t;
  }

  (* note: when computing an event duration, we check if the last flush event happened
     inbetween. if so, we deduce the duration of the flush event from this event. *)
  let update t name v_start v_end =
    match Hashtbl.find_opt t.events name with
    | Some l ->
      let last_flush_ts = fst t.last_flush in
      let v =
        if (v_start < last_flush_ts) && (last_flush_ts < v_end) then
          (v_end - v_start) - (snd t.last_flush)
        else
          (v_end - v_start)
      in
      Hashtbl.replace t.events name (v::l)
    | None -> Hashtbl.add t.events name [v_end - v_start]

  let handle_exit ({ h; _ } as t) name v =
    match Hashtbl.find_opt h name with
    | Some v' -> if v' > v then assert false else
        begin
          Hashtbl.remove h name;
          update t name v' v
        end;
    | None -> ()

  let handle_entry { h;_ } name v =
    match Hashtbl.find_opt h name with
    | Some _ -> ()
    | None -> Hashtbl.add h name v

  let handle_flush t ts dur = t.last_flush <- ts, dur

  let create () = {
    h = Hashtbl.create 12;
    events = Hashtbl.create 12;
    last_flush = (0, 0);
  }

  let get { events; _ } name = Hashtbl.find events name

  let iter t f = Hashtbl.iter f t.events

end

type t = {
  events : Events.t;
  allocs : allocs;
  counters : counters;
  mutable flushs : int list;
}

let read_event { Eventlog.payload; timestamp; _ } ({ allocs; events; counters; _ } as t) =
  match payload with
  | Alloc { bucket; count; } -> begin
    match Hashtbl.find_opt allocs bucket with
    | Some v -> Hashtbl.replace allocs bucket (v + count)
    | _ -> Hashtbl.add allocs bucket count
  end
  | Entry {phase; } -> Events.handle_entry events phase timestamp
  | Exit {phase; } -> Events.handle_exit events phase timestamp
  | Counter { kind; count; } -> begin
     match Hashtbl.find_opt counters kind with
     | Some l -> Hashtbl.replace counters kind (count::l)
     | None -> Hashtbl.add counters kind [count]
  end
  | Flush {duration; } ->
    t.flushs <- duration::t.flushs;
    Events.handle_flush events timestamp duration


(* pretty-printing and output *)

(* borrowed from https://github.com/ocaml/ocaml/blob/trunk/utils/misc.ml#L780 *)
let pp_two_columns ?max_lines ppf name (lines: (string * string) list) =
  let left_column_size =
    List.fold_left (fun acc (s, _) -> max acc (String.length s)) 0 lines in
  let lines_nb = List.length lines in
  let ellipsed_first, ellipsed_last =
    match max_lines with
    | Some max_lines when lines_nb > max_lines ->
        let printed_lines = max_lines - 1 in (* the ellipsis uses one line *)
        let lines_before = printed_lines / 2 + printed_lines mod 2 in
        let lines_after = printed_lines / 2 in
        (lines_before, lines_nb - lines_after - 1)
    | _ -> (-1, -1)
  in
  Format.fprintf ppf "@[<v>";
  Format.fprintf ppf "==== %s\n" name;
  List.iteri (fun k (line_l, line_r) ->
    if k = ellipsed_first then Format.fprintf ppf "...@,";
    if ellipsed_first <= k && k <= ellipsed_last then ()
    else Format.fprintf ppf "%*s: %s@," left_column_size line_l line_r
  ) lines;
  Format.fprintf ppf "@]"


let cons' a l = List.cons l a

let print_allocs allocs =
  let l =
    Hashtbl.fold begin fun bucket count acc ->
      (Printf.sprintf "%s" (Eventlog.string_of_alloc_bucket bucket), Printf.sprintf "%d" count)
      |> cons' acc
    end allocs []
  in
  pp_two_columns Format.std_formatter "allocs" l

let pprint_time ns =
  if ns < 1000. then
    Printf.sprintf "%.0fns" ns
  else if ns < (1_000_000.) then
    Printf.sprintf "%.1fus" (ns /. 1_000.)
  else if ns < (1_000_000_000.) then
    Printf.sprintf "%.1fms" (ns /. 1_000_000.)
  else
    Printf.sprintf "%.1fs" (ns /. 1_000_000_000.)

let pprint_quantity q =
  if q < 1000. then
    Printf.sprintf "%.0f" q
  else if q < (1_000_000.) then
    Printf.sprintf "%.1fK" (q /. 1_000.)
  else
    Printf.sprintf "%.1fM" (q /. 1_000_000.)

let bins mul =
  Array.map (( *. ) mul) [| 100.; 200.; 300.; 500.; |]

let default_bins = Array.concat [
    [| 0. |];
    bins 1.;
    bins 10.;
    bins 100.;
    bins 1000.;
    bins 10000.;
    bins 100000.;
    bins 1000000.;
  ]

let make_bins max =
  let max_in_default_bins = Array.get default_bins (Array.length default_bins - 1) in
  let bins = Array.concat [
    default_bins;
    if max > max_in_default_bins then [|max|] else [||];
  ]
  in
  `Bins bins

let print_histogram name l pprint =
  let open Owl_base_stats in
  let arr = l |> Array.of_list |> Array.map float_of_int in
  let bins = make_bins (max arr) in
  let h = histogram bins arr in
  let l = ref [] in
  for i = 0 to (Array.length h.bins - 2) do
    if h.counts.(i) > 0 then
          l := (Printf.sprintf "%s..%s" (pprint h.bins.(i)) (pprint h.bins.(i + 1)),
               Printf.sprintf "%d" h.counts.(i))::!l
  done;
  pp_two_columns Format.std_formatter name !l

let print_events_stats name events =
  match Events.get events name with
  | exception Not_found -> ()
  | events -> print_histogram (Eventlog.string_of_phase name) events pprint_time

let print_flushes flushs =
  let a = Array.of_list flushs |> Array.map float_of_int in
  let median = Owl_base_stats.median a in
  let total = Owl_base_stats.sum a in
  pp_two_columns Format.std_formatter  "eventlog/flush" [
    "median flush time", (pprint_time median);
    "total flush time", (pprint_time total);
    "flush count", (List.length flushs |> string_of_int);
  ]

(* file traversing gluing everything together *)
let traverse f t =
  let module P = Eventlog.Parser in
  Common.load_file f >>= fun data ->
  let decoder = P.decoder () in
  let total_len = Bigstringaf.length data in
  P.src decoder data 0 total_len true;
  let rec aux () =
    match P.decode decoder with
    | `Ok Event ev ->
      read_event ev t;
      aux ()
    | `Ok Header _ -> aux ()
    | `Error (`Msg msg) -> Printf.eprintf "some events were discarded: %s" msg; Ok ()
    | `End -> Ok ()
    | `Await -> Ok ()
  in
  aux ()

let main in_dir =
  let allocs = Hashtbl.create 10 in
  let events = Events.create () in
  let counters = Hashtbl.create 10 in
  let t = { allocs; events; flushs = []; counters; } in
  Common.load [in_dir] >>= fun tracedir ->
  (List.fold_left
     (fun err f -> if Result.is_error err then err else traverse f t)
     (Result.ok ()) tracedir)
  >>= fun () ->
  print_allocs allocs;
  Events.iter events (fun phase _ -> print_events_stats phase events);
  Hashtbl.iter (fun s l -> print_histogram (Eventlog.string_of_gc_counter s) l pprint_quantity) counters;
  print_flushes t.flushs;
  Ok ()

module Args = struct
  type t = {
    events : Events.t;
    allocs : allocs;
    counters : counters;
    mutable flushs : int list;
  }


  open Cmdliner

  let trace =
    let doc = "input OCaml CTF trace dir" in
    Arg.(required & pos 0 (some string) None  & info [] ~doc )

  let info =
    let doc = "" in
    let man = [
      `S Manpage.s_bugs;
    ]
    in
    Term.info "ocaml-eventlog-report" ~version:"%%VERSION%%" ~doc ~exits:Term.default_exits ~man

end

let () =
  let open Cmdliner in
  Term.exit @@ Term.eval Term.(const main  $ Args.trace, Args.info)
