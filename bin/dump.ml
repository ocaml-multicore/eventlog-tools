open Rresult.R.Infix

let print_event { Traces.timestamp; pid; payload; } =
  match payload with 
  | Entry { phase; } ->
    Printf.printf "entry\tts: %d\tpid: %d\tphase: %s\n" timestamp pid phase 
  | Exit { phase; } ->
    Printf.printf "exit\tts: %d\tpid: %d\tphase: %s\n" timestamp pid phase 
  | Flush { duration; } ->
    Printf.printf "flush\tts: %d\tpid: %d\tduration: %d\n" timestamp pid duration 
  | Alloc { bucket; count; } ->
    Printf.printf "alloc\tts: %d\tpid: %d\tbucket: %s\t count: %d\n" timestamp pid bucket count 
  | Counter { kind; count; } ->
    Printf.printf "alloc\tts: %d\tpid: %d\tkind: %s\t count: %d\n" timestamp pid kind count 

let main in_file =
  let module P = Traces.Parser in
  Commons.load_file in_file >>= fun data ->
  let decoder = P.decoder () in
  let total_len = Bigstringaf.length data in
  P.src decoder data 0 total_len true;
  let rec aux () =
    match P.decode decoder with
    | `Ok ev ->
      print_event ev;
      aux ()
    | `Error (`Msg msg) -> Error (`Msg msg)
    | `End -> Ok ()
    | `Await -> Ok ()
  in
  aux ()

module Args = struct

  open Cmdliner

  let trace =
    let doc = "Dump an OCaml CTF trace" in
    Arg.(required & pos 0 (some string) None  & info [] ~doc )

  let info =
    let doc = "" in
    let man = [
      `S Manpage.s_bugs;
    ]
    in
    Term.info "caml-eventlog-dump" ~version:"%‌%VERSION%%" ~doc ~exits:Term.default_exits ~man

end

let () =
  let open Cmdliner in
  Term.exit @@ Term.eval Term.(const main  $ Args.trace, Args.info)
