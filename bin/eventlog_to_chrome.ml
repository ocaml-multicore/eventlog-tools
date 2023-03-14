open Rresult.R.Infix
open Eventlog
    
let load_file path =
  let open Rresult.R.Infix in
  Fpath.of_string path
  >>= Bos.OS.File.read
  >>= fun content ->
  Ok (Bigstringaf.of_string ~off:0 ~len:(String.length content) content)

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

let encode_event e { timestamp; pid; payload; } =
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
  enc e oe

    
let main in_file out_file =
  let module P = Eventlog.Parser in
  load_file in_file >>= fun data ->
  let decoder = P.decoder () in
  let total_len = Bigstringaf.length data in
  P.src decoder data 0 total_len true;
  let convert oc () =
    let encoder = Jsonm.encoder (`Channel oc) in
    encode_catapult_prelude encoder;
    let rec aux () =
      match P.decode decoder with
      | `Ok Event ev ->
        encode_event encoder ev;
        aux ()
      | `Ok _ -> aux ()
      | `Error (`Msg msg) -> Printf.eprintf "some events were discarded: %s" msg; Ok ()
      | `End -> Ok ()
      | `Await -> Ok ()
    in
    aux () >>= fun () ->
    enc encoder ae;
    enc encoder oe;
    enc encoder `End;
    Ok ()
  in 
  Fpath.of_string out_file  >>= fun out_file ->
  match Bos.OS.File.with_oc out_file convert () with
  | Error (`Msg s) -> Error (`Msg s)
  | o -> o

let main in_file out_file =
  match main in_file out_file with
  | Ok (Ok _) -> 0
  | _ -> 1 

module Args = struct

  open Cmdliner

  let trace =
    let doc = "input OCaml CTF trace" in
    Arg.(required & pos 0 (some string) None  & info [] ~doc )

  let outfile = 
    let doc = "output JSON file" in
    Arg.(required & pos 1 (some string) None  & info [] ~doc )

  let info =
    let doc = "" in
    let man = [
      `S Manpage.s_bugs;
    ]
    in
    Cmd.info "ocaml-eventlog-to-chrome" ~version:"%%VERSION%%" ~doc ~man

end

let () =
  let open Cmdliner in
  let main_t = Term.(const main  $ Args.trace $ Args.outfile) in
  let r = Cmd.eval' (Cmd.v Args.info main_t) in
  Stdlib.exit r