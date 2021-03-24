open Rresult.R.Infix
open Eventlog

let load_dir path =
  Fpath.of_string path
  >>= Bos.OS.Dir.contents

let load_file path =
  let open Rresult.R.Infix in
  Bos.OS.File.read path
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

let traverse file_in encoder =
  let module P = Eventlog.Parser in
  load_file file_in >>= fun data ->
  let decoder = P.decoder () in
  let total_len = Bigstringaf.length data in
  P.src decoder data 0 total_len true;
  let convert () =
    let rec aux () =
      match P.decode decoder with
      | `Ok Event ev ->
        encode_event encoder ev;
        aux ()
      | `Ok _ -> aux ()
      | `Error (`Msg msg) -> Printf.eprintf "[%s] some events were discarded: %s\n" (Fpath.to_string file_in) msg; Ok ()
      | `End -> Ok ()
      | `Await -> Ok ()
    in
    aux ()
  in
  convert ()

let main in_dir out_file =
  Fpath.of_string out_file >>= fun out_file ->
  Bos.OS.File.with_oc out_file begin fun out () ->
    let encoder = Jsonm.encoder (`Channel out) in
    encode_catapult_prelude encoder;
    load_dir in_dir >>= fun tracedir ->
    (List.fold_left
       (fun err f -> if Result.is_error err then err else traverse f encoder)
       (Result.ok ()) tracedir) >>= fun () ->
    enc encoder ae;
    enc encoder oe;
    enc encoder `End;
    Ok ()
  end ()

module Args = struct

  open Cmdliner

  let tracedir =
    let doc = "input OCaml CTF trace dir" in
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
    Term.info "ocaml-eventlog-to-chrome" ~version:"%%VERSION%%" ~doc ~exits:Term.default_exits ~man

end

let () =
  let open Cmdliner in
  Term.exit @@ Term.eval Term.(const main  $ Args.tracedir $ Args.outfile, Args.info)
