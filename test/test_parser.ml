module E = Eventlog
module R = Rresult.R
open R.Infix

let load_file path =
  let open Rresult.R.Infix in
  Fpath.of_string path
  >>= Bos.OS.File.read
  >>= fun content ->
  Ok (Bigstringaf.of_string ~off:0 ~len:(String.length content) content)

let parse path expected =
  load_file path >>= fun file ->
  let decoder = E.Parser.decoder () in
  let len = Bigstringaf.length file in
  E.Parser.src decoder file 0 len true;
  let rec aux count =
    match E.Parser.decode decoder with
    | `Await -> R.error_msg "unexpected await returned by decode"
    | `End -> Ok count
    | `Error msg -> Error msg
    | `Ok _ -> aux (succ count)
  in
  aux 0 >>= fun count ->
  if count != expected then
    R.error_msgf
      "%s: decoded event count %d doesn't match expected count: %d"
      path count expected
  else
    Ok ()

let parse_simple () = parse "assets/simple.eventlog" 137
let parse_be () = parse "assets/be.eventlog" 137
let parse_empty () = parse "assets/empty.eventlog" 0

let add_trace () =
  Ok ()

let tests = [
  "parse_simple", parse_simple;
  "parse_empty", parse_empty;
  "parse_be", parse_be;
]
