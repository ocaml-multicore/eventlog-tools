open Rresult.R.Infix

let load_as_dir path =
  Fpath.of_string path >>= fun path ->
  match Bos.OS.File.exists path with
  | Ok true -> Ok [ path ]
  | Ok false -> Bos.OS.Dir.contents path
  | Error err -> Error err

let load_file path =
  let open Rresult.R.Infix in
  Bos.OS.File.read path
  >>= fun content ->
  Ok (Bigstringaf.of_string ~off:0 ~len:(String.length content) content)

let load paths =
  let rec aux l acc =
    match l with
    | [] -> Ok acc
    | path::xs ->
      match load_as_dir path with
      | Ok res -> aux xs (res::acc)
      | Error err -> Error err
  in
  aux paths [] |> Result.map List.flatten
