open Crowbar
module Bs = Bigstringaf
module E = Eventlog
  
let buffer = map [uint8] Bigarray.(Array1.create Char c_layout)  

let dec =
  map [buffer] (fun s ->
      let dec = E.Parser.decoder () in
      let len = Bigstringaf.length s in
      E.Parser.src dec s 0 len true;
      let rec aux () =
        match E.Parser.decode dec with
        | `Ok _  -> aux ()
        | _ -> ()
      in
      aux () 
    )
    
let () = add_test ~name:"dec" [dec] @@ fun p -> p
