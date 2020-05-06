open Crowbar
module Bs = Bigstringaf
module E = Eventlog
  
let dec =
  map [bytes] (fun s ->
      let dec = E.Parser.decoder () in
      let bs = Bigstringaf.of_string s ~off:0 ~len:(String.length s) in
      let len = Bigstringaf.length bs in
      E.Parser.src dec bs 0 len true;
      let rec aux () =
        match E.Parser.decode dec with
        | `Error _ -> bad_test () 
        | `Await -> fail "await"
        | `End -> ()
        | `Ok _ -> aux ()
      in
      aux () 
    )
    
let () = add_test ~name:"dec" [dec] @@ fun p -> p
