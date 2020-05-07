type error = Rresult.R.msg
let pp_error ppf = Rresult.R.pp_msg ppf
let error = Alcotest.testable pp_error (=)
let t = Alcotest.(result unit error)

let check (name, fn) = name, `Quick, (fun () -> Alcotest.check t name (Ok ()) (fn ()))

let test_parser = List.map check Test_parser.tests

let () =
  Alcotest.run "eventlog-tools" [
    "test_parser", test_parser;
  ]
