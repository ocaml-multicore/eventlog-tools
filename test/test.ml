type error = Rresult.R.msg
let pp_error ppf = Rresult.R.pp_msg ppf
let error = Alcotest.testable pp_error (=)
let t = Alcotest.(result unit error)

let check (name, fn) = name, `Quick, (fun () -> Alcotest.check t name (Ok ()) (fn ()))

let test_parser = List.map check Test_parser.tests

let () =
  Junit_alcotest.run_and_report "eventlog-tools" [
    "test_parser", test_parser;
  ]
  |> fun (r, _) ->
  Junit.(to_file (make [r]) "alcotest-junit.xml")
