let () =
  let _ = Array.make 128 '\000' in
  Gc.minor();
  print_endline "minor";
  Gc.full_major();
  print_endline "major"
