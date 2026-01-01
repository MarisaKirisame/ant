let usage = "Usage: GeneratedMain <live-simple|live-left-to-right|live-demand-driven|test|tailrec|hazel>"

let () =
  match Array.to_list Sys.argv with
  | [ _; mode ] -> (
      match String.lowercase_ascii mode with
      | "test" -> RunTest.run ()
      | _ ->
          prerr_endline usage;
          exit 1)
  | _ ->
      prerr_endline usage;
      exit 1
