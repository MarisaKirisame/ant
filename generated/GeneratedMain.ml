let usage = "Usage: GeneratedMain <live|test|tailrec>"

let () =
  match Array.to_list Sys.argv with
  | [ _; mode ] -> (
      match String.lowercase_ascii mode with
      | "live" -> RunLive.run ()
      | "test" -> RunTest.run ()
      | "tailrec" -> RunTailRec.run ()
      | _ ->
          prerr_endline usage;
          exit 1)
  | _ ->
      prerr_endline usage;
      exit 1
