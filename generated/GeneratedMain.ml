let usage = "Usage: GeneratedMain <live-simple|live-left-to-right|live-demand-driven|test|tailrec|hazel|lisp>"

let () =
  match Array.to_list Sys.argv with
  | [ _; mode ] -> (
      match String.lowercase_ascii mode with
      | "live-simple" -> RunLiveSimple.run ()
      | "live-left-to-right" -> RunLiveLeftToRight.run ()
      | "live-demand-driven" -> RunLiveDemandDriven.run ()
      | "test" -> RunTest.run ()
      | "tailrec" -> RunTailRec.run ()
      | "lisp" -> RunLisp.run ()
      | "hazel" -> FromHazel.run ()
      | _ ->
          prerr_endline usage;
          exit 1)
  | _ ->
      prerr_endline usage;
      exit 1
