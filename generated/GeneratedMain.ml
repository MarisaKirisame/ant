let usage = "Usage: GeneratedMain <map|qs>"

let () =
  match Array.to_list Sys.argv with
  | [ _; mode ] -> (
      match String.lowercase_ascii mode with
      | "map" -> HazelMap.run ()
      | "qs" -> HazelQS.run ()
      | _ ->
          prerr_endline usage;
          exit 1)
  | _ ->
      prerr_endline usage;
      exit 1
