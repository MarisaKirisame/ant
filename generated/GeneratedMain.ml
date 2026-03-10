let usage = "Usage: GeneratedMain <append|filter|map|qs|asymptotics-live|asymptotics>"

let () =
  match Array.to_list Sys.argv with
  | [ _; mode ] -> (
      match String.lowercase_ascii mode with
      | "append" -> HazelAppend.run ()
      | "filter" -> HazelFilter.run ()
      | "map" -> HazelMap.run ()
      | "qs" -> HazelQS.run ()
      | "asymptotics-live" -> RunLiveAsymptotics.run ()
      | "asymptotics" -> RunTestAsymptotics.run ()
      | _ ->
          prerr_endline usage;
          exit 1)
  | _ ->
      prerr_endline usage;
      exit 1
