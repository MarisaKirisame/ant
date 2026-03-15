let usage = Printf.sprintf "Usage: GeneratedMain <%s|arith>" (String.concat "|" HazelExperiment.all_modes)

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
      | "arith" -> RunArith.run ()
      | _ ->
          if not (HazelExperiment.run_mode mode) then (
            prerr_endline usage;
            exit 1))
  | _ ->
      prerr_endline usage;
      exit 1
