let usage = Printf.sprintf "Usage: GeneratedMain <%s|arith|asymptotics>" (String.concat "|" HazelExperiment.all_modes)

let () =
  match Array.to_list Sys.argv with
  | [ _; mode ] -> (
      match String.lowercase_ascii mode with
      | "arith" -> RunArith.run ()
      | "asymptotics" -> RunTestAsymptotics.run ()
      | _ ->
          if not (HazelExperiment.run_mode mode) then (
            prerr_endline usage;
            exit 1))
  | _ ->
      prerr_endline usage;
      exit 1
