let usage =
  Printf.sprintf "Usage: GeneratedMain <%s|arith|asymptotics|hazel-convert ...>"
    (String.concat "|" HazelExperiment.all_modes)

let () =
  match Array.to_list Sys.argv with
  | _ :: "hazel-convert" :: _ as argv -> (
      try HazelBackConvert.run_cli argv
      with Invalid_argument msg ->
        prerr_endline msg;
        exit 1)
  | [ _; "hazel-compare"; mode ] ->
      if not (HazelExperiment.run_compare_mode mode) then (
        prerr_endline
          (Printf.sprintf "Usage: GeneratedMain hazel-compare <%s>" (String.concat "|" HazelExperiment.all_modes));
        exit 1)
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
