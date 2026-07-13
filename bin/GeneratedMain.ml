let usage =
  Printf.sprintf "Usage: GeneratedMain <%s|arith|entropy-scaling <size>|hazel-convert ...>"
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
  | [ _; "arith-scaling"; size; output ] -> (
      match int_of_string_opt size with
      | Some size when size > 0 -> RunArith.run ~term_size:size ~sample_count:20 ~steps_file:output ()
      | _ ->
          prerr_endline "Usage: GeneratedMain arith-scaling <positive-size> <output>";
          exit 1)
  | [ _; "hazel-scaling"; mode; size; output ] -> (
      match int_of_string_opt size with
      | Some size when size > 0 ->
          if not (HazelExperiment.run_scaling_mode ~mode ~input_size:size ~steps_file:output) then (
            prerr_endline
              (Printf.sprintf "Usage: GeneratedMain hazel-scaling <%s> <positive-size> <output>"
                 (String.concat "|" HazelExperiment.all_modes));
            exit 1)
      | _ ->
          prerr_endline "hazel-scaling size must be a positive integer";
          exit 1)
  | [ _; "entropy-scaling"; size ] -> (
      match int_of_string_opt size with
      | Some size when size > 0 -> RunTestAsymptotics.run ~length:size ()
      | _ ->
          prerr_endline "Usage: GeneratedMain entropy-scaling <positive-size>";
          exit 1)
  | [ _; mode ] -> (
      match String.lowercase_ascii mode with
      | "arith" -> RunArith.run ()
      | _ ->
          if not (HazelExperiment.run_mode mode) then (
            prerr_endline usage;
            exit 1))
  | _ ->
      prerr_endline usage;
      exit 1
