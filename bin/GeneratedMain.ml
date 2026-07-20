let usage =
  Printf.sprintf "Usage: GeneratedMain <%s|arith|entropy-scaling <size>|hazel-convert ...>"
    (String.concat "|" HazelExperiment.all_modes)

let positive_int s = match int_of_string_opt s with Some n when n > 0 -> Some n | _ -> None
let nonnegative_int s = match int_of_string_opt s with Some n when n >= 0 -> Some n | _ -> None

let chop_suffix s ~suffix =
  let suffix_len = String.length suffix in
  let len = String.length s in
  if len >= suffix_len && String.equal (String.sub s (len - suffix_len) suffix_len) suffix then
    Some (String.sub s 0 (len - suffix_len))
  else None

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
          (Printf.sprintf "Usage: GeneratedMain hazel-compare <%s> [output]"
             (String.concat "|" HazelExperiment.all_modes));
        exit 1)
  | [ _; "hazel-compare"; mode; output ] ->
      if not (HazelExperiment.run_compare_mode ~steps_file:output mode) then (
        prerr_endline
          (Printf.sprintf "Usage: GeneratedMain hazel-compare <%s> <output>"
             (String.concat "|" HazelExperiment.all_modes));
        exit 1)
  | [ _; "hazel-compare"; mode; input_size; output ] -> (
      match positive_int input_size with
      | Some input_size ->
          if not (HazelExperiment.run_compare_mode ~input_size ~steps_file:output mode) then (
            prerr_endline
              (Printf.sprintf "Usage: GeneratedMain hazel-compare <%s> <positive-input-size> <output>"
                 (String.concat "|" HazelExperiment.all_modes));
            exit 1)
      | _ ->
          prerr_endline "Usage: GeneratedMain hazel-compare <mode> <positive-input-size> <output>";
          exit 1)
  | [ _; "hazel-no-evict"; mode; output ] ->
      if not (HazelExperiment.run_mode ~evict:false ~baseline:false ~steps_file:output mode) then (
        prerr_endline
          (Printf.sprintf "Usage: GeneratedMain hazel-no-evict <%s> <output>"
             (String.concat "|" HazelExperiment.all_modes));
        exit 1)
  | [ _; "hazel-no-evict"; mode; input_size; max_candidates; output ] -> (
      match (positive_int input_size, nonnegative_int max_candidates) with
      | Some input_size, Some max_candidates ->
          if
            not
              (HazelExperiment.run_mode ~evict:false ~baseline:false ~input_size ~max_candidates ~steps_file:output mode)
          then (
            prerr_endline
              (Printf.sprintf
                 "Usage: GeneratedMain hazel-no-evict <%s> <positive-input-size> <nonnegative-max-candidates> <output>"
                 (String.concat "|" HazelExperiment.all_modes));
            exit 1)
      | _ ->
          prerr_endline
            "Usage: GeneratedMain hazel-no-evict <mode> <positive-input-size> <nonnegative-max-candidates> <output>";
          exit 1)
  | [ _; "hazel-compare"; mode; input_size; max_candidates; timeout_seconds; output ] -> (
      match (positive_int input_size, nonnegative_int max_candidates, positive_int timeout_seconds) with
      | Some input_size, Some max_candidates, Some timeout_seconds ->
          if
            not
              (HazelExperiment.run_compare_mode ~input_size ~max_candidates ~hazel_compare_max_candidates:max_candidates
                 ~hazel_compare_timeout_seconds:timeout_seconds ~steps_file:output mode)
          then (
            prerr_endline
              (Printf.sprintf
                 "Usage: GeneratedMain hazel-compare <%s> <positive-input-size> <nonnegative-max-candidates> \
                  <positive-timeout-seconds> <output>"
                 (String.concat "|" HazelExperiment.all_modes));
            exit 1)
      | _ ->
          prerr_endline
            "Usage: GeneratedMain hazel-compare <mode> <positive-input-size> <nonnegative-max-candidates> \
             <positive-timeout-seconds> <output>";
          exit 1)
  | [ _; "arith"; term_size; sample_count; output ] -> (
      match (positive_int term_size, positive_int sample_count) with
      | Some term_size, Some sample_count -> RunArith.run ~term_size ~sample_count ~steps_file:output ()
      | _ ->
          prerr_endline "Usage: GeneratedMain arith <positive-term-size> <positive-sample-count> <output>";
          exit 1)
  | [ _; "arith-scaling"; size; output ] -> (
      match positive_int size with
      | Some size when size > 0 ->
          RunArith.run ~term_size:size ~sample_count:RunArith.default_sample_count ~steps_file:output ()
      | _ ->
          prerr_endline "Usage: GeneratedMain arith-scaling <positive-size> <output>";
          exit 1)
  | [ _; "arith-scaling"; size; sample_count; output ] -> (
      match (positive_int size, positive_int sample_count) with
      | Some size, Some sample_count -> RunArith.run ~term_size:size ~sample_count ~steps_file:output ()
      | _ ->
          prerr_endline "Usage: GeneratedMain arith-scaling <positive-size> <positive-sample-count> <output>";
          exit 1)
  | [ _; "hazel-scaling"; mode; size; output ] -> (
      match positive_int size with
      | Some size when size > 0 ->
          if not (HazelExperiment.run_scaling_mode ~mode ~input_size:size ~steps_file:output ()) then (
            prerr_endline
              (Printf.sprintf "Usage: GeneratedMain hazel-scaling <%s> <positive-size> <output>"
                 (String.concat "|" HazelExperiment.all_modes));
            exit 1)
      | _ ->
          prerr_endline "hazel-scaling size must be a positive integer";
          exit 1)
  | [ _; "hazel-scaling-no-evict"; mode; size; output ] -> (
      match positive_int size with
      | Some size when size > 0 ->
          if
            not
              (HazelExperiment.run_scaling_mode ~evict:false ~baseline:false ~mode ~input_size:size ~steps_file:output
                 ())
          then (
            prerr_endline
              (Printf.sprintf "Usage: GeneratedMain hazel-scaling-no-evict <%s> <positive-size> <output>"
                 (String.concat "|" HazelExperiment.all_modes));
            exit 1)
      | _ ->
          prerr_endline "hazel-scaling-no-evict size must be a positive integer";
          exit 1)
  | [ _; "hazel-scaling"; mode; size; max_candidates; output ] -> (
      match (positive_int size, nonnegative_int max_candidates) with
      | Some size, Some max_candidates ->
          if not (HazelExperiment.run_scaling_mode ~mode ~input_size:size ~max_candidates ~steps_file:output ()) then (
            prerr_endline
              (Printf.sprintf
                 "Usage: GeneratedMain hazel-scaling <%s> <positive-size> <nonnegative-max-candidates> <output>"
                 (String.concat "|" HazelExperiment.all_modes));
            exit 1)
      | _ ->
          prerr_endline "hazel-scaling size must be positive and max-candidates must be nonnegative";
          exit 1)
  | [ _; "hazel-scaling-no-evict"; mode; size; max_candidates; output ] -> (
      match (positive_int size, nonnegative_int max_candidates) with
      | Some size, Some max_candidates ->
          if
            not
              (HazelExperiment.run_scaling_mode ~evict:false ~baseline:false ~mode ~input_size:size ~max_candidates
                 ~steps_file:output ())
          then (
            prerr_endline
              (Printf.sprintf
                 "Usage: GeneratedMain hazel-scaling-no-evict <%s> <positive-size> <nonnegative-max-candidates> \
                  <output>"
                 (String.concat "|" HazelExperiment.all_modes));
            exit 1)
      | _ ->
          prerr_endline "hazel-scaling-no-evict size must be positive and max-candidates must be nonnegative";
          exit 1)
  | [ _; "entropy-scaling"; size ] -> (
      match positive_int size with
      | Some size when size > 0 -> RunTestAsymptotics.run ~length:size ()
      | _ ->
          prerr_endline "Usage: GeneratedMain entropy-scaling <positive-size>";
          exit 1)
  | [ _; mode; input_size; max_candidates ] -> (
      match (positive_int input_size, nonnegative_int max_candidates) with
      | Some input_size, Some max_candidates ->
          let evict, mode =
            match chop_suffix ~suffix:"-no-evict" mode with None -> (true, mode) | Some mode -> (false, mode)
          in
          if not (HazelExperiment.run_mode ~evict ~baseline:evict ~input_size ~max_candidates mode) then (
            prerr_endline usage;
            exit 1)
      | _ ->
          prerr_endline "Usage: GeneratedMain <hazel-mode> <positive-input-size> <nonnegative-max-candidates>";
          exit 1)
  | [ _; mode ] -> (
      match String.lowercase_ascii mode with
      | "arith" -> RunArith.run ()
      | _ ->
          let evict, mode =
            match chop_suffix ~suffix:"-no-evict" mode with None -> (true, mode) | Some mode -> (false, mode)
          in
          if not (HazelExperiment.run_mode ~evict ~baseline:evict mode) then (
            prerr_endline usage;
            exit 1))
  | _ ->
      prerr_endline usage;
      exit 1
