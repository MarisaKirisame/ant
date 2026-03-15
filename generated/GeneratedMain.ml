let usage = Printf.sprintf "Usage: GeneratedMain <%s|arith>" (String.concat "|" HazelExperiment.all_modes)

let dump_live_cek_source () =
  let source_path = "generated/LiveCEK.ml" in
  let output_path = "a.txt" in
  In_channel.with_open_text source_path (fun ic ->
      Out_channel.with_open_text output_path (fun oc -> output_string oc (In_channel.input_all ic)))

let () =
  dump_live_cek_source ();
  match Array.to_list Sys.argv with
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
