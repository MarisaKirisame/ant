(* Dispatches to either RunLive.exe or RunTest.exe based on the CLI argument. *)

let usage = "Usage: GeneratedMain <live|test> [args...]\n- live: run RunLive executable\n- test: run RunTest executable"

let candidate_paths target =
  let base_names =
    let lower = String.lowercase_ascii target in
    if Sys.win32 then [ target ^ ".exe"; lower ^ ".exe"; target; lower ]
    else [ target ^ ".exe"; target; lower; lower ^ ".exe" ]
  in
  let exe_dir = Filename.dirname Sys.executable_name in
  let cwd = Sys.getcwd () in
  let build_dir = Filename.concat cwd "_build" in
  let default_dir = Filename.concat build_dir "default/generated" in
  let install_dir = Filename.concat build_dir "install/default/bin" in
  let dirs = [ exe_dir; cwd; default_dir; install_dir ] in
  List.concat_map (fun dir -> List.map (Filename.concat dir) base_names) dirs

let find_executable target = candidate_paths target |> List.find_opt Sys.file_exists

let exec_child target extra_args =
  match find_executable target with
  | Some exe_path ->
      let argv = Array.of_list (exe_path :: extra_args) in
      Unix.execv exe_path argv
  | None ->
      prerr_endline ("Unable to locate executable for target: " ^ target);
      exit 1

let () =
  match Array.to_list Sys.argv with
  | _ :: mode :: rest -> (
      let target =
        match String.lowercase_ascii mode with "live" -> Some "RunLive" | "test" -> Some "RunTest" | _ -> None
      in
      match target with
      | Some target -> (
          try exec_child target rest
          with Unix.Unix_error (err, _, _) ->
            prerr_endline (Printf.sprintf "Failed to execute %s: %s" target (Unix.error_message err));
            exit 1)
      | None ->
          prerr_endline usage;
          exit 1)
  | _ ->
      prerr_endline usage;
      exit 1
