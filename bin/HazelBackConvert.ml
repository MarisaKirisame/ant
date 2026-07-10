let convert_programs ~program_path ~test_expr ~candidate_index : string list =
  let programs = FromHazel.read_program_strings ~program_path in
  let test = Option.map RunLiveCommon.parse_nexpr test_expr in
  let convert_one i program =
    let sexp = FromHazel.parse ~candidate_index:i ~program_path program in
    let _, impl = FromHazel.extract_program_with_meta sexp in
    let expr = FromHazel.expr_of_sexp impl in
    let nexpr =
      let base = FromHazel.nexpr_of_expr expr in
      match test with None -> base | Some t -> base |> FromHazel.subst_deepest_hole t |> FromHazel.clean
    in
    ToHazel.source_of_nexpr nexpr
  in
  match candidate_index with
  | None -> List.mapi convert_one programs
  | Some i ->
      if i < 0 || i >= List.length programs then
        invalid_arg
          (Printf.sprintf "candidate index %d out of bounds for %s (len=%d)" i program_path (List.length programs));
      [ convert_one i (List.nth programs i) ]

let write_output ~output_path ~(programs : string list) ~(as_json : bool) : unit =
  let oc = open_out output_path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () ->
      if as_json then (
        `List (List.map (fun p -> `String p) programs) |> Yojson.Safe.to_channel oc;
        output_char oc '\n')
      else
        List.iter
          (fun p ->
            output_string oc p;
            output_char oc '\n')
          programs)

let run ~program_path ~output_path ~test_expr ~candidate_index ~as_json =
  let programs = convert_programs ~program_path ~test_expr ~candidate_index in
  write_output ~output_path ~programs ~as_json;
  Printf.printf "Wrote %d converted program(s) to %s\n" (List.length programs) output_path

let usage =
  String.concat "\n"
    [
      "Usage:";
      "  GeneratedMain hazel-convert <program_path> <output_path> [test_expr] [candidate_index] [json|text]";
      "";
      "Examples:";
      "  GeneratedMain hazel-convert data/mk_Append.json out.json";
      "  GeneratedMain hazel-convert data/mk_Append.json out.hz \"my_append (1 :: []) (2 :: [])\"";
      "  GeneratedMain hazel-convert data/mk_Append.json out.hz \"my_append (1 :: []) (2 :: [])\" 0 text";
    ]

let parse_bool_mode = function
  | None -> true
  | Some s ->
      let normalized = String.lowercase_ascii s in
      if normalized = "json" then true
      else if normalized = "text" then false
      else invalid_arg "last argument must be either json or text"

let run_cli argv =
  match argv with
  | [ _exe; _mode; program_path; output_path ] ->
      run ~program_path ~output_path ~test_expr:None ~candidate_index:None ~as_json:true
  | [ _exe; _mode; program_path; output_path; test_expr ] ->
      run ~program_path ~output_path ~test_expr:(Some test_expr) ~candidate_index:None ~as_json:true
  | [ _exe; _mode; program_path; output_path; test_expr; candidate_index ] ->
      let idx = int_of_string candidate_index in
      run ~program_path ~output_path ~test_expr:(Some test_expr) ~candidate_index:(Some idx) ~as_json:true
  | [ _exe; _mode; program_path; output_path; test_expr; candidate_index; format ] ->
      let idx = int_of_string candidate_index in
      run ~program_path ~output_path ~test_expr:(Some test_expr) ~candidate_index:(Some idx)
        ~as_json:(parse_bool_mode (Some format))
  | _ -> invalid_arg usage
