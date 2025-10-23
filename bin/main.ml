open Cmdliner
open Ant

let read_all file = In_channel.with_open_text file In_channel.input_all

let parse content =
  let lexbuf = Lexing.from_string content in
  try Parser.prog Lexer.tokenize lexbuf with
  | Parser.Error i ->
      let cur = lexbuf.Lexing.lex_curr_p in
      let line = cur.Lexing.pos_lnum in
      let cnum = cur.Lexing.pos_cnum - cur.Lexing.pos_bol + 1 in
      let tok = Lexing.lexeme lexbuf in
      Printf.eprintf "Syntax error at line %d, character %d, after token `%s`\n" line cnum tok;
      Printf.eprintf "Error state: %d\n" i;
      failwith "Failed due to syntax error"
  | Lexer.Error (e, p) ->
      let line = p.Lexing.pos_lnum in
      let cnum = p.Lexing.pos_cnum - p.Lexing.pos_bol + 1 in
      Printf.eprintf "Lexing error at line %d, character %d: %s\n" line cnum @@ Lexer.string_of_error e;
      failwith "Failed due to lexing error"

let driver input output print_ast compile_pat print_ant print_cek_ant tyck print_cps_transformed print_de print_cps_de =
  let src = read_all input in
  let ast = Resolve.resolve (parse src) in
  let debug_pp = PPrint.ToChannel.pretty 0.8 80 stdout in
  let debug = false in
  let output_pp = PPrint.ToChannel.pretty 0.8 80 (Out_channel.open_text output) in
  let _ =
    if debug then debug_pp (Syntax.pp_prog ast);
    if print_ast then output_pp (Syntax.pp_prog ast);
    if compile_pat then output_pp (Pat.show_all_pattern_matrixes ast);
    if print_ant then output_pp (Syntax.pp_ant ast);
    if print_cek_ant then output_pp (GenerateMemo.pp_cek_ant ast);
    if tyck then output_pp (Tyck.pp_inferred (Tyck.infer_prog ast));
    if print_cps_transformed then output_pp (Syntax.pp_prog (Transform.cps_prog ast))
    else if print_de then output_pp (Syntax.pp_prog (Transform.defunc_prog ast))
    else if print_cps_de then output_pp (Syntax.pp_prog (Transform.defunc_prog (Transform.cps_prog ast)))
  in
  ()

let input =
  let doc = "The name of the input file" in
  let docv = "INPUT" in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv)

let output =
  let doc = "The name of the output file" in
  let docv = "OUTPUT" in
  Arg.(required & pos 1 (some string) None & info [] ~doc ~docv)

let print_ast =
  let doc = "Print the AST" in
  Arg.(value & flag & info [ "p"; "print-ast" ] ~doc)

let print_ant =
  let doc = "Print the AST in ant" in
  Arg.(value & flag & info [ "a"; "print-ant" ] ~doc)

let compile_pat =
  let doc = "Compile the pattern" in
  Arg.(value & flag & info [ "pat"; "compile-pat" ] ~doc)

let print_cek_ant =
  let doc = "Print the AST in ant (CEK)" in
  Arg.(value & flag & info [ "cek"; "print-cek-ant" ] ~doc)

let tyck =
  let doc = "Typechecking" in
  Arg.(value & flag & info [ "t"; "tyck" ] ~doc)

let print_cps_transformed =
  let doc = "Print the AST after CPS transformation" in
  Arg.(value & flag & info [ "c"; "print-cps" ] ~doc)

let print_de =
  let doc = "Print the AST after defunctionalization" in
  Arg.(value & flag & info [ "d"; "print-defunc" ] ~doc)

let print_cps_de =
  let doc = "Print the AST after CPS transformation and defunctionalization" in
  Arg.(value & flag & info [ "D"; "print-cps-defunc" ] ~doc)

let cmd =
  let doc = "Ant Compiler" in
  let man = [ `S Manpage.s_bugs ] in
  let info = Cmd.info "ant" ~version:"0.1" ~doc ~man in
  Cmd.v info
    Term.(
      const driver $ input $ output $ print_ast $ compile_pat $ print_ant $ print_cek_ant $ tyck $ print_cps_transformed
      $ print_de $ print_cps_de)

let i = Cmd.eval cmd

let () =
  print_endline "";
  exit i
