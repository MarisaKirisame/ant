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
      Printf.eprintf "Syntax error at line %d, character %d, at token `%s`\n" line cnum tok;
      Printf.eprintf "Error state: %d\n" i;
      failwith "Failed due to syntax error"
  | Lexer.Error (e, p) ->
      let line = p.Lexing.pos_lnum in
      let cnum = p.Lexing.pos_cnum - p.Lexing.pos_bol + 1 in
      Printf.eprintf "Lexing error at line %d, character %d: %s\n" line cnum @@ Lexer.string_of_error e;
      failwith "Failed due to lexing error"

let driver input output print_ast compile_pat compile_ant (module Backend : Compile.Backend) typing print_level
    print_cps_transformed print_de print_cps_de =
  let src = read_all input in
  let syn = parse src in
  let ast = Typing.top_type_of_prog syn in
  let debug_pp = PPrint.ToChannel.pretty 0.8 80 stdout in
  let debug = false in
  let output_pp = PPrint.ToChannel.pretty 0.8 80 (Out_channel.open_text output) in
  let _ =
    if debug then debug_pp (Syntax.pp_prog ast);
    if print_ast then output_pp (Syntax.pp_prog ast);
    if compile_pat then output_pp (Pat.show_all_pattern_matrixes ast);
    if compile_ant then output_pp (Backend.compile ast);
    if typing then output_pp (Typing.pp_top_type_of_prog ~print_level ast);
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

let backend =
  let doc = "Backend used to compile the ant source, defaulting to memo" in
  let cdds =
    [
      ("memo", (module CompileMemo.Backend : Compile.Backend));
      ("seq", (module CompileSeq.Backend : Compile.Backend));
      ("plain", (module CompilePlain.Backend));
    ]
  in
  Arg.(value & opt (enum cdds) (module CompileMemo.Backend : Compile.Backend) & info [ "b"; "backend" ] ~doc)

let compile_ant =
  let doc = "Compile the ant source" in
  Arg.(value & flag & info [ "compile" ] ~doc)

let compile_pat =
  let doc = "Compile the pattern" in
  Arg.(value & flag & info [ "pat"; "compile-pat" ] ~doc)

let typing =
  let doc = "Typing" in
  Arg.(value & flag & info [ "t"; "typing" ] ~doc)

let print_level =
  let doc = "Print the type with level information" in
  Arg.(value & flag & info [ "L"; "print-level" ] ~doc)

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
      const driver $ input $ output $ print_ast $ compile_pat $ compile_ant $ backend $ typing $ print_level
      $ print_cps_transformed $ print_de $ print_cps_de)

let i = Cmd.eval cmd

let () =
  print_endline "";
  exit i
