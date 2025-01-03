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
      Printf.eprintf "Syntax error at line %d, character %d, after token `%s`\n"
        line cnum tok;
      Printf.eprintf "Error state: %d\n" i;
      failwith "Failed due to syntax error"
  | Lexer.Error (e, p) ->
      let line = p.Lexing.pos_lnum in
      let cnum = p.Lexing.pos_cnum - p.Lexing.pos_bol + 1 in
      Printf.eprintf "Lexing error at line %d, character %d: %s\n" line cnum
      @@ Lexer.string_of_error e;
      failwith "Failed due to lexing error"

let driver input print_ast print_ant print_cps_transformed =
  let src = read_all input in
  let ast = parse src in
  let _ =
    if print_ast then PPrint.ToChannel.pretty 0.8 80 stdout (Syntax.pp_prog ast);
    if print_ant then PPrint.ToChannel.pretty 0.8 80 stdout (Syntax.pp_ant ast);
    if print_cps_transformed then
      PPrint.ToChannel.pretty 0.8 80 stdout (Syntax.pp_prog (Transform.cps_prog ast))
  in
  ()

let input =
  let doc = "The name of the input file" in
  let docv = "INPUT" in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv)

let print_ast =
  let doc = "Print the AST" in
  Arg.(value & flag & info [ "p"; "print-ast" ] ~doc)

let print_ant =
  let doc = "Print the AST in ant" in
  Arg.(value & flag & info [ "a"; "print-ant" ] ~doc)

let print_cps_transformed =
  let doc = "Print the AST after CPS transformation" in
  Arg.(value & flag & info [ "c"; "print-cps" ] ~doc)

let cmd =
  let doc = "ant Compiler" in
  let man = [ `S Manpage.s_bugs ] in
  let info = Cmd.info "ant" ~version:"0.1" ~doc ~man in
  Cmd.v info
    Term.(const driver $ input $ print_ast $ print_ant $ print_cps_transformed)

let i = Cmd.eval cmd

let () =
  print_endline "";
  exit i
