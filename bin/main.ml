open Cmdliner
open Ant

let read_all file = In_channel.with_open_text file In_channel.input_all

let parse content =
  let lexbuf = Lexing.from_string content in
  try 
  Parser.prog Lexer.tokenize lexbuf
  with
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

let driver input print_ast =
  let src = read_all input in
  let ast = parse src in
  let _ = if print_ast then PPrint.ToChannel.pretty 0.8 120 stdout (Syntax.pp_prog ast) in
  ()

let input =
  let doc = "The name of the input file" in
  let docv = "INPUT" in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv)

let print_ast =
  let doc = "Print the AST" in
  Arg.(value & flag & info ["p"; "print-ast"] ~doc)

let cmd =
  let doc = "ant Compiler" in
  let man = [ `S Manpage.s_bugs ] in
  let info = Cmd.info "ant" ~version:"0.1" ~doc ~man in
  Cmd.v info Term.(const driver $ input $ print_ast)

let () = exit (Cmd.eval cmd)
=======
module Fg = BatFingerTree

let () = Printf.printf "\n"

let time f s =
  let t = Sys.time () in
  let fx = f () in
  Printf.printf "%s Time: %fms\n" s (1000. *. (Sys.time () -. t));
  fx

let times = 1000 * 1000
let rec build_seq n aux = if n == 0 then aux else build_seq (n - 1) (Fg.cons aux 1)
let rec build_list n aux = if n == 0 then aux else build_list (n - 1) (0 :: aux)
let () = time (fun _ -> ignore (build_seq times Fg.empty)) "seq  cons"
let () = time (fun _ -> ignore (build_list times [])) "list cons"
let seq = build_seq times Fg.empty
let list = build_list times []

open Core
let bb = Hashtbl.create (module Int)
let () =
  time
    (fun _ ->
      let rec insert n =
        if n != 0 then (Hashtbl.add_exn bb n n;
        insert (n - 1))
      in
      insert times)
    ""

let () = time (fun _ -> ignore (Fg.fold_left ( + ) 0 seq)) "seq  fold"
let () = time (fun _ -> ignore (List.length list)) "list fold"
let () = print_endline "Hello, World!"
