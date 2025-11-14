{
open Lexing
open LiveParser

exception Error of string * position

let keyword_of ident =
  match String.lowercase_ascii ident with
  | "let" -> Some LET
  | "in" -> Some IN
  | "fun" -> Some FUN
  | "if" -> Some IF
  | "then" -> Some THEN
  | "else" -> Some ELSE
  | "_" -> Some HOLE
  | "match" -> Some MATCH
  | "with" -> Some WITH
  | "fix" -> Some FIX
  | "true" -> Some TRUE
  | "false" -> Some FALSE
  | _ -> None
}

let whitespace = [' ' '\t' '\r']+
let newline = "\r\n" | "\n" | "\r"
let digit = ['0'-'9']
let ident_start = ['a'-'z' 'A'-'Z' '_']
let ident_char = ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']
let ident = ident_start ident_char*
let int = digit+

rule token =
  parse
  | eof { EOF }
  | whitespace { token lexbuf }
  | newline { new_line lexbuf; token lexbuf }
  | "(*" { comment 0 lexbuf; token lexbuf }
  | "->" { ARROW }
  | "::" { CONS }
  | "+" { PLUS }
  | "=" { EQUAL }
  | "." { DOT }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "|" { BAR }
  | '-' int as lit { INT (int_of_string lit) }
  | int as lit { INT (int_of_string lit) }
  | ident as id {
      match keyword_of id with
      | Some kw -> kw
      | None -> IDENT id }
  | _ as ch { raise (Error (Printf.sprintf "Unexpected character %c" ch, lexeme_start_p lexbuf)) }

and comment depth =
  parse
  | "(*" { comment (depth + 1) lexbuf }
  | "*)" {
      if depth = 0 then () else comment (depth - 1) lexbuf
    }
  | newline { new_line lexbuf; comment depth lexbuf }
  | eof { raise (Error ("Unterminated comment", lexeme_start_p lexbuf)) }
  | _ { comment depth lexbuf }
