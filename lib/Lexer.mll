{
open Lexing
open Tokens

type error =
  | UnexpectedToken of string
  | UnexpectedEof of string 
  | InvalidStringLiteral

let string_of_error = function
  | UnexpectedToken s -> "Unexpected token: " ^ s
  | UnexpectedEof s -> "Unexpected end of file: " ^ s
  | InvalidStringLiteral -> "Invalid string literal"

exception Error of error * position

let convert_escaped c =
  match c with
  | 'n' -> '\n'
  | 't' -> '\t'
  | 'r' -> '\r'
  | 'b' -> '\b'
  | 'f' -> '\012'
  | _   -> c

let builtin_table = Hashtbl.of_seq @@ List.to_seq
    [ ("print_endline"      , ())
    ; ("print_string"       , ())
    ; ("failwith"       , ())
    ; ("failwith_int"       , ())
    ]

let keywords_table = Hashtbl.of_seq @@ List.to_seq
    [ ("if"         , TK_IF)
    ; ("then"       , TK_THEN)
    ; ("else"       , TK_ELSE)
    ; ("match"      , TK_MATCH)
    ; ("of"         , TK_OF)
    ; ("with"       , TK_WITH)
    ; ("let"        , TK_LET)
    ; ("in"         , TK_IN)
    ; ("type"       , TK_TYPE)
    ; ("fun"        , TK_FUN)
    ; ("rec"        , TK_REC)
    ; ("and"        , TK_AND)
    ]

let sops_table = Hashtbl.of_seq @@ List.to_seq
    [ ('.'  , TK_DOT)
    ; (','  , TK_COMMA)
    ; (';'  , TK_SEMICOLON)
    ; ('_'  , TK_UNDERSCORE)
    ; ('('  , TK_L_PAREN)
    ; (')'  , TK_R_PAREN)
    ; ('['  , TK_L_BRACKET)
    ; (']'  , TK_R_BRACKET)
    ; ('<'  , TK_LT)
    ; ('>'  , TK_GT)
    ; ('+'  , TK_ADD)
    ; ('-'  , TK_SUB)
    ; ('*'  , TK_MUL)
    ; ('/'  , TK_DIV)
    ; ('='  , TK_ASGN)
    ; ('|'  , TK_OR)
    ; ('\'' , TK_SQUOTE)
    ]

let mops_table = Hashtbl.of_seq @@ List.to_seq
    [ ("->"   , TK_R_ARROW)
    ; ("<="   , TK_LE)
    ; (">="   , TK_GE)
    ; (";;"   , TK_SEMISEMI)
    ; ("&&"   , TK_LAND)
    ; ("||"   , TK_LOR)
    ]
}

let newline       = "\r\n" | "\n" | "\r"
let whitespace    = ['\t' ' ']+
let ddigit        = ['0'-'9']
let hdigit        = ['0'-'9' 'a'-'f' 'A'-'F']
let alpha         = ['a'-'z' 'A'-'Z']
let smallcaps     = ['a'-'z']
let capital       = ['A'-'Z']
let alphanum      = alpha | ddigit
let identifier    = (smallcaps | '_') (alphanum | '_') *
let ctor          = capital (alphanum | '_') *
let int_lit       = ddigit+
let bool_lit      = "true" | "false"
let escaped       = ['\\' '\'' '"' 'n' 't' 'r' 'f' 'b']
let sops          = ['.' ',' ';' '_' '(' ')' '[' ']' '<' '>' '+' '-' '*' '/' '=' '|' '\'']
let mops          = "->" | "<=" | ">=" | ";;" | "&&" | "||"

rule tokenize = parse
  | eof             { TK_EOF }
  | whitespace      { tokenize lexbuf }
  | newline         { new_line lexbuf; tokenize lexbuf }
  | "(*"            { comment 0 lexbuf }
  | "\""            { string (Buffer.create 16) lexbuf }
  | int_lit   as x  { TK_INT_LITERAL (int_of_string x) }
  | bool_lit  as x  { TK_BOOL_LITERAL (bool_of_string x) }
  | '`' (ctor as x) { TK_RAW_CTOR x }
  | ctor      as x  { TK_CTOR x }
  | identifier as x { 
                      match Hashtbl.find_opt keywords_table x with
                      | Some token -> token
                      | None       -> (
                        match Hashtbl.find_opt builtin_table x with
                        | Some _ -> TK_BUILTIN x
                        | None   -> TK_ID x
                      )
                    }
  | mops as op      { Option.get @@ Hashtbl.find_opt mops_table op }
  | sops as op      { Option.get @@ Hashtbl.find_opt sops_table op }
  | _ as ch         { raise @@ Error (UnexpectedToken (String.make 1 ch), lexeme_start_p lexbuf) }

and comment level = parse
  | newline     { new_line lexbuf; comment level lexbuf }
  | "(*"        { comment (level + 1) lexbuf }
  | "*)"        { 
                  if level = 0 then tokenize lexbuf
                  else comment (level - 1) lexbuf
                }
  | eof         { raise @@ Error (UnexpectedEof "comment doesn't terminate", lexeme_start_p lexbuf) }
  | _           { comment level lexbuf }

and string buffer = parse
  | '"'                 { TK_STRING_LITERAL (Buffer.contents buffer) }
  | '\\' (escaped as c) { Buffer.add_char buffer (convert_escaped c) ; string buffer lexbuf }
  | '\\' _ | newline    { raise @@ Error (InvalidStringLiteral, lexeme_start_p lexbuf) }
  | _ as c              { Buffer.add_char buffer c; string buffer lexbuf }
  | eof                 { raise @@ Error (UnexpectedEof "string literal doesn't terminate", lexeme_start_p lexbuf)}
