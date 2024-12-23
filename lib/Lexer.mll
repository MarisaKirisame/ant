{
open Lexing
open Tokens

type error =
  | UnexpectedToken of string
  | UnexpectedEof of string 
  | InvalidStringLiteral

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
    ]

let keywords_table = Hashtbl.of_seq @@ List.to_seq
    [ ("if"         , TK_IF)
    ; ("then"       , TK_THEN)
    ; ("else"       , TK_ELSE)
    ; ("end"        , TK_END)
    ; ("match"      , TK_MATCH)
    ; ("case"       , TK_CASE)
    ; ("of"         , TK_OF)
    ; ("let"        , TK_LET)
    ; ("in"         , TK_IN)
    ; ("type"       , TK_TYPE)
    ; ("fun"        , TK_FUN)
    ]

let sops_table = Hashtbl.of_seq @@ List.to_seq
    [ ('.'  , TK_DOT)
    ; (','  , TK_COMMA)
    ; (':'  , TK_COLON)
    ; (';'  , TK_SEMICOLON)
    ; ('^'  , TK_CARET)
    ; ('_'  , TK_UNDERSCORE)
    ; ('#'  , TK_HASH)
    ; ('('  , TK_L_PAREN)
    ; (')'  , TK_R_PAREN)
    ; ('{'  , TK_L_BRACE)
    ; ('}'  , TK_R_BRACE)
    ; ('['  , TK_L_BRACKET)
    ; (']'  , TK_R_BRACKET)
    ; ('+'  , TK_ADD)
    ; ('-'  , TK_SUB)
    ; ('*'  , TK_MUL)
    ; ('/'  , TK_DIV)
    ; ('%'  , TK_MOD)
    ; ('='  , TK_ASGN)
    ; ('<'  , TK_LT)
    ; ('>'  , TK_GT)
    ; ('!'  , TK_LNOT)
    ; ('|'  , TK_OR)
    ; ('\'' , TK_SQUOTE)
    ; ('~'  , TK_TILDE)
    ; ('\\' , TK_BACKSLASH)
    ]

let mops_table = Hashtbl.of_seq @@ List.to_seq
    [ ("<-"   , TK_L_ARROW)
    ; ("->"   , TK_R_ARROW)
    ; ("=>"   , TK_D_ARROW)
    ; ("~>"   , TK_C_ARROW)
    ; ("=="   , TK_EQ)
    ; ("!="   , TK_NEQ)
    ; (">="   , TK_GE)
    ; ("<="   , TK_LE)
    ; ("&&"   , TK_LAND)
    ; ("||"   , TK_LOR)
    ; (";;"   , TK_SEMISEMI)
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
let sops          = ['.' ',' ':' ';' '^' '_' '#' '(' ')' '{' '}' '[' ']' '+' '-' '*' '/' '%' '=' '>' '<' '!' '|' '\'' '~' '\\']
let mops          = "<-" | "->" | "=>" | "==" | "!=" | ">=" | "<=" | "&&" | "||" | ";;"

rule tokenize = parse
  | eof             { TK_EOF }
  | whitespace      { tokenize lexbuf }
  | newline         { new_line lexbuf; tokenize lexbuf }
  | "//"            { comment lexbuf }
  | int_lit   as x  { TK_INT_LITERAL (int_of_string x) }
  | bool_lit  as x  { TK_BOOL_LITERAL (bool_of_string x) }
  | ctor      as x  {
                      TK_CTOR x
                    }
  | identifier as x  { 
                      match Hashtbl.find_opt keywords_table x with
                      | Some token -> token
                      | None       -> (
                        match Hashtbl.find_opt builtin_table x with
                        | Some _ -> TK_BUILTIN x
                        | None   -> TK_ID x
                      )
                    }
  | mops as op      { Option.get @@ Hashtbl.find_opt mops_table op}
  | sops as op      { Option.get @@ Hashtbl.find_opt sops_table op }
  | _               { raise @@ Error (UnexpectedToken "unexpected token", lexeme_start_p lexbuf) }

and comment = parse
  | newline     { new_line lexbuf; tokenize lexbuf }
  | _           { comment lexbuf }

and string buffer = parse
  | '"'                 { () }
  | '\\' (escaped as c)  { Buffer.add_char buffer (convert_escaped c) ; string buffer lexbuf }
  | '\\' _ | newline     { raise @@ Error (InvalidStringLiteral, lexeme_start_p lexbuf) }
  | _ as c              { Buffer.add_char buffer c; string buffer lexbuf }
  | eof                 { raise @@ Error (UnexpectedEof "string literal doesn't terminate", lexeme_start_p lexbuf)}