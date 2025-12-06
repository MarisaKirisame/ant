module LC = LispCEK

exception Parse_error of string

type token = LParen | RParen | Quote | Number of int | Symbol of string
type sexpr = SNumber of int | SSymbol of string | SList of sexpr list

let is_symbol_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '?' | '*' | '+' | '/' | '<' | '>' | '=' -> true
  | _ -> false

let tokenize input =
  let len = String.length input in
  let rec aux i acc =
    if i >= len then List.rev acc
    else
      match input.[i] with
      | ' ' | '\n' | '\t' | '\r' -> aux (i + 1) acc
      | '(' -> aux (i + 1) (LParen :: acc)
      | ')' -> aux (i + 1) (RParen :: acc)
      | '\'' -> aux (i + 1) (Quote :: acc)
      | ch ->
          if (ch >= '0' && ch <= '9') || ch = '-' then (
            let j = ref i in
            while !j < len && ((input.[!j] >= '0' && input.[!j] <= '9') || input.[!j] = '-') do
              incr j
            done;
            let token = String.sub input i (!j - i) in
            let rest = match int_of_string_opt token with Some value -> Number value | None -> Symbol token in
            aux !j (rest :: acc))
          else if is_symbol_char ch then (
            let j = ref i in
            while !j < len && is_symbol_char input.[!j] do
              incr j
            done;
            let token = String.sub input i (!j - i) in
            aux !j (Symbol token :: acc))
          else raise (Parse_error (Printf.sprintf "unexpected character %c" ch))
  in
  aux 0 []

let rec parse_sexpr tokens =
  match tokens with
  | [] -> raise (Parse_error "unexpected end of input")
  | LParen :: rest ->
      let exprs, rest' = parse_list [] rest in
      (SList exprs, rest')
  | RParen :: _ -> raise (Parse_error "unexpected )")
  | Quote :: rest ->
      let expr, rest' = parse_sexpr rest in
      (SList [ SSymbol "quote"; expr ], rest')
  | Number n :: rest -> (SNumber n, rest)
  | Symbol sym :: rest -> (SSymbol sym, rest)

and parse_list acc tokens =
  match tokens with
  | [] -> raise (Parse_error "unterminated list")
  | RParen :: rest -> (List.rev acc, rest)
  | _ ->
      let expr, rest = parse_sexpr tokens in
      parse_list (expr :: acc) rest

let parse_exn input =
  let tokens = tokenize input in
  let expr, rest = parse_sexpr tokens in
  match rest with [] -> expr | _ -> raise (Parse_error "extra tokens after expression")

let rec list_to_expr = function [] -> LC.EAtom LC.ANIL | x :: xs -> LC.ECons (x, list_to_expr xs)

let builtin_symbol = function
  | "quote" -> Some LC.SQuote
  | "atom" -> Some LC.SAtom
  | "eq" -> Some LC.SEq
  | "car" -> Some LC.SCar
  | "cdr" -> Some LC.SCdr
  | "cons" -> Some LC.SCons
  | "cond" -> Some LC.SCond
  | "label" -> Some LC.SLabel
  | _ -> None

let expr_nil = LC.EAtom LC.ANIL

let rec compile_quote expr =
  match expr with
  | SNumber n -> LC.EAtom (LC.ANumber n)
  | SSymbol "nil" -> expr_nil
  | SSymbol sym -> (
      match builtin_symbol sym with
      | Some s -> LC.EAtom (LC.ASymbol s)
      | None -> raise (Parse_error ("unknown symbol in quote: " ^ sym)))
  | SList lst -> list_to_expr (List.map compile_quote lst)

let rec index_of name ctx idx =
  match ctx with [] -> None | hd :: tl -> if String.equal hd name then Some idx else index_of name tl (idx + 1)

let lookup_var ctx name =
  match index_of name ctx 0 with Some idx -> idx | None -> raise (Parse_error ("unbound variable " ^ name))

let rec compile_expr ctx sexpr =
  match sexpr with
  | SNumber n -> LC.EAtom (LC.ANumber n)
  | SSymbol "nil" -> expr_nil
  | SSymbol sym -> (
      match builtin_symbol sym with
      | Some builtin -> LC.EAtom (LC.ASymbol builtin)
      | None -> LC.EAtom (LC.AVar (lookup_var ctx sym)))
  | SList [] -> expr_nil
  | SList (SSymbol "quote" :: rest) -> (
      match rest with
      | [ value ] -> list_to_expr [ LC.EAtom (LC.ASymbol LC.SQuote); compile_quote value ]
      | _ -> raise (Parse_error "quote expects one argument"))
  | SList [ SSymbol "lambda"; params; body ] -> compile_lambda ctx params body
  | SList [ SSymbol "label"; SSymbol name; lambda_expr ] ->
      let ctx_with_label = name :: ctx in
      let lambda_compiled = compile_expr ctx_with_label lambda_expr in
      list_to_expr [ LC.EAtom (LC.ASymbol LC.SLabel); expr_nil; lambda_compiled ]
  | SList (head :: tail) -> list_to_expr (compile_expr ctx head :: List.map (compile_expr ctx) tail)
  | SList _ -> raise (Parse_error "unsupported list form")

and compile_lambda ctx params body =
  let param_names =
    match params with
    | SList lst ->
        List.map (function SSymbol name -> name | _ -> raise (Parse_error "lambda parameters must be symbols")) lst
    | _ -> raise (Parse_error "lambda parameter list expected")
  in
  let nargs = List.length param_names in
  let ctx_with_params = List.fold_left (fun acc name -> name :: acc) ctx param_names in
  let body_expr = compile_expr ctx_with_params body in
  list_to_expr [ LC.EAtom (LC.ASymbol (LC.SLambda nargs)); expr_nil; body_expr ]

let compile_string code =
  let parsed = parse_exn code in
  compile_expr [] parsed
