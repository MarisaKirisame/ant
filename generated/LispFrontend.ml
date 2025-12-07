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
      | ';' ->
          let j = ref i in
          while !j < len && input.[!j] != '\n' do
            j := !j + 1
          done;
          aux (!j + 1) acc (* skip \n *)
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
  | "define" -> Some LC.SDefine
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

type compile_ctx = (string * int) list

let fresh_id next_id = (next_id, next_id + 1)

let rec lookup_var ctx name =
  match ctx with
  | [] -> raise (Parse_error ("unbound variable " ^ name))
  | (hd, id) :: tl -> if String.equal hd name then id else lookup_var tl name

let parse_param_list params =
  match params with
  | SList lst ->
      List.map (function SSymbol name -> name | _ -> raise (Parse_error "lambda parameters must be symbols")) lst
  | _ -> raise (Parse_error "lambda parameter list expected")

let bind_params ctx next_id names =
  let rec aux ctx next_id acc = function
    | [] -> (list_to_expr (List.rev acc), ctx, next_id)
    | name :: rest ->
        let id, next_id = fresh_id next_id in
        let ctx = (name, id) :: ctx in
        aux ctx next_id (LC.EAtom (LC.ANumber id) :: acc) rest
  in
  aux ctx next_id [] names

let rec compile_expr ctx next_id sexpr =
  match sexpr with
  | SNumber n -> (LC.EAtom (LC.ANumber n), next_id)
  | SSymbol "nil" -> (expr_nil, next_id)
  | SSymbol sym -> (
      match builtin_symbol sym with
      | Some builtin -> (LC.EAtom (LC.ASymbol builtin), next_id)
      | None ->
          let id = lookup_var ctx sym in
          (LC.EAtom (LC.AVar id), next_id))
  | SList [] -> (expr_nil, next_id)
  | SList (SSymbol "quote" :: rest) -> (
      match rest with
      | [ value ] -> (list_to_expr [ LC.EAtom (LC.ASymbol LC.SQuote); compile_quote value ], next_id)
      | _ -> raise (Parse_error "quote expects one argument"))
  | SList [ SSymbol "lambda"; params; body ] -> compile_lambda ctx next_id params body
  | SList elements ->
      let compiled_elements, next_id = compile_seq ctx next_id elements in
      (list_to_expr compiled_elements, next_id)

and compile_lambda ctx next_id params body =
  let param_names = parse_param_list params in
  let params_expr, ctx_with_params, next_id = bind_params ctx next_id param_names in
  let body_expr, next_id = compile_expr ctx_with_params next_id body in
  (list_to_expr [ LC.EAtom (LC.ASymbol LC.SLambda); params_expr; body_expr ], next_id)

and compile_define ctx next_id name args =
  match args with
  | [ params; body ] ->
      let name_id, next_id = fresh_id next_id in
      let ctx_with_name = (name, name_id) :: ctx in
      let param_names = parse_param_list params in
      let params_expr, ctx_for_body, next_id = bind_params ctx_with_name next_id param_names in
      let body_expr, next_id = compile_expr ctx_for_body next_id body in
      ( list_to_expr [ LC.EAtom (LC.ASymbol LC.SDefine); LC.EAtom (LC.ANumber name_id); params_expr; body_expr ],
        next_id,
        (name, name_id) )
  | _ -> raise (Parse_error "define expects a name, parameter list, and body")

and compile_seq ctx next_id sexprs =
  match sexprs with
  | [] -> ([], next_id)
  | sexpr :: rest -> (
      match sexpr with
      | SList (SSymbol "define" :: SSymbol name :: define_tail) ->
          let define_expr, next_id, binding = compile_define ctx next_id name define_tail in
          let rest_exprs, next_id = compile_seq (binding :: ctx) next_id rest in
          (define_expr :: rest_exprs, next_id)
      | SList (SSymbol "define" :: _ :: _) -> raise (Parse_error "define name must be a symbol")
      | _ ->
          let expr, next_id = compile_expr ctx next_id sexpr in
          let rest_exprs, next_id = compile_seq ctx next_id rest in
          (expr :: rest_exprs, next_id))

let compile_string code =
  let parsed = parse_exn code in
  let expr, _ = compile_expr [] 0 parsed in
  expr
