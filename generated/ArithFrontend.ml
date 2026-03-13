module LC = ArithCEK

exception ParseError of string

type token = Plus | Star | LParen | RParen | Number of int | Ident of string | Eof

let is_digit = function '0' .. '9' -> true | _ -> false
let is_ident_char = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

let tokenize input =
  let len = String.length input in
  let rec aux i acc =
    if i >= len then List.rev (Eof :: acc)
    else
      match input.[i] with
      | ' ' | '\n' | '\t' | '\r' -> aux (i + 1) acc
      | '+' -> aux (i + 1) (Plus :: acc)
      | '*' -> aux (i + 1) (Star :: acc)
      | '(' -> aux (i + 1) (LParen :: acc)
      | ')' -> aux (i + 1) (RParen :: acc)
      | '-' as ch ->
          if i + 1 < len && is_digit input.[i + 1] then (
            let j = ref (i + 1) in
            while !j < len && is_digit input.[!j] do
              incr j
            done;
            let token = String.sub input i (!j - i) in
            let value = int_of_string token in
            aux !j (Number value :: acc))
          else raise (ParseError (Printf.sprintf "unexpected character %c" ch))
      | ch when is_digit ch ->
          let j = ref i in
          while !j < len && is_digit input.[!j] do
            incr j
          done;
          let token = String.sub input i (!j - i) in
          let value = int_of_string token in
          aux !j (Number value :: acc)
      | ch when is_ident_char ch ->
          let j = ref i in
          while !j < len && is_ident_char input.[!j] do
            incr j
          done;
          let token = String.sub input i (!j - i) in
          aux !j (Ident token :: acc)
      | ch -> raise (ParseError (Printf.sprintf "unexpected character %c" ch))
  in
  aux 0 []

let rec parse_expr tokens = parse_sum tokens

and parse_sum tokens =
  let lhs, rest = parse_product tokens in
  parse_sum_tail lhs rest

and parse_sum_tail lhs tokens =
  match tokens with
  | Plus :: rest ->
      let rhs, rest' = parse_product rest in
      parse_sum_tail (LC.Add (lhs, rhs)) rest'
  | _ -> (lhs, tokens)

and parse_product tokens =
  let lhs, rest = parse_unary tokens in
  parse_product_tail lhs rest

and parse_product_tail lhs tokens =
  match tokens with
  | Star :: rest ->
      let rhs, rest' = parse_unary rest in
      parse_product_tail (LC.Mul (lhs, rhs)) rest'
  | _ -> (lhs, tokens)

and parse_unary tokens =
  match tokens with
  | Ident "exp" :: rest ->
      let arg, rest' = parse_unary rest in
      (LC.Exp arg, rest')
  | Ident "log" :: rest ->
      let arg, rest' = parse_unary rest in
      (LC.Log arg, rest')
  | _ -> parse_atom tokens

and parse_atom tokens =
  match tokens with
  | Number n :: rest -> (LC.Const n, rest)
  | Ident "X" :: rest -> (LC.Var LC.X, rest)
  | Ident "Y" :: rest -> (LC.Var LC.Y, rest)
  | Ident name :: _ -> raise (ParseError ("unknown identifier: " ^ name))
  | LParen :: rest -> (
      let expr, rest' = parse_expr rest in
      match rest' with RParen :: rest'' -> (expr, rest'') | _ -> raise (ParseError "expected ')'"))
  | _ -> raise (ParseError "unexpected token while parsing expression")

let compile_string input =
  let tokens = tokenize input in
  let expr, rest = parse_expr tokens in
  match rest with [ Eof ] -> expr | _ -> raise (ParseError "unexpected tokens after expression")
