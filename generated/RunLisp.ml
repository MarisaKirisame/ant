module LC = LispCEK
module Memo = Ant.Memo
module Word = Ant.Word.Word
module Frontend = LispFrontend

let with_memo f =
  let memo = Memo.init_memo () in
  f memo

let rec lisp_list_of_list = function [] -> LC.Nil | x :: xs -> LC.Cons (x, lisp_list_of_list xs)
let rec list_of_lisp = function LC.Nil -> [] | LC.Cons (hd, tl) -> hd :: list_of_lisp tl

let env_seq_of_list exprs =
  let env = LC.MkEnv (lisp_list_of_list exprs) in
  LC.from_ocaml_env env

let env_values_of_seq seq = match LC.to_ocaml_env seq with LC.MkEnv values -> list_of_lisp values
let empty_env_seq = env_seq_of_list []
let rec expr_list exprs = match exprs with [] -> LC.EAtom LC.ANIL | hd :: tl -> LC.ECons (hd, expr_list tl)
let seq_of_expr_list exprs = lisp_list_of_list exprs |> LC.from_ocaml_list LC.from_ocaml_expr
let int_of_seq seq = Word.get_value (Memo.to_word seq)

let string_of_symbol = function
  | LC.SLambda -> "lambda"
  | LC.SDefine -> "define"
  | LC.SQuote -> "quote"
  | LC.SEq -> "eq"
  | LC.SCons -> "cons"
  | LC.SCond -> "cond"
  | LC.SAtom -> "atom"
  | LC.SCar -> "car"
  | LC.SCdr -> "cdr"
  | LC.SNull -> "null"
  | LC.SError -> "error"
  | LC.SIf -> "if"
  | LC.SDefvar -> "defvar"
  | LC.SPair -> "pair"
  | LC.SSymbol -> "symbol"
  | LC.STrue -> "true"
  | LC.SFalse -> "false"
  | LC.SVar -> "var"
  | LC.SNum -> "num"
  | LC.SAnd -> "and"

let string_of_atom = function
  | LC.AVar i -> Printf.sprintf "#%d" i
  | LC.ANumber i -> Printf.sprintf "%d" i
  | LC.ASymbol sym -> Printf.sprintf "%s" (string_of_symbol sym)
  | LC.ANIL -> "NIL"

let rec string_of_expr = function
  | LC.EAtom atom -> string_of_atom atom
  | LC.ECons (hd, tl) ->
      let arr = Dynarray.create () in
      let t = ref tl in
      while match !t with LC.ECons _ -> true | _ -> false do
        match !t with
        | LC.ECons (x, y) ->
            Dynarray.add_last arr x;
            t := y
        | _ -> failwith "impossible"
      done;
      (* assert (!t = LC.EAtom LC.ANIL); *)
      "(" ^ Dynarray.fold_left (fun acc x -> acc ^ " " ^ string_of_expr x) (string_of_expr hd) arr ^ ")"

let rec string_of_value = function
  | LC.VNumber x -> Printf.sprintf "%d" x
  | LC.VSymbol sym -> Printf.sprintf "%s" (string_of_symbol sym)
  | LC.VQuote e -> Printf.sprintf "(quote %s)" (string_of_expr e)
  | LC.VNIL -> "()"
  | LC.VCons (x, y) -> Printf.sprintf "(%s . %s)" (string_of_value x) (string_of_value y)
  | LC.VClosure _ -> "PROCEDURE"

let string_of_expr_list exprs = "[" ^ String.concat "; " (List.map string_of_expr exprs) ^ "]"
let string_of_option show = function LC.None -> "None" | LC.Some value -> "Some " ^ show value

let expect_equal ?(show = fun _ -> "<value>") label expected actual =
  if expected = actual then Printf.printf "[ok] %s\n" label
  else
    let message = Printf.sprintf "%s: expected %s but got %s" label (show expected) (show actual) in
    failwith message

let expect_value msg (x : LC.value) (y : LC.value) = expect_equal ~show:string_of_value msg x y
let expr_of_int_list ints = List.map (fun n -> LC.EAtom (LC.ANumber n)) ints |> expr_list
let quoted_int_list ints = LC.VQuote (expr_of_int_list ints)
let quoted_number n = LC.VQuote (LC.EAtom (LC.ANumber n))

let eval_expr expr =
  let seq = LC.from_ocaml_expr expr in
  let res = with_memo (fun memo -> LC.eval memo seq empty_env_seq) in
  LC.to_ocaml_value res.words

let eval_string code =
  let expr = Frontend.compile_string code in
  Printf.printf "compiled: %s\n" (string_of_expr expr);
  print_endline "";
  eval_expr expr

let expect_eval label code expected =
  let result = eval_string code in
  expect_value label expected result

let test_eval_cdr () =
  let code = "(cdr (quote (1 2 3)))" in
  expect_eval "cdr returns the tail of a quoted list" code (quoted_int_list [ 2; 3 ])

let test_label_recursion () =
  let code =
    "((define copy (xs)\n\
    \          (cond\n\
    \            ((atom xs) (quote ()))\n\
    \            ((quote (0)) (cons (car xs) (copy (cdr xs))))  ))\n\
    \    (quote (1 2 3)))"
  in
  expect_eval "define installs a recursive label" code (quoted_int_list [ 1; 2; 3 ])

let test_atom_rejects_cons () =
  let code = "(atom (cons (quote 1) (quote 2)))" in
  expect_eval "atom reports pairs as false" code LC.VNIL

let test_eq_number_literals () = expect_eval "eq returns true for identical numbers" "(eq 4 4)" (LC.VNumber 0)
let test_eq_number_literals_false () = expect_eval "eq returns false for distinct numbers" "(eq 4 5)" LC.VNIL

let test_cond_short_circuits () =
  let code = "(cond ((atom (quote ())) 42) ((quote (0)) 0))" in
  expect_eval "cond picks the first true branch" code (LC.VNumber 42)

let test_car_after_cons () =
  let code = "(car (cons 5 (quote ())))" in
  expect_eval "car unwraps the head of cons cells" code (LC.VNumber 5)

let read_file_content filename = In_channel.with_open_text filename In_channel.input_all

let run () =
  test_eval_cdr ();
  (* test_label_recursion (); *)
  test_atom_rejects_cons ();
  test_eq_number_literals ();
  test_eq_number_literals_false ();
  test_cond_short_circuits ();
  test_car_after_cons ();
  (* ignore (eval_string "(cdr '(1 2 3))"); *)
  let code = read_file_content "./generated/Lisp.lisp" in
  let r = eval_string code in
  print_endline (string_of_value r);
  print_endline "LispCEK smoke tests completed."
