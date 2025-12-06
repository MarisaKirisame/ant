module LC = LispCEK
module Memo = Ant.Memo
module Word = Ant.Word.Word

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
  | LC.SLambda n -> Printf.sprintf "SLambda(%d)" n
  | LC.SLabel -> "SLabel"
  | LC.SQuote -> "SQuote"
  | LC.SEq -> "SEq"
  | LC.SCons -> "SCons"
  | LC.SCond -> "SCond"
  | LC.SAtom -> "SAtom"
  | LC.SCar -> "SCar"
  | LC.SCdr -> "SCdr"

let string_of_atom = function
  | LC.AVar i -> Printf.sprintf "AVar(%d)" i
  | LC.ANumber i -> Printf.sprintf "ANumber(%d)" i
  | LC.ASymbol sym -> Printf.sprintf "ASymbol(%s)" (string_of_symbol sym)
  | LC.ANIL -> "ANIL"

let rec string_of_expr = function
  | LC.EAtom atom -> "EAtom(" ^ string_of_atom atom ^ ")"
  | LC.ECons (hd, tl) -> "ECons(" ^ string_of_expr hd ^ ", " ^ string_of_expr tl ^ ")"

let string_of_expr_list exprs = "[" ^ String.concat "; " (List.map string_of_expr exprs) ^ "]"
let string_of_option show = function LC.None -> "None" | LC.Some value -> "Some " ^ show value

let expect_equal ?(show = fun _ -> "<value>") label expected actual =
  if expected = actual then Printf.printf "[ok] %s\n" label
  else
    let message = Printf.sprintf "%s: expected %s but got %s" label (show expected) (show actual) in
    failwith message

let expect_expr = expect_equal ~show:string_of_expr
let expect_expr_list = expect_equal ~show:string_of_expr_list
let expect_option show = expect_equal ~show:(string_of_option show)

let test_quote () =
  let quoted = LC.EAtom (LC.ANumber 5) in
  let seq_arg = LC.from_ocaml_expr quoted in
  let res = with_memo (fun memo -> LC.quote memo seq_arg) in
  let actual = LC.to_ocaml_expr res.words in
  let expected = LC.ECons (LC.EAtom (LC.ASymbol LC.SQuote), quoted) in
  expect_expr "quote wraps its input with SQuote" expected actual

let test_const_values () =
  let unit_seq = LC.from_ocaml_unit LC.Unit in
  let res_true = with_memo (fun memo -> LC.const_true memo unit_seq) in
  let res_false = with_memo (fun memo -> LC.const_false memo unit_seq) in
  let res_nil = with_memo (fun memo -> LC.const_Nil memo unit_seq) in
  expect_expr "const_true produces Lisp true" (LC.EAtom (LC.ANumber 0)) (LC.to_ocaml_expr res_true.words);
  expect_expr "const_false produces Lisp false" (LC.EAtom LC.ANIL) (LC.to_ocaml_expr res_false.words);
  expect_expr "const_Nil produces NIL" (LC.EAtom LC.ANIL) (LC.to_ocaml_expr res_nil.words)

let test_is_symbol () =
  let symbol_expr = LC.EAtom (LC.ASymbol LC.SCdr) in
  let seq_symbol = LC.from_ocaml_expr symbol_expr in
  let seq_number = LC.from_ocaml_expr (LC.EAtom (LC.ANumber 7)) in
  let res_symbol = with_memo (fun memo -> LC.is_symbol memo seq_symbol) in
  let res_number = with_memo (fun memo -> LC.is_symbol memo seq_number) in
  let actual_symbol = LC.to_ocaml_option LC.to_ocaml_symbol res_symbol.words in
  let actual_number = LC.to_ocaml_option LC.to_ocaml_symbol res_number.words in
  expect_option string_of_symbol "is_symbol returns Some for atoms" (LC.Some LC.SCdr) actual_symbol;
  expect_option string_of_symbol "is_symbol returns None for non-symbol atoms" LC.None actual_number

let test_is_var () =
  let var_expr = LC.EAtom (LC.AVar 3) in
  let non_var_expr = LC.EAtom (LC.ANumber 0) in
  let seq_var = LC.from_ocaml_expr var_expr in
  let seq_non_var = LC.from_ocaml_expr non_var_expr in
  let res_var = with_memo (fun memo -> LC.is_var memo seq_var) in
  let res_non_var = with_memo (fun memo -> LC.is_var memo seq_non_var) in
  let actual_var = LC.to_ocaml_option (fun seq -> int_of_seq seq) res_var.words in
  let actual_non_var = LC.to_ocaml_option (fun seq -> int_of_seq seq) res_non_var.words in
  expect_option string_of_int "is_var extracts indices" (LC.Some 3) actual_var;
  expect_option string_of_int "is_var returns None for other atoms" LC.None actual_non_var

let test_car_cdr () =
  let tail = LC.EAtom LC.ANIL in
  let head = LC.EAtom (LC.ANumber 42) in
  let pair_expr = LC.ECons (head, tail) in
  let seq_pair = LC.from_ocaml_expr pair_expr in
  let res_car = with_memo (fun memo -> LC.car_ memo seq_pair) in
  let res_cdr = with_memo (fun memo -> LC.cdr_ memo seq_pair) in
  expect_expr "car_ returns the first element" head (LC.to_ocaml_expr res_car.words);
  expect_expr "cdr_ returns the second element" tail (LC.to_ocaml_expr res_cdr.words)

let test_lookup () =
  let env_values = [ LC.EAtom (LC.ANumber 10); LC.EAtom (LC.ANumber 20) ] in
  let env_seq = env_seq_of_list env_values in
  let res_zero = with_memo (fun memo -> LC.lookup memo (Memo.from_int 0) env_seq) in
  let res_one = with_memo (fun memo -> LC.lookup memo (Memo.from_int 1) env_seq) in
  expect_expr "lookup finds index 0" (List.nth env_values 0) (LC.to_ocaml_expr res_zero.words);
  expect_expr "lookup finds index 1" (List.nth env_values 1) (LC.to_ocaml_expr res_one.words)

let test_pairlis () =
  let env_seq = env_seq_of_list [ LC.EAtom (LC.ANumber 99) ] in
  let to_bind = [ LC.EAtom (LC.ANumber 1); LC.EAtom (LC.ANumber 2) ] in
  let ys_seq = seq_of_expr_list to_bind in
  let res = with_memo (fun memo -> LC.pairlis memo (Memo.from_int 2) ys_seq env_seq) in
  let actual = env_values_of_seq res.words in
  let expected = [ LC.EAtom (LC.ANumber 2); LC.EAtom (LC.ANumber 1); LC.EAtom (LC.ANumber 99) ] in
  expect_expr_list "pairlis conses arguments onto env" expected actual

let test_eval_atom () =
  let expr = LC.EAtom (LC.ANumber 9) in
  let res = with_memo (fun memo -> LC.eval memo (LC.from_ocaml_expr expr) empty_env_seq) in
  expect_expr "eval leaves literals untouched" expr (LC.to_ocaml_expr res.words)

let test_eval_var () =
  let env_values = [ LC.EAtom (LC.ANumber 5); LC.EAtom (LC.ANumber 7) ] in
  let env_seq = env_seq_of_list env_values in
  let expr = LC.EAtom (LC.AVar 1) in
  let res = with_memo (fun memo -> LC.eval memo (LC.from_ocaml_expr expr) env_seq) in
  expect_expr "eval resolves variables using the environment" (List.nth env_values 1) (LC.to_ocaml_expr res.words)

let test_eval_quote () =
  let args = expr_list [ LC.EAtom (LC.ANumber 12) ] in
  let expr = LC.ECons (LC.EAtom (LC.ASymbol LC.SQuote), args) in
  let res = with_memo (fun memo -> LC.eval memo (LC.from_ocaml_expr expr) empty_env_seq) in
  expect_expr "eval handles quote special form" expr (LC.to_ocaml_expr res.words)

let test_is_eq () =
  let expr_one = LC.EAtom (LC.ANumber 3) in
  let expr_two = LC.EAtom (LC.ANumber 3) in
  let expr_three = LC.EAtom (LC.ANumber 4) in
  let seq_one = LC.from_ocaml_expr expr_one in
  let seq_two = LC.from_ocaml_expr expr_two in
  let seq_three = LC.from_ocaml_expr expr_three in
  let res_true = with_memo (fun memo -> LC.is_eq_ memo empty_env_seq seq_one seq_two) in
  let res_false = with_memo (fun memo -> LC.is_eq_ memo empty_env_seq seq_one seq_three) in
  expect_expr "is_eq_ returns true when values match" (LC.EAtom (LC.ANumber 0)) (LC.to_ocaml_expr res_true.words);
  expect_expr "is_eq_ returns false when values differ" (LC.EAtom LC.ANIL) (LC.to_ocaml_expr res_false.words)

let test_evlis () =
  let exprs = expr_list [ LC.EAtom (LC.ANumber 1); LC.EAtom (LC.ANumber 2) ] in
  let res = with_memo (fun memo -> LC.evlis memo (LC.from_ocaml_expr exprs) empty_env_seq) in
  let actual = LC.to_ocaml_list LC.to_ocaml_expr res.words |> list_of_lisp in
  let expected = [ LC.EAtom (LC.ANumber 1); LC.EAtom (LC.ANumber 2) ] in
  expect_expr_list "evlis evaluates each list member" expected actual

let run () =
  test_quote ();
  test_const_values ();
  test_is_symbol ();
  test_is_var ();
  test_car_cdr ();
  test_lookup ();
  test_pairlis ();
  test_eval_atom ();
  test_eval_var ();
  test_eval_quote ();
  test_is_eq ();
  test_evlis ();
  print_endline "LispCEK smoke tests completed."
