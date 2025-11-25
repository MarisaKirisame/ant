open LiveCEK
open Ant
open Common
open Word
open NamedExpr

type nat = OZ | OS of nat

type expr =
  | OEInt of int
  | OEPlus of expr * expr
  | OEVar of int
  | OEAbs of expr
  | OEApp of expr * expr
  | OELet of expr * expr
  | OETrue
  | OEFalse
  | OEIf of expr * expr * expr
  | OENil
  | OECons of expr * expr
  | OEMatchList of expr * expr * expr
  | OEFix of expr
  | OEHole
[@@deriving show]

type value =
  | OVInt of int
  | OVTrue
  | OVFalse
  | OVNil
  | OVCons of value * value
  | OVAbs of expr * value list
  | OVFix of expr * value list
  | OVStuck of stuck

and stuck =
  | SHole of value list
  | STypeError of value * vtype
  | SIndexError
  | SApp of stuck * expr
  | SAdd0 of stuck * expr
  | SAdd1 of value * stuck
  | SIf of value * expr * expr
  | SMatchList of value * expr * expr

and vtype = VTInt | VTFunc | VTBool | VTList [@@deriving show]

let base_names =
  [|
    "a";
    "b";
    "c";
    "d";
    "e";
    "f";
    "g";
    "h";
    "i";
    "j";
    "k";
    "l";
    "m";
    "n";
    "o";
    "p";
    "q";
    "r";
    "s";
    "t";
    "u";
    "v";
    "w";
    "x";
    "y";
    "z";
  |]

let make_name_generator () =
  let used = Hashtbl.create 16 in
  let counter = ref 0 in
  let rec fresh ?hint () =
    let base =
      match hint with
      | Some h -> h
      | None ->
          let n = !counter in
          incr counter;
          let candidate = base_names.(n mod Array.length base_names) in
          let suffix = n / Array.length base_names in
          if suffix = 0 then candidate else candidate ^ string_of_int suffix
    in
    let count = match Hashtbl.find_opt used base with Some c -> c | None -> 0 in
    Hashtbl.replace used base (count + 1);
    if count = 0 then base else base ^ string_of_int count
  in
  fresh

let rec lookup_name ctx idx =
  match (ctx, idx) with
  | name :: _, 0 -> name
  | _ :: rest, n when n > 0 -> lookup_name rest (n - 1)
  | _ -> Printf.sprintf "free%d" idx

let rec index_of_name (ctx : string list) (target : string) (offset : int) : int option =
  match ctx with
  | [] -> None
  | name :: rest -> if String.equal name target then Some offset else index_of_name rest target (offset + 1)

let[@warning "-32"] expr_of_nexpr ?(ctx = []) nexpr =
  let rec aux ctx = function
    | NEInt i -> OEInt i
    | NEPlus (lhs, rhs) -> OEPlus (aux ctx lhs, aux ctx rhs)
    | NEVar name -> (
        match index_of_name ctx name 0 with
        | Some idx -> OEVar idx
        | None -> invalid_arg (Printf.sprintf "expr_of_nexpr: unbound variable %S" name))
    | NEAbs (param, body) -> OEAbs (aux (param :: ctx) body)
    | NEApp (fn, arg) -> OEApp (aux ctx fn, aux ctx arg)
    | NELet (name, bound, body) -> OELet (aux ctx bound, aux (name :: ctx) body)
    | NETrue -> OETrue
    | NEFalse -> OEFalse
    | NEIf (cond, thn, els) -> OEIf (aux ctx cond, aux ctx thn, aux ctx els)
    | NENil -> OENil
    | NECons (hd, tl) -> OECons (aux ctx hd, aux ctx tl)
    | NEMatchList (target, nil_case, head_name, tail_name, cons_case) ->
        OEMatchList (aux ctx target, aux ctx nil_case, aux (tail_name :: head_name :: ctx) cons_case)
    | NEFix (func_name, arg_name, body) -> OEFix (aux (arg_name :: func_name :: ctx) body)
    | NEHole -> OEHole
  in
  aux ctx nexpr

let nexpr_of_expr ?(ctx = []) expr =
  let fresh_name = make_name_generator () in
  let rec aux ctx expr =
    match expr with
    | OEInt i -> NEInt i
    | OEPlus (lhs, rhs) -> NEPlus (aux ctx lhs, aux ctx rhs)
    | OEVar idx -> NEVar (lookup_name ctx idx)
    | OEAbs body ->
        let param = fresh_name ~hint:"x" () in
        NEAbs (param, aux (param :: ctx) body)
    | OEApp (fn, arg) -> NEApp (aux ctx fn, aux ctx arg)
    | OELet (bound, body) ->
        let name = fresh_name ~hint:"x" () in
        NELet (name, aux ctx bound, aux (name :: ctx) body)
    | OETrue -> NETrue
    | OEFalse -> NEFalse
    | OEIf (cond, thn, els) -> NEIf (aux ctx cond, aux ctx thn, aux ctx els)
    | OENil -> NENil
    | OECons (hd, tl) -> NECons (aux ctx hd, aux ctx tl)
    | OEMatchList (target, nil_case, cons_case) ->
        let head_name = fresh_name ~hint:"hd" () in
        let tail_name = fresh_name ~hint:"tl" () in
        NEMatchList
          (aux ctx target, aux ctx nil_case, head_name, tail_name, aux (tail_name :: head_name :: ctx) cons_case)
    | OEFix body ->
        let func_name = fresh_name ~hint:"f" () in
        let arg_name = fresh_name ~hint:"xs" () in
        NEFix (func_name, arg_name, aux (arg_name :: func_name :: ctx) body)
    | OEHole -> NEHole
  in
  aux ctx expr

let parse_nexpr input =
  let lexbuf = Lexing.from_string input in
  let report_position pos =
    let open Lexing in
    (pos.pos_lnum, pos.pos_cnum - pos.pos_bol + 1)
  in
  try LiveParser.nexpr LiveLexer.token lexbuf with
  | LiveLexer.Error (msg, pos) ->
      let line, col = report_position pos in
      invalid_arg (Printf.sprintf "Lexer error at line %d, column %d: %s" line col msg)
  | LiveParser.Error ->
      let line, col = report_position lexbuf.Lexing.lex_curr_p in
      invalid_arg (Printf.sprintf "Parse error at line %d, column %d" line col)

let pp_nexpr fmt nexpr =
  let rec aux fmt expr =
    match expr with
    | NEInt i -> Format.pp_print_int fmt i
    | NEPlus (lhs, rhs) -> Format.fprintf fmt "(%a + %a)" aux lhs aux rhs
    | NEVar name -> Format.pp_print_string fmt name
    | NEAbs (param, body) -> Format.fprintf fmt "(fun %s -> %a)" param aux body
    | NEApp (fn, arg) -> Format.fprintf fmt "(%a %a)" aux fn aux arg
    | NELet (name, bound, body) -> Format.fprintf fmt "(let %s = %a in %a)" name aux bound aux body
    | NETrue -> Format.pp_print_string fmt "true"
    | NEFalse -> Format.pp_print_string fmt "false"
    | NEIf (cond, thn, els) -> Format.fprintf fmt "(if %a then %a else %a)" aux cond aux thn aux els
    | NENil -> Format.pp_print_string fmt "[]"
    | NECons (hd, tl) -> Format.fprintf fmt "(%a :: %a)" aux hd aux tl
    | NEMatchList (target, nil_case, head_name, tail_name, cons_case) ->
        Format.fprintf fmt "(match %a with [] -> %a | %s :: %s -> %a)" aux target aux nil_case head_name tail_name aux
          cons_case
    | NEFix (func_name, arg_name, body) -> Format.fprintf fmt "(fix %s %s. %a)" func_name arg_name aux body
    | NEHole -> Format.pp_print_string fmt "hole"
  in
  aux fmt nexpr

let pp_expr fmt expr = pp_nexpr fmt (nexpr_of_expr expr)
let expr_to_string expr = Format.asprintf "%a" pp_expr expr

let rec nat_from_int i =
  assert (i >= 0);
  if i == 0 then OZ else OS (nat_from_int (i - 1))

let rec int_from_ocaml n = match n with OZ -> nat_Z | OS n_ -> nat_S (int_from_ocaml n_)

let rec expr_from_ocaml e =
  match e with
  | OEInt i -> expr_EInt (Memo.from_int i)
  | OEPlus (x, y) -> expr_EPlus (expr_from_ocaml x) (expr_from_ocaml y)
  | OEVar i -> expr_EVar (int_from_ocaml (nat_from_int i))
  | OELet (l, r) -> expr_ELet (expr_from_ocaml l) (expr_from_ocaml r)
  | OETrue -> expr_ETrue
  | OEFalse -> expr_EFalse
  | OENil -> expr_ENil
  | OECons (x, y) -> expr_ECons (expr_from_ocaml x) (expr_from_ocaml y)
  | OEAbs x -> expr_EAbs (expr_from_ocaml x)
  | OEApp (x, y) -> expr_EApp (expr_from_ocaml x) (expr_from_ocaml y)
  | OEIf (i, t, e) -> expr_EIf (expr_from_ocaml i) (expr_from_ocaml t) (expr_from_ocaml e)
  | OEMatchList (l, n, c) -> expr_EMatchList (expr_from_ocaml l) (expr_from_ocaml n) (expr_from_ocaml c)
  | OEFix x -> expr_EFix (expr_from_ocaml x)
  | OEHole -> expr_EHole

let rec nat_seq_to_int seq = match to_ocaml_nat seq with Z -> 0 | S rest -> 1 + nat_seq_to_int rest

let int_of_word_seq seq =
  match Memo.to_word seq with
  | Word.Int i -> i
  | Word.ConstructorTag tag -> invalid_arg (Printf.sprintf "expected int word, found constructor %d" tag)

let rec expr_of_value_seq seq =
  match to_ocaml_expr seq with
  | EInt data -> OEInt (int_of_word_seq data)
  | EPlus (lhs, rhs) -> OEPlus (expr_of_value_seq lhs, expr_of_value_seq rhs)
  | EVar idx -> OEVar (nat_seq_to_int idx)
  | EAbs body -> OEAbs (expr_of_value_seq body)
  | EApp (fn, arg) -> OEApp (expr_of_value_seq fn, expr_of_value_seq arg)
  | ELet (lhs, rhs) -> OELet (expr_of_value_seq lhs, expr_of_value_seq rhs)
  | ETrue -> OETrue
  | EFalse -> OEFalse
  | EIf (cond, thn, els) -> OEIf (expr_of_value_seq cond, expr_of_value_seq thn, expr_of_value_seq els)
  | ENil -> OENil
  | ECons (hd, tl) -> OECons (expr_of_value_seq hd, expr_of_value_seq tl)
  | EMatchList (target, nil_case, cons_case) ->
      OEMatchList (expr_of_value_seq target, expr_of_value_seq nil_case, expr_of_value_seq cons_case)
  | EFix body -> OEFix (expr_of_value_seq body)
  | EHole -> OEHole

let vtype_of_seq seq =
  match to_ocaml_vtype seq with VTInt -> VTInt | VTFunc -> VTFunc | VTBool -> VTBool | VTList -> VTList

let rec value_to_ocaml v =
  match to_ocaml_value v with
  | VInt i -> OVInt (Word.get_value (Memo.to_word i))
  | VTrue -> OVTrue
  | VFalse -> OVFalse
  | VNil -> OVNil
  | VCons (x, y) -> OVCons (value_to_ocaml x, value_to_ocaml y)
  | VAbs (body, env) -> OVAbs (expr_of_value_seq body, value_list_of_seq env)
  | VFix (body, env) -> OVFix (expr_of_value_seq body, value_list_of_seq env)
  | VStuck stuck -> OVStuck (stuck_of_seq stuck)

and value_list_of_seq seq =
  match to_ocaml_list seq with Nil -> [] | Cons (hd, tl) -> value_to_ocaml hd :: value_list_of_seq tl

and stuck_of_seq seq =
  match to_ocaml_stuck seq with
  | SHole env -> SHole (value_list_of_seq env)
  | STypeError (value, ty) -> STypeError (value_to_ocaml value, vtype_of_seq ty)
  | SIndexError -> SIndexError
  | SApp (stuck, expr) -> SApp (stuck_of_seq stuck, expr_of_value_seq expr)
  | SAdd0 (stuck, expr) -> SAdd0 (stuck_of_seq stuck, expr_of_value_seq expr)
  | SAdd1 (value, stuck) -> SAdd1 (value_to_ocaml value, stuck_of_seq stuck)
  | SIf (value, thn, els) -> SIf (value_to_ocaml value, expr_of_value_seq thn, expr_of_value_seq els)
  | SMatchList (value, nil_case, cons_case) ->
      SMatchList (value_to_ocaml value, expr_of_value_seq nil_case, expr_of_value_seq cons_case)

let rec pp_value fmt value =
  match value with
  | OVInt i -> Format.pp_print_int fmt i
  | OVTrue -> Format.pp_print_string fmt "true"
  | OVFalse -> Format.pp_print_string fmt "false"
  | OVNil -> Format.pp_print_string fmt "[]"
  | OVCons _ as cons -> (
      let rec gather acc = function
        | OVCons (h, t) -> gather (h :: acc) t
        | OVNil -> `List (List.rev acc)
        | tail -> `Improper (List.rev acc, tail)
      in
      let render_list fmt elems =
        Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "; ") pp_value fmt elems
      in
      match gather [] cons with
      | `List elems -> Format.fprintf fmt "[%a]" render_list elems
      | `Improper (elems, tail) -> Format.fprintf fmt "[%a | %a]" render_list elems pp_value tail)
  | OVAbs (body, env) -> Format.fprintf fmt "<fun %a | env=%d>" pp_expr body (List.length env)
  | OVFix (body, env) -> Format.fprintf fmt "<fix %a | env=%d>" pp_expr body (List.length env)
  | OVStuck stuck -> pp_stuck fmt stuck

and pp_stuck fmt = function
  | SHole env -> Format.fprintf fmt "<hole env=%d>" (List.length env)
  | STypeError (value, ty) -> Format.fprintf fmt "<type-error %a : %a>" pp_value value pp_vtype ty
  | SIndexError -> Format.pp_print_string fmt "<index-error>"
  | SApp (stuck, expr) -> Format.fprintf fmt "<stuck app %a %a>" pp_stuck stuck pp_expr expr
  | SAdd0 (stuck, expr) -> Format.fprintf fmt "<stuck add0 %a %a>" pp_stuck stuck pp_expr expr
  | SAdd1 (value, stuck) -> Format.fprintf fmt "<stuck add1 %a %a>" pp_value value pp_stuck stuck
  | SIf (value, thn, els) -> Format.fprintf fmt "<stuck if %a %a %a>" pp_value value pp_expr thn pp_expr els
  | SMatchList (value, nil_case, cons_case) ->
      Format.fprintf fmt "<stuck match %a %a %a>" pp_value value pp_expr nil_case pp_expr cons_case

and pp_vtype fmt = function
  | VTInt -> Format.pp_print_string fmt "int"
  | VTFunc -> Format.pp_print_string fmt "func"
  | VTBool -> Format.pp_print_string fmt "bool"
  | VTList -> Format.pp_print_string fmt "list"

let value_to_string value = Format.asprintf "%a" pp_value value
let steps_output_path = "eval_steps.json"

let steps_out : out_channel Lazy.t =
  lazy (open_out_gen [ Open_creat; Open_trunc; Open_text; Open_wronly ] 0o644 steps_output_path)

let () = at_exit (fun () -> if Lazy.is_val steps_out then close_out_noerr (Lazy.force steps_out))

let write_steps_json (r : Memo.exec_result) : unit =
  let oc = Lazy.force steps_out in
  Printf.fprintf oc "{\"step\":%d,\"without_memo_step\":%d}\n" r.step r.without_memo_step;
  flush oc

let eval_expression x =
<<<<<<< HEAD
  let exec_res =
    Memo.exec_cek
      (Memo.pc_to_exp (int_to_pc 4))
      (Dynarray.of_list [ expr_from_ocaml x; list_Nil ])
      (Memo.from_constructor tag_cont_done) memo
  in
  write_steps_json exec_res;
  value_to_ocaml exec_res.words

let parse_expr x = expr_of_nexpr (parse_nexpr x)

let mapinc =
  let source = "fix f xs. match xs with [] -> xs | hd :: tl -> (hd + 1) :: (f tl)" in
  parse_expr source
=======
  let exec_res = eval (expr_from_ocaml x) list_Nil in
  write_steps_json exec_res;
  value_to_ocaml exec_res.words

let mapinc = OEFix (OEMatchList (OEVar 0, OEVar 0, OECons (OEPlus (OEInt 1, OEVar 1), OEApp (OEVar 3, OEVar 0))))
>>>>>>> 6f6e55a (save)

let run () : unit =
  print_endline "mapinc:";
  print_endline (expr_to_string mapinc);
  print_endline (value_to_string (eval_expression (OEInt 42)));
  let repeat_list x =
    let rec build n acc = if n == 0 then acc else build (n - 1) (OECons (OEInt 1, acc)) in
    build x OENil
  in
  let nats x acc =
    let rec build n = if n == x then acc else OECons (OEInt n, build (n + 1)) in
    build 0
  in
  print_endline (value_to_string (eval_expression (OEApp (mapinc, repeat_list 2))));
  print_endline (value_to_string (eval_expression (OEApp (mapinc, repeat_list 40))));
  print_endline (value_to_string (eval_expression (OEApp (mapinc, repeat_list 45))));
<<<<<<< HEAD
  let rec loop n =
    if n > 0 then (
      ignore (eval_expression (OEApp (mapinc, nats 40)));
      loop (n - 1))
  in
  (*loop 50;*)
  print_endline (value_to_string (eval_expression (OEApp (mapinc, nats 40))));
  print_endline (value_to_string (eval_expression (OEApp (mapinc, nats 45))));
  ignore (eval_expression (OELet (mapinc, OEApp (OEVar 0, nats 45))));
  let rec repl () =
    print_string ">>> ";
    flush stdout;
    match read_line () with
    | exception End_of_file -> print_endline "bye!"
    | line ->
        let line = String.trim line in
        if String.equal line "" || String.equal line "#" then repl ()
        else if String.equal line ":quit" || String.equal line ":q" then print_endline "bye!"
        else (
          (try
             let expr = expr_of_nexpr (parse_nexpr line) in
             let value = eval_expression expr in
             print_endline (value_to_string value)
           with
          | Invalid_argument msg -> Printf.printf "Error: %s\n%!" msg
          | exn -> Printf.printf "Error: %s\n%!" (Printexc.to_string exn));
          repl ())
  in
  (*repl ();*)
=======
  print_endline (value_to_string (eval_expression (OEApp (mapinc, nats 40 OENil))));
  print_endline (value_to_string (eval_expression (OEApp (mapinc, nats 45 OENil))));
  print_endline (value_to_string (eval_expression (OEApp (mapinc, nats 46 OENil))));
  print_endline (value_to_string (eval_expression (OEApp (mapinc, nats 45 (nats 45 OENil)))));
  print_endline (value_to_string (eval_expression (OELet (mapinc, OEApp (OEVar 0, nats 45 OENil)))));
>>>>>>> 71db6e1 (save)
  ()
(*
(fix f xs. match xs with [] -> xs | hd :: tl -> (hd + 1) :: (f tl))(0 :: 1 :: 2 :: [])
(fix f xs. match xs with [] -> xs | hd :: tl -> (hd + 1) :: (f tl))(0 :: _ :: 2 :: [])
(fix f xs. match xs with [] -> xs | hd :: tl -> (hd + _) :: (f tl))(0 :: 1 :: 2 :: [])
(fix f xs. match xs with [] -> xs | hd :: tl -> (hd + 2) :: (f tl))(0 :: 1 :: 2 :: [])
*)
