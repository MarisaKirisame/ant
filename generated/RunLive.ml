open LiveCEK
open Ant
open Common

type nat = OZ | OS of nat

type expr =
  | OEInt of int
  | OEPlus of expr * expr
  | OEVar of int
  | OEAbs of expr
  | OEApp of expr * expr
  | OETrue
  | OEFalse
  | OEIf of expr * expr * expr
  | OENil
  | OECons of expr * expr
  | OEMatchList of expr * expr * expr
  | OEFix of expr

type value = OVInt of int | OVNil | OVCons of value * value [@@deriving show]

let rec pp_expr fmt expr =
  match expr with
  | OEInt i -> Format.pp_print_int fmt i
  | OEPlus (lhs, rhs) -> Format.fprintf fmt "(%a + %a)" pp_expr lhs pp_expr rhs
  | OEVar i -> Format.fprintf fmt "v%d" i
  | OEAbs body -> Format.fprintf fmt "(fun . %a)" pp_expr body
  | OEApp (fn, arg) -> Format.fprintf fmt "(%a %a)" pp_expr fn pp_expr arg
  | OETrue -> Format.pp_print_string fmt "true"
  | OEFalse -> Format.pp_print_string fmt "false"
  | OEIf (cond, thn, els) -> Format.fprintf fmt "(if %a then %a else %a)" pp_expr cond pp_expr thn pp_expr els
  | OENil -> Format.pp_print_string fmt "[]"
  | OECons (hd, tl) -> Format.fprintf fmt "(%a :: %a)" pp_expr hd pp_expr tl
  | OEMatchList (target, nil_case, cons_case) ->
      Format.fprintf fmt "(match %a with [] -> %a | Cons -> %a)" pp_expr target pp_expr nil_case pp_expr cons_case
  | OEFix body -> Format.fprintf fmt "(fix %a)" pp_expr body

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
  | OETrue -> expr_ETrue
  | OEFalse -> expr_EFalse
  | OENil -> expr_ENil
  | OECons (x, y) -> expr_ECons (expr_from_ocaml x) (expr_from_ocaml y)
  | OEAbs x -> expr_EAbs (expr_from_ocaml x)
  | OEApp (x, y) -> expr_EApp (expr_from_ocaml x) (expr_from_ocaml y)
  | OEIf (i, t, e) -> expr_EIf (expr_from_ocaml i) (expr_from_ocaml t) (expr_from_ocaml e)
  | OEMatchList (l, n, c) -> expr_EMatchList (expr_from_ocaml l) (expr_from_ocaml n) (expr_from_ocaml c)
  | OEFix x -> expr_EFix (expr_from_ocaml x)

let rec value_to_ocaml v =
  match to_ocaml_value v with
  | VInt i -> OVInt (Memo.to_int i)
  | VNil -> OVNil
  | VCons (x, y) -> OVCons (value_to_ocaml x, value_to_ocaml y)

let rec pp_value fmt value =
  match value with
  | OVInt i -> Format.pp_print_int fmt i
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

let value_to_string value = Format.asprintf "%a" pp_value value
let eval_expression x = value_to_ocaml (eval (expr_from_ocaml x) list_Nil)
let mapinc = OEFix (OEMatchList (OEVar 0, OEVar 0, OECons (OEPlus (OEInt 1, OEVar 1), OEApp (OEVar 3, OEVar 0))))

let run () : unit =
  print_endline "mapinc:";
  print_endline (expr_to_string mapinc);
  print_endline (value_to_string (eval_expression (OEInt 42)));
  let repeat_list x =
    let rec build n acc = if n == 0 then acc else build (n - 1) (OECons (OEInt 1, acc)) in
    build x OENil
  in
  print_endline (value_to_string (eval_expression (OEApp (mapinc, repeat_list 40))));
  print_endline (value_to_string (eval_expression (OEApp (mapinc, repeat_list 45))));
  ignore (eval_expression (OECons (OEInt 12, OEApp (mapinc, repeat_list 45))));
  ()
