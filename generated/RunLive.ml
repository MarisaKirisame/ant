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

let run x = value_to_ocaml (eval (expr_from_ocaml x) list_Nil)

let () =
  print_endline (show_value (run (OEInt 42)));
  print_endline (show_value (run (OEApp (OEAbs (OEVar 0), OEInt 42))))
