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
  | OELet of expr * expr
  | OETrue
  | OEFalse
  | OEIf of expr * expr * expr
  | OENil
  | OECons of expr * expr
  | OEMatchList of expr * expr * expr
  | OEFix of expr

type value = OVInt of int | OVNil | OVCons of value * value [@@deriving show]

let pp_expr : Format.formatter -> expr -> unit =
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
  in
  let fresh_name =
    let used = Hashtbl.create 16 in
    let counter = ref 0 in
    fun ?hint () ->
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
  let rec lookup ctx idx =
    match (ctx, idx) with
    | name :: _, 0 -> name
    | _ :: rest, n when n > 0 -> lookup rest (n - 1)
    | _ -> Printf.sprintf "free%d" idx
  in
  let rec aux ctx fmt expr =
    match expr with
    | OEInt i -> Format.pp_print_int fmt i
    | OEPlus (lhs, rhs) -> Format.fprintf fmt "(%a + %a)" (aux ctx) lhs (aux ctx) rhs
    | OEVar idx -> Format.pp_print_string fmt (lookup ctx idx)
    | OEAbs body ->
        let name = fresh_name ~hint:"x" () in
        Format.fprintf fmt "(fun %s -> %a)" name (aux (name :: ctx)) body
    | OEApp (fn, arg) -> Format.fprintf fmt "(%a %a)" (aux ctx) fn (aux ctx) arg
    | OETrue -> Format.pp_print_string fmt "true"
    | OEFalse -> Format.pp_print_string fmt "false"
    | OEIf (cond, thn, els) -> Format.fprintf fmt "(if %a then %a else %a)" (aux ctx) cond (aux ctx) thn (aux ctx) els
    | OENil -> Format.pp_print_string fmt "[]"
    | OECons (hd, tl) -> Format.fprintf fmt "(%a :: %a)" (aux ctx) hd (aux ctx) tl
    | OEMatchList (target, nil_case, cons_case) ->
        let head_name = fresh_name ~hint:"hd" () in
        let tail_name = fresh_name ~hint:"tl" () in
        Format.fprintf fmt "(match %a with [] -> %a | %s :: %s -> %a)" (aux ctx) target (aux ctx) nil_case head_name
          tail_name
          (aux (tail_name :: head_name :: ctx))
          cons_case
    | OEFix body ->
        let func_name = fresh_name ~hint:"f" () in
        let arg_name = fresh_name ~hint:"xs" () in
        Format.fprintf fmt "(fix %s %s. %a)" func_name arg_name (aux (arg_name :: func_name :: ctx)) body
  in
  aux []

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
  let nats x =
    let rec build n = if n == x then OENil else OECons (OEInt n, build (n + 1)) in
    build 0
  in
  print_endline (value_to_string (eval_expression (OEApp (mapinc, repeat_list 40))));
  print_endline (value_to_string (eval_expression (OEApp (mapinc, repeat_list 45))));
  let rec loop n =
    if n > 0 then (
      ignore (eval_expression (OEApp (mapinc, nats 40)));
      loop (n - 1))
  in
  loop 50;
  print_endline (value_to_string (eval_expression (OEApp (mapinc, nats 40))));
  print_endline (value_to_string (eval_expression (OEApp (mapinc, nats 45))));
  ignore (eval_expression (OELet (mapinc, OEApp (OEVar 0, nats 45))));
  ()
