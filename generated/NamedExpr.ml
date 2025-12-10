type nexpr =
  | NEInt of int
  | NEPlus of nexpr * nexpr
  | NELt of nexpr * nexpr
  | NELe of nexpr * nexpr
  | NEGt of nexpr * nexpr
  | NEGe of nexpr * nexpr
  | NEAnd of nexpr * nexpr
  | NEVar of string
  | NEAbs of string * nexpr
  | NEApp of nexpr * nexpr
  | NELet of string * nexpr * nexpr
  | NETrue
  | NEFalse
  | NEIf of nexpr * nexpr * nexpr
  | NENil
  | NECons of nexpr * nexpr
  | NEMatchList of nexpr * nexpr * string * string * nexpr
  | NEFix of string * string * nexpr
  | NEPair of nexpr * nexpr
  | NEZro of nexpr
  | NEFst of nexpr
  | NEUnit
  | NEHole
[@@deriving eq]

let pp_nexpr fmt nexpr =
  let rec aux fmt expr =
    match expr with
    | NEInt i -> Format.pp_print_int fmt i
    | NEPlus (lhs, rhs) -> Format.fprintf fmt "(%a + %a)" aux lhs aux rhs
    | NELt (lhs, rhs) -> Format.fprintf fmt "(%a < %a)" aux lhs aux rhs
    | NELe (lhs, rhs) -> Format.fprintf fmt "(%a <= %a)" aux lhs aux rhs
    | NEGt (lhs, rhs) -> Format.fprintf fmt "(%a > %a)" aux lhs aux rhs
    | NEGe (lhs, rhs) -> Format.fprintf fmt "(%a >= %a)" aux lhs aux rhs
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
    | NEPair (a, b) -> Format.fprintf fmt "(%a, %a)" aux a aux b
    | NEZro p -> Format.fprintf fmt "(zro %a)" aux p
    | NEFst p -> Format.fprintf fmt "(fst %a)" aux p
    | NEHole -> Format.pp_print_string fmt "hole"
    | NEUnit -> Format.pp_print_string fmt "()"
    | NEAnd (lhs, rhs) -> Format.fprintf fmt "(%a && %a)" aux lhs aux rhs
  in
  aux fmt nexpr
