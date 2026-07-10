(* Reads Hazel-generated program JSON blobs (list of s-expr strings)
   and parses them into OCaml S-expressions. *)

open Yojson.Safe
open Yojson.Safe.Util
open NamedExpr
open RunLiveCommon
module Sexp = Sexplib.Sexp

let read_program_strings ~program_path =
  if not (Sys.file_exists program_path) then invalid_arg (Printf.sprintf "program file not found at %s" program_path);
  from_file program_path |> to_list |> List.map to_string

type expr =
  | Let of expr * expr * expr
  | If of expr * expr * expr
  | EmptyHole
  | MultiHole
  | Fun of expr * expr
  | TuplePat of expr list
  | TupLabelPat of expr * expr
  | LabelPat of string
  | VarPat of string
  | Var of string
  | Atom of expr
  | Bool of bool
  | Int of int
  | BinOp of string * expr * expr
  | Match of expr * (expr * expr) list
  | ListPat of expr list
  | ListLit of expr list
  | Tuple of expr list
  | ConsPat of expr * expr
  | Wild
  | PMultiHole
  | PEmptyHole
  | Ap of expr * expr
  | Cons of expr * expr
  | Zro of expr
  | Fst of expr
  | Seq of expr * expr

let rec expr_of_sexp_helper = function
  | Sexp.List [ Sexp.Atom "Let"; lhs; rhs; body ] ->
      Let (expr_of_sexp_helper lhs, expr_of_sexp_helper rhs, expr_of_sexp_helper body)
  | Sexp.List [ Sexp.Atom "If"; cond; thn; els ] ->
      If (expr_of_sexp_helper cond, expr_of_sexp_helper thn, expr_of_sexp_helper els)
  | Sexp.Atom "EmptyHole" -> EmptyHole
  | Sexp.Atom "MultiHole" -> MultiHole
  | Sexp.List [ Sexp.Atom "Fun"; param; body ] -> Fun (expr_of_sexp_helper param, expr_of_sexp_helper body)
  | Sexp.List [ Sexp.Atom "TuplePat"; Sexp.List elems ] -> TuplePat (List.map expr_of_sexp_helper elems)
  | Sexp.List [ Sexp.Atom "TupLabelPat"; x; y ] -> PEmptyHole
  | Sexp.List [ Sexp.Atom "VarPat"; Sexp.Atom v ] -> VarPat v
  | Sexp.List [ Sexp.Atom "Var"; Sexp.Atom v ] -> Var v
  | Sexp.List [ Sexp.Atom "Atom"; payload ] -> Atom (expr_of_sexp_helper payload)
  | Sexp.List [ Sexp.Atom "Bool"; Sexp.Atom b ] -> Bool (bool_of_string b)
  | Sexp.List [ Sexp.Atom "Int"; Sexp.Atom i ] -> Int (int_of_string i)
  | Sexp.List [ Sexp.Atom "BinOp"; Sexp.Atom op; lhs; rhs ] ->
      BinOp (op, expr_of_sexp_helper lhs, expr_of_sexp_helper rhs)
  | Sexp.List [ Sexp.Atom "UnOp"; Sexp.Atom "Minus"; x ] -> BinOp ("Minus", Int 0, expr_of_sexp_helper x)
  | Sexp.List [ Sexp.Atom "Match"; scrut; Sexp.List cases ] ->
      Match
        ( expr_of_sexp_helper scrut,
          List.map
            (function
              | Sexp.List [ pat; expr ] -> (expr_of_sexp_helper pat, expr_of_sexp_helper expr)
              | sexp -> failwith (Printf.sprintf "Unrecognized match case s-expression: %s" (Sexp.to_string_hum sexp)))
            cases )
  | Sexp.List [ Sexp.Atom "ListPat"; Sexp.List elems ] -> ListPat (List.map expr_of_sexp_helper elems)
  | Sexp.Atom "Wild" -> Wild
  | Sexp.List [ Sexp.Atom "ConsPat"; head; tail ] -> ConsPat (expr_of_sexp_helper head, expr_of_sexp_helper tail)
  | Sexp.Atom "PMultiHole" -> PMultiHole
  | Sexp.Atom "PEmptyHole" -> PEmptyHole
  | Sexp.List [ Sexp.Atom "Ap"; Sexp.Atom "Forward"; func; arg ] ->
      Ap (expr_of_sexp_helper func, expr_of_sexp_helper arg)
  | Sexp.List [ Sexp.Atom "ApPat"; func; arg ] -> PEmptyHole
  | Sexp.List [ Sexp.Atom "Tuple"; Sexp.List elems ] -> Tuple (List.map expr_of_sexp_helper elems)
  | Sexp.List [ Sexp.Atom "TupLabel"; x; y ] -> expr_of_sexp_helper y
  | Sexp.List [ Sexp.Atom "Cons"; head; tail ] -> Cons (expr_of_sexp_helper head, expr_of_sexp_helper tail)
  | Sexp.List [ Sexp.Atom "ListLit"; Sexp.List elems ] -> ListLit (List.map expr_of_sexp_helper elems)
  | Sexp.List [ Sexp.Atom "LabelPat"; Sexp.Atom str ] -> LabelPat str
  | Sexp.List [ Sexp.Atom "Constructor"; _ ] -> EmptyHole
  | Sexp.List [ Sexp.Atom "Seq"; x; y ] -> Seq (expr_of_sexp_helper x, expr_of_sexp_helper y)
  | sexp -> failwith (Printf.sprintf "Unrecognized expression s-expression: %s" (Sexp.to_string_hum sexp))

let expr_of_sexp sexp =
  try expr_of_sexp_helper sexp
  with exn ->
    Printf.eprintf "expr_of_sexp conversion failed for input: %s\n%!" (Sexp.to_string_hum sexp);
    raise exn

let rec pp_expr fmt = function
  | Let (v, b, body) -> Format.fprintf fmt "(Let %a %a %a)" pp_expr v pp_expr b pp_expr body
  | If (c, t, e) -> Format.fprintf fmt "(If %a %a %a)" pp_expr c pp_expr t pp_expr e
  | EmptyHole -> Format.pp_print_string fmt "EmptyHole"
  | MultiHole -> Format.pp_print_string fmt "MultiHole"
  | Fun (param, body) -> Format.fprintf fmt "(Fun %a %a)" pp_expr param pp_expr body
  | TuplePat elems ->
      Format.pp_print_string fmt "(TuplePat (";
      let rec loop = function
        | [] -> ()
        | [ x ] -> Format.fprintf fmt "%a" pp_expr x
        | x :: xs ->
            Format.fprintf fmt "%a " pp_expr x;
            loop xs
      in
      loop elems;
      Format.pp_print_string fmt "))"
  | VarPat v -> Format.fprintf fmt "(VarPat %s)" v
  | Var v -> Format.fprintf fmt "(Var %s)" v
  | Atom e -> Format.fprintf fmt "(Atom %a)" pp_expr e
  | Bool b -> Format.fprintf fmt "(Bool %b)" b
  | Int i -> Format.fprintf fmt "(Int %d)" i
  | Zro x -> Format.fprintf fmt "(Zro %a)" pp_expr x
  | Fst x -> Format.fprintf fmt "(Fst %a)" pp_expr x
  | TupLabelPat (x, y) -> Format.fprintf fmt "(TupLablePat %a %a)" pp_expr x pp_expr y
  | LabelPat str -> Format.fprintf fmt "(LabelPat %s)" str
  | BinOp (op, l, r) -> Format.fprintf fmt "(BinOp %s %a %a)" op pp_expr l pp_expr r
  | Match (scrut, cases) ->
      let pp_case fmt (pat, expr) = Format.fprintf fmt "(%a %a)" pp_expr pat pp_expr expr in
      let pp_cases fmt cs = Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_char fmt ' ') pp_case fmt cs in
      Format.fprintf fmt "(Match %a (%a))" pp_expr scrut pp_cases cases
  | ListPat elems ->
      Format.pp_print_string fmt "(ListPat (";
      let rec loop = function
        | [] -> ()
        | [ x ] -> Format.fprintf fmt "%a" pp_expr x
        | x :: xs ->
            Format.fprintf fmt "%a " pp_expr x;
            loop xs
      in
      loop elems;
      Format.pp_print_string fmt "))"
  | ListLit elems ->
      Format.pp_print_string fmt "(ListLit (";
      let rec loop = function
        | [] -> ()
        | [ x ] -> Format.fprintf fmt "%a" pp_expr x
        | x :: xs ->
            Format.fprintf fmt "%a " pp_expr x;
            loop xs
      in
      loop elems;
      Format.pp_print_string fmt "))"
  | Tuple elems ->
      Format.pp_print_string fmt "(Tuple (";
      let rec loop = function
        | [] -> ()
        | [ x ] -> Format.fprintf fmt "%a" pp_expr x
        | x :: xs ->
            Format.fprintf fmt "%a " pp_expr x;
            loop xs
      in
      loop elems;
      Format.pp_print_string fmt "))"
  | ConsPat (h, t) -> Format.fprintf fmt "(ConsPat %a %a)" pp_expr h pp_expr t
  | Wild -> Format.pp_print_string fmt "Wild"
  | PMultiHole -> Format.pp_print_string fmt "PMultiHole"
  | PEmptyHole -> Format.pp_print_string fmt "PEmptyHole"
  | Ap (f, a) -> Format.fprintf fmt "(Ap %a %a)" pp_expr f pp_expr a
  | Cons (h, t) -> Format.fprintf fmt "(Cons %a %a)" pp_expr h pp_expr t
  | Seq (x, y) -> Format.fprintf fmt "(Seq %a %a)" pp_expr x pp_expr y

type case = { patterns : expr list; body : nexpr; letlist : (string * nexpr) list }

let rec subst (lhs : string) (rhs : nexpr) (expr : nexpr) =
  match expr with
  | NEInt _ | NETrue | NEFalse | NEUnit | NEHole | NENil -> expr
  | NEVar v -> if v = lhs then rhs else expr
  | NEApp (f, a) -> NEApp (subst lhs rhs f, subst lhs rhs a)
  | NEAnd (l, r) -> NEAnd (subst lhs rhs l, subst lhs rhs r)
  | NECons (h, t) -> NECons (subst lhs rhs h, subst lhs rhs t)
  | NELet (name, binding, body) ->
      assert (name != lhs);
      NELet (name, subst lhs rhs binding, subst lhs rhs body)
  | NEFix (name, arg, body) -> NEFix (name, arg, subst lhs rhs body)
  | NEAbs (arg, body) -> NEAbs (arg, subst lhs rhs body)
  | NEPlus (x, y) -> NEPlus (subst lhs rhs x, subst lhs rhs y)
  | NEMinus (x, y) -> NEMinus (subst lhs rhs x, subst lhs rhs y)
  | NELt (x, y) -> NELt (subst lhs rhs x, subst lhs rhs y)
  | NELe (x, y) -> NELe (subst lhs rhs x, subst lhs rhs y)
  | NEGt (x, y) -> NEGt (subst lhs rhs x, subst lhs rhs y)
  | NEGe (x, y) -> NEGe (subst lhs rhs x, subst lhs rhs y)
  | NESeq (x, y) -> NESeq (subst lhs rhs x, subst lhs rhs y)
  | NEPair (x, y) -> NEPair (subst lhs rhs x, subst lhs rhs y)
  | NEIf (i, t, e) -> NEIf (subst lhs rhs i, subst lhs rhs t, subst lhs rhs e)
  | NEMatchList (l, nil_case, x, xs, cons_case) ->
      assert (lhs != x);
      assert (lhs != xs);
      NEMatchList (subst lhs rhs l, subst lhs rhs nil_case, x, xs, subst lhs rhs cons_case)
  | NEZro x -> NEZro (subst lhs rhs x)
  | NEFst x -> NEFst (subst lhs rhs x)

let rec substs (bindings : (string * nexpr) list) (expr : nexpr) : nexpr =
  List.fold_right (fun (l, r) acc -> subst l r acc) bindings expr

let fresh_name names =
  let n = !names in
  names := n + 1;
  "fresh" ^ string_of_int n

let rec compile_pattern (matched : string list) (cases : case list) names =
  let ml = List.length matched in
  let rec good_pattern p =
    match p with
    | ListPat pats -> List.for_all good_pattern pats
    | PMultiHole -> false
    | PEmptyHole -> false
    | TuplePat _ -> false
    | VarPat _ -> true
    | Wild -> true
    | ConsPat (x, y) -> good_pattern x && good_pattern y
    | _ -> failwith ("good pattern not implemented for expr: " ^ Format.asprintf "%a" pp_expr p)
  in
  let cases = List.filter (fun case -> List.for_all good_pattern case.patterns) cases in
  assert (List.for_all (fun case -> List.length case.patterns = ml) cases);
  match matched with
  | [] -> ( match cases with [] -> NEHole | case :: _ -> substs case.letlist case.body)
  | m :: mrest ->
      let look =
        List.exists
          (fun case ->
            match case.patterns with
            | [] -> failwith "compile_pattern (look): case with no patterns"
            | ListPat _ :: _ -> true
            | ConsPat _ :: _ -> true
            | Wild :: _ -> false
            | VarPat _ :: _ -> false
            | x :: _ -> failwith ("compile_pattern (look) not implemented for expr: " ^ Format.asprintf "%a" pp_expr x))
          cases
      in
      if look then
        let mh = fresh_name names in
        let mt = fresh_name names in
        NEMatchList
          ( NEVar m,
            compile_pattern mrest
              (List.filter_map
                 (fun case ->
                   match case.patterns with
                   | [] -> failwith "compile_pattern (uncase nil): case with no patterns"
                   | ListPat [] :: rest -> Some { case with patterns = rest }
                   | ListPat (_ :: _) :: _ -> None
                   | ConsPat (_, _) :: _ -> None
                   | VarPat name :: rest ->
                       Some { case with patterns = rest; letlist = (name, NEVar m) :: case.letlist }
                   | Wild :: rest -> Some { case with patterns = rest }
                   | x :: _ ->
                       failwith
                         ("compile_pattern (uncase nil) not implemented for pattern: " ^ Format.asprintf "%a" pp_expr x))
                 cases)
              names,
            mh,
            mt,
            compile_pattern (mh :: mt :: mrest)
              (List.filter_map
                 (fun case ->
                   match case.patterns with
                   | [] -> failwith "compile_pattern (uncase cons): case with no patterns"
                   | ListPat [] :: _ -> None
                   | ListPat (hd :: tl) :: rest -> Some { case with patterns = hd :: ListPat tl :: rest }
                   | ConsPat (hd, tl) :: rest -> Some { case with patterns = hd :: tl :: rest }
                   | VarPat name :: rest ->
                       Some { case with patterns = Wild :: Wild :: rest; letlist = (name, NEVar m) :: case.letlist }
                   | Wild :: rest -> Some { case with patterns = Wild :: Wild :: rest }
                   | x :: _ ->
                       failwith
                         ("compile_pattern (uncase cons) not implemented for pattern: " ^ Format.asprintf "%a" pp_expr x))
                 cases)
              names )
      else
        compile_pattern mrest
          (List.map
             (fun case ->
               match case.patterns with
               | [] -> failwith "compile_pattern (nolook): case with no patterns"
               | Wild :: rest -> { case with patterns = rest }
               | VarPat name :: rest -> { case with patterns = rest; letlist = (name, NEVar m) :: case.letlist }
               | x :: _ ->
                   failwith ("compile_pattern (nolook) not implemented for pattern: " ^ Format.asprintf "%a" pp_expr x))
             cases)
          names

let rec dedup eq xs =
  match xs with
  | [] -> []
  | [ x ] -> [ x ]
  | x :: y :: rest -> if eq x y then dedup eq (y :: rest) else x :: dedup eq (y :: rest)

let add_if_absent v acc = if List.mem v acc then acc else v :: acc

let rec bound_vars pat acc =
  match pat with
  | VarPat name -> add_if_absent name acc
  | TuplePat ps | ListPat ps -> List.fold_left (fun a p -> bound_vars p a) acc ps
  | ConsPat (h, t) -> bound_vars t (bound_vars h acc)
  | Wild | PMultiHole -> acc
  | TupLabelPat _ -> acc
  | PEmptyHole -> acc
  | _ -> failwith ("bound_vars not implemented for pattern: " ^ Format.asprintf "%a" pp_expr pat)

let rec free_vars_aux (x : expr) bound acc =
  match x with
  | Var name -> if List.mem name bound then acc else add_if_absent name acc
  | EmptyHole | MultiHole | PMultiHole | Wild | Atom (Bool _) | Atom (Int _) | ListLit [] -> acc
  | BinOp (_, lhs, rhs) -> free_vars_aux lhs bound (free_vars_aux rhs bound acc)
  | ListLit (x :: xs) -> free_vars_aux x bound (free_vars_aux (ListLit xs) bound acc)
  | Match (scrut, cases) ->
      let acc = free_vars_aux scrut bound acc in
      let rec loop cases acc =
        match cases with [] -> acc | (pat, expr) :: rest -> loop rest (free_vars_aux expr (bound_vars pat bound) acc)
      in
      loop cases acc
  | Ap (func, arg) -> free_vars_aux func bound (free_vars_aux arg bound acc)
  | Tuple ps -> List.fold_left (fun a p -> free_vars_aux p bound a) acc ps
  | Cons (hd, tl) -> free_vars_aux hd bound (free_vars_aux tl bound acc)
  | Seq (x, y) -> free_vars_aux x bound (free_vars_aux y bound acc)
  | Fun (args, body) -> free_vars_aux body (bound_vars args bound) acc
  | Let (lhs, rhs, body) -> free_vars_aux body (bound_vars lhs bound) (free_vars_aux rhs bound acc)
  | If (i, t, e) -> free_vars_aux i bound (free_vars_aux t bound (free_vars_aux e bound acc))
  | _ -> failwith ("free_vars not implemented for expr: " ^ Format.asprintf "%a" pp_expr x)

let free_vars x = free_vars_aux x [] []

let rec nexpr_of_expr_aux e names : nexpr =
  match e with
  (*todo: this is suboptimal coding. we want a separate pass which remove pattern matching in function header*)
  | Let (VarPat name, Fun (TuplePat params, fun_body), let_body) when List.mem name (free_vars fun_body) -> (
      let param_names =
        List.map
          (function
            | VarPat p -> p
            | PEmptyHole -> fresh_name names
            | p ->
                failwith
                  ("Only VarPat supported in recursive function parameters. but got: " ^ Format.asprintf "%a" pp_expr p))
          params
      in
      match param_names with
      | [] -> failwith "Recursive function needs at least one parameter"
      | first :: rest ->
          let body = nexpr_of_expr_aux fun_body names in
          let body_with_rest = List.fold_right (fun p acc -> NEAbs (p, acc)) rest body in
          NELet (name, NEFix (name, first, body_with_rest), nexpr_of_expr_aux let_body names))
  | Let (VarPat name, Fun (VarPat param, fun_body), let_body) when List.mem name (free_vars fun_body) ->
      NELet (name, NEFix (name, param, nexpr_of_expr_aux fun_body names), nexpr_of_expr_aux let_body names)
  | Let (VarPat name, bound, body) -> NELet (name, nexpr_of_expr_aux bound names, nexpr_of_expr_aux body names)
  | Let (PEmptyHole, _, x) -> nexpr_of_expr_aux x names
  | Let (PMultiHole, _, x) -> nexpr_of_expr_aux x names
  | Let (TuplePat [ x; y ], rhs, body) ->
      let p = fresh_name names in
      let l = fresh_name names in
      let r = fresh_name names in
      nexpr_of_expr_aux (Let (VarPat p, rhs, Let (VarPat l, Zro (Var p), Let (VarPat r, Fst (Var p), body)))) names
  (*todo: use the new tuple mechanism*)
  | Fun (TuplePat params, body) ->
      let rec build_nested_fun params body =
        match params with
        | [] -> nexpr_of_expr_aux body names
        | VarPat name :: rest -> NEAbs (name, build_nested_fun rest body)
        | PEmptyHole :: rest -> NEAbs (fresh_name names, build_nested_fun rest body)
        | TupLabelPat _ :: rest -> NEHole
        | p :: rest ->
            failwith ("Only VarPat supported in function parameters, but got: " ^ Format.asprintf "%a" pp_expr p)
      in
      build_nested_fun params body
  | Fun (VarPat name, body) -> NEAbs (name, nexpr_of_expr_aux body names)
  | Fun (PEmptyHole, body) -> NEHole
  | Fun (PMultiHole, body) -> NEHole
  | Fun (Wild, body) -> NEAbs (fresh_name names, nexpr_of_expr_aux body names)
  | Ap (func, Tuple [ x; y ]) ->
      NEApp (NEApp (nexpr_of_expr_aux func names, nexpr_of_expr_aux x names), nexpr_of_expr_aux y names)
  | Ap (func, arg) -> NEApp (nexpr_of_expr_aux func names, nexpr_of_expr_aux arg names)
  | If (cond, thn, els) -> NEIf (nexpr_of_expr_aux cond names, nexpr_of_expr_aux thn names, nexpr_of_expr_aux els names)
  | Atom (Bool true) -> NETrue
  | Atom (Bool false) -> NEFalse
  | Atom (Int i) -> NEInt i
  | Var name -> NEVar name
  | BinOp (op, l, r) -> (
      let nl = nexpr_of_expr_aux l names in
      let nr = nexpr_of_expr_aux r names in
      match op with
      | "Plus" -> NEPlus (nl, nr)
      | "Lt" -> NELt (nl, nr)
      | "Le" -> NELe (nl, nr)
      | "Gt" -> NEGt (nl, nr)
      | "Ge" -> NEGe (nl, nr)
      | "And" -> NEAnd (nl, nr)
      | "Minus" -> NEMinus (nl, nr)
      | _ -> failwith ("nexpr_of_expr not implemented for binop: " ^ op))
  | EmptyHole | MultiHole -> NEHole
  | Int i -> NEInt i
  | Match (list, cases) ->
      let matched = nexpr_of_expr_aux list names in
      let m = fresh_name names in
      NELet
        ( m,
          matched,
          compile_pattern [ m ]
            (List.map
               (fun (pat, expr) -> { patterns = [ pat ]; body = nexpr_of_expr_aux expr names; letlist = [] })
               cases)
            names )
  | Tuple [] -> NEUnit
  | Tuple [ x ] -> nexpr_of_expr_aux x names
  | Cons (hd, tl) -> NECons (nexpr_of_expr_aux hd names, nexpr_of_expr_aux tl names)
  | ListLit [] -> NENil
  | ListLit (x :: xs) -> NECons (nexpr_of_expr_aux x names, nexpr_of_expr_aux (ListLit xs) names)
  | Tuple [ x; y ] -> NEPair (nexpr_of_expr_aux x names, nexpr_of_expr_aux y names)
  | Zro x -> NEZro (nexpr_of_expr_aux x names)
  | Fst x -> NEFst (nexpr_of_expr_aux x names)
  | Seq (x, y) -> NESeq (nexpr_of_expr_aux x names, nexpr_of_expr_aux y names)
  | _ -> failwith ("nexpr_of_expr not implemented for expr: " ^ Format.asprintf "%a" pp_expr e)

let nexpr_of_expr e =
  try nexpr_of_expr_aux e (ref 0)
  with exn ->
    let detail = Printexc.to_string exn in
    failwith (Printf.sprintf "Failed in nexpr_of_expr: %s\nExpr:\n%s" detail (Format.asprintf "%a" pp_expr e))

let extract_program sexp =
  match sexp with
  | Sexp.List [ Sexp.Atom "exercise"; Sexp.Atom _exercise_id; _prelude; impl; _tests ] -> impl
  | _ -> invalid_arg (Printf.sprintf "Expected (exercise <id> <focus> <impl> <test>), got %s" (Sexp.to_string_hum sexp))

let extract_program_with_meta sexp =
  match sexp with
  | Sexp.List [ Sexp.Atom "exercise"; Sexp.Atom exercise_id; _prelude; impl; _tests ] ->
      (int_of_string_opt exercise_id, impl)
  | _ -> invalid_arg (Printf.sprintf "Expected (exercise <id> <focus> <impl> <test>), got %s" (Sexp.to_string_hum sexp))

let parse ~candidate_index ~program_path program =
  try Sexp.of_string program
  with exn ->
    let detail = Printexc.to_string exn in
    failwith
      (Printf.sprintf "Failed to parse Hazel candidate %d from %s: %s\nProgram:\n%s" candidate_index program_path detail
         program)

let rec clean_aux x (seen : string list) =
  match x with
  | NEHole | NETrue | NEFalse | NEUnit | NENil | NEInt _ -> x
  | NEVar v -> if List.mem v seen then x else NEHole
  | NELet (name, bound, body) -> NELet (name, clean_aux bound seen, clean_aux body (name :: seen))
  | NEAbs (param, body) -> NEAbs (param, clean_aux body (param :: seen))
  | NEIf (cond, thn, els) -> NEIf (clean_aux cond seen, clean_aux thn seen, clean_aux els seen)
  | NEGt (l, r) -> NEGt (clean_aux l seen, clean_aux r seen)
  | NEGe (l, r) -> NEGe (clean_aux l seen, clean_aux r seen)
  | NELt (l, r) -> NELt (clean_aux l seen, clean_aux r seen)
  | NELe (l, r) -> NELe (clean_aux l seen, clean_aux r seen)
  | NEAnd (l, r) -> NEAnd (clean_aux l seen, clean_aux r seen)
  | NEPlus (l, r) -> NEPlus (clean_aux l seen, clean_aux r seen)
  | NEMinus (l, r) -> NEMinus (clean_aux l seen, clean_aux r seen)
  | NESeq (l, r) -> NESeq (clean_aux l seen, clean_aux r seen)
  | NEMatchList (target, nil_case, head_name, tail_name, cons_case) ->
      NEMatchList
        ( clean_aux target seen,
          clean_aux nil_case seen,
          head_name,
          tail_name,
          clean_aux cons_case (tail_name :: head_name :: seen) )
  | NEApp (f, a) -> NEApp (clean_aux f seen, clean_aux a seen)
  | NECons (h, t) -> NECons (clean_aux h seen, clean_aux t seen)
  | NEPair (x, y) -> NEPair (clean_aux x seen, clean_aux y seen)
  | NEFix (name, param, body) -> NEFix (name, param, clean_aux body (param :: name :: seen))
  | NEZro x -> NEZro (clean_aux x seen)
  | NEFst x -> NEFst (clean_aux x seen)

let clean x = clean_aux x []

let rec subst_deepest_hole y x =
  match x with
  | NEHole -> y
  | NELet (name, bound, body) -> NELet (name, bound, subst_deepest_hole y body)
  | NEVar _ | NECons _ | NEAbs _ | NEMinus _ -> x
  | _ -> failwith ("subst_deepest_hole not implemented for expr: " ^ Format.asprintf "%a" pp_nexpr x)

type parsed_candidate = { source_index : int; source_exercise_id : int option; nexpr : nexpr }
type collapsed_candidate = { nexpr : nexpr; source_indices : int list; source_exercise_ids : int option list }
type hazel_compare_config = { hazel_cmd : string }
type hazel_compare_result = { parse_eval_ns : int; eval_only_ns : int; status : string; error : string }

let hazel_input_placeholder_name = "hazel_input_random_list"

let rec int_list_of_nexpr = function
  | NENil -> Some []
  | NECons (NEInt i, tl) -> Option.map (fun xs -> i :: xs) (int_list_of_nexpr tl)
  | _ -> None

let choose_largest_int_list (expr : nexpr) : int list option =
  let best = ref None in
  let consider ints =
    match !best with
    | None -> best := Some ints
    | Some prev when List.length ints > List.length prev -> best := Some ints
    | Some _ -> ()
  in
  let rec walk = function
    | NEInt _ | NETrue | NEFalse | NEUnit | NEHole | NENil | NEVar _ -> ()
    | NEAbs (_, body) -> walk body
    | NELet (_, bound, body) ->
        walk bound;
        walk body
    | NEFix (_, _, body) -> walk body
    | NEIf (c, t, e) ->
        walk c;
        walk t;
        walk e
    | NEMatchList (target, nil_case, _, _, cons_case) ->
        walk target;
        walk nil_case;
        walk cons_case
    | NEApp (f, a) ->
        walk f;
        walk a
    | NEAnd (l, r)
    | NEPlus (l, r)
    | NEMinus (l, r)
    | NELt (l, r)
    | NELe (l, r)
    | NEGt (l, r)
    | NEGe (l, r)
    | NESeq (l, r)
    | NEPair (l, r) ->
        walk l;
        walk r
    | NECons (_, _) as expr -> (
        match int_list_of_nexpr expr with
        | Some ints -> consider ints
        | None -> (
            match expr with
            | NECons (hd, tl) ->
                walk hd;
                walk tl
            | _ -> ()))
    | NEZro x | NEFst x -> walk x
  in
  walk expr;
  !best

let rewrite_large_int_lists ~(min_length : int) (expr : nexpr) : nexpr * bool =
  let rec go expr =
    match int_list_of_nexpr expr with
    | Some ints when List.length ints >= min_length -> (NEVar hazel_input_placeholder_name, true)
    | _ -> (
        match expr with
        | NEInt _ | NETrue | NEFalse | NEUnit | NEHole | NENil | NEVar _ -> (expr, false)
        | NEAbs (arg, body) ->
            let body', replaced = go body in
            (NEAbs (arg, body'), replaced)
        | NELet (name, bound, body) ->
            let bound', replaced_bound = go bound in
            let body', replaced_body = go body in
            (NELet (name, bound', body'), replaced_bound || replaced_body)
        | NEFix (name, arg, body) ->
            let body', replaced = go body in
            (NEFix (name, arg, body'), replaced)
        | NEIf (c, t, e) ->
            let c', rc = go c in
            let t', rt = go t in
            let e', re = go e in
            (NEIf (c', t', e'), rc || rt || re)
        | NEMatchList (target_expr, nil_case, head_name, tail_name, cons_case) ->
            let target_expr', r_target = go target_expr in
            let nil_case', r_nil = go nil_case in
            let cons_case', r_cons = go cons_case in
            (NEMatchList (target_expr', nil_case', head_name, tail_name, cons_case'), r_target || r_nil || r_cons)
        | NEApp (f, a) ->
            let f', rf = go f in
            let a', ra = go a in
            (NEApp (f', a'), rf || ra)
        | NEAnd (l, r) ->
            let l', rl = go l in
            let r', rr = go r in
            (NEAnd (l', r'), rl || rr)
        | NEPlus (l, r) ->
            let l', rl = go l in
            let r', rr = go r in
            (NEPlus (l', r'), rl || rr)
        | NEMinus (l, r) ->
            let l', rl = go l in
            let r', rr = go r in
            (NEMinus (l', r'), rl || rr)
        | NELt (l, r) ->
            let l', rl = go l in
            let r', rr = go r in
            (NELt (l', r'), rl || rr)
        | NELe (l, r) ->
            let l', rl = go l in
            let r', rr = go r in
            (NELe (l', r'), rl || rr)
        | NEGt (l, r) ->
            let l', rl = go l in
            let r', rr = go r in
            (NEGt (l', r'), rl || rr)
        | NEGe (l, r) ->
            let l', rl = go l in
            let r', rr = go r in
            (NEGe (l', r'), rl || rr)
        | NESeq (l, r) ->
            let l', rl = go l in
            let r', rr = go r in
            (NESeq (l', r'), rl || rr)
        | NEPair (l, r) ->
            let l', rl = go l in
            let r', rr = go r in
            (NEPair (l', r'), rl || rr)
        | NECons (hd, tl) ->
            let hd', rh = go hd in
            let tl', rt = go tl in
            (NECons (hd', tl'), rh || rt)
        | NEZro x ->
            let x', replaced = go x in
            (NEZro x', replaced)
        | NEFst x ->
            let x', replaced = go x in
            (NEFst x', replaced))
  in
  go expr

let extract_side_input_list (expr : nexpr) : nexpr * int list option =
  match choose_largest_int_list expr with
  | Some ints when List.length ints >= 6 ->
      let rewritten, replaced = rewrite_large_int_lists ~min_length:6 expr in
      if replaced then (rewritten, Some ints) else (expr, None)
  | _ -> (expr, None)

let collapse_adjacent_candidates (candidates : parsed_candidate list) : collapsed_candidate list =
  let rec loop current acc = function
    | [] -> List.rev (current :: acc)
    | (x : parsed_candidate) :: xs ->
        if NamedExpr.equal_nexpr x.nexpr current.nexpr then
          let merged =
            {
              current with
              source_indices = current.source_indices @ [ x.source_index ];
              source_exercise_ids = current.source_exercise_ids @ [ x.source_exercise_id ];
            }
          in
          loop merged acc xs
        else
          let next =
            { nexpr = x.nexpr; source_indices = [ x.source_index ]; source_exercise_ids = [ x.source_exercise_id ] }
          in
          loop next (current :: acc) xs
  in
  match candidates with
  | [] -> []
  | (x : parsed_candidate) :: xs ->
      let first =
        { nexpr = x.nexpr; source_indices = [ x.source_index ]; source_exercise_ids = [ x.source_exercise_id ] }
      in
      loop first [] xs

let parse_program_with_test ~program_path test =
  let programs = read_program_strings ~program_path in
  let last_expr = ref None in
  let parse_candidate i program =
    let sexp = parse ~candidate_index:i ~program_path program in
    let source_exercise_id, impl = sexp |> extract_program_with_meta in
    let expr = expr_of_sexp impl in
    last_expr := Some expr;
    let nexpr = nexpr_of_expr expr in
    { source_index = i; source_exercise_id; nexpr = nexpr |> subst_deepest_hole test |> clean }
  in
  let programs = programs |> List.mapi parse_candidate |> collapse_adjacent_candidates in
  (programs, !last_expr)

let hazel_compare_items_of_candidates ~(program_name : string) (candidates : collapsed_candidate list) : Yojson.Safe.t =
  `List
    (List.mapi
       (fun i candidate ->
         let source_nexpr, side_input_list = extract_side_input_list candidate.nexpr in
         let input_int_list_json =
           match side_input_list with None -> `Null | Some ints -> `List (List.map (fun x -> `Int x) ints)
         in
         let fields =
           [
             ("id", `String (Printf.sprintf "%s:%d" program_name i));
             ("source", `String (ToHazel.source_of_nexpr source_nexpr));
             ("input_int_list", input_int_list_json);
           ]
         in
         `Assoc fields)
       candidates)

let hazel_compare_result_of_yojson json : hazel_compare_result =
  let open Yojson.Safe.Util in
  let int_of_int_or_float member_name =
    match json |> member member_name with
    | `Int x -> x
    | `Float x -> int_of_float x
    | `Intlit x -> int_of_string x
    | _ -> failwith (Printf.sprintf "hazel compare field %s was not int/float" member_name)
  in
  {
    parse_eval_ns = int_of_int_or_float "parse_eval_ns";
    eval_only_ns = int_of_int_or_float "eval_only_ns";
    status = json |> member "status" |> to_string;
    error = json |> member "error" |> to_string;
  }

let run_hazel_compare ~program_name ~(candidates : collapsed_candidate list) ~(cfg : hazel_compare_config) :
    hazel_compare_result list =
  let candidates =
    match Sys.getenv_opt "ANT_HAZEL_COMPARE_MAX_CANDIDATES" with
    | None -> candidates
    | Some raw -> ( match int_of_string_opt raw with Some n when n >= 0 -> List.take n candidates | _ -> candidates)
  in
  let input_path = Filename.temp_file (program_name ^ "_hazel_batch") ".json" in
  let output_path = Filename.temp_file (program_name ^ "_hazel_batch_out") ".json" in
  Fun.protect
    ~finally:(fun () ->
      (try Sys.remove input_path with _ -> ());
      try Sys.remove output_path with _ -> ())
    (fun () ->
      let oc = open_out input_path in
      Fun.protect
        ~finally:(fun () -> close_out_noerr oc)
        (fun () ->
          hazel_compare_items_of_candidates ~program_name candidates |> Yojson.Safe.to_channel oc;
          output_char oc '\n');
      let quoted_input = Filename.quote input_path in
      let quoted_output = Filename.quote output_path in
      let cmd = Printf.sprintf "%s eval-batch %s --output %s" cfg.hazel_cmd quoted_input quoted_output in
      let status = Sys.command cmd in
      if status <> 0 then failwith (Printf.sprintf "hazel eval-batch failed for %s (exit=%d)" program_name status);
      let results_json = Yojson.Safe.from_file output_path in
      match results_json with
      | `List items -> List.map hazel_compare_result_of_yojson items
      | _ -> failwith (Printf.sprintf "hazel eval-batch output was not a JSON list for %s" program_name))

let run_with_test ?(hazel_compare = None) ~program_name ~program_path ~steps_file ~test =
  with_outchannel steps_file (fun oc ->
      RunLiveCommon.LC.populate_state ();
      let memo = Ant.Memo.init_memo () in
      let candidates, last_expr = parse_program_with_test ~program_path test in
      let indexed_candidates =
        candidates
        |> List.mapi (fun i candidate ->
            Format.printf "%s candidate %d: %a@." program_name i pp_nexpr candidate.nexpr;
            let expr = expr_of_nexpr candidate.nexpr in
            Format.printf "%s candidate %d expr: %a@." program_name i RunLiveCommon.pp_expr expr;
            (i, candidate, expr))
      in
      let hazel_compare_results =
        match hazel_compare with None -> None | Some cfg -> Some (run_hazel_compare ~program_name ~candidates ~cfg)
      in
      record_resting_heap_size ();
      let baseline_pass =
        indexed_candidates
        |> List.map (fun (i, candidate, expr) ->
            let baseline_result = eval_expression_baseline_only expr in
            (i, candidate, baseline_result))
      in
      record_resting_heap_size ();
      let memo_pass =
        indexed_candidates
        |> List.map (fun (i, candidate, expr) ->
            let memo_result = eval_expression_memo_only ~memo expr in
            Printf.printf "%s candidate %d value: %s\n" program_name i (value_to_string memo_result.value);
            (i, candidate, memo_result))
      in
      List.iter2
        (fun (memo_idx, memo_candidate, memo_result) (baseline_idx, baseline_candidate, baseline_result) ->
          if memo_idx <> baseline_idx then failwith "candidate order mismatch between memo and baseline passes";
          if memo_candidate.source_indices <> baseline_candidate.source_indices then
            failwith "candidate source mapping mismatch between memo and baseline passes";
          let source_indices = memo_candidate.source_indices in
          let source_first_index, source_last_index =
            match source_indices with
            | [] -> failwith "candidate source index list unexpectedly empty"
            | first :: _ -> (first, List.hd (List.rev source_indices))
          in
          let source_indices_json = `List (List.map (fun idx -> `Int idx) source_indices) in
          let source_exercise_ids_json =
            `List
              (List.map
                 (function Some exercise_id -> `Int exercise_id | None -> `Null)
                 memo_candidate.source_exercise_ids)
          in
          let hazel_fields =
            match hazel_compare_results with
            | None -> []
            | Some results -> (
                match List.nth_opt results memo_idx with
                | None -> []
                | Some result ->
                    [
                      ("hazel_parse_eval_ns", `Int result.parse_eval_ns);
                      ("hazel_eval_only_ns", `Int result.eval_only_ns);
                      ("hazel_status", `String result.status);
                      ("hazel_error", `String result.error);
                    ])
          in
          let extra_fields =
            [
              ("trace_exec_index", `Int memo_idx);
              ("trace_source_indices", source_indices_json);
              ("trace_source_count", `Int (List.length source_indices));
              ("trace_source_first_index", `Int source_first_index);
              ("trace_source_last_index", `Int source_last_index);
              ("trace_source_exercise_ids", source_exercise_ids_json);
            ]
            @ hazel_fields
          in
          write_steps_json_from_parts oc ~exec_res:memo_result.exec_res ~memo_profile:memo_result.memo_profile
            ~plain_profile:baseline_result.plain_profile ~cek_profile:baseline_result.cek_profile
            ~memo_heap_words:memo_result.memo_heap_words ~cek_heap_words:baseline_result.cek_heap_words ~extra_fields ())
        memo_pass baseline_pass;
      (match last_expr with
      | None -> failwith "why"
      | Some expr -> Format.printf "%s last candidate parsed expr: %a@." program_name pp_expr expr);
      write_memo_stats_json oc memo)
