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
  | Sexp.List [ Sexp.Atom "Tuple"; Sexp.List elems ] -> Tuple (List.map expr_of_sexp_helper elems)
  | Sexp.List [ Sexp.Atom "TupLabel"; x; y ] -> expr_of_sexp_helper y
  | Sexp.List [ Sexp.Atom "Cons"; head; tail ] -> Cons (expr_of_sexp_helper head, expr_of_sexp_helper tail)
  | Sexp.List [ Sexp.Atom "ListLit"; Sexp.List elems ] -> ListLit (List.map expr_of_sexp_helper elems)
  | Sexp.List [ Sexp.Atom "LabelPat"; Sexp.Atom str ] -> LabelPat str
  | Sexp.List [ Sexp.Atom "Constructor"; _ ] -> EmptyHole
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

type case = { patterns : expr list; body : nexpr; letlist : (string * nexpr) list }

let rec subst (lhs : string) (rhs : nexpr) (expr : nexpr) =
  match expr with
  | NEInt _ | NETrue | NEFalse | NEUnit | NEHole -> expr
  | NEVar v -> if v = lhs then rhs else expr
  | NEApp (f, a) -> NEApp (subst lhs rhs f, subst lhs rhs a)
  | NEAnd (l, r) -> NEAnd (subst lhs rhs l, subst lhs rhs r)
  | NECons (h, t) -> NECons (subst lhs rhs h, subst lhs rhs t)
  | NELet (name, binding, body) ->
      assert (name != lhs);
      NELet (name, subst lhs rhs binding, subst lhs rhs body)
  | NEFix (name, arg, body) -> NEFix (name, arg, subst lhs rhs body)
  | NEAbs (arg, body) -> NEAbs (arg, subst lhs rhs body)
  | NELt (x, y) -> NELt (subst lhs rhs x, subst lhs rhs y)
  | NEGe (x, y) -> NEGe (subst lhs rhs x, subst lhs rhs y)
  | NELe (x, y) -> NELe (subst lhs rhs x, subst lhs rhs y)
  | NEIf (i, t, e) -> NEIf (subst lhs rhs i, subst lhs rhs t, subst lhs rhs e)
  | _ -> failwith ("subst not implemented for: " ^ Format.asprintf "%a" pp_nexpr expr)

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
  | PEmptyHole _ -> acc
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
            | PEmptyHole -> "WILD"
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
  | Let (PEmptyHole, _, _) -> NEHole
  (*todo: use the new tuple mechanism*)
  | Fun (TuplePat params, body) ->
      let rec build_nested_fun params body =
        match params with
        | [] -> nexpr_of_expr_aux body names
        | VarPat name :: rest -> NEAbs (name, build_nested_fun rest body)
        | PEmptyHole :: rest -> NEAbs ("WILD", build_nested_fun rest body)
        | TupLabelPat _ :: rest -> NEHole
        | p :: rest ->
            failwith ("Only VarPat supported in function parameters, but got: " ^ Format.asprintf "%a" pp_expr p)
      in
      build_nested_fun params body
  | Fun (VarPat name, body) -> NEAbs (name, nexpr_of_expr_aux body names)
  | Fun (PEmptyHole, body) -> NEHole
  | Fun (PMultiHole, body) -> NEHole
  | Fun (Wild, body) -> NEAbs ("WILD", nexpr_of_expr_aux body names)
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
      | _ -> failwith ("nexpr_of_expr not implemented for binop: " ^ op))
  | EmptyHole | MultiHole -> NEHole
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
  | NEMatchList (target, nil_case, head_name, tail_name, cons_case) ->
      NEMatchList
        ( clean_aux target seen,
          clean_aux nil_case seen,
          head_name,
          tail_name,
          clean_aux cons_case (tail_name :: head_name :: seen) )
  | NEApp (f, a) -> NEApp (clean_aux f seen, clean_aux a seen)
  | NECons (h, t) -> NECons (clean_aux h seen, clean_aux t seen)
  | NEFix (name, param, body) -> NEFix (name, param, clean_aux body (param :: name :: seen))
  | _ -> failwith ("clean not implemented for expr: " ^ Format.asprintf "%a" pp_nexpr x)

let clean x = clean_aux x []

let rec subst_deepest_hole y x =
  match x with
  | NEHole -> y
  | NELet (name, bound, body) -> NELet (name, bound, subst_deepest_hole y body)
  | _ -> failwith ("subst_deepest_hole not implemented for expr: " ^ Format.asprintf "%a" pp_nexpr x)

let parse_program_with_test ~program_path test =
  let programs = read_program_strings ~program_path in
  let last_expr = ref None in
  let parse_candidate i program =
    let sexp = parse ~candidate_index:i ~program_path program in
    let impl = sexp |> extract_program in
    let expr = expr_of_sexp impl in
    last_expr := Some expr;
    let nexpr = nexpr_of_expr expr in
    nexpr |> subst_deepest_hole test |> clean
  in
  let programs = programs |> List.mapi parse_candidate |> dedup NamedExpr.equal_nexpr in
  (programs, !last_expr)

let run_with_test ~program_name ~program_path ~steps_file ~test =
  with_outchannel steps_file (fun oc ->
      let write_steps = write_steps_json oc in
      RunLiveCommon.LC.populate_state ();
      let memo = Ant.Memo.init_memo () in
      let eval expr = eval_expression ~memo ~write_steps expr in
      let candidates, last_expr = parse_program_with_test ~program_path test in
      candidates
      |> List.iteri (fun i nexpr ->
          Format.printf "%s candidate %d: %a@." program_name i pp_nexpr nexpr;
          let expr = expr_of_nexpr nexpr in
          Format.printf "%s candidate %d expr: %a@." program_name i RunLiveCommon.pp_expr expr;
          let value = eval expr in
          Printf.printf "%s candidate %d value: %s\n" program_name i (value_to_string value));
      (match last_expr with
      | None -> failwith "why"
      | Some expr -> Format.printf "%s last candidate parsed expr: %a@." program_name pp_expr expr);
      write_memo_stats_json oc memo)
