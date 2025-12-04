(* Reads the Hazel-generated mk_program JSON blob (list of s-expr strings)
   and parses it into OCaml S-expressions. *)

open Yojson.Safe
open Yojson.Safe.Util
open NamedExpr
module Sexp = Sexplib.Sexp

let program_path = "data/mk_program.json"

let read_program_strings () =
  if not (Sys.file_exists program_path) then invalid_arg (Printf.sprintf "program file not found at %s" program_path);
  from_file program_path |> to_list |> List.map to_string

type expr =
  | Let of expr * expr * expr
  | If of expr * expr * expr
  | EmptyHole
  | MultiHole
  | Fun of expr * expr
  | TuplePat of expr list
  | VarPat of string
  | Var of string
  | Atom of expr
  | Bool of bool
  | Int of int
  | BinOp of string * expr * expr
  | Match of expr * (expr * expr) list
  | ListPat of expr list
  | Tuple of expr list
  | ConsPat of expr * expr
  | Wild
  | PMultiHole
  | Ap of expr * expr
  | Cons of expr * expr

let rec expr_of_sexp = function
  | Sexp.List [ Sexp.Atom "Let"; lhs; rhs; body ] -> Let (expr_of_sexp lhs, expr_of_sexp rhs, expr_of_sexp body)
  | Sexp.List [ Sexp.Atom "If"; cond; thn; els ] -> If (expr_of_sexp cond, expr_of_sexp thn, expr_of_sexp els)
  | Sexp.Atom "EmptyHole" -> EmptyHole
  | Sexp.Atom "MultiHole" -> MultiHole
  | Sexp.List [ Sexp.Atom "Fun"; param; body ] -> Fun (expr_of_sexp param, expr_of_sexp body)
  | Sexp.List [ Sexp.Atom "TuplePat"; Sexp.List elems ] -> TuplePat (List.map expr_of_sexp elems)
  | Sexp.List [ Sexp.Atom "VarPat"; Sexp.Atom v ] -> VarPat v
  | Sexp.List [ Sexp.Atom "Var"; Sexp.Atom v ] -> Var v
  | Sexp.List [ Sexp.Atom "Atom"; payload ] -> Atom (expr_of_sexp payload)
  | Sexp.List [ Sexp.Atom "Bool"; Sexp.Atom b ] -> Bool (bool_of_string b)
  | Sexp.List [ Sexp.Atom "Int"; Sexp.Atom i ] -> Int (int_of_string i)
  | Sexp.List [ Sexp.Atom "BinOp"; Sexp.Atom op; lhs; rhs ] -> BinOp (op, expr_of_sexp lhs, expr_of_sexp rhs)
  | Sexp.List [ Sexp.Atom "Match"; scrut; Sexp.List cases ] ->
      Match
        ( expr_of_sexp scrut,
          List.map
            (function
              | Sexp.List [ pat; expr ] -> (expr_of_sexp pat, expr_of_sexp expr)
              | sexp -> failwith (Printf.sprintf "Unrecognized match case s-expression: %s" (Sexp.to_string_hum sexp)))
            cases )
  | Sexp.List [ Sexp.Atom "ListPat"; Sexp.List elems ] -> ListPat (List.map expr_of_sexp elems)
  | Sexp.Atom "Wild" -> Wild
  | Sexp.List [ Sexp.Atom "ConsPat"; head; tail ] -> ConsPat (expr_of_sexp head, expr_of_sexp tail)
  | Sexp.Atom "PMultiHole" -> PMultiHole
  | Sexp.List [ Sexp.Atom "Ap"; Sexp.Atom "Forward"; func; arg ] -> Ap (expr_of_sexp func, expr_of_sexp arg)
  | Sexp.List [ Sexp.Atom "Tuple"; Sexp.List elems ] -> Tuple (List.map expr_of_sexp elems)
  | Sexp.List [ Sexp.Atom "Cons"; head; tail ] -> Cons (expr_of_sexp head, expr_of_sexp tail)
  | sexp -> failwith (Printf.sprintf "Unrecognized expression s-expression: %s" (Sexp.to_string_hum sexp))

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
  | Ap (f, a) -> Format.fprintf fmt "(Ap %a %a)" pp_expr f pp_expr a
  | Cons (h, t) -> Format.fprintf fmt "(Cons %a %a)" pp_expr h pp_expr t

let rec nexpr_of_expr_aux e names : nexpr =
  let fresh_name () =
    let n = !names in
    names := n + 1;
    "x" ^ string_of_int n
  in
  match e with
  | Let (VarPat name, bound, body) -> NELet (name, nexpr_of_expr_aux bound names, nexpr_of_expr_aux body names)
  | Fun (TuplePat params, body) ->
      let rec build_nested_fun params body =
        match params with
        | [] -> nexpr_of_expr_aux body names
        | VarPat name :: rest -> NEAbs (name, build_nested_fun rest body)
        | _ -> failwith "Only VarPat supported in function parameters"
      in
      build_nested_fun params body
  | EmptyHole -> NEHole
  | _ -> failwith ("nexpr_of_expr not implemented for expr: " ^ Format.asprintf "%a" pp_expr e)

let nexpr_of_expr e = nexpr_of_expr_aux e (ref 0)

let process sexp =
  match sexp with
  | Sexp.List [ Sexp.Atom "exercise"; Sexp.Atom "1"; _prelude; impl; _tests ] -> nexpr_of_expr (expr_of_sexp impl)
  | _ -> invalid_arg (Printf.sprintf "Expected (exercise 1 <focus> <impl> <test>), got %s" (Sexp.to_string_hum sexp))

let parse_program () = read_program_strings () |> List.map (fun s -> s |> Sexp.of_string |> process)
let run () = parse_program () |> List.iter (fun expr -> Format.printf "%a@." pp_nexpr expr)
