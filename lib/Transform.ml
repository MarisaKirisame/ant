open Syntax
open Fresh
module Fresh = Fresh.Make ()

let empty_info = SynInfo.empty_info

let mk_fresh_pat prefix =
  let name = Fresh.next_fresh prefix in
  (PVar (name, empty_info), Var (name, empty_info))

let rec anf_prog ((stmts, info) : 'a prog) : 'a prog =
  Fresh.reset ();
  (List.map anf_stmt stmts, info)

and anf_stmt = function Type _ as stmt -> stmt | Term binding -> Term (anf_binding binding)

and anf_binding = function
  | BSeq (expr, info) -> BSeq (anf_tail expr, info)
  | BOne (pattern, expr, info) -> BOne (pattern, anf_tail expr, info)
  | BRec entries -> BRec (List.map (fun (pattern, expr, info) -> (pattern, anf_tail expr, info)) entries)
  | BCont (pattern, expr, info) -> BCont (pattern, anf_tail expr, info)
  | BRecC entries -> BRecC (List.map (fun (pattern, expr, info) -> (pattern, anf_tail expr, info)) entries)

and anf_atom = function
  | Lam (params, body, info) -> Lam (params, anf_tail body, info)
  | (Unit | Int _ | Float _ | Bool _ | Str _ | Builtin _ | Var _ | GVar _ | Ctor _) as expr -> expr
  | expr -> failwith ("not an atomic expr in anf_atom: " ^ string_of_document @@ pp_expr expr)

and bind_value expr k =
  match expr with
  | Unit | Int _ | Float _ | Bool _ | Str _ | Builtin _ | Var _ | GVar _ | Ctor _ | Lam _ -> k (anf_atom expr)
  | Let (binding, body, info) -> Let (anf_binding binding, bind_value body k, info)
  | If (cond, if_true, if_false, info) ->
      bind_value cond (fun cond' ->
          let join_pat, join_var = mk_fresh_pat "_'join" in
          let arg_pat, arg_var = mk_fresh_pat "_'arg" in
          Let
            ( BCont (join_pat, Lam ([ arg_pat ], k arg_var, empty_info), empty_info),
              If (cond', jump_to if_true join_var, jump_to if_false join_var, info),
              info ))
  | Match (scrutinee, MatchPattern cases, info) ->
      bind_value scrutinee (fun scrutinee' ->
          let join_pat, join_var = mk_fresh_pat "_'join" in
          let arg_pat, arg_var = mk_fresh_pat "_'arg" in
          let cases' = List.map (fun (pattern, body) -> (pattern, jump_to body join_var)) cases in
          Let
            ( BCont (join_pat, Lam ([ arg_pat ], k arg_var, empty_info), empty_info),
              Match (scrutinee', MatchPattern cases', info),
              info ))
  | Jump _ -> failwith "jump cannot appear in a value context"
  | expr ->
      let binding_pat, binding_var = mk_fresh_pat "_'anf" in
      Let (BOne (binding_pat, anf_tail expr, empty_info), k binding_var, empty_info)

and bind_values exprs k =
  match exprs with
  | [] -> k []
  | expr :: rest -> bind_value expr (fun expr' -> bind_values rest (fun rest' -> k (expr' :: rest')))

and jump_to expr join_var =
  match expr with
  | Let (binding, body, info) -> Let (anf_binding binding, jump_to body join_var, info)
  | If (cond, if_true, if_false, info) ->
      bind_value cond (fun cond' -> If (cond', jump_to if_true join_var, jump_to if_false join_var, info))
  | Match (scrutinee, MatchPattern cases, info) ->
      bind_value scrutinee (fun scrutinee' ->
          Match
            (scrutinee', MatchPattern (List.map (fun (pattern, body) -> (pattern, jump_to body join_var)) cases), info))
  | Jump (target, args, info) ->
      bind_value target (fun target' -> bind_values args (fun args' -> Jump (target', args', info)))
  | expr -> bind_value expr (fun expr' -> Jump (join_var, [ expr' ], empty_info))

and anf_tail expr =
  match expr with
  | Unit | Int _ | Float _ | Bool _ | Str _ | Builtin _ | Var _ | GVar _ | Ctor _ | Lam _ -> anf_atom expr
  | Let (binding, body, info) -> Let (anf_binding binding, anf_tail body, info)
  | App (fn, args, info) -> bind_value fn (fun fn' -> bind_values args (fun args' -> App (fn', args', info)))
  | Jump (target, args, info) ->
      bind_value target (fun target' -> bind_values args (fun args' -> Jump (target', args', info)))
  | Op (op, lhs, rhs, info) -> bind_value lhs (fun lhs' -> bind_value rhs (fun rhs' -> Op (op, lhs', rhs', info)))
  | Tup (values, info) -> bind_values values (fun values' -> Tup (values', info))
  | Arr (values, info) -> bind_values values (fun values' -> Arr (values', info))
  | Sel (target, field, info) -> bind_value target (fun target' -> Sel (target', field, info))
  | If (cond, if_true, if_false, info) ->
      bind_value cond (fun cond' -> If (cond', anf_tail if_true, anf_tail if_false, info))
  | Match (scrutinee, MatchPattern cases, info) ->
      bind_value scrutinee (fun scrutinee' ->
          Match (scrutinee', MatchPattern (List.map (fun (pattern, body) -> (pattern, anf_tail body)) cases), info))
