open Syntax
module Fresh = Fresh.Make ()

let empty_info = SynInfo.empty_info
let ( let* ) a b = a b

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

and bind_computation expr k =
  let binding_pat, binding_var = mk_fresh_pat "_'anf" in
  Let (BOne (binding_pat, expr, empty_info), k binding_var, empty_info)

and bind_value expr k =
  match expr with
  | Unit | Int _ | Float _ | Bool _ | Str _ | Builtin _ | Var _ | GVar _ | Ctor _ | Lam _ -> k (anf_atom expr)
  | Let (binding, body, info) -> Let (anf_binding binding, bind_value body k, info)
  | App (fn, args, info) ->
      let* fn' = bind_value fn in
      let* args' = bind_values args in
      bind_computation (App (fn', args', info)) k
  | Op (op, lhs, rhs, info) ->
      let* lhs' = bind_value lhs in
      let* rhs' = bind_value rhs in
      bind_computation (Op (op, lhs', rhs', info)) k
  | Tup (values, info) ->
      let* values' = bind_values values in
      bind_computation (Tup (values', info)) k
  | Arr (values, info) ->
      let* values' = bind_values values in
      bind_computation (Arr (values', info)) k
  | Sel (target, field, info) ->
      let* target' = bind_value target in
      bind_computation (Sel (target', field, info)) k
  | If (cond, if_true, if_false, info) ->
      let* cond' = bind_value cond in
      let join_pat, join_var = mk_fresh_pat "_'join" in
      let arg_pat, arg_var = mk_fresh_pat "_'arg" in
      Let
        ( BCont (join_pat, Lam ([ arg_pat ], k arg_var, empty_info), empty_info),
          If (cond', jump_to if_true join_var, jump_to if_false join_var, info),
          info )
  | Match (scrutinee, MatchPattern cases, info) ->
      let* scrutinee' = bind_value scrutinee in
      let join_pat, join_var = mk_fresh_pat "_'join" in
      let arg_pat, arg_var = mk_fresh_pat "_'arg" in
      let cases' = List.map (fun (pattern, body) -> (pattern, jump_to body join_var)) cases in
      Let
        ( BCont (join_pat, Lam ([ arg_pat ], k arg_var, empty_info), empty_info),
          Match (scrutinee', MatchPattern cases', info),
          info )
  | Jump _ -> failwith "jump cannot appear in a value context"

and bind_values exprs k =
  match exprs with
  | [] -> k []
  | expr :: rest ->
      let* expr' = bind_value expr in
      let* rest' = bind_values rest in
      k (expr' :: rest')

and jump_to expr join_var =
  match expr with
  | Let (binding, body, info) -> Let (anf_binding binding, jump_to body join_var, info)
  | If (cond, if_true, if_false, info) ->
      let* cond' = bind_value cond in
      If (cond', jump_to if_true join_var, jump_to if_false join_var, info)
  | Match (scrutinee, MatchPattern cases, info) ->
      let* scrutinee' = bind_value scrutinee in
      Match (scrutinee', MatchPattern (List.map (fun (pattern, body) -> (pattern, jump_to body join_var)) cases), info)
  | Jump (target, args, info) ->
      let* target' = bind_value target in
      let* args' = bind_values args in
      Jump (target', args', info)
  | expr ->
      let* expr' = bind_value expr in
      Jump (join_var, [ expr' ], empty_info)

and anf_tail expr =
  match expr with
  | Unit | Int _ | Float _ | Bool _ | Str _ | Builtin _ | Var _ | GVar _ | Ctor _ | Lam _ -> anf_atom expr
  | Let (binding, body, info) -> Let (anf_binding binding, anf_tail body, info)
  | App (fn, args, info) ->
      let* fn' = bind_value fn in
      let* args' = bind_values args in
      App (fn', args', info)
  | Jump (target, args, info) ->
      let* target' = bind_value target in
      let* args' = bind_values args in
      Jump (target', args', info)
  | Op (op, lhs, rhs, info) ->
      let* lhs' = bind_value lhs in
      let* rhs' = bind_value rhs in
      Op (op, lhs', rhs', info)
  | Tup (values, info) ->
      let* values' = bind_values values in
      Tup (values', info)
  | Arr (values, info) ->
      let* values' = bind_values values in
      Arr (values', info)
  | Sel (target, field, info) ->
      let* target' = bind_value target in
      Sel (target', field, info)
  | If (cond, if_true, if_false, info) ->
      let* cond' = bind_value cond in
      If (cond', anf_tail if_true, anf_tail if_false, info)
  | Match (scrutinee, MatchPattern cases, info) ->
      let* scrutinee' = bind_value scrutinee in
      Match (scrutinee', MatchPattern (List.map (fun (pattern, body) -> (pattern, anf_tail body)) cases), info)
