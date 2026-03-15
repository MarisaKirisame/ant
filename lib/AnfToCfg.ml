open Syntax
open ControlFlowGraph
module StrMap = Map.Make (String)

type join_ref = { binder_id : value_id; block_id : block_id }
type env = { values : value_id StrMap.t; joins : join_ref StrMap.t }

type block_builder = {
  id : block_id;
  label : string;
  kind : block_kind;
  params : value_id list;
  mutable stmts_rev : stmt list;
  mutable term : terminator option;
}

type lower_state = {
  mutable next_value_id : int;
  mutable next_block_id : int;
  mutable next_function_id : int;
  mutable next_toplevel_id : int;
}

type func_ctx = {
  state : lower_state;
  name : string;
  id : function_id;
  mutable values_rev : value_info list;
  mutable blocks_rev : block list;
}

let failf fmt = Printf.ksprintf failwith fmt
let unsupported construct = failf "AnfToCfg unsupported: %s" construct
let new_state () = { next_value_id = 0; next_block_id = 0; next_function_id = 0; next_toplevel_id = 0 }
let new_env () = { values = StrMap.empty; joins = StrMap.empty }

let fresh_function_id (state : lower_state) =
  let id = state.next_function_id in
  state.next_function_id <- id + 1;
  id

let add_value (ctx : func_ctx) (name : string) (kind : value_kind) : value_id =
  let id = ctx.state.next_value_id in
  ctx.state.next_value_id <- id + 1;
  ctx.values_rev <- { id; name; kind } :: ctx.values_rev;
  id

let add_block_id (ctx : func_ctx) =
  let id = ctx.state.next_block_id in
  ctx.state.next_block_id <- id + 1;
  id

let new_builder (ctx : func_ctx) (label : string) (kind : block_kind) (params : value_id list) : block_builder =
  { id = add_block_id ctx; label; kind; params; stmts_rev = []; term = None }

let emit (builder : block_builder) (stmt : stmt) : unit = builder.stmts_rev <- stmt :: builder.stmts_rev

let seal_block (ctx : func_ctx) (builder : block_builder) (term : terminator) : unit =
  if Option.is_some builder.term then failf "block b%d sealed twice" builder.id;
  builder.term <- Some term;
  ctx.blocks_rev <-
    {
      id = builder.id;
      label = builder.label;
      kind = builder.kind;
      params = builder.params;
      body = List.rev builder.stmts_rev;
      term;
    }
    :: ctx.blocks_rev

let lookup_value (env : env) (name : string) : value_id =
  match StrMap.find_opt name env.values with Some id -> id | None -> failf "AnfToCfg: unbound local `%s`" name

let lookup_join (env : env) (name : string) : join_ref =
  match StrMap.find_opt name env.joins with
  | Some join_ref -> join_ref
  | None -> failf "AnfToCfg: jump target `%s` is not a known join binder" name

let operand_of_atom (env : env) (expr : 'a expr) : operand =
  match expr with
  | Unit -> OUnit
  | Int i -> OInt i
  | Float f -> OFloat f
  | Bool b -> OBool b
  | Str s -> OString s
  | Builtin (Builtin name, _) -> OBuiltin name
  | Var (name, _) ->
      if StrMap.mem name env.joins then failf "AnfToCfg: join binder `%s` cannot be used as a value" name;
      OLocal (lookup_value env name)
  | GVar (name, _) -> OGlobal name
  | Ctor (name, _) -> OCtor name
  | Lam _ -> unsupported "nested lambda literal"
  | App _ | Jump _ | Op _ | Tup _ | Arr _ | Let _ | Sel _ | If _ | Match _ ->
      failf "AnfToCfg: expected ANF atom, got `%s`" (string_of_document @@ pp_expr expr)

let call_target_of_expr (env : env) (expr : 'a expr) : call_target =
  match expr with GVar (name, _) -> Direct name | _ -> Indirect (operand_of_atom env expr)

let rhs_of_simple_expr (env : env) (expr : 'a expr) : rhs =
  match expr with
  | Unit | Int _ | Float _ | Bool _ | Str _ | Builtin _ | Var _ | GVar _ | Ctor _ -> Move (operand_of_atom env expr)
  | App (Ctor (name, _), args, _) -> Construct (name, List.map (operand_of_atom env) args)
  | App (fn, args, _) -> Call (call_target_of_expr env fn, List.map (operand_of_atom env) args)
  | Op (op, lhs, rhs, _) -> BinOp (op, operand_of_atom env lhs, operand_of_atom env rhs)
  | Tup (values, _) -> Tuple (List.map (operand_of_atom env) values)
  | Arr (values, _) -> Array (List.map (operand_of_atom env) values)
  | Sel (target, field, _) -> Select (operand_of_atom env target, field)
  | Lam _ -> unsupported "nested lambda literal"
  | Let _ | If _ | Match _ | Jump _ ->
      failf "AnfToCfg: non-tail control flow in simple binding `%s`" (string_of_document @@ pp_expr expr)

let lower_var_param (ctx : func_ctx) (kind : value_kind) (pat : 'a pattern) : string * value_id =
  match pat with
  | PVar (name, _) -> (name, add_value ctx name kind)
  | _ -> unsupported "only variable binders are accepted for function/join parameters in Phase 1"

let lower_let_var (ctx : func_ctx) (pat : 'a pattern) : string * value_id =
  match pat with PVar (name, _) -> (name, add_value ctx name Local) | _ -> unsupported "non-variable let binder"

let unsupported_match_pattern pat =
  failf "AnfToCfg unsupported: complex match pattern `%s`; only patterns accepted by CompileMemo are supported"
    (string_of_document @@ Syntax.pp_pattern pat)

let lower_match_binder (ctx : func_ctx) (pat : 'a pattern) : ir_pattern * (string * value_id) list =
  match pat with
  | PAny -> (RPAny, [])
  | PVar (name, _) ->
      let id = add_value ctx name MatchBinder in
      (RPBind id, [ (name, id) ])
  | _ -> unsupported_match_pattern pat

let lower_match_tuple_payload (ctx : func_ctx) (patterns : 'a pattern list) : ir_pattern * (string * value_id) list =
  let patterns_rev, bindings_rev =
    List.fold_left
      (fun (patterns_acc, bindings_acc) pat ->
        let pat', bindings = lower_match_binder ctx pat in
        (pat' :: patterns_acc, List.rev_append bindings bindings_acc))
      ([], []) patterns
  in
  (RPTuple (List.rev patterns_rev), List.rev bindings_rev)

let lower_match_pattern (ctx : func_ctx) (pat : 'a pattern) : ir_pattern * (string * value_id) list =
  match pat with
  | PAny | PVar _ -> lower_match_binder ctx pat
  | PCtorApp (ctor, None, _) -> (RPCtor (ctor, None), [])
  | PCtorApp (ctor, Some ((PVar _ | PAny) as payload), _) ->
      let payload', bindings = lower_match_binder ctx payload in
      (RPCtor (ctor, Some payload'), bindings)
  | PCtorApp (ctor, Some (PTup (patterns, _)), _)
    when List.for_all (function PAny | PVar _ -> true | _ -> false) patterns ->
      let payload', bindings = lower_match_tuple_payload ctx patterns in
      (RPCtor (ctor, Some payload'), bindings)
  | PCtorApp (_, Some payload, _) -> unsupported_match_pattern payload
  | _ -> unsupported_match_pattern pat

let value_ids_of_bindings bindings = List.map snd bindings

let env_with_values (env : env) (bindings : (string * value_id) list) : env =
  { env with values = List.fold_left (fun acc (name, id) -> StrMap.add name id acc) env.values bindings }

let env_with_joins (env : env) (joins : (string * join_ref) list) : env =
  { env with joins = List.fold_left (fun acc (name, join_ref) -> StrMap.add name join_ref acc) env.joins joins }

let return_simple_expr (ctx : func_ctx) (env : env) (builder : block_builder) (expr : 'a expr) : unit =
  match expr with
  | Unit | Int _ | Float _ | Bool _ | Str _ | Builtin _ | Var _ | GVar _ | Ctor _ ->
      seal_block ctx builder (Return (operand_of_atom env expr))
  | App _ | Op _ | Tup _ | Arr _ | Sel _ ->
      let tmp = add_value ctx "_ret" Temp in
      emit builder (Bind (tmp, rhs_of_simple_expr env expr));
      seal_block ctx builder (Return (OLocal tmp))
  | Lam _ -> unsupported "nested lambda literal"
  | Let _ | If _ | Match _ | Jump _ ->
      failf "AnfToCfg: expected simple tail expression, got `%s`" (string_of_document @@ pp_expr expr)

let rec lower_tail_expr (ctx : func_ctx) (env : env) (builder : block_builder) (expr : 'a expr) : unit =
  match expr with
  | Let (binding, body, _) ->
      let env' = lower_binding ctx env builder binding in
      lower_tail_expr ctx env' builder body
  | If (cond, if_true, if_false, _) ->
      let cond_op = operand_of_atom env cond in
      let then_builder = new_builder ctx (builder.label ^ ".then") IfThen [] in
      let else_builder = new_builder ctx (builder.label ^ ".else") IfElse [] in
      seal_block ctx builder (Branch (cond_op, then_builder.id, else_builder.id));
      lower_tail_expr ctx env then_builder if_true;
      lower_tail_expr ctx env else_builder if_false
  | Match (scrutinee, MatchPattern cases, _) ->
      let scrutinee_op = operand_of_atom env scrutinee in
      let arms =
        List.mapi
          (fun index (pattern, arm_expr) ->
            let pattern', bindings = lower_match_pattern ctx pattern in
            let params = value_ids_of_bindings bindings in
            let arm_builder = new_builder ctx (Printf.sprintf "%s.match%d" builder.label index) MatchArm params in
            let arm_env = env_with_values env bindings in
            lower_tail_expr ctx arm_env arm_builder arm_expr;
            { pattern = pattern'; block = arm_builder.id })
          cases
      in
      seal_block ctx builder (Match (scrutinee_op, arms))
  | Jump (Var (name, _), args, _) ->
      let join_ref = lookup_join env name in
      let args = List.map (operand_of_atom env) args in
      seal_block ctx builder (Jump (join_ref.block_id, args))
  | Jump (target, _, _) ->
      failf "AnfToCfg: jump target must be a known join variable, got `%s`" (string_of_document @@ pp_expr target)
  | Unit | Int _ | Float _ | Bool _ | Str _ | Builtin _ | Var _ | GVar _ | Ctor _ | App _ | Op _ | Tup _ | Arr _ | Sel _
    ->
      return_simple_expr ctx env builder expr
  | Lam _ -> unsupported "tail lambda literal"

and lower_binding (ctx : func_ctx) (env : env) (builder : block_builder) (binding : 'a binding) : env =
  match binding with
  | BSeq (expr, _) ->
      let tmp = add_value ctx "_seq" Temp in
      emit builder (Bind (tmp, rhs_of_simple_expr env expr));
      env
  | BOne (pat, expr, _) ->
      let name, id = lower_let_var ctx pat in
      emit builder (Bind (id, rhs_of_simple_expr env expr));
      env_with_values env [ (name, id) ]
  | BCont (pat, expr, _) ->
      let env', _ = lower_nonrec_join ctx env pat expr in
      env'
  | BRecC entries ->
      let env', _ = lower_rec_joins ctx env entries in
      env'
  | BRec _ -> unsupported "nested recursive function group"

and lower_nonrec_join (ctx : func_ctx) (env : env) (pat : 'a pattern) (expr : 'a expr) : env * join_ref =
  match (pat, expr) with
  | PVar (name, _), Lam (params, body, _) ->
      let binder_id = add_value ctx name JoinBinder in
      let param_bindings = List.map (lower_var_param ctx JoinParam) params in
      let builder = new_builder ctx name (Join binder_id) (List.map snd param_bindings) in
      let body_env = env_with_values env param_bindings in
      lower_tail_expr ctx body_env builder body;
      let join_ref = { binder_id; block_id = builder.id } in
      (env_with_joins env [ (name, join_ref) ], join_ref)
  | _ -> unsupported "BCont must have the shape `PVar = Lam`"

and lower_rec_joins (ctx : func_ctx) (env : env) (entries : ('a pattern * 'a expr * 'a) list) : env * join_ref list =
  let specs =
    List.map
      (function
        | PVar (name, _), Lam (params, body, _), _ ->
            let binder_id = add_value ctx name JoinBinder in
            (name, binder_id, params, body)
        | _ -> unsupported "BRecC entries must have the shape `PVar = Lam`")
      entries
  in
  let builders : (string * value_id * (string * value_id) list * block_builder) list =
    List.map
      (fun (name, binder_id, params, _body) ->
        let param_bindings = List.map (lower_var_param ctx JoinParam) params in
        let builder = new_builder ctx name (Join binder_id) (List.map snd param_bindings) in
        (name, binder_id, param_bindings, builder))
      specs
  in
  let join_refs =
    List.map
      (fun (name, binder_id, _params, (builder : block_builder)) -> (name, { binder_id; block_id = builder.id }))
      builders
  in
  let env_with_group = env_with_joins env join_refs in
  List.iter2
    (fun (_name, _binder_id, _params, body) (_name', _binder_id', param_bindings, (builder : block_builder)) ->
      let body_env = env_with_values env_with_group param_bindings in
      lower_tail_expr ctx body_env builder body)
    specs builders;
  (env_with_group, List.map snd join_refs)

let lower_unit (state : lower_state) (kind : unit_kind) (name : string) (params : 'a pattern list) (body : 'a expr) :
    unit_ir =
  let ctx = { state; name; id = fresh_function_id state; values_rev = []; blocks_rev = [] } in
  let param_bindings = List.map (lower_var_param ctx Param) params in
  let entry_builder = new_builder ctx (name ^ ".entry") Entry (List.map snd param_bindings) in
  let env = env_with_values (new_env ()) param_bindings in
  lower_tail_expr ctx env entry_builder body;
  {
    id = ctx.id;
    name;
    kind;
    entry = entry_builder.id;
    params = entry_builder.params;
    values = List.rev ctx.values_rev;
    blocks = List.rev ctx.blocks_rev;
  }

let lower_toplevel_binding (state : lower_state) (binding : 'a binding) : unit_ir list =
  match binding with
  | BOne (PVar (name, _), Lam (params, body, _), _) -> [ lower_unit state Function name params body ]
  | BOne (PVar (name, _), expr, _) -> [ lower_unit state Global name [] expr ]
  | BOne _ -> unsupported "top-level BOne must bind a variable"
  | BRec entries ->
      List.map
        (function
          | PVar (name, _), Lam (params, body, _), _ -> lower_unit state Function name params body
          | _ -> unsupported "top-level recursive bindings must have the shape `PVar = Lam`")
        entries
  | BSeq (expr, _) ->
      let name = Printf.sprintf "__toplevel_%d" state.next_toplevel_id in
      state.next_toplevel_id <- state.next_toplevel_id + 1;
      [ lower_unit state Global name [] expr ]
  | BCont _ -> unsupported "top-level BCont"
  | BRecC _ -> unsupported "top-level BRecC"

let lower_prog ((stmts, _) : 'a prog) : prog_ir =
  let state = new_state () in
  let units =
    List.fold_left
      (fun acc stmt -> match stmt with Type _ -> acc | Term binding -> acc @ lower_toplevel_binding state binding)
      [] stmts
  in
  { units }
