open PPrint
open Syntax

module StrMap = Map.Make (String)

type value_id = int
type block_id = int
type function_id = int

type value_kind =
  | Param
  | Local
  | Temp
  | JoinBinder
  | JoinParam
  | MatchBinder

type value_info = {
  id : value_id;
  name : string;
  kind : value_kind;
}

type operand =
  | OLocal of value_id
  | OGlobal of string
  | OBuiltin of string
  | OInt of int
  | OFloat of float
  | OBool of bool
  | OString of string
  | OUnit
  | OCtor of string

type call_target =
  | Direct of string
  | Indirect of operand

type rhs =
  | Move of operand
  | Call of call_target * operand list
  | Construct of string * operand list
  | Tuple of operand list
  | Array of operand list
  | Select of operand * field
  | BinOp of string * operand * operand

type stmt = Bind of value_id * rhs

type ir_pattern =
  | RPAny
  | RPInt of int
  | RPBool of bool
  | RPUnit
  | RPBind of value_id
  | RPTuple of ir_pattern list
  | RPCtor of string * ir_pattern option

type match_arm = {
  pattern : ir_pattern;
  block : block_id;
}

type terminator =
  | Jump of block_id * operand list
  | Branch of operand * block_id * block_id
  | Match of operand * match_arm list
  | Return of operand

type block_kind =
  | Entry
  | Join of value_id
  | IfThen
  | IfElse
  | MatchArm

type block = {
  id : block_id;
  label : string;
  kind : block_kind;
  params : value_id list;
  body : stmt list;
  term : terminator;
}

type unit_kind =
  | Function
  | Global

type unit_ir = {
  id : function_id;
  name : string;
  kind : unit_kind;
  entry : block_id;
  params : value_id list;
  values : value_info list;
  blocks : block list;
}

type prog_ir = { units : unit_ir list }

type join_ref = {
  binder_id : value_id;
  block_id : block_id;
}

type env = {
  values : value_id StrMap.t;
  joins : join_ref StrMap.t;
}

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

let unsupported construct = failf "CompileRegMemo Phase 1 unsupported: %s" construct

let new_state () =
  {
    next_value_id = 0;
    next_block_id = 0;
    next_function_id = 0;
    next_toplevel_id = 0;
  }

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
  match StrMap.find_opt name env.values with
  | Some id -> id
  | None -> failf "CompileRegMemo Phase 1: unbound local `%s`" name

let lookup_join (env : env) (name : string) : join_ref =
  match StrMap.find_opt name env.joins with
  | Some join_ref -> join_ref
  | None -> failf "CompileRegMemo Phase 1: jump target `%s` is not a known join binder" name

let operand_of_atom (env : env) (expr : 'a expr) : operand =
  match expr with
  | Unit -> OUnit
  | Int i -> OInt i
  | Float f -> OFloat f
  | Bool b -> OBool b
  | Str s -> OString s
  | Builtin (Builtin name, _) -> OBuiltin name
  | Var (name, _) ->
      if StrMap.mem name env.joins then
        failf "CompileRegMemo Phase 1: join binder `%s` cannot be used as a value" name;
      OLocal (lookup_value env name)
  | GVar (name, _) -> OGlobal name
  | Ctor (name, _) -> OCtor name
  | Lam _ -> unsupported "nested lambda literal"
  | App _ | Jump _ | Op _ | Tup _ | Arr _ | Let _ | Sel _ | If _ | Match _ ->
      failf "CompileRegMemo Phase 1: expected ANF atom, got `%s`" (string_of_document @@ pp_expr expr)

let call_target_of_expr (env : env) (expr : 'a expr) : call_target =
  match expr with
  | GVar (name, _) -> Direct name
  | _ -> Indirect (operand_of_atom env expr)

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
      failf "CompileRegMemo Phase 1: non-tail control flow in simple binding `%s`" (string_of_document @@ pp_expr expr)

let lower_var_param (ctx : func_ctx) (kind : value_kind) (pat : 'a pattern) : string * value_id =
  match pat with
  | PVar (name, _) -> (name, add_value ctx name kind)
  | _ -> unsupported "only variable binders are accepted for function/join parameters in Phase 1"

let lower_let_var (ctx : func_ctx) (pat : 'a pattern) : string * value_id =
  match pat with
  | PVar (name, _) -> (name, add_value ctx name Local)
  | _ -> unsupported "non-variable let binder"

let rec lower_match_pattern (ctx : func_ctx) (pat : 'a pattern) :
    ir_pattern * (string * value_id) list =
  match pat with
  | PAny -> (RPAny, [])
  | PInt n -> (RPInt n, [])
  | PBool b -> (RPBool b, [])
  | PUnit -> (RPUnit, [])
  | PVar (name, _) ->
      let id = add_value ctx name MatchBinder in
      (RPBind id, [ (name, id) ])
  | PTup (patterns, _) ->
      let patterns_rev, bindings_rev =
        List.fold_left
          (fun (patterns_acc, bindings_acc) pat ->
            let pat', bindings = lower_match_pattern ctx pat in
            (pat' :: patterns_acc, List.rev_append bindings bindings_acc))
          ([], []) patterns
      in
      (RPTuple (List.rev patterns_rev), List.rev bindings_rev)
  | PCtorApp (ctor, payload, _) -> (
      match payload with
      | None -> (RPCtor (ctor, None), [])
      | Some payload ->
          let payload', bindings = lower_match_pattern ctx payload in
          (RPCtor (ctor, Some payload'), bindings))

let value_ids_of_bindings bindings = List.map snd bindings

let env_with_values (env : env) (bindings : (string * value_id) list) : env =
  {
    env with
    values = List.fold_left (fun acc (name, id) -> StrMap.add name id acc) env.values bindings;
  }

let env_with_joins (env : env) (joins : (string * join_ref) list) : env =
  {
    env with
    joins = List.fold_left (fun acc (name, join_ref) -> StrMap.add name join_ref acc) env.joins joins;
  }

let kind_name = function
  | Param -> "param"
  | Local -> "local"
  | Temp -> "temp"
  | JoinBinder -> "join"
  | JoinParam -> "join_param"
  | MatchBinder -> "match"

let block_kind_name = function
  | Entry -> "entry"
  | Join value_id -> "join(v" ^ string_of_int value_id ^ ")"
  | IfThen -> "if_then"
  | IfElse -> "if_else"
  | MatchArm -> "match_arm"

let rec pp_list pp sep = function
  | [] -> empty
  | [ x ] -> pp x
  | x :: xs -> pp x ^^ sep ^^ pp_list pp sep xs

let pp_field = function FName name -> string "." ^^ string name | FIndex index -> string "." ^^ string (string_of_int index)

let pp_value_ref (values : value_info StrMap.t) (id : value_id) =
  let label =
    match StrMap.find_opt (string_of_int id) values with
    | Some info -> Printf.sprintf "v%d[%s]" info.id info.name
    | None -> Printf.sprintf "v%d" id
  in
  string label

let value_table (unit_ir : unit_ir) : value_info StrMap.t =
  List.fold_left (fun acc (info : value_info) -> StrMap.add (string_of_int info.id) info acc) StrMap.empty unit_ir.values

let pp_operand (values : value_info StrMap.t) = function
  | OLocal id -> pp_value_ref values id
  | OGlobal name -> string ("@" ^ name)
  | OBuiltin name -> string ("builtin:" ^ name)
  | OInt i -> string (string_of_int i)
  | OFloat f -> string (string_of_float f)
  | OBool b -> string (string_of_bool b)
  | OString s -> dquotes (string (String.escaped s))
  | OUnit -> string "()"
  | OCtor name -> string ("#" ^ name)

let pp_call_target values = function
  | Direct name -> string ("@" ^ name)
  | Indirect operand -> pp_operand values operand

let pp_rhs values = function
  | Move operand -> pp_operand values operand
  | Call (target, args) ->
      string "call " ^^ pp_call_target values target ^^ parens (pp_list (pp_operand values) (comma ^^ space) args)
  | Construct (name, args) ->
      string "construct " ^^ string name ^^ parens (pp_list (pp_operand values) (comma ^^ space) args)
  | Tuple values' -> string "tuple" ^^ parens (pp_list (pp_operand values) (comma ^^ space) values')
  | Array values' -> string "array" ^^ brackets (pp_list (pp_operand values) (semi ^^ space) values')
  | Select (target, field) -> pp_operand values target ^^ pp_field field
  | BinOp (op, lhs, rhs) -> pp_operand values lhs ^^ space ^^ string op ^^ space ^^ pp_operand values rhs

let pp_stmt values = function
  | Bind (id, rhs) -> pp_value_ref values id ^^ space ^^ string "<-" ^^ space ^^ pp_rhs values rhs

let rec pp_pattern values = function
  | RPAny -> underscore
  | RPInt n -> string (string_of_int n)
  | RPBool b -> string (string_of_bool b)
  | RPUnit -> string "()"
  | RPBind id -> pp_value_ref values id
  | RPTuple patterns -> parens (pp_list (pp_pattern values) (comma ^^ space) patterns)
  | RPCtor (ctor, None) -> string ctor
  | RPCtor (ctor, Some payload) -> string ctor ^^ space ^^ pp_pattern values payload

let pp_terminator values = function
  | Jump (block_id, args) ->
      string "jump" ^^ space ^^ string ("b" ^ string_of_int block_id)
      ^^ parens (pp_list (pp_operand values) (comma ^^ space) args)
  | Branch (cond, if_true, if_false) ->
      string "branch" ^^ space ^^ pp_operand values cond ^^ space ^^ string "?"
      ^^ space ^^ string ("b" ^ string_of_int if_true)
      ^^ space ^^ colon ^^ space ^^ string ("b" ^ string_of_int if_false)
  | Match (scrutinee, arms) ->
      let pp_arm { pattern; block } = pp_pattern values pattern ^^ space ^^ string "->" ^^ space ^^ string ("b" ^ string_of_int block) in
      string "match" ^^ space ^^ pp_operand values scrutinee ^^ space ^^ string "with"
      ^^ space ^^ pp_list pp_arm (space ^^ string "|" ^^ space) arms
  | Return operand -> string "return" ^^ space ^^ pp_operand values operand

let pp_block values (block : block) =
  let header =
    string "block"
    ^^ space
    ^^ string ("b" ^ string_of_int block.id)
    ^^ parens (pp_list (pp_value_ref values) (comma ^^ space) block.params)
    ^^ space
    ^^ lbrace
    ^^ space
    ^^ string block.label
    ^^ comma
    ^^ space
    ^^ string (block_kind_name block.kind)
    ^^ space ^^ rbrace
  in
  let body =
    match block.body with
    | [] -> empty
    | stmts -> hardline ^^ separate_map hardline (fun stmt -> string "  " ^^ pp_stmt values stmt) stmts
  in
  header ^^ body ^^ hardline ^^ string "  " ^^ pp_terminator values block.term

let pp_value_info (info : value_info) =
  string (Printf.sprintf "v%d[%s]:%s" info.id info.name (kind_name info.kind))

let pp_unit_kind = function Function -> "function" | Global -> "global"

let pp_unit_ir (unit_ir : unit_ir) =
  let values = value_table unit_ir in
  let header =
    string (pp_unit_kind unit_ir.kind)
    ^^ space
    ^^ string unit_ir.name
    ^^ string "#"
    ^^ string (string_of_int unit_ir.id)
    ^^ parens (pp_list (pp_value_ref values) (comma ^^ space) unit_ir.params)
    ^^ space
    ^^ string "entry=b"
    ^^ string (string_of_int unit_ir.entry)
  in
  let values_doc =
    match unit_ir.values with
    | [] -> empty
    | values' ->
        hardline ^^ string "values:"
        ^^ hardline
        ^^ separate_map hardline (fun info -> string "  " ^^ pp_value_info info) values'
  in
  let blocks_doc =
    match unit_ir.blocks with
    | [] -> empty
    | blocks ->
        hardline ^^ string "blocks:"
        ^^ hardline
        ^^ separate_map (hardline ^^ hardline) (pp_block values) blocks
  in
  header ^^ values_doc ^^ blocks_doc

let pp_prog_ir (prog : prog_ir) =
  match prog.units with
  | [] -> string "regmemo.phase1 empty"
  | units -> separate_map (hardline ^^ hardline) pp_unit_ir units

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
      failf "CompileRegMemo Phase 1: expected simple tail expression, got `%s`" (string_of_document @@ pp_expr expr)

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
      failf "CompileRegMemo Phase 1: jump target must be a known join variable, got `%s`"
        (string_of_document @@ pp_expr target)
  | Unit | Int _ | Float _ | Bool _ | Str _ | Builtin _ | Var _ | GVar _ | Ctor _ | App _ | Op _ | Tup _ | Arr _
  | Sel _ ->
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

let lower_unit (state : lower_state) (kind : unit_kind) (name : string) (params : 'a pattern list) (body : 'a expr) : unit_ir =
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
      (fun acc stmt ->
        match stmt with
        | Type _ -> acc
        | Term binding -> acc @ lower_toplevel_binding state binding)
      [] stmts
  in
  { units }

let compile_reg_memo (prog : 'a prog) : document = pp_prog_ir (lower_prog prog)

module Backend = struct
  let compile = compile_reg_memo
end
