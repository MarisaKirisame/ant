open Syntax

let rec mark_tail_expr_impl (is_tail : bool) (expr : 'a expr) : bool expr =
  match expr with
  | Unit -> Unit
  | Int i -> Int i
  | Float f -> Float f
  | Bool b -> Bool b
  | Str s -> Str s
  | Builtin (builtin, _) -> Builtin (builtin, is_tail)
  | Var (name, _) -> Var (name, is_tail)
  | GVar (name, _) -> GVar (name, is_tail)
  | Ctor (ctor, _) -> Ctor (ctor, is_tail)
  | App (fn, args, _) -> App (mark_tail_expr_impl false fn, List.map (mark_tail_expr_impl false) args, is_tail)
  | Op (op, lhs, rhs, _) -> Op (op, mark_tail_expr_impl false lhs, mark_tail_expr_impl false rhs, is_tail)
  | Tup (values, _) -> Tup (List.map (mark_tail_expr_impl false) values, is_tail)
  | Arr (values, _) -> Arr (List.map (mark_tail_expr_impl false) values, is_tail)
  | Lam (params, body, _) -> Lam (List.map mark_tail_pattern params, mark_tail_expr_impl true body, is_tail)
  | Let (binding, body, _) -> Let (mark_tail_binding binding, mark_tail_expr_impl is_tail body, is_tail)
  | Sel (target, field, _) -> Sel (mark_tail_expr_impl false target, field, is_tail)
  | If (cond, if_true, if_false, _) ->
      If
        ( mark_tail_expr_impl false cond,
          mark_tail_expr_impl is_tail if_true,
          mark_tail_expr_impl is_tail if_false,
          is_tail )
  | Match (cond, cases, _) -> Match (mark_tail_expr_impl false cond, mark_tail_cases is_tail cases, is_tail)

and mark_tail_binding (binding : 'a binding) : bool binding =
  match binding with
  | BSeq (expr, _) -> BSeq (mark_tail_expr_impl false expr, false)
  | BOne (pattern, expr, _) -> BOne (mark_tail_pattern pattern, mark_tail_expr_impl false expr, false)
  | BRec bindings ->
      BRec
        (List.map
           (fun (pattern, expr, _) -> (mark_tail_pattern pattern, mark_tail_expr_impl false expr, false))
           bindings)
  | BCont (pattern, expr, _) -> BCont (mark_tail_pattern pattern, mark_tail_expr_impl false expr, false)
  | BRecC bindings ->
      BRecC
        (List.map
           (fun (pattern, expr, _) -> (mark_tail_pattern pattern, mark_tail_expr_impl false expr, false))
           bindings)

and mark_tail_pattern (pattern : 'a pattern) : bool pattern =
  match pattern with
  | PAny -> PAny
  | PInt i -> PInt i
  | PBool b -> PBool b
  | PUnit -> PUnit
  | PVar (name, _) -> PVar (name, false)
  | PTup (patterns, _) -> PTup (List.map mark_tail_pattern patterns, false)
  | PCtorApp (ctor, ps, _) -> PCtorApp (ctor, List.map mark_tail_pattern ps, false)

and mark_tail_cases is_tail (MatchPattern cases) =
  MatchPattern (List.map (fun (pattern, expr) -> (mark_tail_pattern pattern, mark_tail_expr_impl is_tail expr)) cases)

let mark_tail (e : 'a expr) : bool expr = mark_tail_expr_impl true e

let rec mark_ty (t : 'a ty) : bool ty =
  match t with
  | TUnit -> TUnit
  | TInt -> TInt
  | TFloat -> TFloat
  | TBool -> TBool
  | TApply (a, b) -> TApply (mark_ty a, List.map mark_ty b)
  | TArrow (a, b) -> TArrow (mark_ty a, mark_ty b)
  | TTuple a -> TTuple (List.map mark_ty a)
  | TNamed a -> TNamed a
  | TNamedVar a -> TNamedVar a

let mark_tail_ty_binding (binding : 'a ty_binding) : bool ty_binding =
  let mark_kind = function
    | Enum { params; ctors } ->
        Enum { params; ctors = List.map (fun (name, tys, _) -> (name, List.map mark_ty tys, false)) ctors }
  in
  match binding with
  | TBOne (name, kind) -> TBOne (name, mark_kind kind)
  | TBRec kinds -> TBRec (List.map (fun (name, kind) -> (name, mark_kind kind)) kinds)

let mark_tail_stmt (stmt : 'a stmt) : bool stmt =
  match stmt with
  | Type binding -> Type (mark_tail_ty_binding binding)
  | Term (BSeq (expr, _)) -> Term (BSeq (mark_tail expr, false))
  | Term binding -> Term (mark_tail_binding binding)

let mark_tail_prog (prog : 'a prog) : bool prog =
  let stmts, _ = prog in
  (List.map mark_tail_stmt stmts, false)
