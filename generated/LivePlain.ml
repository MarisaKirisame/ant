open LiveCEK

type nat = LiveCEK.nat
type 'a list = 'a LiveCEK.list
type 'a option = 'a LiveCEK.option
type expr = LiveCEK.expr

type value = LiveCEK.value
and vtype = LiveCEK.vtype
and stuck = LiveCEK.stuck

let rec index =
 fun x n -> match x with Cons (xh, xt) -> ( match n with Z -> Some xh | S m -> index xt m) | _ -> None

let rec eval =
 fun expr env ->
  match expr with
  | EInt i -> VInt i
  | EPlus (x, y) -> (
      let xv = eval x env in
      match xv with
      | VInt xi -> (
          let yv = eval y env in
          match yv with
          | VInt yi -> VInt (xi + yi)
          | VStuck ys -> VStuck (SAdd1 (xv, ys))
          | _ -> VStuck (STypeError (yv, VTInt)))
      | VStuck xs -> VStuck (SAdd0 (xs, y))
      | _ -> VStuck (STypeError (xv, VTInt)))
  | ELt (x, y) -> (
      let xv = eval x env in
      match xv with
      | VInt xi -> (
          let yv = eval y env in
          match yv with
          | VInt yi -> if xi < yi then VTrue else VFalse
          | VStuck ys -> VStuck (SGt1 (xv, ys))
          | _ -> VStuck (STypeError (yv, VTInt)))
      | VStuck xs -> VStuck (SGt0 (xs, y))
      | _ -> VStuck (STypeError (xv, VTInt)))
  | ELe (x, y) -> (
      let xv = eval x env in
      match xv with
      | VInt xi -> (
          let yv = eval y env in
          match yv with
          | VInt yi -> if xi <= yi then VTrue else VFalse
          | VStuck ys -> VStuck (SGt1 (xv, ys))
          | _ -> VStuck (STypeError (yv, VTInt)))
      | VStuck xs -> VStuck (SGt0 (xs, y))
      | _ -> VStuck (STypeError (xv, VTInt)))
  | EGt (x, y) -> (
      let xv = eval x env in
      match xv with
      | VInt xi -> (
          let yv = eval y env in
          match yv with
          | VInt yi -> if xi > yi then VTrue else VFalse
          | VStuck ys -> VStuck (SGt1 (xv, ys))
          | _ -> VStuck (STypeError (yv, VTInt)))
      | VStuck xs -> VStuck (SGt0 (xs, y))
      | _ -> VStuck (STypeError (xv, VTInt)))
  | EGe (x, y) -> (
      let xv = eval x env in
      match xv with
      | VInt xi -> (
          let yv = eval y env in
          match yv with
          | VInt yi -> if xi >= yi then VTrue else VFalse
          | VStuck ys -> VStuck (SGt1 (xv, ys))
          | _ -> VStuck (STypeError (yv, VTInt)))
      | VStuck xs -> VStuck (SGt0 (xs, y))
      | _ -> VStuck (STypeError (xv, VTInt)))
  | EVar idx -> ( match index env idx with Some v -> v | None -> VStuck SIndexError)
  | EAbs e -> VAbs (e, env)
  | ELet (lhs, rhs) -> eval rhs (Cons (eval lhs env, env))
  | EFix e -> VFix (e, env)
  | EApp (f, x) -> (
      let fv = eval f env in
      match fv with
      | VAbs (e, env_) -> eval e (Cons (eval x env, env_))
      | VFix (e, env_) -> eval e (Cons (eval x env, Cons (fv, env_)))
      | VStuck fs -> VStuck (SApp (fs, x))
      | _ -> VStuck (STypeError (fv, VTFunc)))
  | EHole x -> VStuck (SHole x)
  | ETrue -> VTrue
  | EFalse -> VFalse
  | EIf (i, t, e) -> (
      match eval i env with
      | VTrue -> eval t env
      | VFalse -> eval e env
      | VStuck is -> VStuck (SIf (is, t, e))
      | iv -> VStuck (STypeError (iv, VTBool)))
  | ENil -> VNil
  | ECons (x, xs) -> VCons (eval x env, eval xs env)
  | EPair (x, y) -> VPair (eval x env, eval y env)
  | EZro p -> (
      match eval p env with VPair (a, b) -> a | VStuck ps -> VStuck (SZro ps) | pv -> VStuck (STypeError (pv, VTPair)))
  | EFst p -> (
      match eval p env with VPair (a, b) -> b | VStuck ps -> VStuck (SFst ps) | pv -> VStuck (STypeError (pv, VTPair)))
  | EMatchList (v, n, c) -> (
      match eval v env with
      | VNil -> eval n env
      | VCons (x, xs) -> eval c (Cons (xs, Cons (x, env)))
      | VStuck vs -> VStuck (SMatchList (vs, n, c))
      | vv -> VStuck (STypeError (vv, VTList)))
  | EUnit -> VUnit
