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
          | VInt yi ->
              let _0 = xi + yi in
              VInt _0
          | VStuck ys ->
              let _1 = SAdd1 (xv, ys) in
              VStuck _1
          | _ ->
              let _2 = STypeError (yv, VTInt) in
              VStuck _2)
      | VStuck xs ->
          let _3 = SAdd0 (xs, y) in
          VStuck _3
      | _ ->
          let _4 = STypeError (xv, VTInt) in
          VStuck _4)
  | ELt (x, y) -> (
      let xv = eval x env in
      match xv with
      | VInt xi -> (
          let yv = eval y env in
          match yv with
          | VInt yi ->
              let _5 = xi < yi in
              if _5 then VTrue else VFalse
          | VStuck ys ->
              let _6 = SGt1 (xv, ys) in
              VStuck _6
          | _ ->
              let _7 = STypeError (yv, VTInt) in
              VStuck _7)
      | VStuck xs ->
          let _8 = SGt0 (xs, y) in
          VStuck _8
      | _ ->
          let _9 = STypeError (xv, VTInt) in
          VStuck _9)
  | ELe (x, y) -> (
      let xv = eval x env in
      match xv with
      | VInt xi -> (
          let yv = eval y env in
          match yv with
          | VInt yi ->
              let _10 = xi <= yi in
              if _10 then VTrue else VFalse
          | VStuck ys ->
              let _11 = SGt1 (xv, ys) in
              VStuck _11
          | _ ->
              let _12 = STypeError (yv, VTInt) in
              VStuck _12)
      | VStuck xs ->
          let _13 = SGt0 (xs, y) in
          VStuck _13
      | _ ->
          let _14 = STypeError (xv, VTInt) in
          VStuck _14)
  | EGt (x, y) -> (
      let xv = eval x env in
      match xv with
      | VInt xi -> (
          let yv = eval y env in
          match yv with
          | VInt yi ->
              let _15 = xi > yi in
              if _15 then VTrue else VFalse
          | VStuck ys ->
              let _16 = SGt1 (xv, ys) in
              VStuck _16
          | _ ->
              let _17 = STypeError (yv, VTInt) in
              VStuck _17)
      | VStuck xs ->
          let _18 = SGt0 (xs, y) in
          VStuck _18
      | _ ->
          let _19 = STypeError (xv, VTInt) in
          VStuck _19)
  | EGe (x, y) -> (
      let xv = eval x env in
      match xv with
      | VInt xi -> (
          let yv = eval y env in
          match yv with
          | VInt yi ->
              let _20 = xi >= yi in
              if _20 then VTrue else VFalse
          | VStuck ys ->
              let _21 = SGt1 (xv, ys) in
              VStuck _21
          | _ ->
              let _22 = STypeError (yv, VTInt) in
              VStuck _22)
      | VStuck xs ->
          let _23 = SGt0 (xs, y) in
          VStuck _23
      | _ ->
          let _24 = STypeError (xv, VTInt) in
          VStuck _24)
  | EVar idx -> (
      let _25 = index env idx in
      match _25 with Some v -> v | None -> VStuck SIndexError)
  | EAbs e -> VAbs (e, env)
  | ELet (lhs, rhs) ->
      let _26 = eval lhs env in
      let _27 = Cons (_26, env) in
      eval rhs _27
  | EFix e -> VFix (e, env)
  | EApp (f, x) -> (
      let fv = eval f env in
      match fv with
      | VAbs (e, env_) ->
          let _28 = eval x env in
          let _29 = Cons (_28, env_) in
          eval e _29
      | VFix (e, env_) ->
          let _30 = eval x env in
          let _31 = Cons (fv, env_) in
          let _32 = Cons (_30, _31) in
          eval e _32
      | VStuck fs ->
          let _33 = SApp (fs, x) in
          VStuck _33
      | _ ->
          let _34 = STypeError (fv, VTFunc) in
          VStuck _34)
  | EHole x ->
      let _35 = SHole x in
      VStuck _35
  | ETrue -> VTrue
  | EFalse -> VFalse
  | EIf (i, t, e) -> (
      let _36 = eval i env in
      match _36 with
      | VTrue -> eval t env
      | VFalse -> eval e env
      | VStuck is ->
          let _37 = SIf (is, t, e) in
          VStuck _37
      | iv ->
          let _38 = STypeError (iv, VTBool) in
          VStuck _38)
  | ENil -> VNil
  | ECons (x, xs) ->
      let _39 = eval x env in
      let _40 = eval xs env in
      VCons (_39, _40)
  | EPair (x, y) ->
      let _41 = eval x env in
      let _42 = eval y env in
      VPair (_41, _42)
  | EZro p -> (
      let _43 = eval p env in
      match _43 with
      | VPair (a, b) -> a
      | VStuck ps ->
          let _44 = SZro ps in
          VStuck _44
      | pv ->
          let _45 = STypeError (pv, VTPair) in
          VStuck _45)
  | EFst p -> (
      let _46 = eval p env in
      match _46 with
      | VPair (a, b) -> b
      | VStuck ps ->
          let _47 = SFst ps in
          VStuck _47
      | pv ->
          let _48 = STypeError (pv, VTPair) in
          VStuck _48)
  | EMatchList (v, n, c) -> (
      let _49 = eval v env in
      match _49 with
      | VNil -> eval n env
      | VCons (x, xs) ->
          let _50 = Cons (x, env) in
          let _51 = Cons (xs, _50) in
          eval c _51
      | VStuck vs ->
          let _52 = SMatchList (vs, n, c) in
          VStuck _52
      | vv ->
          let _53 = STypeError (vv, VTList) in
          VStuck _53)
  | EUnit -> VUnit
