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
              let _'anf0 = xi + yi in
              VInt _'anf0
          | VStuck ys ->
              let _'anf1 = SAdd1 (xv, ys) in
              VStuck _'anf1
          | _ ->
              let _'anf2 = STypeError (yv, VTInt) in
              VStuck _'anf2)
      | VStuck xs ->
          let _'anf3 = SAdd0 (xs, y) in
          VStuck _'anf3
      | _ ->
          let _'anf4 = STypeError (xv, VTInt) in
          VStuck _'anf4)
  | ELt (x, y) -> (
      let xv = eval x env in
      match xv with
      | VInt xi -> (
          let yv = eval y env in
          match yv with
          | VInt yi ->
              let _'anf5 = xi < yi in
              if _'anf5 then VTrue else VFalse
          | VStuck ys ->
              let _'anf6 = SGt1 (xv, ys) in
              VStuck _'anf6
          | _ ->
              let _'anf7 = STypeError (yv, VTInt) in
              VStuck _'anf7)
      | VStuck xs ->
          let _'anf8 = SGt0 (xs, y) in
          VStuck _'anf8
      | _ ->
          let _'anf9 = STypeError (xv, VTInt) in
          VStuck _'anf9)
  | ELe (x, y) -> (
      let xv = eval x env in
      match xv with
      | VInt xi -> (
          let yv = eval y env in
          match yv with
          | VInt yi ->
              let _'anf10 = xi <= yi in
              if _'anf10 then VTrue else VFalse
          | VStuck ys ->
              let _'anf11 = SGt1 (xv, ys) in
              VStuck _'anf11
          | _ ->
              let _'anf12 = STypeError (yv, VTInt) in
              VStuck _'anf12)
      | VStuck xs ->
          let _'anf13 = SGt0 (xs, y) in
          VStuck _'anf13
      | _ ->
          let _'anf14 = STypeError (xv, VTInt) in
          VStuck _'anf14)
  | EGt (x, y) -> (
      let xv = eval x env in
      match xv with
      | VInt xi -> (
          let yv = eval y env in
          match yv with
          | VInt yi ->
              let _'anf15 = xi > yi in
              if _'anf15 then VTrue else VFalse
          | VStuck ys ->
              let _'anf16 = SGt1 (xv, ys) in
              VStuck _'anf16
          | _ ->
              let _'anf17 = STypeError (yv, VTInt) in
              VStuck _'anf17)
      | VStuck xs ->
          let _'anf18 = SGt0 (xs, y) in
          VStuck _'anf18
      | _ ->
          let _'anf19 = STypeError (xv, VTInt) in
          VStuck _'anf19)
  | EGe (x, y) -> (
      let xv = eval x env in
      match xv with
      | VInt xi -> (
          let yv = eval y env in
          match yv with
          | VInt yi ->
              let _'anf20 = xi >= yi in
              if _'anf20 then VTrue else VFalse
          | VStuck ys ->
              let _'anf21 = SGt1 (xv, ys) in
              VStuck _'anf21
          | _ ->
              let _'anf22 = STypeError (yv, VTInt) in
              VStuck _'anf22)
      | VStuck xs ->
          let _'anf23 = SGt0 (xs, y) in
          VStuck _'anf23
      | _ ->
          let _'anf24 = STypeError (xv, VTInt) in
          VStuck _'anf24)
  | EVar idx -> (
      let _'anf25 = index env idx in
      match _'anf25 with Some v -> v | None -> VStuck SIndexError)
  | EAbs e -> VAbs (e, env)
  | ELet (lhs, rhs) ->
      let _'anf26 = eval lhs env in
      let _'anf27 = Cons (_'anf26, env) in
      eval rhs _'anf27
  | EFix e -> VFix (e, env)
  | EApp (f, x) -> (
      let fv = eval f env in
      match fv with
      | VAbs (e, env_) ->
          let _'anf28 = eval x env in
          let _'anf29 = Cons (_'anf28, env_) in
          eval e _'anf29
      | VFix (e, env_) ->
          let _'anf30 = eval x env in
          let _'anf31 = Cons (fv, env_) in
          let _'anf32 = Cons (_'anf30, _'anf31) in
          eval e _'anf32
      | VStuck fs ->
          let _'anf33 = SApp (fs, x) in
          VStuck _'anf33
      | _ ->
          let _'anf34 = STypeError (fv, VTFunc) in
          VStuck _'anf34)
  | EHole x ->
      let _'anf35 = SHole x in
      VStuck _'anf35
  | ETrue -> VTrue
  | EFalse -> VFalse
  | EIf (i, t, e) -> (
      let _'anf36 = eval i env in
      match _'anf36 with
      | VTrue -> eval t env
      | VFalse -> eval e env
      | VStuck is ->
          let _'anf37 = SIf (is, t, e) in
          VStuck _'anf37
      | iv ->
          let _'anf38 = STypeError (iv, VTBool) in
          VStuck _'anf38)
  | ENil -> VNil
  | ECons (x, xs) ->
      let _'anf39 = eval x env in
      let _'anf40 = eval xs env in
      VCons (_'anf39, _'anf40)
  | EPair (x, y) ->
      let _'anf41 = eval x env in
      let _'anf42 = eval y env in
      VPair (_'anf41, _'anf42)
  | EZro p -> (
      let _'anf43 = eval p env in
      match _'anf43 with
      | VPair (a, b) -> a
      | VStuck ps ->
          let _'anf44 = SZro ps in
          VStuck _'anf44
      | pv ->
          let _'anf45 = STypeError (pv, VTPair) in
          VStuck _'anf45)
  | EFst p -> (
      let _'anf46 = eval p env in
      match _'anf46 with
      | VPair (a, b) -> b
      | VStuck ps ->
          let _'anf47 = SFst ps in
          VStuck _'anf47
      | pv ->
          let _'anf48 = STypeError (pv, VTPair) in
          VStuck _'anf48)
  | EMatchList (v, n, c) -> (
      let _'anf49 = eval v env in
      match _'anf49 with
      | VNil -> eval n env
      | VCons (x, xs) ->
          let _'anf50 = Cons (x, env) in
          let _'anf51 = Cons (xs, _'anf50) in
          eval c _'anf51
      | VStuck vs ->
          let _'anf52 = SMatchList (vs, n, c) in
          VStuck _'anf52
      | vv ->
          let _'anf53 = STypeError (vv, VTList) in
          VStuck _'anf53)
  | EUnit -> VUnit
