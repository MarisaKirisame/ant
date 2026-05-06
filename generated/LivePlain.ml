open LiveCEK

type nat = LiveCEK.nat
type 'a list = 'a LiveCEK.list
type 'a option = 'a LiveCEK.option
type expr = LiveCEK.expr

type value = LiveCEK.value
and vtype = LiveCEK.vtype
and stuck = LiveCEK.stuck

let rec index =
 fun x n ->
  match x with
  | Cons (xh, xt) -> (
      match n with
      | Z ->
          let _0 = Some xh in
          _0
      | S m -> index xt m)
  | _ ->
      let _1 = None in
      _1

let rec eval =
 fun expr env ->
  match expr with
  | EInt i ->
      let _2 = VInt i in
      _2
  | EPlus (x, y) -> (
      let _3 = eval x env in
      let xv = _3 in
      match xv with
      | VInt xi -> (
          let _4 = eval y env in
          let yv = _4 in
          match yv with
          | VInt yi ->
              let _5 = xi + yi in
              let _6 = VInt _5 in
              _6
          | VStuck ys ->
              let _7 = SAdd1 (xv, ys) in
              let _8 = VStuck _7 in
              _8
          | _ ->
              let _9 = VTInt in
              let _10 = STypeError (yv, _9) in
              let _11 = VStuck _10 in
              _11)
      | VStuck xs ->
          let _12 = SAdd0 (xs, y) in
          let _13 = VStuck _12 in
          _13
      | _ ->
          let _14 = VTInt in
          let _15 = STypeError (xv, _14) in
          let _16 = VStuck _15 in
          _16)
  | ELt (x, y) -> (
      let _17 = eval x env in
      let xv = _17 in
      match xv with
      | VInt xi -> (
          let _18 = eval y env in
          let yv = _18 in
          match yv with
          | VInt yi ->
              let _19 = xi < yi in
              if _19 then
                let _21 = VTrue in
                _21
              else
                let _20 = VFalse in
                _20
          | VStuck ys ->
              let _22 = SGt1 (xv, ys) in
              let _23 = VStuck _22 in
              _23
          | _ ->
              let _24 = VTInt in
              let _25 = STypeError (yv, _24) in
              let _26 = VStuck _25 in
              _26)
      | VStuck xs ->
          let _27 = SGt0 (xs, y) in
          let _28 = VStuck _27 in
          _28
      | _ ->
          let _29 = VTInt in
          let _30 = STypeError (xv, _29) in
          let _31 = VStuck _30 in
          _31)
  | ELe (x, y) -> (
      let _32 = eval x env in
      let xv = _32 in
      match xv with
      | VInt xi -> (
          let _33 = eval y env in
          let yv = _33 in
          match yv with
          | VInt yi ->
              let _34 = xi <= yi in
              if _34 then
                let _36 = VTrue in
                _36
              else
                let _35 = VFalse in
                _35
          | VStuck ys ->
              let _37 = SGt1 (xv, ys) in
              let _38 = VStuck _37 in
              _38
          | _ ->
              let _39 = VTInt in
              let _40 = STypeError (yv, _39) in
              let _41 = VStuck _40 in
              _41)
      | VStuck xs ->
          let _42 = SGt0 (xs, y) in
          let _43 = VStuck _42 in
          _43
      | _ ->
          let _44 = VTInt in
          let _45 = STypeError (xv, _44) in
          let _46 = VStuck _45 in
          _46)
  | EGt (x, y) -> (
      let _47 = eval x env in
      let xv = _47 in
      match xv with
      | VInt xi -> (
          let _48 = eval y env in
          let yv = _48 in
          match yv with
          | VInt yi ->
              let _49 = xi > yi in
              if _49 then
                let _51 = VTrue in
                _51
              else
                let _50 = VFalse in
                _50
          | VStuck ys ->
              let _52 = SGt1 (xv, ys) in
              let _53 = VStuck _52 in
              _53
          | _ ->
              let _54 = VTInt in
              let _55 = STypeError (yv, _54) in
              let _56 = VStuck _55 in
              _56)
      | VStuck xs ->
          let _57 = SGt0 (xs, y) in
          let _58 = VStuck _57 in
          _58
      | _ ->
          let _59 = VTInt in
          let _60 = STypeError (xv, _59) in
          let _61 = VStuck _60 in
          _61)
  | EGe (x, y) -> (
      let _62 = eval x env in
      let xv = _62 in
      match xv with
      | VInt xi -> (
          let _63 = eval y env in
          let yv = _63 in
          match yv with
          | VInt yi ->
              let _64 = xi >= yi in
              if _64 then
                let _66 = VTrue in
                _66
              else
                let _65 = VFalse in
                _65
          | VStuck ys ->
              let _67 = SGt1 (xv, ys) in
              let _68 = VStuck _67 in
              _68
          | _ ->
              let _69 = VTInt in
              let _70 = STypeError (yv, _69) in
              let _71 = VStuck _70 in
              _71)
      | VStuck xs ->
          let _72 = SGt0 (xs, y) in
          let _73 = VStuck _72 in
          _73
      | _ ->
          let _74 = VTInt in
          let _75 = STypeError (xv, _74) in
          let _76 = VStuck _75 in
          _76)
  | EVar idx -> (
      let _77 = index env idx in
      match _77 with
      | Some v -> v
      | None ->
          let _78 = SIndexError in
          let _79 = VStuck _78 in
          _79)
  | EAbs e ->
      let _80 = VAbs (e, env) in
      _80
  | ELet (lhs, rhs) ->
      let _81 = eval lhs env in
      let _82 = Cons (_81, env) in
      eval rhs _82
  | EFix e ->
      let _83 = VFix (e, env) in
      _83
  | EApp (f, x) -> (
      let _84 = eval f env in
      let fv = _84 in
      match fv with
      | VAbs (e, env_) ->
          let _85 = eval x env in
          let _86 = Cons (_85, env_) in
          eval e _86
      | VFix (e, env_) ->
          let _87 = eval x env in
          let _88 = Cons (fv, env_) in
          let _89 = Cons (_87, _88) in
          eval e _89
      | VStuck fs ->
          let _90 = SApp (fs, x) in
          let _91 = VStuck _90 in
          _91
      | _ ->
          let _92 = VTFunc in
          let _93 = STypeError (fv, _92) in
          let _94 = VStuck _93 in
          _94)
  | EHole x ->
      let _95 = SHole x in
      let _96 = VStuck _95 in
      _96
  | ETrue ->
      let _97 = VTrue in
      _97
  | EFalse ->
      let _98 = VFalse in
      _98
  | EIf (i, t, e) -> (
      let _99 = eval i env in
      match _99 with
      | VTrue -> eval t env
      | VFalse -> eval e env
      | VStuck is ->
          let _100 = SIf (is, t, e) in
          let _101 = VStuck _100 in
          _101
      | iv ->
          let _102 = VTBool in
          let _103 = STypeError (iv, _102) in
          let _104 = VStuck _103 in
          _104)
  | ENil ->
      let _105 = VNil in
      _105
  | ECons (x, xs) ->
      let _106 = eval x env in
      let _107 = eval xs env in
      let _108 = VCons (_106, _107) in
      _108
  | EPair (x, y) ->
      let _109 = eval x env in
      let _110 = eval y env in
      let _111 = VPair (_109, _110) in
      _111
  | EZro p -> (
      let _112 = eval p env in
      match _112 with
      | VPair (a, b) -> a
      | VStuck ps ->
          let _113 = SZro ps in
          let _114 = VStuck _113 in
          _114
      | pv ->
          let _115 = VTPair in
          let _116 = STypeError (pv, _115) in
          let _117 = VStuck _116 in
          _117)
  | EFst p -> (
      let _118 = eval p env in
      match _118 with
      | VPair (a, b) -> b
      | VStuck ps ->
          let _119 = SFst ps in
          let _120 = VStuck _119 in
          _120
      | pv ->
          let _121 = VTPair in
          let _122 = STypeError (pv, _121) in
          let _123 = VStuck _122 in
          _123)
  | EMatchList (v, n, c) -> (
      let _124 = eval v env in
      match _124 with
      | VNil -> eval n env
      | VCons (x, xs) ->
          let _125 = Cons (x, env) in
          let _126 = Cons (xs, _125) in
          eval c _126
      | VStuck vs ->
          let _127 = SMatchList (vs, n, c) in
          let _128 = VStuck _127 in
          _128
      | vv ->
          let _129 = VTList in
          let _130 = STypeError (vv, _129) in
          let _131 = VStuck _130 in
          _131)
  | EUnit ->
      let _132 = VUnit in
      _132
