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
      | S m ->
          let _1 = index xt m in
          _1)
  | _ ->
      let _2 = None in
      _2

let rec eval =
 fun expr env ->
  match expr with
  | EInt i ->
      let _3 = VInt i in
      _3
  | EPlus (x, y) -> (
      let _4 = eval x env in
      let xv = _4 in
      match xv with
      | VInt xi -> (
          let _5 = eval y env in
          let yv = _5 in
          match yv with
          | VInt yi ->
              let _6 = xi + yi in
              let _7 = VInt _6 in
              _7
          | VStuck ys ->
              let _8 = SAdd1 (xv, ys) in
              let _9 = VStuck _8 in
              _9
          | _ ->
              let _10 = VTInt in
              let _11 = STypeError (yv, _10) in
              let _12 = VStuck _11 in
              _12)
      | VStuck xs ->
          let _13 = SAdd0 (xs, y) in
          let _14 = VStuck _13 in
          _14
      | _ ->
          let _15 = VTInt in
          let _16 = STypeError (xv, _15) in
          let _17 = VStuck _16 in
          _17)
  | ELt (x, y) -> (
      let _18 = eval x env in
      let xv = _18 in
      match xv with
      | VInt xi -> (
          let _19 = eval y env in
          let yv = _19 in
          match yv with
          | VInt yi ->
              let _20 = xi < yi in
              if _20 then
                let _22 = VTrue in
                _22
              else
                let _21 = VFalse in
                _21
          | VStuck ys ->
              let _23 = SGt1 (xv, ys) in
              let _24 = VStuck _23 in
              _24
          | _ ->
              let _25 = VTInt in
              let _26 = STypeError (yv, _25) in
              let _27 = VStuck _26 in
              _27)
      | VStuck xs ->
          let _28 = SGt0 (xs, y) in
          let _29 = VStuck _28 in
          _29
      | _ ->
          let _30 = VTInt in
          let _31 = STypeError (xv, _30) in
          let _32 = VStuck _31 in
          _32)
  | ELe (x, y) -> (
      let _33 = eval x env in
      let xv = _33 in
      match xv with
      | VInt xi -> (
          let _34 = eval y env in
          let yv = _34 in
          match yv with
          | VInt yi ->
              let _35 = xi <= yi in
              if _35 then
                let _37 = VTrue in
                _37
              else
                let _36 = VFalse in
                _36
          | VStuck ys ->
              let _38 = SGt1 (xv, ys) in
              let _39 = VStuck _38 in
              _39
          | _ ->
              let _40 = VTInt in
              let _41 = STypeError (yv, _40) in
              let _42 = VStuck _41 in
              _42)
      | VStuck xs ->
          let _43 = SGt0 (xs, y) in
          let _44 = VStuck _43 in
          _44
      | _ ->
          let _45 = VTInt in
          let _46 = STypeError (xv, _45) in
          let _47 = VStuck _46 in
          _47)
  | EGt (x, y) -> (
      let _48 = eval x env in
      let xv = _48 in
      match xv with
      | VInt xi -> (
          let _49 = eval y env in
          let yv = _49 in
          match yv with
          | VInt yi ->
              let _50 = xi > yi in
              if _50 then
                let _52 = VTrue in
                _52
              else
                let _51 = VFalse in
                _51
          | VStuck ys ->
              let _53 = SGt1 (xv, ys) in
              let _54 = VStuck _53 in
              _54
          | _ ->
              let _55 = VTInt in
              let _56 = STypeError (yv, _55) in
              let _57 = VStuck _56 in
              _57)
      | VStuck xs ->
          let _58 = SGt0 (xs, y) in
          let _59 = VStuck _58 in
          _59
      | _ ->
          let _60 = VTInt in
          let _61 = STypeError (xv, _60) in
          let _62 = VStuck _61 in
          _62)
  | EGe (x, y) -> (
      let _63 = eval x env in
      let xv = _63 in
      match xv with
      | VInt xi -> (
          let _64 = eval y env in
          let yv = _64 in
          match yv with
          | VInt yi ->
              let _65 = xi >= yi in
              if _65 then
                let _67 = VTrue in
                _67
              else
                let _66 = VFalse in
                _66
          | VStuck ys ->
              let _68 = SGt1 (xv, ys) in
              let _69 = VStuck _68 in
              _69
          | _ ->
              let _70 = VTInt in
              let _71 = STypeError (yv, _70) in
              let _72 = VStuck _71 in
              _72)
      | VStuck xs ->
          let _73 = SGt0 (xs, y) in
          let _74 = VStuck _73 in
          _74
      | _ ->
          let _75 = VTInt in
          let _76 = STypeError (xv, _75) in
          let _77 = VStuck _76 in
          _77)
  | EVar idx -> (
      let _78 = index env idx in
      match _78 with
      | Some v -> v
      | None ->
          let _79 = SIndexError in
          let _80 = VStuck _79 in
          _80)
  | EAbs e ->
      let _81 = VAbs (e, env) in
      _81
  | ELet (lhs, rhs) ->
      let _82 = eval lhs env in
      let _83 = Cons (_82, env) in
      let _84 = eval rhs _83 in
      _84
  | EFix e ->
      let _85 = VFix (e, env) in
      _85
  | EApp (f, x) -> (
      let _86 = eval f env in
      let fv = _86 in
      match fv with
      | VAbs (e, env_) ->
          let _87 = eval x env in
          let _88 = Cons (_87, env_) in
          let _89 = eval e _88 in
          _89
      | VFix (e, env_) ->
          let _90 = eval x env in
          let _91 = Cons (fv, env_) in
          let _92 = Cons (_90, _91) in
          let _93 = eval e _92 in
          _93
      | VStuck fs ->
          let _94 = SApp (fs, x) in
          let _95 = VStuck _94 in
          _95
      | _ ->
          let _96 = VTFunc in
          let _97 = STypeError (fv, _96) in
          let _98 = VStuck _97 in
          _98)
  | EHole x ->
      let _99 = SHole x in
      let _100 = VStuck _99 in
      _100
  | ETrue ->
      let _101 = VTrue in
      _101
  | EFalse ->
      let _102 = VFalse in
      _102
  | EIf (i, t, e) -> (
      let _103 = eval i env in
      match _103 with
      | VTrue ->
          let _104 = eval t env in
          _104
      | VFalse ->
          let _105 = eval e env in
          _105
      | VStuck is ->
          let _106 = SIf (is, t, e) in
          let _107 = VStuck _106 in
          _107
      | iv ->
          let _108 = VTBool in
          let _109 = STypeError (iv, _108) in
          let _110 = VStuck _109 in
          _110)
  | ENil ->
      let _111 = VNil in
      _111
  | ECons (x, xs) ->
      let _112 = eval x env in
      let _113 = eval xs env in
      let _114 = VCons (_112, _113) in
      _114
  | EPair (x, y) ->
      let _115 = eval x env in
      let _116 = eval y env in
      let _117 = VPair (_115, _116) in
      _117
  | EZro p -> (
      let _118 = eval p env in
      match _118 with
      | VPair (a, b) -> a
      | VStuck ps ->
          let _119 = SZro ps in
          let _120 = VStuck _119 in
          _120
      | pv ->
          let _121 = VTPair in
          let _122 = STypeError (pv, _121) in
          let _123 = VStuck _122 in
          _123)
  | EFst p -> (
      let _124 = eval p env in
      match _124 with
      | VPair (a, b) -> b
      | VStuck ps ->
          let _125 = SFst ps in
          let _126 = VStuck _125 in
          _126
      | pv ->
          let _127 = VTPair in
          let _128 = STypeError (pv, _127) in
          let _129 = VStuck _128 in
          _129)
  | EMatchList (v, n, c) -> (
      let _130 = eval v env in
      match _130 with
      | VNil ->
          let _131 = eval n env in
          _131
      | VCons (x, xs) ->
          let _132 = Cons (x, env) in
          let _133 = Cons (xs, _132) in
          let _134 = eval c _133 in
          _134
      | VStuck vs ->
          let _135 = SMatchList (vs, n, c) in
          let _136 = VStuck _135 in
          _136
      | vv ->
          let _137 = VTList in
          let _138 = STypeError (vv, _137) in
          let _139 = VStuck _138 in
          _139)
  | EUnit ->
      let _140 = VUnit in
      _140
