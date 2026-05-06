open ArithCEK

type nat = ArithCEK.nat
type var = ArithCEK.var
type expr = ArithCEK.expr
type factor_result = ArithCEK.factor_result
type expr_list = ArithCEK.expr_list
type pick_result = ArithCEK.pick_result

let rec var_rank =
 fun v ->
  match v with
  | X ->
      let _0 = 0 in
      _0
  | Y ->
      let _1 = 1 in
      _1

let rec expr_rank =
 fun e ->
  match e with
  | Const _ ->
      let _2 = 0 in
      _2
  | Var _ ->
      let _3 = 1 in
      _3
  | Add (_, _) ->
      let _4 = 2 in
      _4
  | Mul (_, _) ->
      let _5 = 3 in
      _5

let rec compare_expr =
 fun a b ->
  let _6 = expr_rank a in
  let ra = _6 in
  let _7 = expr_rank b in
  let rb = _7 in
  let _8 = ra < rb in
  if _8 then
    let _37 = 0 in
    let _38 = 1 in
    let _39 = _37 - _38 in
    _39
  else
    let _9 = ra > rb in
    if _9 then
      let _36 = 1 in
      _36
    else
      match a with
      | Const x -> (
          match b with
          | Const y ->
              let _10 = x < y in
              if _10 then
                let _14 = 0 in
                let _15 = 1 in
                let _16 = _14 - _15 in
                _16
              else
                let _11 = x > y in
                if _11 then
                  let _13 = 1 in
                  _13
                else
                  let _12 = 0 in
                  _12
          | _ ->
              let _17 = 0 in
              _17)
      | Var va -> (
          match b with
          | Var vb ->
              let _18 = var_rank va in
              let rva = _18 in
              let _19 = var_rank vb in
              let rvb = _19 in
              let _20 = rva < rvb in
              if _20 then
                let _24 = 0 in
                let _25 = 1 in
                let _26 = _24 - _25 in
                _26
              else
                let _21 = rva > rvb in
                if _21 then
                  let _23 = 1 in
                  _23
                else
                  let _22 = 0 in
                  _22
          | _ ->
              let _27 = 0 in
              _27)
      | Add (a1, a2) -> (
          match b with
          | Add (b1, b2) ->
              let _28 = compare_expr a1 b1 in
              let c1 = _28 in
              let _29 = 0 in
              let _30 = c1 = _29 in
              if _30 then compare_expr a2 b2 else c1
          | _ ->
              let _31 = 0 in
              _31)
      | Mul (a1, a2) -> (
          match b with
          | Mul (b1, b2) ->
              let _32 = compare_expr a1 b1 in
              let c1 = _32 in
              let _33 = 0 in
              let _34 = c1 = _33 in
              if _34 then compare_expr a2 b2 else c1
          | _ ->
              let _35 = 0 in
              _35)

let rec expr_equal =
 fun a b ->
  match a with
  | Const x -> (
      match b with
      | Const y ->
          let _40 = x = y in
          _40
      | _ ->
          let _41 = false in
          _41)
  | Var va -> (
      match b with
      | Var vb ->
          let _42 = var_rank va in
          let _43 = var_rank vb in
          let _44 = _42 = _43 in
          _44
      | _ ->
          let _45 = false in
          _45)
  | Add (a1, a2) -> (
      match b with
      | Add (b1, b2) ->
          let _46 = expr_equal a1 b1 in
          let _47 = expr_equal a2 b2 in
          let _48 = _46 && _47 in
          _48
      | _ ->
          let _49 = false in
          _49)
  | Mul (a1, a2) -> (
      match b with
      | Mul (b1, b2) ->
          let _50 = expr_equal a1 b1 in
          let _51 = expr_equal a2 b2 in
          let _52 = _50 && _51 in
          _52
      | _ ->
          let _53 = false in
          _53)

let rec expr_size =
 fun e ->
  match e with
  | Const _ ->
      let _54 = 1 in
      _54
  | Var _ ->
      let _55 = 1 in
      _55
  | Add (a, b) ->
      let _56 = 1 in
      let _57 = expr_size a in
      let _58 = _56 + _57 in
      let _59 = expr_size b in
      let _60 = _58 + _59 in
      _60
  | Mul (a, b) ->
      let _61 = 1 in
      let _62 = expr_size a in
      let _63 = _61 + _62 in
      let _64 = expr_size b in
      let _65 = _63 + _64 in
      _65

let rec better_expr =
 fun a b ->
  let _66 = expr_size a in
  let sa = _66 in
  let _67 = expr_size b in
  let sb = _67 in
  let _68 = sa < sb in
  if _68 then a
  else
    let _69 = sb < sa in
    if _69 then b
    else
      let _70 = compare_expr a b in
      let _71 = 0 in
      let _72 = _70 <= _71 in
      if _72 then a else b

let rec scale =
 fun c e ->
  let _73 = 0 in
  let _74 = c = _73 in
  if _74 then
    let _81 = 0 in
    let _82 = Const _81 in
    _82
  else
    match e with
    | Const x ->
        let _75 = c * x in
        let _76 = Const _75 in
        _76
    | _ ->
        let _77 = 1 in
        let _78 = c = _77 in
        if _78 then e
        else
          let _79 = Const c in
          let _80 = Mul (_79, e) in
          _80

let rec coeff_value =
 fun e ->
  match e with
  | Const x -> x
  | Mul (lhs, rhs) -> (
      match lhs with
      | Const c -> c
      | _ ->
          let _83 = 1 in
          _83)
  | _ ->
      let _84 = 1 in
      _84

let rec coeff_base =
 fun e ->
  match e with
  | Const _ ->
      let _85 = 1 in
      let _86 = Const _85 in
      _86
  | Mul (lhs, rhs) -> ( match lhs with Const _ -> rhs | _ -> e)
  | _ -> e

let rec extract_factor =
 fun needle e ->
  let _87 = expr_equal needle e in
  if _87 then
    let _96 = 1 in
    let _97 = Const _96 in
    let _98 = Found _97 in
    _98
  else
    match e with
    | Mul (a, b) -> (
        let _88 = extract_factor needle a in
        let left = _88 in
        match left with
        | Found rest ->
            let _89 = Mul (rest, b) in
            let _90 = Found _89 in
            _90
        | Missing -> (
            let _91 = extract_factor needle b in
            let right = _91 in
            match right with
            | Found rest ->
                let _92 = Mul (a, rest) in
                let _93 = Found _92 in
                _93
            | Missing ->
                let _94 = Missing in
                _94))
    | _ ->
        let _95 = Missing in
        _95

let rec search_factor =
 fun left right ->
  match left with
  | Mul (a, b) -> (
      let _99 = extract_factor a right in
      let fa = _99 in
      match fa with
      | Found rest ->
          let _100 = Add (b, rest) in
          let _101 = Mul (a, _100) in
          _101
      | Missing -> (
          let _102 = extract_factor b right in
          let fb = _102 in
          match fb with
          | Found rest ->
              let _103 = Add (a, rest) in
              let _104 = Mul (b, _103) in
              _104
          | Missing ->
              let _105 = Add (left, right) in
              _105))
  | _ ->
      let _106 = Add (left, right) in
      _106

let rec append_exprs =
 fun xs ys ->
  match xs with
  | ENil -> ys
  | ECons (x, rest) ->
      let _107 = append_exprs rest ys in
      let _108 = ECons (x, _107) in
      _108

let rec insert_expr =
 fun e xs ->
  match xs with
  | ENil ->
      let _109 = ENil in
      let _110 = ECons (e, _109) in
      _110
  | ECons (x, rest) ->
      let _111 = compare_expr e x in
      let _112 = 0 in
      let _113 = _111 <= _112 in
      if _113 then
        let _116 = ECons (e, xs) in
        _116
      else
        let _114 = insert_expr e rest in
        let _115 = ECons (x, _114) in
        _115

let rec sort_exprs =
 fun xs ->
  match xs with
  | ENil ->
      let _117 = ENil in
      _117
  | ECons (x, rest) ->
      let _118 = sort_exprs rest in
      insert_expr x _118

let rec compare_add_term =
 fun a b ->
  let _119 = coeff_base a in
  let abase = _119 in
  let _120 = coeff_base b in
  let bbase = _120 in
  let _121 = compare_expr abase bbase in
  let cbase = _121 in
  let _122 = 0 in
  let _123 = cbase = _122 in
  if _123 then
    let _124 = coeff_value a in
    let acoeff = _124 in
    let _125 = coeff_value b in
    let bcoeff = _125 in
    let _126 = acoeff < bcoeff in
    if _126 then
      let _129 = 0 in
      let _130 = 1 in
      let _131 = _129 - _130 in
      _131
    else
      let _127 = acoeff > bcoeff in
      if _127 then
        let _128 = 1 in
        _128
      else compare_expr a b
  else cbase

let rec insert_add_term =
 fun e xs ->
  match xs with
  | ENil ->
      let _132 = ENil in
      let _133 = ECons (e, _132) in
      _133
  | ECons (x, rest) ->
      let _134 = compare_add_term e x in
      let _135 = 0 in
      let _136 = _134 <= _135 in
      if _136 then
        let _139 = ECons (e, xs) in
        _139
      else
        let _137 = insert_add_term e rest in
        let _138 = ECons (x, _137) in
        _138

let rec sort_add_terms =
 fun xs ->
  match xs with
  | ENil ->
      let _140 = ENil in
      _140
  | ECons (x, rest) ->
      let _141 = sort_add_terms rest in
      insert_add_term x _141

let rec reverse_exprs_aux =
 fun xs acc ->
  match xs with
  | ENil -> acc
  | ECons (x, rest) ->
      let _142 = ECons (x, acc) in
      reverse_exprs_aux rest _142

let rec reverse_exprs =
 fun xs ->
  let _143 = ENil in
  reverse_exprs_aux xs _143

let rec flatten_add =
 fun e ->
  match e with
  | Add (a, b) ->
      let _144 = flatten_add a in
      let _145 = flatten_add b in
      append_exprs _144 _145
  | Const x ->
      let _146 = 0 in
      let _147 = x = _146 in
      if _147 then
        let _150 = ENil in
        _150
      else
        let _148 = ENil in
        let _149 = ECons (e, _148) in
        _149
  | _ ->
      let _151 = ENil in
      let _152 = ECons (e, _151) in
      _152

let rec flatten_mul =
 fun e ->
  match e with
  | Mul (a, b) ->
      let _153 = flatten_mul a in
      let _154 = flatten_mul b in
      append_exprs _153 _154
  | _ ->
      let _155 = ENil in
      let _156 = ECons (e, _155) in
      _156

let rec mul_coeff =
 fun e ->
  match e with
  | Const x -> x
  | Mul (lhs, rhs) -> (
      match lhs with
      | Const c -> c
      | _ ->
          let _157 = 1 in
          _157)
  | _ ->
      let _158 = 1 in
      _158

let rec mul_base =
 fun e ->
  match e with
  | Const _ ->
      let _159 = 1 in
      let _160 = Const _159 in
      _160
  | Mul (lhs, rhs) -> ( match lhs with Const _ -> rhs | _ -> e)
  | _ -> e

let rec mul_total_coeff =
 fun xs ->
  match xs with
  | ENil ->
      let _161 = 1 in
      _161
  | ECons (x, rest) ->
      let _162 = mul_coeff x in
      let _163 = mul_total_coeff rest in
      let _164 = _162 * _163 in
      _164

let rec mul_bases =
 fun xs ->
  match xs with
  | ENil ->
      let _165 = ENil in
      _165
  | ECons (x, rest) -> (
      let _166 = mul_base x in
      let base = _166 in
      match base with
      | Const one ->
          let _167 = 1 in
          let _168 = one = _167 in
          if _168 then mul_bases rest
          else
            let _169 = mul_bases rest in
            insert_expr base _169
      | _ ->
          let _170 = mul_bases rest in
          insert_expr base _170)

let rec build_mul =
 fun xs ->
  match xs with
  | ENil ->
      let _171 = 1 in
      let _172 = Const _171 in
      _172
  | ECons (x, rest) -> (
      match rest with
      | ENil -> x
      | _ ->
          let _173 = build_mul rest in
          let _174 = Mul (x, _173) in
          _174)

let rec normalize_mul_flat =
 fun left right ->
  let _175 = flatten_mul left in
  let _176 = flatten_mul right in
  let _177 = append_exprs _175 _176 in
  let factors = _177 in
  let _178 = mul_total_coeff factors in
  let coeff = _178 in
  let _179 = 0 in
  let _180 = coeff = _179 in
  if _180 then
    let _185 = 0 in
    let _186 = Const _185 in
    _186
  else
    let _181 = mul_bases factors in
    let bases = _181 in
    let _182 = build_mul bases in
    let base_expr = _182 in
    let _183 = 1 in
    let _184 = coeff = _183 in
    if _184 then base_expr else scale coeff base_expr

let rec combine_like_terms_acc =
 fun base coeff xs ->
  match xs with
  | ENil ->
      let _187 = 0 in
      let _188 = coeff = _187 in
      if _188 then
        let _192 = ENil in
        _192
      else
        let _189 = scale coeff base in
        let _190 = ENil in
        let _191 = ECons (_189, _190) in
        _191
  | ECons (x, rest) ->
      let _193 = coeff_base x in
      let xbase = _193 in
      let _194 = coeff_value x in
      let xcoeff = _194 in
      let _195 = expr_equal base xbase in
      if _195 then
        let _200 = coeff + xcoeff in
        combine_like_terms_acc base _200 rest
      else
        let _196 = combine_like_terms_acc xbase xcoeff rest in
        let tail = _196 in
        let _197 = 0 in
        let _198 = coeff = _197 in
        if _198 then tail
        else
          let _199 = scale coeff base in
          insert_add_term _199 tail

let rec combine_like_terms =
 fun xs ->
  match xs with
  | ENil ->
      let _201 = ENil in
      _201
  | ECons (x, rest) ->
      let _202 = coeff_base x in
      let base = _202 in
      let _203 = coeff_value x in
      let coeff = _203 in
      combine_like_terms_acc base coeff rest

let rec factor_adjacent =
 fun xs ->
  match xs with
  | ENil ->
      let _204 = ENil in
      _204
  | ECons (x, rest) -> (
      match rest with
      | ENil ->
          let _205 = ENil in
          let _206 = ECons (x, _205) in
          _206
      | ECons (y, ys) ->
          let _207 = search_factor x y in
          let merged = _207 in
          let _208 = Add (x, y) in
          let _209 = expr_equal merged _208 in
          if _209 then
            let _211 = factor_adjacent rest in
            let _212 = ECons (x, _211) in
            _212
          else
            let _210 = insert_add_term merged ys in
            factor_adjacent _210)

let rec pick_factored =
 fun x xs ->
  match xs with
  | ENil ->
      let _213 = NoPick in
      _213
  | ECons (y, rest) ->
      let _214 = search_factor x y in
      let merged = _214 in
      let _215 = Add (x, y) in
      let _216 = expr_equal merged _215 in
      if _216 then
        let _218 = pick_factored x rest in
        match _218 with
        | NoPick ->
            let _219 = NoPick in
            _219
        | Pick (term, remain) ->
            let _220 = ECons (y, remain) in
            let _221 = Pick (term, _220) in
            _221
      else
        let _217 = Pick (merged, rest) in
        _217

let rec search_terms =
 fun xs ->
  match xs with
  | ENil ->
      let _222 = ENil in
      _222
  | ECons (x, rest) -> (
      let _223 = pick_factored x rest in
      match _223 with
      | NoPick ->
          let _224 = search_terms rest in
          let _225 = ECons (x, _224) in
          _225
      | Pick (merged, remain) ->
          let _226 = insert_add_term merged remain in
          search_terms _226)

let rec build_add =
 fun xs ->
  match xs with
  | ENil ->
      let _227 = 0 in
      let _228 = Const _227 in
      _228
  | ECons (x, rest) -> (
      match rest with
      | ENil -> x
      | _ ->
          let _229 = build_add rest in
          let _230 = Add (x, _229) in
          _230)

let rec search_round =
 fun xs ->
  let _231 = factor_adjacent xs in
  let factored0 = _231 in
  let _232 = search_terms factored0 in
  let factored1 = _232 in
  let _233 = factor_adjacent factored1 in
  let factored2 = _233 in
  search_terms factored2

let rec normalize_add_flat =
 fun left right ->
  let _234 = flatten_add left in
  let _235 = flatten_add right in
  let _236 = append_exprs _234 _235 in
  let terms = _236 in
  let _237 = sort_add_terms terms in
  let sorted = _237 in
  let _238 = combine_like_terms sorted in
  let combined = _238 in
  let _239 = search_round combined in
  let forward = _239 in
  let _240 = reverse_exprs combined in
  let _241 = search_round _240 in
  let backward = _241 in
  let _242 = build_add forward in
  let forward_expr = _242 in
  let _243 = build_add backward in
  let backward_expr = _243 in
  let _244 = better_expr forward_expr backward_expr in
  let best = _244 in
  let _245 = flatten_add best in
  let _246 = sort_add_terms _245 in
  let _247 = combine_like_terms _246 in
  let rescanned = _247 in
  build_add rescanned

let rec search_opt =
 fun fuel e ->
  match fuel with
  | Z -> e
  | S fuel1 -> (
      match e with
      | Add (a, b) ->
          let _248 = search_opt fuel1 a in
          let sa = _248 in
          let _249 = search_opt fuel1 b in
          let sb = _249 in
          let _250 = normalize_add_flat sa sb in
          let merged = _250 in
          let _251 = normalize_add_flat sb sa in
          let flipped = _251 in
          let _252 = normalize_add_flat merged flipped in
          let next = _252 in
          let _253 = normalize_add_flat next merged in
          let final = _253 in
          let _254 = expr_equal final merged in
          if _254 then merged else search_opt fuel1 final
      | Mul (a, b) ->
          let _255 = search_opt fuel1 a in
          let sa = _255 in
          let _256 = search_opt fuel1 b in
          let sb = _256 in
          let _257 = normalize_mul_flat sa sb in
          let merged = _257 in
          let _258 = normalize_mul_flat sb sa in
          let flipped = _258 in
          let _259 = normalize_mul_flat merged flipped in
          let next = _259 in
          let _260 = normalize_mul_flat next merged in
          let final = _260 in
          let _261 = expr_equal final merged in
          if _261 then merged else search_opt fuel1 final
      | _ -> e)

let rec normalize =
 fun e ->
  match e with
  | Const _ -> e
  | Var _ -> e
  | Add (a, b) -> (
      let _262 = normalize a in
      let na = _262 in
      let _263 = normalize b in
      let nb = _263 in
      match na with
      | Const x ->
          let _264 = 0 in
          let _265 = x = _264 in
          if _265 then nb else normalize_add_flat na nb
      | _ -> normalize_add_flat na nb)
  | Mul (a, b) -> (
      let _266 = normalize a in
      let na = _266 in
      let _267 = normalize b in
      let nb = _267 in
      match na with
      | Const x ->
          let _268 = 0 in
          let _269 = x = _268 in
          if _269 then na
          else
            let _270 = 1 in
            let _271 = x = _270 in
            if _271 then nb else normalize_mul_flat na nb
      | _ -> normalize_mul_flat na nb)

let rec simplify_aux =
 fun e ->
  let _272 = normalize e in
  let next = _272 in
  let _273 = expr_equal e next in
  if _273 then next else simplify_aux next

let rec diffx =
 fun e ->
  match e with
  | Const _ ->
      let _274 = 0 in
      let _275 = Const _274 in
      _275
  | Var v -> (
      match v with
      | X ->
          let _276 = 1 in
          let _277 = Const _276 in
          _277
      | Y ->
          let _278 = 0 in
          let _279 = Const _278 in
          _279)
  | Add (a, b) ->
      let _280 = diffx a in
      let _281 = diffx b in
      let _282 = Add (_280, _281) in
      _282
  | Mul (a, b) ->
      let _283 = diffx a in
      let _284 = Mul (_283, b) in
      let _285 = diffx b in
      let _286 = Mul (a, _285) in
      let _287 = Add (_284, _286) in
      _287

let rec eval =
 fun e x y ->
  match e with
  | Const c -> c
  | Var v -> ( match v with X -> x | Y -> y)
  | Add (a, b) ->
      let _288 = eval a x y in
      let _289 = eval b x y in
      let _290 = _288 + _289 in
      _290
  | Mul (a, b) ->
      let _291 = eval a x y in
      let _292 = eval b x y in
      let _293 = _291 * _292 in
      _293

let rec main =
 fun e ->
  let _294 = diffx e in
  let d = _294 in
  let _295 = diffx d in
  let f = _295 in
  simplify_aux f
