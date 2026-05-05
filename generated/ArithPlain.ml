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
    let _39 = 0 in
    let _40 = 1 in
    let _41 = _39 - _40 in
    _41
  else
    let _9 = ra > rb in
    if _9 then
      let _38 = 1 in
      _38
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
              if _30 then
                let _31 = compare_expr a2 b2 in
                _31
              else c1
          | _ ->
              let _32 = 0 in
              _32)
      | Mul (a1, a2) -> (
          match b with
          | Mul (b1, b2) ->
              let _33 = compare_expr a1 b1 in
              let c1 = _33 in
              let _34 = 0 in
              let _35 = c1 = _34 in
              if _35 then
                let _36 = compare_expr a2 b2 in
                _36
              else c1
          | _ ->
              let _37 = 0 in
              _37)

let rec expr_equal =
 fun a b ->
  match a with
  | Const x -> (
      match b with
      | Const y ->
          let _42 = x = y in
          _42
      | _ ->
          let _43 = false in
          _43)
  | Var va -> (
      match b with
      | Var vb ->
          let _44 = var_rank va in
          let _45 = var_rank vb in
          let _46 = _44 = _45 in
          _46
      | _ ->
          let _47 = false in
          _47)
  | Add (a1, a2) -> (
      match b with
      | Add (b1, b2) ->
          let _48 = expr_equal a1 b1 in
          let _49 = expr_equal a2 b2 in
          let _50 = _48 && _49 in
          _50
      | _ ->
          let _51 = false in
          _51)
  | Mul (a1, a2) -> (
      match b with
      | Mul (b1, b2) ->
          let _52 = expr_equal a1 b1 in
          let _53 = expr_equal a2 b2 in
          let _54 = _52 && _53 in
          _54
      | _ ->
          let _55 = false in
          _55)

let rec expr_size =
 fun e ->
  match e with
  | Const _ ->
      let _56 = 1 in
      _56
  | Var _ ->
      let _57 = 1 in
      _57
  | Add (a, b) ->
      let _58 = 1 in
      let _59 = expr_size a in
      let _60 = _58 + _59 in
      let _61 = expr_size b in
      let _62 = _60 + _61 in
      _62
  | Mul (a, b) ->
      let _63 = 1 in
      let _64 = expr_size a in
      let _65 = _63 + _64 in
      let _66 = expr_size b in
      let _67 = _65 + _66 in
      _67

let rec better_expr =
 fun a b ->
  let _68 = expr_size a in
  let sa = _68 in
  let _69 = expr_size b in
  let sb = _69 in
  let _70 = sa < sb in
  if _70 then a
  else
    let _71 = sb < sa in
    if _71 then b
    else
      let _72 = compare_expr a b in
      let _73 = 0 in
      let _74 = _72 <= _73 in
      if _74 then a else b

let rec scale =
 fun c e ->
  let _75 = 0 in
  let _76 = c = _75 in
  if _76 then
    let _83 = 0 in
    let _84 = Const _83 in
    _84
  else
    match e with
    | Const x ->
        let _77 = c * x in
        let _78 = Const _77 in
        _78
    | _ ->
        let _79 = 1 in
        let _80 = c = _79 in
        if _80 then e
        else
          let _81 = Const c in
          let _82 = Mul (_81, e) in
          _82

let rec coeff_value =
 fun e ->
  match e with
  | Const x -> x
  | Mul (lhs, rhs) -> (
      match lhs with
      | Const c -> c
      | _ ->
          let _85 = 1 in
          _85)
  | _ ->
      let _86 = 1 in
      _86

let rec coeff_base =
 fun e ->
  match e with
  | Const _ ->
      let _87 = 1 in
      let _88 = Const _87 in
      _88
  | Mul (lhs, rhs) -> ( match lhs with Const _ -> rhs | _ -> e)
  | _ -> e

let rec extract_factor =
 fun needle e ->
  let _89 = expr_equal needle e in
  if _89 then
    let _98 = 1 in
    let _99 = Const _98 in
    let _100 = Found _99 in
    _100
  else
    match e with
    | Mul (a, b) -> (
        let _90 = extract_factor needle a in
        let left = _90 in
        match left with
        | Found rest ->
            let _91 = Mul (rest, b) in
            let _92 = Found _91 in
            _92
        | Missing -> (
            let _93 = extract_factor needle b in
            let right = _93 in
            match right with
            | Found rest ->
                let _94 = Mul (a, rest) in
                let _95 = Found _94 in
                _95
            | Missing ->
                let _96 = Missing in
                _96))
    | _ ->
        let _97 = Missing in
        _97

let rec search_factor =
 fun left right ->
  match left with
  | Mul (a, b) -> (
      let _101 = extract_factor a right in
      let fa = _101 in
      match fa with
      | Found rest ->
          let _102 = Add (b, rest) in
          let _103 = Mul (a, _102) in
          _103
      | Missing -> (
          let _104 = extract_factor b right in
          let fb = _104 in
          match fb with
          | Found rest ->
              let _105 = Add (a, rest) in
              let _106 = Mul (b, _105) in
              _106
          | Missing ->
              let _107 = Add (left, right) in
              _107))
  | _ ->
      let _108 = Add (left, right) in
      _108

let rec append_exprs =
 fun xs ys ->
  match xs with
  | ENil -> ys
  | ECons (x, rest) ->
      let _109 = append_exprs rest ys in
      let _110 = ECons (x, _109) in
      _110

let rec insert_expr =
 fun e xs ->
  match xs with
  | ENil ->
      let _111 = ENil in
      let _112 = ECons (e, _111) in
      _112
  | ECons (x, rest) ->
      let _113 = compare_expr e x in
      let _114 = 0 in
      let _115 = _113 <= _114 in
      if _115 then
        let _118 = ECons (e, xs) in
        _118
      else
        let _116 = insert_expr e rest in
        let _117 = ECons (x, _116) in
        _117

let rec sort_exprs =
 fun xs ->
  match xs with
  | ENil ->
      let _119 = ENil in
      _119
  | ECons (x, rest) ->
      let _120 = sort_exprs rest in
      let _121 = insert_expr x _120 in
      _121

let rec compare_add_term =
 fun a b ->
  let _122 = coeff_base a in
  let abase = _122 in
  let _123 = coeff_base b in
  let bbase = _123 in
  let _124 = compare_expr abase bbase in
  let cbase = _124 in
  let _125 = 0 in
  let _126 = cbase = _125 in
  if _126 then
    let _127 = coeff_value a in
    let acoeff = _127 in
    let _128 = coeff_value b in
    let bcoeff = _128 in
    let _129 = acoeff < bcoeff in
    if _129 then
      let _133 = 0 in
      let _134 = 1 in
      let _135 = _133 - _134 in
      _135
    else
      let _130 = acoeff > bcoeff in
      if _130 then
        let _132 = 1 in
        _132
      else
        let _131 = compare_expr a b in
        _131
  else cbase

let rec insert_add_term =
 fun e xs ->
  match xs with
  | ENil ->
      let _136 = ENil in
      let _137 = ECons (e, _136) in
      _137
  | ECons (x, rest) ->
      let _138 = compare_add_term e x in
      let _139 = 0 in
      let _140 = _138 <= _139 in
      if _140 then
        let _143 = ECons (e, xs) in
        _143
      else
        let _141 = insert_add_term e rest in
        let _142 = ECons (x, _141) in
        _142

let rec sort_add_terms =
 fun xs ->
  match xs with
  | ENil ->
      let _144 = ENil in
      _144
  | ECons (x, rest) ->
      let _145 = sort_add_terms rest in
      let _146 = insert_add_term x _145 in
      _146

let rec reverse_exprs_aux =
 fun xs acc ->
  match xs with
  | ENil -> acc
  | ECons (x, rest) ->
      let _147 = ECons (x, acc) in
      let _148 = reverse_exprs_aux rest _147 in
      _148

let rec reverse_exprs =
 fun xs ->
  let _149 = ENil in
  let _150 = reverse_exprs_aux xs _149 in
  _150

let rec flatten_add =
 fun e ->
  match e with
  | Add (a, b) ->
      let _151 = flatten_add a in
      let _152 = flatten_add b in
      let _153 = append_exprs _151 _152 in
      _153
  | Const x ->
      let _154 = 0 in
      let _155 = x = _154 in
      if _155 then
        let _158 = ENil in
        _158
      else
        let _156 = ENil in
        let _157 = ECons (e, _156) in
        _157
  | _ ->
      let _159 = ENil in
      let _160 = ECons (e, _159) in
      _160

let rec flatten_mul =
 fun e ->
  match e with
  | Mul (a, b) ->
      let _161 = flatten_mul a in
      let _162 = flatten_mul b in
      let _163 = append_exprs _161 _162 in
      _163
  | _ ->
      let _164 = ENil in
      let _165 = ECons (e, _164) in
      _165

let rec mul_coeff =
 fun e ->
  match e with
  | Const x -> x
  | Mul (lhs, rhs) -> (
      match lhs with
      | Const c -> c
      | _ ->
          let _166 = 1 in
          _166)
  | _ ->
      let _167 = 1 in
      _167

let rec mul_base =
 fun e ->
  match e with
  | Const _ ->
      let _168 = 1 in
      let _169 = Const _168 in
      _169
  | Mul (lhs, rhs) -> ( match lhs with Const _ -> rhs | _ -> e)
  | _ -> e

let rec mul_total_coeff =
 fun xs ->
  match xs with
  | ENil ->
      let _170 = 1 in
      _170
  | ECons (x, rest) ->
      let _171 = mul_coeff x in
      let _172 = mul_total_coeff rest in
      let _173 = _171 * _172 in
      _173

let rec mul_bases =
 fun xs ->
  match xs with
  | ENil ->
      let _174 = ENil in
      _174
  | ECons (x, rest) -> (
      let _175 = mul_base x in
      let base = _175 in
      match base with
      | Const one ->
          let _176 = 1 in
          let _177 = one = _176 in
          if _177 then
            let _180 = mul_bases rest in
            _180
          else
            let _178 = mul_bases rest in
            let _179 = insert_expr base _178 in
            _179
      | _ ->
          let _181 = mul_bases rest in
          let _182 = insert_expr base _181 in
          _182)

let rec build_mul =
 fun xs ->
  match xs with
  | ENil ->
      let _183 = 1 in
      let _184 = Const _183 in
      _184
  | ECons (x, rest) -> (
      match rest with
      | ENil -> x
      | _ ->
          let _185 = build_mul rest in
          let _186 = Mul (x, _185) in
          _186)

let rec normalize_mul_flat =
 fun left right ->
  let _187 = flatten_mul left in
  let _188 = flatten_mul right in
  let _189 = append_exprs _187 _188 in
  let factors = _189 in
  let _190 = mul_total_coeff factors in
  let coeff = _190 in
  let _191 = 0 in
  let _192 = coeff = _191 in
  if _192 then
    let _198 = 0 in
    let _199 = Const _198 in
    _199
  else
    let _193 = mul_bases factors in
    let bases = _193 in
    let _194 = build_mul bases in
    let base_expr = _194 in
    let _195 = 1 in
    let _196 = coeff = _195 in
    if _196 then base_expr
    else
      let _197 = scale coeff base_expr in
      _197

let rec combine_like_terms_acc =
 fun base coeff xs ->
  match xs with
  | ENil ->
      let _200 = 0 in
      let _201 = coeff = _200 in
      if _201 then
        let _205 = ENil in
        _205
      else
        let _202 = scale coeff base in
        let _203 = ENil in
        let _204 = ECons (_202, _203) in
        _204
  | ECons (x, rest) ->
      let _206 = coeff_base x in
      let xbase = _206 in
      let _207 = coeff_value x in
      let xcoeff = _207 in
      let _208 = expr_equal base xbase in
      if _208 then
        let _214 = coeff + xcoeff in
        let _215 = combine_like_terms_acc base _214 rest in
        _215
      else
        let _209 = combine_like_terms_acc xbase xcoeff rest in
        let tail = _209 in
        let _210 = 0 in
        let _211 = coeff = _210 in
        if _211 then tail
        else
          let _212 = scale coeff base in
          let _213 = insert_add_term _212 tail in
          _213

let rec combine_like_terms =
 fun xs ->
  match xs with
  | ENil ->
      let _216 = ENil in
      _216
  | ECons (x, rest) ->
      let _217 = coeff_base x in
      let base = _217 in
      let _218 = coeff_value x in
      let coeff = _218 in
      let _219 = combine_like_terms_acc base coeff rest in
      _219

let rec factor_adjacent =
 fun xs ->
  match xs with
  | ENil ->
      let _220 = ENil in
      _220
  | ECons (x, rest) -> (
      match rest with
      | ENil ->
          let _221 = ENil in
          let _222 = ECons (x, _221) in
          _222
      | ECons (y, ys) ->
          let _223 = search_factor x y in
          let merged = _223 in
          let _224 = Add (x, y) in
          let _225 = expr_equal merged _224 in
          if _225 then
            let _228 = factor_adjacent rest in
            let _229 = ECons (x, _228) in
            _229
          else
            let _226 = insert_add_term merged ys in
            let _227 = factor_adjacent _226 in
            _227)

let rec pick_factored =
 fun x xs ->
  match xs with
  | ENil ->
      let _230 = NoPick in
      _230
  | ECons (y, rest) ->
      let _231 = search_factor x y in
      let merged = _231 in
      let _232 = Add (x, y) in
      let _233 = expr_equal merged _232 in
      if _233 then
        let _235 = pick_factored x rest in
        match _235 with
        | NoPick ->
            let _236 = NoPick in
            _236
        | Pick (term, remain) ->
            let _237 = ECons (y, remain) in
            let _238 = Pick (term, _237) in
            _238
      else
        let _234 = Pick (merged, rest) in
        _234

let rec search_terms =
 fun xs ->
  match xs with
  | ENil ->
      let _239 = ENil in
      _239
  | ECons (x, rest) -> (
      let _240 = pick_factored x rest in
      match _240 with
      | NoPick ->
          let _241 = search_terms rest in
          let _242 = ECons (x, _241) in
          _242
      | Pick (merged, remain) ->
          let _243 = insert_add_term merged remain in
          let _244 = search_terms _243 in
          _244)

let rec build_add =
 fun xs ->
  match xs with
  | ENil ->
      let _245 = 0 in
      let _246 = Const _245 in
      _246
  | ECons (x, rest) -> (
      match rest with
      | ENil -> x
      | _ ->
          let _247 = build_add rest in
          let _248 = Add (x, _247) in
          _248)

let rec search_round =
 fun xs ->
  let _249 = factor_adjacent xs in
  let factored0 = _249 in
  let _250 = search_terms factored0 in
  let factored1 = _250 in
  let _251 = factor_adjacent factored1 in
  let factored2 = _251 in
  let _252 = search_terms factored2 in
  _252

let rec normalize_add_flat =
 fun left right ->
  let _253 = flatten_add left in
  let _254 = flatten_add right in
  let _255 = append_exprs _253 _254 in
  let terms = _255 in
  let _256 = sort_add_terms terms in
  let sorted = _256 in
  let _257 = combine_like_terms sorted in
  let combined = _257 in
  let _258 = search_round combined in
  let forward = _258 in
  let _259 = reverse_exprs combined in
  let _260 = search_round _259 in
  let backward = _260 in
  let _261 = build_add forward in
  let forward_expr = _261 in
  let _262 = build_add backward in
  let backward_expr = _262 in
  let _263 = better_expr forward_expr backward_expr in
  let best = _263 in
  let _264 = flatten_add best in
  let _265 = sort_add_terms _264 in
  let _266 = combine_like_terms _265 in
  let rescanned = _266 in
  let _267 = build_add rescanned in
  _267

let rec search_opt =
 fun fuel e ->
  match fuel with
  | Z -> e
  | S fuel1 -> (
      match e with
      | Add (a, b) ->
          let _268 = search_opt fuel1 a in
          let sa = _268 in
          let _269 = search_opt fuel1 b in
          let sb = _269 in
          let _270 = normalize_add_flat sa sb in
          let merged = _270 in
          let _271 = normalize_add_flat sb sa in
          let flipped = _271 in
          let _272 = normalize_add_flat merged flipped in
          let next = _272 in
          let _273 = normalize_add_flat next merged in
          let final = _273 in
          let _274 = expr_equal final merged in
          if _274 then merged
          else
            let _275 = search_opt fuel1 final in
            _275
      | Mul (a, b) ->
          let _276 = search_opt fuel1 a in
          let sa = _276 in
          let _277 = search_opt fuel1 b in
          let sb = _277 in
          let _278 = normalize_mul_flat sa sb in
          let merged = _278 in
          let _279 = normalize_mul_flat sb sa in
          let flipped = _279 in
          let _280 = normalize_mul_flat merged flipped in
          let next = _280 in
          let _281 = normalize_mul_flat next merged in
          let final = _281 in
          let _282 = expr_equal final merged in
          if _282 then merged
          else
            let _283 = search_opt fuel1 final in
            _283
      | _ -> e)

let rec normalize =
 fun e ->
  match e with
  | Const _ -> e
  | Var _ -> e
  | Add (a, b) -> (
      let _284 = normalize a in
      let na = _284 in
      let _285 = normalize b in
      let nb = _285 in
      match na with
      | Const x ->
          let _286 = 0 in
          let _287 = x = _286 in
          if _287 then nb
          else
            let _288 = normalize_add_flat na nb in
            _288
      | _ ->
          let _289 = normalize_add_flat na nb in
          _289)
  | Mul (a, b) -> (
      let _290 = normalize a in
      let na = _290 in
      let _291 = normalize b in
      let nb = _291 in
      match na with
      | Const x ->
          let _292 = 0 in
          let _293 = x = _292 in
          if _293 then na
          else
            let _294 = 1 in
            let _295 = x = _294 in
            if _295 then nb
            else
              let _296 = normalize_mul_flat na nb in
              _296
      | _ ->
          let _297 = normalize_mul_flat na nb in
          _297)

let rec simplify_aux =
 fun e ->
  let _298 = normalize e in
  let next = _298 in
  let _299 = expr_equal e next in
  if _299 then next
  else
    let _300 = simplify_aux next in
    _300

let rec diffx =
 fun e ->
  match e with
  | Const _ ->
      let _301 = 0 in
      let _302 = Const _301 in
      _302
  | Var v -> (
      match v with
      | X ->
          let _303 = 1 in
          let _304 = Const _303 in
          _304
      | Y ->
          let _305 = 0 in
          let _306 = Const _305 in
          _306)
  | Add (a, b) ->
      let _307 = diffx a in
      let _308 = diffx b in
      let _309 = Add (_307, _308) in
      _309
  | Mul (a, b) ->
      let _310 = diffx a in
      let _311 = Mul (_310, b) in
      let _312 = diffx b in
      let _313 = Mul (a, _312) in
      let _314 = Add (_311, _313) in
      _314

let rec eval =
 fun e x y ->
  match e with
  | Const c -> c
  | Var v -> ( match v with X -> x | Y -> y)
  | Add (a, b) ->
      let _315 = eval a x y in
      let _316 = eval b x y in
      let _317 = _315 + _316 in
      _317
  | Mul (a, b) ->
      let _318 = eval a x y in
      let _319 = eval b x y in
      let _320 = _318 * _319 in
      _320

let rec main =
 fun e ->
  let _321 = diffx e in
  let d = _321 in
  let _322 = diffx d in
  let f = _322 in
  let _323 = simplify_aux f in
  _323
