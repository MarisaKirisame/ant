type nat = Z | S of nat
type var = X | Y
type expr = Const of int | Var of var | Add of expr * expr | Mul of expr * expr
type factor_result = Missing | Found of expr
type expr_list = ENil | ECons of expr * expr_list
type pick_result = NoPick | Pick of expr * expr_list

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
    let _35 = 0 in
    let _36 = 1 in
    let _37 = _35 - _36 in
    _37
  else
    let _9 = ra > rb in
    if _9 then
      let _34 = 1 in
      _34
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
                  _12)
      | Var va -> (
          match b with
          | Var vb ->
              let _17 = var_rank va in
              let rva = _17 in
              let _18 = var_rank vb in
              let rvb = _18 in
              let _19 = rva < rvb in
              if _19 then
                let _23 = 0 in
                let _24 = 1 in
                let _25 = _23 - _24 in
                _25
              else
                let _20 = rva > rvb in
                if _20 then
                  let _22 = 1 in
                  _22
                else
                  let _21 = 0 in
                  _21)
      | Add (a1, a2) -> (
          match b with
          | Add (b1, b2) ->
              let _26 = compare_expr a1 b1 in
              let c1 = _26 in
              let _27 = 0 in
              let _28 = c1 = _27 in
              if _28 then
                let _29 = compare_expr a2 b2 in
                _29
              else c1)
      | Mul (a1, a2) -> (
          match b with
          | Mul (b1, b2) ->
              let _30 = compare_expr a1 b1 in
              let c1 = _30 in
              let _31 = 0 in
              let _32 = c1 = _31 in
              if _32 then
                let _33 = compare_expr a2 b2 in
                _33
              else c1)

let rec expr_equal =
 fun a b ->
  match a with
  | Const x -> (
      match b with
      | Const y ->
          let _38 = x = y in
          _38
      | _ ->
          let _39 = false in
          _39)
  | Var va -> (
      match b with
      | Var vb ->
          let _40 = var_rank va in
          let _41 = var_rank vb in
          let _42 = _40 = _41 in
          _42
      | _ ->
          let _43 = false in
          _43)
  | Add (a1, a2) -> (
      match b with
      | Add (b1, b2) ->
          let _44 = expr_equal a1 b1 in
          let _45 = expr_equal a2 b2 in
          let _46 = _44 && _45 in
          _46
      | _ ->
          let _47 = false in
          _47)
  | Mul (a1, a2) -> (
      match b with
      | Mul (b1, b2) ->
          let _48 = expr_equal a1 b1 in
          let _49 = expr_equal a2 b2 in
          let _50 = _48 && _49 in
          _50
      | _ ->
          let _51 = false in
          _51)

let rec expr_size =
 fun e ->
  match e with
  | Const _ ->
      let _52 = 1 in
      _52
  | Var _ ->
      let _53 = 1 in
      _53
  | Add (a, b) ->
      let _54 = 1 in
      let _55 = expr_size a in
      let _56 = _54 + _55 in
      let _57 = expr_size b in
      let _58 = _56 + _57 in
      _58
  | Mul (a, b) ->
      let _59 = 1 in
      let _60 = expr_size a in
      let _61 = _59 + _60 in
      let _62 = expr_size b in
      let _63 = _61 + _62 in
      _63

let rec better_expr =
 fun a b ->
  let _64 = expr_size a in
  let sa = _64 in
  let _65 = expr_size b in
  let sb = _65 in
  let _66 = sa < sb in
  if _66 then a
  else
    let _67 = sb < sa in
    if _67 then b
    else
      let _68 = compare_expr a b in
      let _69 = 0 in
      let _70 = _68 <= _69 in
      if _70 then a else b

let rec scale =
 fun c e ->
  let _71 = 0 in
  let _72 = c = _71 in
  if _72 then
    let _79 = 0 in
    let _80 = Const _79 in
    _80
  else
    match e with
    | Const x ->
        let _73 = c * x in
        let _74 = Const _73 in
        _74
    | _ ->
        let _75 = 1 in
        let _76 = c = _75 in
        if _76 then e
        else
          let _77 = Const c in
          let _78 = Mul (_77, e) in
          _78

let rec coeff_value =
 fun e ->
  match e with
  | Const x -> x
  | Mul (lhs, rhs) -> (
      match lhs with
      | Const c -> c
      | _ ->
          let _81 = 1 in
          _81)
  | _ ->
      let _82 = 1 in
      _82

let rec coeff_base =
 fun e ->
  match e with
  | Const _ ->
      let _83 = 1 in
      let _84 = Const _83 in
      _84
  | Mul (lhs, rhs) -> ( match lhs with Const _ -> rhs | _ -> e)
  | _ -> e

let rec extract_factor =
 fun needle e ->
  let _85 = expr_equal needle e in
  if _85 then
    let _94 = 1 in
    let _95 = Const _94 in
    let _96 = Found _95 in
    _96
  else
    match e with
    | Mul (a, b) -> (
        let _86 = extract_factor needle a in
        let left = _86 in
        match left with
        | Found rest ->
            let _87 = Mul (rest, b) in
            let _88 = Found _87 in
            _88
        | Missing -> (
            let _89 = extract_factor needle b in
            let right = _89 in
            match right with
            | Found rest ->
                let _90 = Mul (a, rest) in
                let _91 = Found _90 in
                _91
            | Missing ->
                let _92 = Missing in
                _92))
    | _ ->
        let _93 = Missing in
        _93

let rec search_factor =
 fun left right ->
  match left with
  | Mul (a, b) -> (
      let _97 = extract_factor a right in
      let fa = _97 in
      match fa with
      | Found rest ->
          let _98 = Add (b, rest) in
          let _99 = Mul (a, _98) in
          _99
      | Missing -> (
          let _100 = extract_factor b right in
          let fb = _100 in
          match fb with
          | Found rest ->
              let _101 = Add (a, rest) in
              let _102 = Mul (b, _101) in
              _102
          | Missing ->
              let _103 = Add (left, right) in
              _103))
  | _ ->
      let _104 = Add (left, right) in
      _104

let rec append_exprs =
 fun xs ys ->
  match xs with
  | ENil -> ys
  | ECons (x, rest) ->
      let _105 = append_exprs rest ys in
      let _106 = ECons (x, _105) in
      _106

let rec insert_expr =
 fun e xs ->
  match xs with
  | ENil ->
      let _107 = ENil in
      let _108 = ECons (e, _107) in
      _108
  | ECons (x, rest) ->
      let _109 = compare_expr e x in
      let _110 = 0 in
      let _111 = _109 <= _110 in
      if _111 then
        let _114 = ECons (e, xs) in
        _114
      else
        let _112 = insert_expr e rest in
        let _113 = ECons (x, _112) in
        _113

let rec sort_exprs =
 fun xs ->
  match xs with
  | ENil ->
      let _115 = ENil in
      _115
  | ECons (x, rest) ->
      let _116 = sort_exprs rest in
      let _117 = insert_expr x _116 in
      _117

let rec compare_add_term =
 fun a b ->
  let _118 = coeff_base a in
  let abase = _118 in
  let _119 = coeff_base b in
  let bbase = _119 in
  let _120 = compare_expr abase bbase in
  let cbase = _120 in
  let _121 = 0 in
  let _122 = cbase = _121 in
  if _122 then
    let _123 = coeff_value a in
    let acoeff = _123 in
    let _124 = coeff_value b in
    let bcoeff = _124 in
    let _125 = acoeff < bcoeff in
    if _125 then
      let _129 = 0 in
      let _130 = 1 in
      let _131 = _129 - _130 in
      _131
    else
      let _126 = acoeff > bcoeff in
      if _126 then
        let _128 = 1 in
        _128
      else
        let _127 = compare_expr a b in
        _127
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
      let _142 = insert_add_term x _141 in
      _142

let rec reverse_exprs_aux =
 fun xs acc ->
  match xs with
  | ENil -> acc
  | ECons (x, rest) ->
      let _143 = ECons (x, acc) in
      let _144 = reverse_exprs_aux rest _143 in
      _144

let rec reverse_exprs =
 fun xs ->
  let _145 = ENil in
  let _146 = reverse_exprs_aux xs _145 in
  _146

let rec flatten_add =
 fun e ->
  match e with
  | Add (a, b) ->
      let _147 = flatten_add a in
      let _148 = flatten_add b in
      let _149 = append_exprs _147 _148 in
      _149
  | Const x ->
      let _150 = 0 in
      let _151 = x = _150 in
      if _151 then
        let _154 = ENil in
        _154
      else
        let _152 = ENil in
        let _153 = ECons (e, _152) in
        _153
  | _ ->
      let _155 = ENil in
      let _156 = ECons (e, _155) in
      _156

let rec flatten_mul =
 fun e ->
  match e with
  | Mul (a, b) ->
      let _157 = flatten_mul a in
      let _158 = flatten_mul b in
      let _159 = append_exprs _157 _158 in
      _159
  | _ ->
      let _160 = ENil in
      let _161 = ECons (e, _160) in
      _161

let rec mul_coeff =
 fun e ->
  match e with
  | Const x -> x
  | Mul (lhs, rhs) -> (
      match lhs with
      | Const c -> c
      | _ ->
          let _162 = 1 in
          _162)
  | _ ->
      let _163 = 1 in
      _163

let rec mul_base =
 fun e ->
  match e with
  | Const _ ->
      let _164 = 1 in
      let _165 = Const _164 in
      _165
  | Mul (lhs, rhs) -> ( match lhs with Const _ -> rhs | _ -> e)
  | _ -> e

let rec mul_total_coeff =
 fun xs ->
  match xs with
  | ENil ->
      let _166 = 1 in
      _166
  | ECons (x, rest) ->
      let _167 = mul_coeff x in
      let _168 = mul_total_coeff rest in
      let _169 = _167 * _168 in
      _169

let rec mul_bases =
 fun xs ->
  match xs with
  | ENil ->
      let _170 = ENil in
      _170
  | ECons (x, rest) -> (
      let _171 = mul_base x in
      let base = _171 in
      match base with
      | Const one ->
          let _172 = 1 in
          let _173 = one = _172 in
          if _173 then
            let _176 = mul_bases rest in
            _176
          else
            let _174 = mul_bases rest in
            let _175 = insert_expr base _174 in
            _175
      | _ ->
          let _177 = mul_bases rest in
          let _178 = insert_expr base _177 in
          _178)

let rec build_mul =
 fun xs ->
  match xs with
  | ENil ->
      let _179 = 1 in
      let _180 = Const _179 in
      _180
  | ECons (x, rest) -> (
      match rest with
      | ENil -> x
      | _ ->
          let _181 = build_mul rest in
          let _182 = Mul (x, _181) in
          _182)

let rec normalize_mul_flat =
 fun left right ->
  let _183 = flatten_mul left in
  let _184 = flatten_mul right in
  let _185 = append_exprs _183 _184 in
  let factors = _185 in
  let _186 = mul_total_coeff factors in
  let coeff = _186 in
  let _187 = 0 in
  let _188 = coeff = _187 in
  if _188 then
    let _194 = 0 in
    let _195 = Const _194 in
    _195
  else
    let _189 = mul_bases factors in
    let bases = _189 in
    let _190 = build_mul bases in
    let base_expr = _190 in
    let _191 = 1 in
    let _192 = coeff = _191 in
    if _192 then base_expr
    else
      let _193 = scale coeff base_expr in
      _193

let rec combine_like_terms_acc =
 fun base coeff xs ->
  match xs with
  | ENil ->
      let _196 = 0 in
      let _197 = coeff = _196 in
      if _197 then
        let _201 = ENil in
        _201
      else
        let _198 = scale coeff base in
        let _199 = ENil in
        let _200 = ECons (_198, _199) in
        _200
  | ECons (x, rest) ->
      let _202 = coeff_base x in
      let xbase = _202 in
      let _203 = coeff_value x in
      let xcoeff = _203 in
      let _204 = expr_equal base xbase in
      if _204 then
        let _210 = coeff + xcoeff in
        let _211 = combine_like_terms_acc base _210 rest in
        _211
      else
        let _205 = combine_like_terms_acc xbase xcoeff rest in
        let tail = _205 in
        let _206 = 0 in
        let _207 = coeff = _206 in
        if _207 then tail
        else
          let _208 = scale coeff base in
          let _209 = insert_add_term _208 tail in
          _209

let rec combine_like_terms =
 fun xs ->
  match xs with
  | ENil ->
      let _212 = ENil in
      _212
  | ECons (x, rest) ->
      let _213 = coeff_base x in
      let base = _213 in
      let _214 = coeff_value x in
      let coeff = _214 in
      let _215 = combine_like_terms_acc base coeff rest in
      _215

let rec factor_adjacent =
 fun xs ->
  match xs with
  | ENil ->
      let _216 = ENil in
      _216
  | ECons (x, rest) -> (
      match rest with
      | ENil ->
          let _217 = ENil in
          let _218 = ECons (x, _217) in
          _218
      | ECons (y, ys) ->
          let _219 = search_factor x y in
          let merged = _219 in
          let _220 = Add (x, y) in
          let _221 = expr_equal merged _220 in
          if _221 then
            let _224 = factor_adjacent rest in
            let _225 = ECons (x, _224) in
            _225
          else
            let _222 = insert_add_term merged ys in
            let _223 = factor_adjacent _222 in
            _223)

let rec pick_factored =
 fun x xs ->
  match xs with
  | ENil ->
      let _226 = NoPick in
      _226
  | ECons (y, rest) ->
      let _227 = search_factor x y in
      let merged = _227 in
      let _228 = Add (x, y) in
      let _229 = expr_equal merged _228 in
      if _229 then
        let _231 = pick_factored x rest in
        match _231 with
        | NoPick ->
            let _232 = NoPick in
            _232
        | Pick (term, remain) ->
            let _233 = ECons (y, remain) in
            let _234 = Pick (term, _233) in
            _234
      else
        let _230 = Pick (merged, rest) in
        _230

let rec search_terms =
 fun xs ->
  match xs with
  | ENil ->
      let _235 = ENil in
      _235
  | ECons (x, rest) -> (
      let _236 = pick_factored x rest in
      match _236 with
      | NoPick ->
          let _237 = search_terms rest in
          let _238 = ECons (x, _237) in
          _238
      | Pick (merged, remain) ->
          let _239 = insert_add_term merged remain in
          let _240 = search_terms _239 in
          _240)

let rec build_add =
 fun xs ->
  match xs with
  | ENil ->
      let _241 = 0 in
      let _242 = Const _241 in
      _242
  | ECons (x, rest) -> (
      match rest with
      | ENil -> x
      | _ ->
          let _243 = build_add rest in
          let _244 = Add (x, _243) in
          _244)

let rec search_round =
 fun xs ->
  let _245 = factor_adjacent xs in
  let factored0 = _245 in
  let _246 = search_terms factored0 in
  let factored1 = _246 in
  let _247 = factor_adjacent factored1 in
  let factored2 = _247 in
  let _248 = search_terms factored2 in
  _248

let rec normalize_add_flat =
 fun left right ->
  let _249 = flatten_add left in
  let _250 = flatten_add right in
  let _251 = append_exprs _249 _250 in
  let terms = _251 in
  let _252 = sort_add_terms terms in
  let sorted = _252 in
  let _253 = combine_like_terms sorted in
  let combined = _253 in
  let _254 = search_round combined in
  let forward = _254 in
  let _255 = reverse_exprs combined in
  let _256 = search_round _255 in
  let backward = _256 in
  let _257 = build_add forward in
  let forward_expr = _257 in
  let _258 = build_add backward in
  let backward_expr = _258 in
  let _259 = better_expr forward_expr backward_expr in
  let best = _259 in
  let _260 = flatten_add best in
  let _261 = sort_add_terms _260 in
  let _262 = combine_like_terms _261 in
  let rescanned = _262 in
  let _263 = build_add rescanned in
  _263

let rec search_opt =
 fun fuel e ->
  match fuel with
  | Z -> e
  | S fuel1 -> (
      match e with
      | Add (a, b) ->
          let _264 = search_opt fuel1 a in
          let sa = _264 in
          let _265 = search_opt fuel1 b in
          let sb = _265 in
          let _266 = normalize_add_flat sa sb in
          let merged = _266 in
          let _267 = normalize_add_flat sb sa in
          let flipped = _267 in
          let _268 = normalize_add_flat merged flipped in
          let next = _268 in
          let _269 = normalize_add_flat next merged in
          let final = _269 in
          let _270 = expr_equal final merged in
          if _270 then merged
          else
            let _271 = search_opt fuel1 final in
            _271
      | Mul (a, b) ->
          let _272 = search_opt fuel1 a in
          let sa = _272 in
          let _273 = search_opt fuel1 b in
          let sb = _273 in
          let _274 = normalize_mul_flat sa sb in
          let merged = _274 in
          let _275 = normalize_mul_flat sb sa in
          let flipped = _275 in
          let _276 = normalize_mul_flat merged flipped in
          let next = _276 in
          let _277 = normalize_mul_flat next merged in
          let final = _277 in
          let _278 = expr_equal final merged in
          if _278 then merged
          else
            let _279 = search_opt fuel1 final in
            _279
      | _ -> e)

let rec normalize =
 fun e ->
  match e with
  | Const _ -> e
  | Var _ -> e
  | Add (a, b) -> (
      let _280 = normalize a in
      let na = _280 in
      let _281 = normalize b in
      let nb = _281 in
      match na with
      | Const x ->
          let _282 = 0 in
          let _283 = x = _282 in
          if _283 then nb
          else
            let _284 = normalize_add_flat na nb in
            _284
      | _ ->
          let _285 = normalize_add_flat na nb in
          _285)
  | Mul (a, b) -> (
      let _286 = normalize a in
      let na = _286 in
      let _287 = normalize b in
      let nb = _287 in
      match na with
      | Const x ->
          let _288 = 0 in
          let _289 = x = _288 in
          if _289 then na
          else
            let _290 = 1 in
            let _291 = x = _290 in
            if _291 then nb
            else
              let _292 = normalize_mul_flat na nb in
              _292
      | _ ->
          let _293 = normalize_mul_flat na nb in
          _293)

let rec simplify_aux =
 fun e ->
  let _294 = normalize e in
  let next = _294 in
  let _295 = expr_equal e next in
  if _295 then next
  else
    let _296 = simplify_aux next in
    _296

let rec diffx =
 fun e ->
  match e with
  | Const _ ->
      let _297 = 0 in
      let _298 = Const _297 in
      _298
  | Var v -> (
      match v with
      | X ->
          let _299 = 1 in
          let _300 = Const _299 in
          _300
      | Y ->
          let _301 = 0 in
          let _302 = Const _301 in
          _302)
  | Add (a, b) ->
      let _303 = diffx a in
      let _304 = diffx b in
      let _305 = Add (_303, _304) in
      _305
  | Mul (a, b) ->
      let _306 = diffx a in
      let _307 = Mul (_306, b) in
      let _308 = diffx b in
      let _309 = Mul (a, _308) in
      let _310 = Add (_307, _309) in
      _310

let rec eval =
 fun e x y ->
  match e with
  | Const c -> c
  | Var v -> ( match v with X -> x | Y -> y)
  | Add (a, b) ->
      let _311 = eval a x y in
      let _312 = eval b x y in
      let _313 = _311 + _312 in
      _313
  | Mul (a, b) ->
      let _314 = eval a x y in
      let _315 = eval b x y in
      let _316 = _314 * _315 in
      _316

let rec main =
 fun e ->
  let _317 = diffx e in
  let d = _317 in
  let _318 = diffx d in
  let f = _318 in
  let _319 = simplify_aux f in
  _319
