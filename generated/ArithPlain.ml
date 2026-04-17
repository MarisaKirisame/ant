open ArithCEK

type nat = ArithCEK.nat
type var = ArithCEK.var
type expr = ArithCEK.expr
type factor_result = ArithCEK.factor_result
type expr_list = ArithCEK.expr_list
type pick_result = ArithCEK.pick_result

let rec var_rank = fun v -> match v with X -> 0 | Y -> 1
let rec expr_rank = fun e -> match e with Const _ -> 0 | Var _ -> 1 | Add (_, _) -> 2 | Mul (_, _) -> 3

let rec compare_expr =
 fun a b ->
  let ra = expr_rank a in
  let rb = expr_rank b in
  let _0 = ra < rb in
  if _0 then 0 - 1
  else
<<<<<<< HEAD
    match a with
    | Const x -> ( match b with Const y -> if x < y then 0 - 1 else if x > y then 1 else 0 | _ -> 0)
    | Var va -> (
        match b with
        | Var vb ->
            let rva = var_rank va in
            let rvb = var_rank vb in
            if rva < rvb then 0 - 1 else if rva > rvb then 1 else 0
        | _ -> 0)
    | Add (a1, a2) -> (
        match b with
        | Add (b1, b2) ->
            let c1 = compare_expr a1 b1 in
            if c1 = 0 then compare_expr a2 b2 else c1
        | _ -> 0)
    | Mul (a1, a2) -> (
        match b with
        | Mul (b1, b2) ->
            let c1 = compare_expr a1 b1 in
            if c1 = 0 then compare_expr a2 b2 else c1
        | _ -> 0)
=======
    let _1 = ra > rb in
    if _1 then 1
    else
      match a with
      | Const x -> (
          match b with
          | Const y ->
              let _2 = x < y in
              if _2 then 0 - 1
              else
                let _3 = x > y in
                if _3 then 1 else 0)
      | Var va -> (
          match b with
          | Var vb ->
              let rva = var_rank va in
              let rvb = var_rank vb in
              let _4 = rva < rvb in
              if _4 then 0 - 1
              else
                let _5 = rva > rvb in
                if _5 then 1 else 0)
      | Add (a1, a2) -> (
          match b with
          | Add (b1, b2) ->
              let c1 = compare_expr a1 b1 in
              let _6 = c1 = 0 in
              if _6 then compare_expr a2 b2 else c1)
      | Mul (a1, a2) -> (
          match b with
          | Mul (b1, b2) ->
              let c1 = compare_expr a1 b1 in
              let _7 = c1 = 0 in
              if _7 then compare_expr a2 b2 else c1)
>>>>>>> 6910b6d (save)

let rec expr_equal =
 fun a b ->
  match a with
  | Const x -> ( match b with Const y -> x = y | _ -> false)
  | Var va -> (
      match b with
      | Var vb ->
          let _8 = var_rank va in
          let _9 = var_rank vb in
          _8 = _9
      | _ -> false)
  | Add (a1, a2) -> (
      match b with
      | Add (b1, b2) ->
          let _10 = expr_equal a1 b1 in
          let _11 = expr_equal a2 b2 in
          _10 && _11
      | _ -> false)
  | Mul (a1, a2) -> (
      match b with
      | Mul (b1, b2) ->
          let _12 = expr_equal a1 b1 in
          let _13 = expr_equal a2 b2 in
          _12 && _13
      | _ -> false)

let rec expr_size =
 fun e ->
  match e with
  | Const _ -> 1
  | Var _ -> 1
  | Add (a, b) ->
      let _14 = expr_size a in
      let _15 = 1 + _14 in
      let _16 = expr_size b in
      _15 + _16
  | Mul (a, b) ->
      let _17 = expr_size a in
      let _18 = 1 + _17 in
      let _19 = expr_size b in
      _18 + _19

let rec better_expr =
 fun a b ->
  let sa = expr_size a in
  let sb = expr_size b in
  let _20 = sa < sb in
  if _20 then a
  else
    let _21 = sb < sa in
    if _21 then b
    else
      let _22 = compare_expr a b in
      let _23 = _22 <= 0 in
      if _23 then a else b

let rec scale =
 fun c e ->
  let _24 = c = 0 in
  if _24 then Const 0
  else
    match e with
    | Const x ->
        let _25 = c * x in
        Const _25
    | _ ->
        let _26 = c = 1 in
        if _26 then e
        else
          let _27 = Const c in
          Mul (_27, e)

let rec coeff_value =
 fun e -> match e with Const x -> x | Mul (lhs, rhs) -> ( match lhs with Const c -> c | _ -> 1) | _ -> 1

let rec coeff_base =
 fun e -> match e with Const _ -> Const 1 | Mul (lhs, rhs) -> ( match lhs with Const _ -> rhs | _ -> e) | _ -> e

let rec extract_factor =
 fun needle e ->
  let _28 = expr_equal needle e in
  if _28 then
    let _31 = Const 1 in
    Found _31
  else
    match e with
    | Mul (a, b) -> (
        let left = extract_factor needle a in
        match left with
        | Found rest ->
            let _29 = Mul (rest, b) in
            Found _29
        | Missing -> (
            let right = extract_factor needle b in
            match right with
            | Found rest ->
                let _30 = Mul (a, rest) in
                Found _30
            | Missing -> Missing))
    | _ -> Missing

let rec search_factor =
 fun left right ->
  match left with
  | Mul (a, b) -> (
      let fa = extract_factor a right in
      match fa with
      | Found rest ->
          let _32 = Add (b, rest) in
          Mul (a, _32)
      | Missing -> (
          let fb = extract_factor b right in
          match fb with
          | Found rest ->
              let _33 = Add (a, rest) in
              Mul (b, _33)
          | Missing -> Add (left, right)))
  | _ -> Add (left, right)

let rec append_exprs =
 fun xs ys ->
  match xs with
  | ENil -> ys
  | ECons (x, rest) ->
      let _34 = append_exprs rest ys in
      ECons (x, _34)

let rec insert_expr =
 fun e xs ->
  match xs with
  | ENil -> ECons (e, ENil)
  | ECons (x, rest) ->
      let _35 = compare_expr e x in
      let _36 = _35 <= 0 in
      if _36 then ECons (e, xs)
      else
        let _37 = insert_expr e rest in
        ECons (x, _37)

let rec sort_exprs =
 fun xs ->
  match xs with
  | ENil -> ENil
  | ECons (x, rest) ->
      let _38 = sort_exprs rest in
      insert_expr x _38

let rec compare_add_term =
 fun a b ->
  let abase = coeff_base a in
  let bbase = coeff_base b in
  let cbase = compare_expr abase bbase in
  let _39 = cbase = 0 in
  if _39 then
    let acoeff = coeff_value a in
    let bcoeff = coeff_value b in
    let _40 = acoeff < bcoeff in
    if _40 then 0 - 1
    else
      let _41 = acoeff > bcoeff in
      if _41 then 1 else compare_expr a b
  else cbase

let rec insert_add_term =
 fun e xs ->
  match xs with
  | ENil -> ECons (e, ENil)
  | ECons (x, rest) ->
      let _42 = compare_add_term e x in
      let _43 = _42 <= 0 in
      if _43 then ECons (e, xs)
      else
        let _44 = insert_add_term e rest in
        ECons (x, _44)

let rec sort_add_terms =
 fun xs ->
  match xs with
  | ENil -> ENil
  | ECons (x, rest) ->
      let _45 = sort_add_terms rest in
      insert_add_term x _45

let rec reverse_exprs_aux =
 fun xs acc ->
  match xs with
  | ENil -> acc
  | ECons (x, rest) ->
      let _46 = ECons (x, acc) in
      reverse_exprs_aux rest _46

let rec reverse_exprs = fun xs -> reverse_exprs_aux xs ENil

let rec flatten_add =
 fun e ->
  match e with
  | Add (a, b) ->
      let _47 = flatten_add a in
      let _48 = flatten_add b in
      append_exprs _47 _48
  | Const x ->
      let _49 = x = 0 in
      if _49 then ENil else ECons (e, ENil)
  | _ -> ECons (e, ENil)

let rec flatten_mul =
 fun e ->
  match e with
  | Mul (a, b) ->
      let _50 = flatten_mul a in
      let _51 = flatten_mul b in
      append_exprs _50 _51
  | _ -> ECons (e, ENil)

let rec mul_coeff =
 fun e -> match e with Const x -> x | Mul (lhs, rhs) -> ( match lhs with Const c -> c | _ -> 1) | _ -> 1

let rec mul_base =
 fun e -> match e with Const _ -> Const 1 | Mul (lhs, rhs) -> ( match lhs with Const _ -> rhs | _ -> e) | _ -> e

let rec mul_total_coeff =
 fun xs ->
  match xs with
  | ENil -> 1
  | ECons (x, rest) ->
      let _52 = mul_coeff x in
      let _53 = mul_total_coeff rest in
      _52 * _53

let rec mul_bases =
 fun xs ->
  match xs with
  | ENil -> ENil
  | ECons (x, rest) -> (
      let base = mul_base x in
      match base with
      | Const one ->
          let _54 = one = 1 in
          if _54 then mul_bases rest
          else
            let _55 = mul_bases rest in
            insert_expr base _55
      | _ ->
          let _56 = mul_bases rest in
          insert_expr base _56)

let rec build_mul =
 fun xs ->
  match xs with
  | ENil -> Const 1
  | ECons (x, rest) -> (
      match rest with
      | ENil -> x
      | _ ->
          let _57 = build_mul rest in
          Mul (x, _57))

let rec normalize_mul_flat =
 fun left right ->
  let factors =
    let _60 = flatten_mul left in
    let _61 = flatten_mul right in
    append_exprs _60 _61
  in
  let coeff = mul_total_coeff factors in
  let _58 = coeff = 0 in
  if _58 then Const 0
  else
    let bases = mul_bases factors in
    let base_expr = build_mul bases in
    let _59 = coeff = 1 in
    if _59 then base_expr else scale coeff base_expr

let rec combine_like_terms_acc =
 fun base coeff xs ->
  match xs with
  | ENil ->
      let _62 = coeff = 0 in
      if _62 then ENil
      else
        let _63 = scale coeff base in
        ECons (_63, ENil)
  | ECons (x, rest) ->
      let xbase = coeff_base x in
      let xcoeff = coeff_value x in
      let _64 = expr_equal base xbase in
      if _64 then
        let _67 = coeff + xcoeff in
        combine_like_terms_acc base _67 rest
      else
        let tail = combine_like_terms_acc xbase xcoeff rest in
        let _65 = coeff = 0 in
        if _65 then tail
        else
          let _66 = scale coeff base in
          insert_add_term _66 tail

let rec combine_like_terms =
 fun xs ->
  match xs with
  | ENil -> ENil
  | ECons (x, rest) ->
      let base = coeff_base x in
      let coeff = coeff_value x in
      combine_like_terms_acc base coeff rest

let rec factor_adjacent =
 fun xs ->
  match xs with
  | ENil -> ENil
  | ECons (x, rest) -> (
      match rest with
      | ENil -> ECons (x, ENil)
      | ECons (y, ys) ->
          let merged = search_factor x y in
          let _68 = Add (x, y) in
          let _69 = expr_equal merged _68 in
          if _69 then
            let _71 = factor_adjacent rest in
            ECons (x, _71)
          else
            let _70 = insert_add_term merged ys in
            factor_adjacent _70)

let rec pick_factored =
 fun x xs ->
  match xs with
  | ENil -> NoPick
  | ECons (y, rest) ->
      let merged = search_factor x y in
      let _72 = Add (x, y) in
      let _73 = expr_equal merged _72 in
      if _73 then
        let _74 = pick_factored x rest in
        match _74 with
        | NoPick -> NoPick
        | Pick (term, remain) ->
            let _75 = ECons (y, remain) in
            Pick (term, _75)
      else Pick (merged, rest)

let rec search_terms =
 fun xs ->
  match xs with
  | ENil -> ENil
  | ECons (x, rest) -> (
      let _76 = pick_factored x rest in
      match _76 with
      | NoPick ->
          let _77 = search_terms rest in
          ECons (x, _77)
      | Pick (merged, remain) ->
          let _78 = insert_add_term merged remain in
          search_terms _78)

let rec build_add =
 fun xs ->
  match xs with
  | ENil -> Const 0
  | ECons (x, rest) -> (
      match rest with
      | ENil -> x
      | _ ->
          let _79 = build_add rest in
          Add (x, _79))

let rec search_round =
 fun xs ->
  let factored0 = factor_adjacent xs in
  let factored1 = search_terms factored0 in
  let factored2 = factor_adjacent factored1 in
  search_terms factored2

let rec normalize_add_flat =
 fun left right ->
  let terms =
    let _83 = flatten_add left in
    let _84 = flatten_add right in
    append_exprs _83 _84
  in
  let sorted = sort_add_terms terms in
  let combined = combine_like_terms sorted in
  let forward = search_round combined in
  let backward =
    let _82 = reverse_exprs combined in
    search_round _82
  in
  let forward_expr = build_add forward in
  let backward_expr = build_add backward in
  let best = better_expr forward_expr backward_expr in
  let rescanned =
    let _80 = flatten_add best in
    let _81 = sort_add_terms _80 in
    combine_like_terms _81
  in
  build_add rescanned

let rec search_opt =
 fun fuel e ->
  match fuel with
  | Z -> e
  | S fuel1 -> (
      match e with
      | Add (a, b) ->
          let sa = search_opt fuel1 a in
          let sb = search_opt fuel1 b in
          let merged = normalize_add_flat sa sb in
          let flipped = normalize_add_flat sb sa in
          let next = normalize_add_flat merged flipped in
          let final = normalize_add_flat next merged in
          let _85 = expr_equal final merged in
          if _85 then merged else search_opt fuel1 final
      | Mul (a, b) ->
          let sa = search_opt fuel1 a in
          let sb = search_opt fuel1 b in
          let merged = normalize_mul_flat sa sb in
          let flipped = normalize_mul_flat sb sa in
          let next = normalize_mul_flat merged flipped in
          let final = normalize_mul_flat next merged in
          let _86 = expr_equal final merged in
          if _86 then merged else search_opt fuel1 final
      | _ -> e)

let rec normalize =
 fun e ->
  match e with
  | Const _ -> e
  | Var _ -> e
  | Add (a, b) -> (
      let na = normalize a in
      let nb = normalize b in
      match na with
      | Const x ->
          let _87 = x = 0 in
          if _87 then nb else normalize_add_flat na nb
      | _ -> normalize_add_flat na nb)
  | Mul (a, b) -> (
      let na = normalize a in
      let nb = normalize b in
      match na with
      | Const x ->
          let _88 = x = 0 in
          if _88 then na
          else
            let _89 = x = 1 in
            if _89 then nb else normalize_mul_flat na nb
      | _ -> normalize_mul_flat na nb)

let rec simplify_aux =
 fun e ->
  let next = normalize e in
  let _90 = expr_equal e next in
  if _90 then next else simplify_aux next

let rec diffx =
 fun e ->
  match e with
  | Const _ -> Const 0
  | Var v -> ( match v with X -> Const 1 | Y -> Const 0)
  | Add (a, b) ->
      let _91 = diffx a in
      let _92 = diffx b in
      Add (_91, _92)
  | Mul (a, b) ->
      let _93 = diffx a in
      let _94 = Mul (_93, b) in
      let _95 = diffx b in
      let _96 = Mul (a, _95) in
      Add (_94, _96)

let rec eval =
 fun e x y ->
  match e with
  | Const c -> c
  | Var v -> ( match v with X -> x | Y -> y)
  | Add (a, b) ->
      let _97 = eval a x y in
      let _98 = eval b x y in
      _97 + _98
  | Mul (a, b) ->
      let _99 = eval a x y in
      let _100 = eval b x y in
      _99 * _100

let rec main =
 fun e ->
  let d = diffx e in
  let f = diffx d in
  simplify_aux f
