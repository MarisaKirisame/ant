type nat = Z | S of nat
type var = X | Y
type expr = Const of int | Var of var | Add of expr * expr | Mul of expr * expr
type factor_result = Missing | Found of expr
type expr_list = ENil | ECons of expr * expr_list
type pick_result = NoPick | Pick of expr * expr_list

let rec var_rank = fun v -> match v with X -> 0 | Y -> 1
let rec expr_rank = fun e -> match e with Const _ -> 0 | Var _ -> 1 | Add (_, _) -> 2 | Mul (_, _) -> 3

let rec compare_expr =
 fun a b ->
  let ra = expr_rank a in
  let rb = expr_rank b in
  let _'anf0 = ra < rb in
  if _'anf0 then 0 - 1
  else
    let _'anf1 = ra > rb in
    if _'anf1 then 1
    else
      match a with
      | Const x -> (
          match b with
          | Const y ->
              let _'anf2 = x < y in
              if _'anf2 then 0 - 1
              else
                let _'anf3 = x > y in
                if _'anf3 then 1 else 0)
      | Var va -> (
          match b with
          | Var vb ->
              let rva = var_rank va in
              let rvb = var_rank vb in
              let _'anf4 = rva < rvb in
              if _'anf4 then 0 - 1
              else
                let _'anf5 = rva > rvb in
                if _'anf5 then 1 else 0)
      | Add (a1, a2) -> (
          match b with
          | Add (b1, b2) ->
              let c1 = compare_expr a1 b1 in
              let _'anf6 = c1 = 0 in
              if _'anf6 then compare_expr a2 b2 else c1)
      | Mul (a1, a2) -> (
          match b with
          | Mul (b1, b2) ->
              let c1 = compare_expr a1 b1 in
              let _'anf7 = c1 = 0 in
              if _'anf7 then compare_expr a2 b2 else c1)

let rec expr_equal =
 fun a b ->
  match a with
  | Const x -> ( match b with Const y -> x = y | _ -> false)
  | Var va -> (
      match b with
      | Var vb ->
          let _'anf8 = var_rank va in
          let _'anf9 = var_rank vb in
          _'anf8 = _'anf9
      | _ -> false)
  | Add (a1, a2) -> (
      match b with
      | Add (b1, b2) ->
          let _'anf10 = expr_equal a1 b1 in
          let _'anf11 = expr_equal a2 b2 in
          _'anf10 && _'anf11
      | _ -> false)
  | Mul (a1, a2) -> (
      match b with
      | Mul (b1, b2) ->
          let _'anf12 = expr_equal a1 b1 in
          let _'anf13 = expr_equal a2 b2 in
          _'anf12 && _'anf13
      | _ -> false)

let rec expr_size =
 fun e ->
  match e with
  | Const _ -> 1
  | Var _ -> 1
  | Add (a, b) ->
      let _'anf14 = expr_size a in
      let _'anf15 = 1 + _'anf14 in
      let _'anf16 = expr_size b in
      _'anf15 + _'anf16
  | Mul (a, b) ->
      let _'anf17 = expr_size a in
      let _'anf18 = 1 + _'anf17 in
      let _'anf19 = expr_size b in
      _'anf18 + _'anf19

let rec better_expr =
 fun a b ->
  let sa = expr_size a in
  let sb = expr_size b in
  let _'anf20 = sa < sb in
  if _'anf20 then a
  else
    let _'anf21 = sb < sa in
    if _'anf21 then b
    else
      let _'anf22 = compare_expr a b in
      let _'anf23 = _'anf22 <= 0 in
      if _'anf23 then a else b

let rec scale =
 fun c e ->
  let _'anf24 = c = 0 in
  if _'anf24 then Const 0
  else
    match e with
    | Const x ->
        let _'anf25 = c * x in
        Const _'anf25
    | _ ->
        let _'anf26 = c = 1 in
        if _'anf26 then e
        else
          let _'anf27 = Const c in
          Mul (_'anf27, e)

let rec coeff_value =
 fun e -> match e with Const x -> x | Mul (lhs, rhs) -> ( match lhs with Const c -> c | _ -> 1) | _ -> 1

let rec coeff_base =
 fun e -> match e with Const _ -> Const 1 | Mul (lhs, rhs) -> ( match lhs with Const _ -> rhs | _ -> e) | _ -> e

let rec extract_factor =
 fun needle e ->
  let _'anf28 = expr_equal needle e in
  if _'anf28 then
    let _'anf31 = Const 1 in
    Found _'anf31
  else
    match e with
    | Mul (a, b) -> (
        let left = extract_factor needle a in
        match left with
        | Found rest ->
            let _'anf29 = Mul (rest, b) in
            Found _'anf29
        | Missing -> (
            let right = extract_factor needle b in
            match right with
            | Found rest ->
                let _'anf30 = Mul (a, rest) in
                Found _'anf30
            | Missing -> Missing))
    | _ -> Missing

let rec search_factor =
 fun left right ->
  match left with
  | Mul (a, b) -> (
      let fa = extract_factor a right in
      match fa with
      | Found rest ->
          let _'anf32 = Add (b, rest) in
          Mul (a, _'anf32)
      | Missing -> (
          let fb = extract_factor b right in
          match fb with
          | Found rest ->
              let _'anf33 = Add (a, rest) in
              Mul (b, _'anf33)
          | Missing -> Add (left, right)))
  | _ -> Add (left, right)

let rec append_exprs =
 fun xs ys ->
  match xs with
  | ENil -> ys
  | ECons (x, rest) ->
      let _'anf34 = append_exprs rest ys in
      ECons (x, _'anf34)

let rec insert_expr =
 fun e xs ->
  match xs with
  | ENil -> ECons (e, ENil)
  | ECons (x, rest) ->
      let _'anf35 = compare_expr e x in
      let _'anf36 = _'anf35 <= 0 in
      if _'anf36 then ECons (e, xs)
      else
        let _'anf37 = insert_expr e rest in
        ECons (x, _'anf37)

let rec sort_exprs =
 fun xs ->
  match xs with
  | ENil -> ENil
  | ECons (x, rest) ->
      let _'anf38 = sort_exprs rest in
      insert_expr x _'anf38

let rec compare_add_term =
 fun a b ->
  let abase = coeff_base a in
  let bbase = coeff_base b in
  let cbase = compare_expr abase bbase in
  let _'anf39 = cbase = 0 in
  if _'anf39 then
    let acoeff = coeff_value a in
    let bcoeff = coeff_value b in
    let _'anf40 = acoeff < bcoeff in
    if _'anf40 then 0 - 1
    else
      let _'anf41 = acoeff > bcoeff in
      if _'anf41 then 1 else compare_expr a b
  else cbase

let rec insert_add_term =
 fun e xs ->
  match xs with
  | ENil -> ECons (e, ENil)
  | ECons (x, rest) ->
      let _'anf42 = compare_add_term e x in
      let _'anf43 = _'anf42 <= 0 in
      if _'anf43 then ECons (e, xs)
      else
        let _'anf44 = insert_add_term e rest in
        ECons (x, _'anf44)

let rec sort_add_terms =
 fun xs ->
  match xs with
  | ENil -> ENil
  | ECons (x, rest) ->
      let _'anf45 = sort_add_terms rest in
      insert_add_term x _'anf45

let rec reverse_exprs_aux =
 fun xs acc ->
  match xs with
  | ENil -> acc
  | ECons (x, rest) ->
      let _'anf46 = ECons (x, acc) in
      reverse_exprs_aux rest _'anf46

let rec reverse_exprs = fun xs -> reverse_exprs_aux xs ENil

let rec flatten_add =
 fun e ->
  match e with
  | Add (a, b) ->
      let _'anf47 = flatten_add a in
      let _'anf48 = flatten_add b in
      append_exprs _'anf47 _'anf48
  | Const x ->
      let _'anf49 = x = 0 in
      if _'anf49 then ENil else ECons (e, ENil)
  | _ -> ECons (e, ENil)

let rec flatten_mul =
 fun e ->
  match e with
  | Mul (a, b) ->
      let _'anf50 = flatten_mul a in
      let _'anf51 = flatten_mul b in
      append_exprs _'anf50 _'anf51
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
      let _'anf52 = mul_coeff x in
      let _'anf53 = mul_total_coeff rest in
      _'anf52 * _'anf53

let rec mul_bases =
 fun xs ->
  match xs with
  | ENil -> ENil
  | ECons (x, rest) -> (
      let base = mul_base x in
      match base with
      | Const one ->
          let _'anf54 = one = 1 in
          if _'anf54 then mul_bases rest
          else
            let _'anf55 = mul_bases rest in
            insert_expr base _'anf55
      | _ ->
          let _'anf56 = mul_bases rest in
          insert_expr base _'anf56)

let rec build_mul =
 fun xs ->
  match xs with
  | ENil -> Const 1
  | ECons (x, rest) -> (
      match rest with
      | ENil -> x
      | _ ->
          let _'anf57 = build_mul rest in
          Mul (x, _'anf57))

let rec normalize_mul_flat =
 fun left right ->
  let factors =
    let _'anf60 = flatten_mul left in
    let _'anf61 = flatten_mul right in
    append_exprs _'anf60 _'anf61
  in
  let coeff = mul_total_coeff factors in
  let _'anf58 = coeff = 0 in
  if _'anf58 then Const 0
  else
    let bases = mul_bases factors in
    let base_expr = build_mul bases in
    let _'anf59 = coeff = 1 in
    if _'anf59 then base_expr else scale coeff base_expr

let rec combine_like_terms_acc =
 fun base coeff xs ->
  match xs with
  | ENil ->
      let _'anf62 = coeff = 0 in
      if _'anf62 then ENil
      else
        let _'anf63 = scale coeff base in
        ECons (_'anf63, ENil)
  | ECons (x, rest) ->
      let xbase = coeff_base x in
      let xcoeff = coeff_value x in
      let _'anf64 = expr_equal base xbase in
      if _'anf64 then
        let _'anf67 = coeff + xcoeff in
        combine_like_terms_acc base _'anf67 rest
      else
        let tail = combine_like_terms_acc xbase xcoeff rest in
        let _'anf65 = coeff = 0 in
        if _'anf65 then tail
        else
          let _'anf66 = scale coeff base in
          insert_add_term _'anf66 tail

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
          let _'anf68 = Add (x, y) in
          let _'anf69 = expr_equal merged _'anf68 in
          if _'anf69 then
            let _'anf71 = factor_adjacent rest in
            ECons (x, _'anf71)
          else
            let _'anf70 = insert_add_term merged ys in
            factor_adjacent _'anf70)

let rec pick_factored =
 fun x xs ->
  match xs with
  | ENil -> NoPick
  | ECons (y, rest) ->
      let merged = search_factor x y in
      let _'anf72 = Add (x, y) in
      let _'anf73 = expr_equal merged _'anf72 in
      if _'anf73 then
        let _'anf74 = pick_factored x rest in
        match _'anf74 with
        | NoPick -> NoPick
        | Pick (term, remain) ->
            let _'anf75 = ECons (y, remain) in
            Pick (term, _'anf75)
      else Pick (merged, rest)

let rec search_terms =
 fun xs ->
  match xs with
  | ENil -> ENil
  | ECons (x, rest) -> (
      let _'anf76 = pick_factored x rest in
      match _'anf76 with
      | NoPick ->
          let _'anf77 = search_terms rest in
          ECons (x, _'anf77)
      | Pick (merged, remain) ->
          let _'anf78 = insert_add_term merged remain in
          search_terms _'anf78)

let rec build_add =
 fun xs ->
  match xs with
  | ENil -> Const 0
  | ECons (x, rest) -> (
      match rest with
      | ENil -> x
      | _ ->
          let _'anf79 = build_add rest in
          Add (x, _'anf79))

let rec search_round =
 fun xs ->
  let factored0 = factor_adjacent xs in
  let factored1 = search_terms factored0 in
  let factored2 = factor_adjacent factored1 in
  search_terms factored2

let rec normalize_add_flat =
 fun left right ->
  let terms =
    let _'anf83 = flatten_add left in
    let _'anf84 = flatten_add right in
    append_exprs _'anf83 _'anf84
  in
  let sorted = sort_add_terms terms in
  let combined = combine_like_terms sorted in
  let forward = search_round combined in
  let backward =
    let _'anf82 = reverse_exprs combined in
    search_round _'anf82
  in
  let forward_expr = build_add forward in
  let backward_expr = build_add backward in
  let best = better_expr forward_expr backward_expr in
  let rescanned =
    let _'anf80 = flatten_add best in
    let _'anf81 = sort_add_terms _'anf80 in
    combine_like_terms _'anf81
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
          let _'anf85 = expr_equal final merged in
          if _'anf85 then merged else search_opt fuel1 final
      | Mul (a, b) ->
          let sa = search_opt fuel1 a in
          let sb = search_opt fuel1 b in
          let merged = normalize_mul_flat sa sb in
          let flipped = normalize_mul_flat sb sa in
          let next = normalize_mul_flat merged flipped in
          let final = normalize_mul_flat next merged in
          let _'anf86 = expr_equal final merged in
          if _'anf86 then merged else search_opt fuel1 final
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
          let _'anf87 = x = 0 in
          if _'anf87 then nb else normalize_add_flat na nb
      | _ -> normalize_add_flat na nb)
  | Mul (a, b) -> (
      let na = normalize a in
      let nb = normalize b in
      match na with
      | Const x ->
          let _'anf88 = x = 0 in
          if _'anf88 then na
          else
            let _'anf89 = x = 1 in
            if _'anf89 then nb else normalize_mul_flat na nb
      | _ -> normalize_mul_flat na nb)

let rec simplify_aux =
 fun e ->
  let next = normalize e in
  let _'anf90 = expr_equal e next in
  if _'anf90 then next else simplify_aux next

let rec diffx =
 fun e ->
  match e with
  | Const _ -> Const 0
  | Var v -> ( match v with X -> Const 1 | Y -> Const 0)
  | Add (a, b) ->
      let _'anf91 = diffx a in
      let _'anf92 = diffx b in
      Add (_'anf91, _'anf92)
  | Mul (a, b) ->
      let _'anf93 = diffx a in
      let _'anf94 = Mul (_'anf93, b) in
      let _'anf95 = diffx b in
      let _'anf96 = Mul (a, _'anf95) in
      Add (_'anf94, _'anf96)

let rec eval =
 fun e x y ->
  match e with
  | Const c -> c
  | Var v -> ( match v with X -> x | Y -> y)
  | Add (a, b) ->
      let _'anf97 = eval a x y in
      let _'anf98 = eval b x y in
      _'anf97 + _'anf98
  | Mul (a, b) ->
      let _'anf99 = eval a x y in
      let _'anf100 = eval b x y in
      _'anf99 * _'anf100

let rec main =
 fun e ->
  let d = diffx e in
  let f = diffx d in
  simplify_aux f
