type var = X | Y
type expr = Const of int | Var of var | Add of expr * expr | Mul of expr * expr

let rec var_rank = fun v -> match v with X -> 0 | Y -> 1
let rec expr_rank = fun e -> match e with Const _ -> 0 | Var _ -> 1 | Add (_, _) -> 2 | Mul (_, _) -> 3

let rec compare_expr =
 fun a b ->
  let ra = expr_rank a in
  let rb = expr_rank b in
  if ra < rb then 0 - 1
  else if ra > rb then 1
  else
    match a with
    | Const x -> ( match b with Const y -> if x < y then 0 - 1 else if x > y then 1 else 0)
    | Var va -> (
        match b with
        | Var vb ->
            let rva = var_rank va in
            let rvb = var_rank vb in
            if rva < rvb then 0 - 1 else if rva > rvb then 1 else 0)
    | Add (a1, a2) -> (
        match b with
        | Add (b1, b2) ->
            let c1 = compare_expr a1 b1 in
            if c1 = 0 then compare_expr a2 b2 else c1)
    | Mul (a1, a2) -> (
        match b with
        | Mul (b1, b2) ->
            let c1 = compare_expr a1 b1 in
            if c1 = 0 then compare_expr a2 b2 else c1)

let rec expr_equal =
 fun a b ->
  match a with
  | Const x -> ( match b with Const y -> x = y | _ -> false)
  | Var va -> ( match b with Var vb -> var_rank va = var_rank vb | _ -> false)
  | Add (a1, a2) -> ( match b with Add (b1, b2) -> expr_equal a1 b1 && expr_equal a2 b2 | _ -> false)
  | Mul (a1, a2) -> ( match b with Mul (b1, b2) -> expr_equal a1 b1 && expr_equal a2 b2 | _ -> false)

let rec normalize =
 fun e ->
  match e with
  | Const _ -> e
  | Var _ -> e
  | Add (a, b) -> (
      let na = normalize a in
      let nb = normalize b in
      match na with
      | Const x -> ( if x = 0 then nb else match nb with Const y -> Const (x + y) | _ -> Add (na, nb))
      | Add (x0, x1) -> (
          match nb with
          | Add (y0, y1) -> (
              match x0 with
              | Const x -> (
                  match y0 with Const y -> Add (Const (x + y), Add (x1, y1)) | _ -> Add (Add (Add (x0, x1), y0), y1))
              | _ -> Add (Add (Add (x0, x1), y0), y1))
          | _ -> Add (na, nb))
      | _ -> (
          match nb with
          | Const y -> if y = 0 then na else if compare_expr na nb <= 0 then Add (na, nb) else Add (nb, na)
          | Add (l, r) -> Add (Add (na, l), r)
          | _ -> Add (na, nb)))
  | Mul (a, b) -> (
      let na = normalize a in
      let nb = normalize b in
      match na with
      | Const x -> (
          if x = 0 then Const 0
          else if x = 1 then nb
          else
            match nb with Const y -> if y = 0 then Const 0 else if y = 1 then na else Const (x * y) | _ -> Mul (na, nb))
      | Add (l, r) -> Add (Mul (l, b), Mul (r, b))
      | _ -> (
          match nb with
          | Const y -> if y = 0 then Const 0 else if y = 1 then na else Mul (nb, na)
          | Add (l, r) -> Add (Mul (na, l), Mul (na, r))
          | Mul (l, r) -> Mul (na, Mul (l, r))
          | _ -> Mul (na, nb)))

let rec simplify_aux =
 fun e ->
  let next = normalize e in
  if expr_equal e next then next else simplify_aux next

let rec diffx =
 fun e ->
  match e with
  | Const _ -> Const 0
  | Var v -> ( match v with X -> Const 1 | Y -> Const 0)
  | Add (a, b) -> Add (diffx a, diffx b)
  | Mul (a, b) -> Add (Mul (diffx a, b), Mul (a, diffx b))

let rec eval =
 fun e x y ->
  match e with
  | Const c -> c
  | Var v -> ( match v with X -> x | Y -> y)
  | Add (a, b) -> eval a x y + eval b x y
  | Mul (a, b) -> eval a x y * eval b x y

let rec main =
 fun e ->
  let d = diffx e in
  let f = diffx d in
  simplify_aux f
