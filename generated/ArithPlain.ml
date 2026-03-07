type var = X | Y
type expr = Const of int | Var of var | Add of expr * expr | Mul of expr * expr | Exp of expr | Log of expr

let rec var_rank = fun v -> match v with X -> 0 | Y -> 1

let rec expr_rank =
 fun e -> match e with Const _ -> 0 | Var _ -> 1 | Add (_, _) -> 2 | Mul (_, _) -> 3 | Exp _ -> 4 | Log _ -> 5

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
    | Exp ax -> ( match b with Exp bx -> compare_expr ax bx)
    | Log ax -> ( match b with Log bx -> compare_expr ax bx)

let rec int_exp = fun n -> if n < 0 then 0 else if n = 0 then 1 else 2 * int_exp (n - 1)
let rec int_log = fun n -> if n <= 1 then 0 else 1 + int_log (n / 2)

let rec expr_eq_struct =
 fun a b ->
  match a with
  | Const x -> ( match b with Const y -> x = y | _ -> false)
  | Var va -> ( match b with Var vb -> var_rank va = var_rank vb | _ -> false)
  | Add (a1, a2) -> ( match b with Add (b1, b2) -> expr_eq_struct a1 b1 && expr_eq_struct a2 b2 | _ -> false)
  | Mul (a1, a2) -> ( match b with Mul (b1, b2) -> expr_eq_struct a1 b1 && expr_eq_struct a2 b2 | _ -> false)
  | Exp ax -> ( match b with Exp bx -> expr_eq_struct ax bx | _ -> false)
  | Log ax -> ( match b with Log bx -> expr_eq_struct ax bx | _ -> false)

let rec normalize =
 fun e ->
  match e with
  | Const _ -> e
  | Var _ -> e
  | Add (a, b) -> (
      let na = normalize a in
      let nb = normalize b in
      match na with
      | Const x -> (
          if x = 0 then nb
          else
            match nb with
            | Const y -> Const (x + y)
            | _ -> (
                let ordered = if compare_expr na nb <= 0 then Add (na, nb) else Add (nb, na) in
                match ordered with Add (l, r) -> if expr_eq_struct l r then Mul (Const 2, l) else ordered))
      | _ -> (
          match nb with
          | Const y -> if y = 0 then na else if compare_expr na nb <= 0 then Add (na, nb) else Add (nb, na)
          | _ -> (
              let ordered = if compare_expr na nb <= 0 then Add (na, nb) else Add (nb, na) in
              match ordered with Add (l, r) -> if expr_eq_struct l r then Mul (Const 2, l) else ordered)))
  | Mul (a, b) -> (
      let na = normalize a in
      let nb = normalize b in
      match na with
      | Const x -> (
          if x = 0 then Const 0
          else if x = 1 then nb
          else
            match nb with
            | Const y -> if y = 0 then Const 0 else if y = 1 then na else Const (x * y)
            | _ -> if compare_expr na nb <= 0 then Mul (na, nb) else Mul (nb, na))
      | _ -> (
          match nb with
          | Const y ->
              if y = 0 then Const 0
              else if y = 1 then na
              else if compare_expr na nb <= 0 then Mul (na, nb)
              else Mul (nb, na)
          | _ -> if compare_expr na nb <= 0 then Mul (na, nb) else Mul (nb, na)))
  | Exp x -> (
      let nx = normalize x in
      match nx with Const n -> if n = 0 then Const 1 else Const (int_exp n) | _ -> Exp nx)
  | Log x -> (
      let nx = normalize x in
      match nx with Const n -> if n = 1 then Const 0 else Const (int_log n) | _ -> Log nx)

let rec expr_eq =
 fun a b ->
  let na = normalize a in
  let nb = normalize b in
  expr_eq_struct na nb

let rec simplify_aux =
 fun e ->
  let next = normalize e in
  if expr_eq e next then next else simplify_aux next

let rec diffx =
 fun e ->
  match e with
  | Const _ -> Const 0
  | Var v -> ( match v with X -> Const 1 | Y -> Const 0)
  | Add (a, b) -> Add (diffx a, diffx b)
  | Mul (a, b) -> Add (Mul (diffx a, b), Mul (a, diffx b))
  | Exp x -> Mul (Exp x, diffx x)
  | Log x -> Mul (diffx x, Exp (Mul (Log x, Const (0 - 1))))

let rec eval =
 fun e x y ->
  match e with
  | Const c -> c
  | Var v -> ( match v with X -> x | Y -> y)
  | Add (a, b) -> eval a x y + eval b x y
  | Mul (a, b) -> eval a x y * eval b x y
  | Exp a -> int_exp (eval a x y)
  | Log a -> int_log (eval a x y)
