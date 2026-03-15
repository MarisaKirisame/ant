type var = X | Y
type expr = Const of int | Var of var | Add of expr * expr | Mul of expr * expr

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

let rec normalize =
 fun e ->
  match e with
  | Const _ -> e
  | Var _ -> e
  | Add (a, b) -> (
      let na = normalize a in
      let nb = normalize b in
      let _'anf14 = expr_equal na nb in
      if _'anf14 then
        let _'anf26 = Const 2 in
        Mul (_'anf26, na)
      else
        match na with
        | Const x -> (
            let _'anf15 = x = 0 in
            if _'anf15 then nb
            else
              match nb with
              | Const y ->
                  let _'anf16 = x + y in
                  Const _'anf16
              | _ -> Add (na, nb))
        | Add (x0, x1) -> (
            match nb with
            | Add (y0, y1) -> (
                match x0 with
                | Const x -> (
                    match y0 with
                    | Const y ->
                        let _'anf17 =
                          let _'anf19 = x + y in
                          Const _'anf19
                        in
                        let _'anf18 = Add (x1, y1) in
                        Add (_'anf17, _'anf18)
                    | _ ->
                        let _'anf20 =
                          let _'anf21 = Add (x0, x1) in
                          Add (_'anf21, y0)
                        in
                        Add (_'anf20, y1))
                | _ ->
                    let _'anf22 =
                      let _'anf23 = Add (x0, x1) in
                      Add (_'anf23, y0)
                    in
                    Add (_'anf22, y1))
            | _ -> Add (na, nb))
        | _ -> (
            match nb with
            | Const y ->
                let _'anf24 = y = 0 in
                if _'anf24 then na else Add (nb, na)
            | Add (l, r) ->
                let _'anf25 = Add (na, l) in
                Add (_'anf25, r)
            | _ -> Add (na, nb)))
  | Mul (a, b) -> (
      let na = normalize a in
      let nb = normalize b in
      match na with
      | Const x -> (
          let _'anf27 = x = 0 in
          if _'anf27 then Const 0
          else
            let _'anf28 = x = 1 in
            if _'anf28 then nb
            else
              match nb with
              | Const y ->
                  let _'anf29 = y = 0 in
                  if _'anf29 then Const 0
                  else
                    let _'anf30 = y = 1 in
                    if _'anf30 then na
                    else
                      let _'anf31 = x * y in
                      Const _'anf31
              | _ -> Mul (na, nb))
      | Add (l, r) ->
          let _'anf32 = Mul (l, b) in
          let _'anf33 = Mul (r, b) in
          Add (_'anf32, _'anf33)
      | _ -> (
          match nb with
          | Const y ->
              let _'anf34 = y = 0 in
              if _'anf34 then Const 0
              else
                let _'anf35 = y = 1 in
                if _'anf35 then na else Mul (nb, na)
          | Add (l, r) ->
              let _'anf36 = Mul (na, l) in
              let _'anf37 = Mul (na, r) in
              Add (_'anf36, _'anf37)
          | Mul (l, r) ->
              let _'anf38 = Mul (l, r) in
              Mul (na, _'anf38)
          | _ -> Mul (na, nb)))

let rec simplify_aux =
 fun e ->
  let next = normalize e in
  let _'anf39 = expr_equal e next in
  if _'anf39 then next else simplify_aux next

let rec diffx =
 fun e ->
  match e with
  | Const _ -> Const 0
  | Var v -> ( match v with X -> Const 1 | Y -> Const 0)
  | Add (a, b) ->
      let _'anf40 = diffx a in
      let _'anf41 = diffx b in
      Add (_'anf40, _'anf41)
  | Mul (a, b) ->
      let _'anf42 =
        let _'anf45 = diffx a in
        Mul (_'anf45, b)
      in
      let _'anf43 =
        let _'anf44 = diffx b in
        Mul (a, _'anf44)
      in
      Add (_'anf42, _'anf43)

let rec eval =
 fun e x y ->
  match e with
  | Const c -> c
  | Var v -> ( match v with X -> x | Y -> y)
  | Add (a, b) ->
      let _'anf46 = eval a x y in
      let _'anf47 = eval b x y in
      _'anf46 + _'anf47
  | Mul (a, b) ->
      let _'anf48 = eval a x y in
      let _'anf49 = eval b x y in
      _'anf48 * _'anf49

let rec main =
 fun e ->
  let d = diffx e in
  let f = diffx d in
  simplify_aux f
