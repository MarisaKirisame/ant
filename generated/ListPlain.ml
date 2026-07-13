open ListCEK

type int_list = ListCEK.int_list
type int_pair_list = ListCEK.int_pair_list
type nat = ListCEK.nat

let rec pair =
 fun x ->
  match x with
  | Nil ->
      let _0 = NilP in
      _0
  | Cons (x1, xt1) -> (
      match xt1 with
      | Nil ->
          let _1 = NilP in
          _1
      | Cons (x2, xt2) ->
          let _2 = pair xt1 in
          let _3 = ConsP (x1, x2, _2) in
          _3)

let rec list_incr =
 fun x ->
  match x with
  | Nil ->
      let _4 = Nil in
      _4
  | Cons (xh, xt) ->
      let _5 = 1 in
      let _6 = xh + _5 in
      let _7 = list_incr xt in
      let _8 = Cons (_6, _7) in
      _8

let rec filter_pos =
 fun x ->
  match x with
  | Nil ->
      let _9 = Nil in
      _9
  | Cons (xh, xt) ->
      let _10 = 50 in
      let _11 = xh > _10 in
      if _11 then
        let _12 = filter_pos xt in
        let _13 = Cons (xh, _12) in
        _13
      else filter_pos xt

let rec append =
 fun x y ->
  match x with
  | Nil -> y
  | Cons (xh, xt) ->
      let _14 = append xt y in
      let _15 = Cons (xh, _14) in
      _15

let rec reverse =
 fun x ->
  match x with
  | Nil ->
      let _16 = Nil in
      _16
  | Cons (xh, xt) ->
      let _17 = reverse xt in
      let _18 = Nil in
      let _19 = Cons (xh, _18) in
      append _17 _19

let rec insert =
 fun elt x ->
  match x with
  | Nil ->
      let _20 = Nil in
      let _21 = Cons (elt, _20) in
      _21
  | Cons (xh, xt) ->
      let _22 = elt <= xh in
      if _22 then
        let _25 = Cons (elt, x) in
        _25
      else
        let _23 = insert elt xt in
        let _24 = Cons (xh, _23) in
        _24

let rec insertion_sort =
 fun x ->
  match x with
  | Nil ->
      let _26 = Nil in
      _26
  | Cons (xh, xt) ->
      let _27 = insertion_sort xt in
      insert xh _27

type pair_int_lists = ListCEK.pair_int_lists

let rec length =
 fun x ->
  match x with
  | Nil ->
      let _28 = Z in
      _28
  | Cons (xh, xt) ->
      let _29 = length xt in
      let _30 = S _29 in
      _30

let rec half =
 fun n ->
  match n with
  | Z ->
      let _31 = Z in
      _31
  | S n1 -> (
      match n1 with
      | Z ->
          let _32 = Z in
          _32
      | S n2 ->
          let _33 = half n2 in
          let _34 = S _33 in
          _34)

let rec take =
 fun n x ->
  match n with
  | Z ->
      let _35 = Nil in
      let _36 = P (_35, x) in
      _36
  | S n1 -> (
      match x with
      | Nil ->
          let _37 = Nil in
          let _38 = Nil in
          let _39 = P (_37, _38) in
          _39
      | Cons (xh, xt) -> (
          let _40 = take n1 xt in
          match _40 with
          | P (l, r) ->
              let _41 = Cons (xh, l) in
              let _42 = P (_41, r) in
              _42))

let rec my_split =
 fun x ->
  let _43 = length x in
  let _44 = half _43 in
  take _44 x

let rec my_merge =
 fun x y ->
  match x with
  | Nil -> y
  | Cons (xh, xt) -> (
      match y with
      | Nil -> x
      | Cons (yh, yt) ->
          let _45 = xh < yh in
          if _45 then
            let _48 = my_merge xt y in
            let _49 = Cons (xh, _48) in
            _49
          else
            let _46 = my_merge x yt in
            let _47 = Cons (yh, _46) in
            _47)

let rec mergesort =
 fun x ->
  match x with
  | Nil ->
      let _50 = Nil in
      _50
  | Cons (x1, xt) -> (
      match xt with
      | Nil -> x
      | Cons (x2, xt2) -> (
          let _51 = my_split x in
          match _51 with
          | P (l, r) ->
              let _52 = mergesort l in
              let _53 = mergesort r in
              my_merge _52 _53))

let rec filter_gt =
 fun x pivot ->
  match x with
  | Nil ->
      let _54 = Nil in
      _54
  | Cons (xh, xt) ->
      let _55 = xh > pivot in
      if _55 then
        let _56 = filter_gt xt pivot in
        let _57 = Cons (xh, _56) in
        _57
      else filter_gt xt pivot

let rec filter_eq =
 fun x pivot ->
  match x with
  | Nil ->
      let _58 = Nil in
      _58
  | Cons (xh, xt) ->
      let _59 = xh = pivot in
      if _59 then
        let _60 = filter_eq xt pivot in
        let _61 = Cons (xh, _60) in
        _61
      else filter_eq xt pivot

let rec filter_lt =
 fun x pivot ->
  match x with
  | Nil ->
      let _62 = Nil in
      _62
  | Cons (xh, xt) ->
      let _63 = xh < pivot in
      if _63 then
        let _64 = filter_lt xt pivot in
        let _65 = Cons (xh, _64) in
        _65
      else filter_lt xt pivot

let rec quicksort =
 fun x ->
  match x with
  | Nil ->
      let _66 = Nil in
      _66
  | Cons (xh, xt) ->
      let _67 = filter_lt x xh in
      let _68 = quicksort _67 in
      let _69 = filter_eq x xh in
      let _70 = filter_gt x xh in
      let _71 = quicksort _70 in
      let _72 = append _69 _71 in
      append _68 _72
