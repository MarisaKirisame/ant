open ListCEK

type int_list = ListCEK.int_list
type int_pair_list = ListCEK.int_pair_list

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

let rec my_split_aux =
 fun x y ->
  match y with
  | Nil ->
      let _28 = Nil in
      let _29 = P (_28, x) in
      _29
  | Cons (y0, yt) -> (
      match yt with
      | Nil ->
          let _30 = Nil in
          let _31 = P (_30, x) in
          _31
      | Cons (y1, ys) -> (
          match x with
          | Nil ->
              let _32 = Nil in
              let _33 = Nil in
              let _34 = P (_32, _33) in
              _34
          | Cons (xh, xt) -> (
              let _35 = my_split_aux xt ys in
              match _35 with
              | P (l, r) ->
                  let _36 = Cons (xh, l) in
                  let _37 = P (_36, r) in
                  _37)))

let rec my_split = fun x -> my_split_aux x x

let rec my_merge =
 fun x y ->
  match x with
  | Nil -> y
  | Cons (xh, xt) -> (
      match y with
      | Nil -> x
      | Cons (yh, yt) ->
          let _38 = xh < yh in
          if _38 then
            let _41 = my_merge xt y in
            let _42 = Cons (xh, _41) in
            _42
          else
            let _39 = my_merge x yt in
            let _40 = Cons (yh, _39) in
            _40)

let rec mergesort =
 fun x ->
  match x with
  | Nil ->
      let _43 = Nil in
      _43
  | Cons (x1, xt) -> (
      match xt with
      | Nil -> x
      | Cons (x2, xt2) -> (
          let _44 = my_split x in
          match _44 with
          | P (l, r) ->
              let _45 = mergesort l in
              let _46 = mergesort r in
              my_merge _45 _46))

let rec filter_gt =
 fun x pivot ->
  match x with
  | Nil ->
      let _47 = Nil in
      _47
  | Cons (xh, xt) ->
      let _48 = xh > pivot in
      if _48 then
        let _49 = filter_gt xt pivot in
        let _50 = Cons (xh, _49) in
        _50
      else filter_gt xt pivot

let rec filter_eq =
 fun x pivot ->
  match x with
  | Nil ->
      let _51 = Nil in
      _51
  | Cons (xh, xt) ->
      let _52 = xh = pivot in
      if _52 then
        let _53 = filter_eq xt pivot in
        let _54 = Cons (xh, _53) in
        _54
      else filter_eq xt pivot

let rec filter_lt =
 fun x pivot ->
  match x with
  | Nil ->
      let _55 = Nil in
      _55
  | Cons (xh, xt) ->
      let _56 = xh < pivot in
      if _56 then
        let _57 = filter_lt xt pivot in
        let _58 = Cons (xh, _57) in
        _58
      else filter_lt xt pivot

let rec quicksort =
 fun x ->
  match x with
  | Nil ->
      let _59 = Nil in
      _59
  | Cons (xh, xt) ->
      let _60 = filter_lt x xh in
      let _61 = quicksort _60 in
      let _62 = filter_eq x xh in
      let _63 = filter_gt x xh in
      let _64 = quicksort _63 in
      let _65 = append _62 _64 in
      append _61 _65
