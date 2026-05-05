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
        let _13 = filter_pos xt in
        let _14 = Cons (xh, _13) in
        _14
      else
        let _12 = filter_pos xt in
        _12

let rec append =
 fun x y ->
  match x with
  | Nil -> y
  | Cons (xh, xt) ->
      let _15 = append xt y in
      let _16 = Cons (xh, _15) in
      _16

let rec reverse =
 fun x ->
  match x with
  | Nil ->
      let _17 = Nil in
      _17
  | Cons (xh, xt) ->
      let _18 = reverse xt in
      let _19 = Nil in
      let _20 = Cons (xh, _19) in
      let _21 = append _18 _20 in
      _21

let rec insert =
 fun elt x ->
  match x with
  | Nil ->
      let _22 = Nil in
      let _23 = Cons (elt, _22) in
      _23
  | Cons (xh, xt) ->
      let _24 = elt <= xh in
      if _24 then
        let _27 = Cons (elt, x) in
        _27
      else
        let _25 = insert elt xt in
        let _26 = Cons (xh, _25) in
        _26

let rec insertion_sort =
 fun x ->
  match x with
  | Nil ->
      let _28 = Nil in
      _28
  | Cons (xh, xt) ->
      let _29 = insertion_sort xt in
      let _30 = insert xh _29 in
      _30

type pair_int_lists = ListCEK.pair_int_lists

let rec my_split_aux =
 fun x y ->
  match y with
  | Nil ->
      let _31 = Nil in
      let _32 = P (_31, x) in
      _32
  | Cons (y0, yt) -> (
      match yt with
      | Nil ->
          let _33 = Nil in
          let _34 = P (_33, x) in
          _34
      | Cons (y1, ys) -> (
          match x with
          | Nil ->
              let _35 = Nil in
              let _36 = Nil in
              let _37 = P (_35, _36) in
              _37
          | Cons (xh, xt) -> (
              let _38 = my_split_aux xt ys in
              match _38 with
              | P (l, r) ->
                  let _39 = Cons (xh, l) in
                  let _40 = P (_39, r) in
                  _40)))

let rec my_split =
 fun x ->
  let _41 = my_split_aux x x in
  _41

let rec my_merge =
 fun x y ->
  match x with
  | Nil -> y
  | Cons (xh, xt) -> (
      match y with
      | Nil -> x
      | Cons (yh, yt) ->
          let _42 = xh < yh in
          if _42 then
            let _45 = my_merge xt y in
            let _46 = Cons (xh, _45) in
            _46
          else
            let _43 = my_merge x yt in
            let _44 = Cons (yh, _43) in
            _44)

let rec mergesort =
 fun x ->
  match x with
  | Nil ->
      let _47 = Nil in
      _47
  | Cons (x1, xt) -> (
      match xt with
      | Nil -> x
      | Cons (x2, xt2) -> (
          let _48 = my_split x in
          match _48 with
          | P (l, r) ->
              let _49 = mergesort l in
              let _50 = mergesort r in
              let _51 = my_merge _49 _50 in
              _51))

let rec filter_gt =
 fun x pivot ->
  match x with
  | Nil ->
      let _52 = Nil in
      _52
  | Cons (xh, xt) ->
      let _53 = xh > pivot in
      if _53 then
        let _55 = filter_gt xt pivot in
        let _56 = Cons (xh, _55) in
        _56
      else
        let _54 = filter_gt xt pivot in
        _54

let rec filter_eq =
 fun x pivot ->
  match x with
  | Nil ->
      let _57 = Nil in
      _57
  | Cons (xh, xt) ->
      let _58 = xh = pivot in
      if _58 then
        let _60 = filter_eq xt pivot in
        let _61 = Cons (xh, _60) in
        _61
      else
        let _59 = filter_eq xt pivot in
        _59

let rec filter_lt =
 fun x pivot ->
  match x with
  | Nil ->
      let _62 = Nil in
      _62
  | Cons (xh, xt) ->
      let _63 = xh < pivot in
      if _63 then
        let _65 = filter_lt xt pivot in
        let _66 = Cons (xh, _65) in
        _66
      else
        let _64 = filter_lt xt pivot in
        _64

let rec quicksort =
 fun x ->
  match x with
  | Nil ->
      let _67 = Nil in
      _67
  | Cons (xh, xt) ->
      let _68 = filter_lt x xh in
      let _69 = quicksort _68 in
      let _70 = filter_eq x xh in
      let _71 = filter_gt x xh in
      let _72 = quicksort _71 in
      let _73 = append _70 _72 in
      let _74 = append _69 _73 in
      _74
