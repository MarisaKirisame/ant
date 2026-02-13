type 'a monoid = { zero : 'a; combine : 'a -> 'a -> 'a }

exception Empty

module Generic = struct
  type ('a, 'm) fg = 'a list
  type ('wrapped_type, 'a, 'm) wrap = 'wrapped_type

  let empty = []
  let singleton x = [ x ]
  let is_empty = function [] -> true | _ -> false
  let size = List.length
  let cons ~monoid:_ ~measure:_ t x = x :: t
  let snoc ~monoid:_ ~measure:_ t x = t @ [ x ]
  let append ~monoid:_ ~measure:_ x y = x @ y
  let front ~monoid:_ ~measure:_ = function [] -> None | x :: xs -> Some (xs, x)
  let front_exn ~monoid ~measure t = match front ~monoid ~measure t with Some v -> v | None -> raise Empty
  let rear ~monoid:_ ~measure:_ t = match List.rev t with [] -> None | x :: xs -> Some (List.rev xs, x)
  let rear_exn ~monoid ~measure t = match rear ~monoid ~measure t with Some v -> v | None -> raise Empty
  let head = function [] -> None | x :: _ -> Some x
  let head_exn = function [] -> raise Empty | x :: _ -> x
  let last = function [] -> None | xs -> Some (List.nth xs (List.length xs - 1))
  let last_exn = function [] -> raise Empty | xs -> List.nth xs (List.length xs - 1)
  let fold_left f acc t = List.fold_left f acc t
  let fold_right f acc t = List.fold_right (fun x acc -> f acc x) t acc
  let iter f t = List.iter f t
  let iter_right f t = List.iter f (List.rev t)

  let compare cmp x y =
    let rec loop a b =
      match (a, b) with
      | [], [] -> 0
      | [], _ -> -1
      | _, [] -> 1
      | xa :: xs, ya :: ys ->
          let c = cmp xa ya in
          if c = 0 then loop xs ys else c
    in
    loop x y

  let equal eq x y =
    let rec loop a b =
      match (a, b) with [], [] -> true | [], _ | _, [] -> false | xa :: xs, ya :: ys -> eq xa ya && loop xs ys
    in
    loop x y

  let to_list t = t
  let to_list_backwards t = List.rev t
  let of_list ~monoid:_ ~measure:_ l = l
  let of_list_backwards ~monoid:_ ~measure:_ l = List.rev l
  let measure ~monoid ~measure t = List.fold_left (fun acc x -> monoid.combine acc (measure x)) monoid.zero t

  let split ~monoid ~measure pred t =
    let rec loop acc_rev acc_measure rest =
      match rest with
      | [] -> (List.rev acc_rev, [])
      | x :: xs ->
          let acc_measure' = monoid.combine acc_measure (measure x) in
          if pred acc_measure' then (List.rev acc_rev, rest) else loop (x :: acc_rev) acc_measure' xs
    in
    loop [] monoid.zero t
end
