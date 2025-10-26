let ( % ) f g x = f (g x)
let ( $ ) f x = f x

let todo msg = failwith ("todo: " ^ msg)

let debug str f =
  print_endline ("try " ^ str);
  let ret = f () in
  print_endline ("ok! " ^ str);
  ret

type 'a linear = { mutable value : 'a option }

let make_linear x = { value = Some x }
let read_linear x = Option.get x.value

let write_linear x =
  let v = Option.get x.value in
  x.value <- None;
  v
