exception EXN of string

let panic msg = raise (EXN msg)
let todo msg = panic ("todo: " ^ msg)

let debug str f =
  print_endline ("try " ^ str);
  let ret = f () in
  print_endline ("ok! " ^ str);
  ret
