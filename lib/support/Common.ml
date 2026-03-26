let ( % ) f g x = f (g x)
let ( $ ) = ( @@ )
let todo msg = failwith ("todo: " ^ msg)

let debug str f =
  print_endline ("try " ^ str);
  let ret = f () in
  print_endline ("ok! " ^ str);
  ret

type pc = Pc of int

let pc_to_int (Pc pc) = pc
let int_to_pc pc = Pc pc

module Rev : sig
  type 'a t

  val from_list : 'a list -> 'a t
  val to_list : 'a t -> 'a list
  val snoc : 'a t -> 'a -> 'a t
  val append : 'a t -> 'a t -> 'a t
end = struct
  type 'a t = 'a list

  let from_list l = List.rev l
  let to_list l = List.rev l
  let snoc xs x = x :: xs
  let append x y = List.append y x
end
