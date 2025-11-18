module Make () : sig
  val reset : unit -> unit
  val next_fresh : string -> string
  val next_sym : unit -> string
end = struct
  let c1 = ref 0

  let next_fresh s =
    let i = !c1 in
    c1 := i + 1;
    s ^ string_of_int i

  let c2 = ref 0

  let next_sym () =
    let i = !c2 in
    c2 := i + 1;
    if i < 26 then String.make 1 (Char.chr (Char.code 'a' + i)) else "t" ^ string_of_int i

  let reset () =
    c1 := 0;
    c2 := 0
end
