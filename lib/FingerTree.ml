exception EXN of string

let panic msg = raise (EXN msg)
let todo msg = panic ("todo: " ^ msg)

(*We are writing our own finger tree, instead of using the one at batteries, for performance reason.

  0 - Chunk the data structure for ultra performance

  1 - The root node should not store the measurement (due to frequent changes to it)

  2 - To inline the measurements monoid operation

  3 - Use laziness for the right big O in persistent setting

  Note that this is a tentative list, describing what we thought we want but not what we implemented.
*)

(*todo: The code in this paper is not meant to be production code.
  In a real implementation, we’d have to be more careful with memory usage.
  In one place particular, where head, tail, and chop are used in the implementation of tail,
  a potential space leak exists (that can be remedied by strictness annotations).
  The actual implementation in Data.Sequence is very optimized and uses lots of strictness annotations.
  Another spot that can be optimized is the use of the (short) helper lists in the implementation of glue.
  In fact, they can be eliminated completely by specializing those functions,
  resulting in a lot of nested cases. (This is also done in Data.Sequence.)
  For the sake of explanation, this was not done here.
  Implementing glue without these helper lists and
  without excessive case analyses is still future work*)

(*one thing i dont like is how more0 match on the tree then call head (which match on the same tree again).
  might worth expanding all case so it only do 1 match.
*)

type 'a seq = Empty | Unit of 'a | More of 'a some * 'a tuple seq * 'a some
and 'a some = One of 'a | Two of 'a * 'a | Three of 'a * 'a * 'a
and 'a tuple = Pair of 'a * 'a | Triple of 'a * 'a * 'a

let rec cons : 'a. 'a -> 'a seq -> 'a seq =
 fun x xs ->
  match xs with
  | Empty -> Unit x
  | Unit y -> More (One x, Empty, One y)
  | More (One y, q, u) -> More (Two (x, y), q, u)
  | More (Two (y, z), q, u) -> More (Three (x, y, z), q, u)
  | More (Three (y, z, w), q, u) -> More (Two (x, y), cons (Pair (z, w)) q, u)

let rec snoc : 'a. 'a seq -> 'a -> 'a seq =
 fun xs x ->
  match xs with
  | Empty -> Unit x
  | Unit y -> More (One y, Empty, One x)
  | More (u, q, One y) -> More (u, q, Two (y, x))
  | More (u, q, Two (y, z)) -> More (u, q, Three (y, z, x))
  | More (u, q, Three (y, z, w)) -> More (u, snoc q (Pair (y, z)), Two (w, x))

let head (xs : 'a seq) : 'a =
  match xs with
  | Empty -> panic "Head on Empty"
  | Unit x -> x
  | More ((One x | Two (x, _) | Three (x, _, _)), _, _) -> x

let rec tail : 'a. 'a seq -> 'a seq =
 fun xs ->
  match xs with
  | Empty -> panic "Tail on Empty"
  | Unit _ -> Empty
  | More (Three (_, y, z), q, u) -> More (Two (y, z), q, u)
  | More (Two (_, z), q, u) -> More (One z, q, u)
  | More (One _, q, u) -> more0 q u

and more0 (q : 'a tuple seq) (u : 'a some) : 'a seq =
  match q with
  | Empty -> (
      match u with
      | One y -> Unit y
      | Two (y, z) -> More (One y, Empty, One z)
      | Three (y, z, w) -> More (One y, Empty, Two (z, w)))
  | _ -> (
      match head q with Pair (x, y) -> More (Two (x, y), tail q, u) | Triple (x, _, _) -> More (One x, map1 chop q, u))

and map1 : 'a. ('a -> 'a) -> 'a seq -> 'a seq =
 fun f xs ->
  match xs with
  | Empty -> panic "Map1 on Empty"
  | Unit x -> Unit (f x)
  | More (One x, q, u) -> More (One (f x), q, u)
  | More (Two (x, y), q, u) -> More (Two (f x, y), q, u)
  | More (Three (x, y, z), q, u) -> More (Three (f x, y, z), q, u)

and chop (xs : 'a tuple) : 'a tuple = match xs with Pair _ -> panic "Chop on Pair" | Triple (_, y, z) -> Pair (y, z)

let rec glue : 'a. 'a seq -> 'a list -> 'a seq -> 'a seq =
 fun x y z ->
  match (x, z) with
  | Empty, _ -> List.fold_right cons y z
  | _, Empty -> List.fold_left snoc x y
  | Unit x, _ -> List.fold_right cons (x :: y) z
  | _, Unit z -> List.fold_left snoc x (List.append y [ z ])
  | More (xu, xq, xv), More (zu, zq, zv) ->
      More (xu, glue xq (toTuples (List.append (toList xv) (List.append y (toList zu)))) zq, zv)

and toTuples (x : 'a list) : 'a tuple list =
  match x with
  | [] -> []
  | [ _ ] -> panic "Impossible case at toTuples"
  | [ x; y ] -> [ Pair (x, y) ]
  | [ x; y; z; w ] -> [ Pair (x, y); Pair (z, w) ]
  | x :: y :: z :: xs -> Triple (x, y, z) :: toTuples xs

and toList (x : 'a some) : 'a list =
  match x with One y -> [ y ] | Two (y, z) -> [ y; z ] | Three (y, z, w) -> [ y; z; w ]

let append (x : 'a seq) (y : 'a seq) : 'a seq = glue x [] y
