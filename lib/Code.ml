open Core
open PPrint
open Ir
open State
open Word
open Common

type 'a code = Code of ir

let code (doc : document) = Code (Raw doc)
let to_ir (Code ir) = ir
let from_ir ir = Code ir

let uncode (Code ir) : document =
  (*print_endline (show_ir ir);*)
  ir_to_doc (optimize_ir ir)

let fresh_name : (string, int) Hashtbl.t = Hashtbl.create (module String)

let gensym (base : string) : string =
  let n = Option.value (Hashtbl.find fresh_name base) ~default:0 in
  Hashtbl.set fresh_name ~key:base ~data:(n + 1);
  base ^ "_" ^ Int.to_string n

let int_ (i : int) : int code = code $ string (Int.to_string i)
let unit_ : unit code = Code Unit

let lam_ (a : string) (f : 'a code -> 'b code) : ('a -> 'b) code =
  let a_doc = string (gensym a) in
  code $ string "(fun " ^^ a_doc ^^ string " -> " ^^ uncode (f (code a_doc)) ^^ string ")"

let lam2_ (a : string) (b : string) (f : 'a code -> 'b code -> 'c code) : ('a -> 'b -> 'c) code =
  let a_doc = string (gensym a) in
  let b_doc = string (gensym b) in
  code
  $ string "(fun " ^^ a_doc ^^ string " " ^^ b_doc ^^ string " -> "
    ^^ uncode (f (code a_doc) (code b_doc))
    ^^ string ")"

let lam3_ (a : string) (b : string) (c : string) (f : 'a code -> 'b code -> 'c code -> 'd code) :
    ('a -> 'b -> 'c -> 'd) code =
  let a_doc = string (gensym a) in
  let b_doc = string (gensym b) in
  let c_doc = string (gensym c) in
  code
  $ string "(fun " ^^ a_doc ^^ string " " ^^ b_doc ^^ string " " ^^ c_doc ^^ string " -> "
    ^^ uncode (f (code a_doc) (code b_doc) (code c_doc))
    ^^ string ")"

let lam4_ (a : string) (b : string) (c : string) (d : string) (f : 'a code -> 'b code -> 'c code -> 'd code -> 'e code)
    : ('a -> 'b -> 'c -> 'd -> 'e) code =
  let a_doc = string (gensym a) in
  let b_doc = string (gensym b) in
  let c_doc = string (gensym c) in
  let d_doc = string (gensym d) in
  code
  $ string "(fun " ^^ a_doc ^^ string " " ^^ b_doc ^^ string " " ^^ c_doc ^^ string " " ^^ d_doc ^^ string " -> "
    ^^ uncode (f (code a_doc) (code b_doc) (code c_doc) (code d_doc))
    ^^ string ")"

let app_ (f : ('a -> 'b) code) (a : 'a code) : 'b code = Code (Paren (App (to_ir f, [ to_ir a ])))
let app2_ (f : ('a -> 'b -> 'c) code) (a : 'a code) (b : 'b code) : 'c code = app_ (app_ f a) b
let app3_ (f : ('a -> 'b -> 'c -> 'd) code) (a : 'a code) (b : 'b code) (c : 'c code) : 'd code = app_ (app2_ f a b) c

let app4_ (f : ('a -> 'b -> 'c -> 'd -> 'e) code) (a : 'a code) (b : 'b code) (c : 'c code) (d : 'd code) : 'e code =
  app_ (app3_ f a b c) d

let app5_ (f : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) code) (a : 'a code) (b : 'b code) (c : 'c code) (d : 'd code)
    (e : 'e code) : 'f code =
  app_ (app4_ f a b c d) e

let assert_env_length_ (w : world code) (e : int code) : unit code = app2_ (code $ string "assert_env_length") w e
let return_n_ (w : world code) (n : int code) (exp : exp code) : unit code = app3_ (code $ string "return_n") w n exp
let drop_n_ (w : world code) (e : int code) (n : int code) : unit code = app3_ (code $ string "drop_n") w e n
let pc_to_int_ (pc : int code) : int code = app_ (code $ string "pc_to_int") pc
let int_to_pc_ (int : int code) : pc code = app_ (code $ string "int_to_pc") int
let pc_to_exp_ (pc : pc code) : exp code = app_ (from_ir (Function "pc_to_exp")) pc
let pc_ (Pc i : pc) : pc code = int_to_pc_ (int_ i)
let seq_ (x : unit code) (y : unit -> 'a code) : 'a code = from_ir (Seqs [ to_ir x; to_ir (y ()) ])
let seqs_ (xs : (unit -> unit code) list) : unit code = Stdlib.List.fold_left seq_ unit_ xs

let seq_b1_ (x : unit code) (y : unit -> unit code) : unit code =
  code $ parens (group (uncode x ^^ string ";" ^^ break 1 ^^ uncode (y ())))

(*Indexing should start at 0*)
let zro_ (x : ('a * 'b) code) : 'a code = app_ (code $ string "fst") x
let pair_value_ (x : (Word.t * Value.seq) code) : Value.seq code = app_ (code $ string "snd") x
let add_ (x : int code) (y : int code) : int code = code $ parens (uncode x ^^ string " + " ^^ uncode y)
let dyn_array_get_ (arr : 'a Dynarray.t code) (i : int code) : 'a code = app2_ (code $ string "Dynarray.get") arr i
let dyn_array_remove_last_ (arr : 'a Dynarray.t code) : unit code = app_ (code $ string "Dynarray.remove_last") arr
let world_state_ (w : world code) : state code = code $ parens (uncode w ^^ string ".state")
let state_env_ (s : state code) : env code = code $ parens (uncode s ^^ string ".e")
let world_env_ (w : world code) : env code = state_env_ @@ world_state_ w
let state_kont_ (s : state code) : kont code = code $ parens (uncode s ^^ string ".k")
let world_kont_ (w : world code) : kont code = state_kont_ @@ world_state_ w
let stepped_ (w : world code) : unit code = app_ (code $ string "stepped") w

let set_c_ (w : world code) (c : exp code) : unit code =
  code $ parens (uncode (world_state_ w) ^^ string ".c <- " ^^ uncode c)

let set_k_ (w : world code) (k : kont code) : unit code =
  code $ parens (uncode (world_state_ w) ^^ string ".k <- " ^^ uncode k)

let from_constructor_ (ctag : int code) : Value.seq code = app_ (code $ string "Memo.from_constructor") ctag
let to_unit_ (x : 'a code) : unit code = app_ (code $ string "ignore") x
let pop_env_ (w : world code) : Value.value code = app_ (code $ string "pop_env") w

let goto_ (w : world code) (pc_value : pc) : unit code =
  seq_ (set_c_ w (pc_to_exp_ (pc_ pc_value))) (fun _ -> stepped_ w)

let push_env_ (w : world code) (v : Value.seq code) : unit code = app2_ (code $ string "push_env") w v
let get_env_ (w : world code) (i : int code) : Value.seq code = dyn_array_get_ (state_env_ @@ world_state_ w) i
let exec_done_ (w : world code) : unit code = app_ (code $ string "exec_done") w

let env_call_ (w : world code) (keep : int list code) (nargs : int code) : Value.seq code =
  app3_ (code $ string "env_call") w keep nargs

let restore_env_ (w : world code) (n : int code) (seqs : Value.seq code) : unit code =
  app3_ (code $ string "restore_env") w n seqs

let get_next_cont_ (seqs : Value.seq code) : Value.seq code = app_ (code $ string "get_next_cont") seqs

let resolve_ (w : world code) (src : Reference.source code) : (Word.t * Value.seq) option code =
  app2_ (code $ string "resolve") w src

let memo_appends_ (xs : Value.seq code list) : Value.seq code =
  app_
    (code $ string "Memo.appends")
    (code (string "[" ^^ separate (string ";") (Stdlib.List.map uncode xs) ^^ string "]"))

let memo_from_int_ (i : int code) : Value.seq code = app_ (code $ string "Memo.from_int") i
let int_from_word_ (w : Word.t code) : int code = app_ (code $ string "Word.get_value") w
let memo_splits_ (seq : Value.seq code) : Value.seq list code = app_ (code $ string "Memo.splits") seq
let word_get_value_ (w : Word.t code) : int code = app_ (code $ string "Word.get_value") w

let let_in_ (a : string) (value : 'a code) (body : 'a code -> 'b code) : 'b code =
  let name_doc = string (gensym a) in
  code $ string "let " ^^ name_doc ^^ string " = " ^^ uncode value ^^ string " in " ^^ uncode (body (code name_doc))

let list_literal_ (xs : 'a code list) : 'a list code =
  code $ string "[" ^^ separate (string ";") (Stdlib.List.map uncode xs) ^^ string "]"

let list_literal_of_ (f : 'a -> 'b code) (xs : 'a list) : 'b list code = list_literal_ (Stdlib.List.map f xs)
let list_nth_ (xs : 'a list code) (i : int code) : 'a code = app2_ (code $ string "List.nth") xs i

let match_option_ (x : 'a option code) (none : unit -> 'b code) (a : string) (some : 'a code -> 'b code) : 'b code =
  let name_doc = string (gensym a) in
  code
  $ string "match " ^^ uncode x ^^ string " with | None -> "
    ^^ uncode (none ())
    ^^ string " | Some " ^^ name_doc ^^ string " -> "
    ^^ uncode (some (code name_doc))

let src_E_ (i : int) : Reference.source code = code $ parens (string "Source.E " ^^ string (Int.to_string i))
