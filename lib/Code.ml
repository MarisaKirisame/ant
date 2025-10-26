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
  print_endline (show_ir ir);
  ir_to_doc (optimize_ir ir)

let fresh_name : (string, int) Hashtbl.t = Hashtbl.create (module String)

let gensym (base : string) : string =
  let n = Option.value (Hashtbl.find fresh_name base) ~default:0 in
  Hashtbl.set fresh_name ~key:base ~data:(n + 1);
  base ^ "_" ^ Int.to_string n

let int (i : int) : int code = code $ string (Int.to_string i)
let unit : unit code = Code Unit

let lam (a : string) (f : 'a code -> 'b code) : ('a -> 'b) code =
  let a_doc = string (gensym a) in
  code $ string "(fun " ^^ a_doc ^^ string " -> " ^^ uncode (f (code a_doc)) ^^ string ")"

let lam2 (a : string) (b : string) (f : 'a code -> 'b code -> 'c code) : ('a -> 'b -> 'c) code =
  let a_doc = string (gensym a) in
  let b_doc = string (gensym b) in
  code
  $ string "(fun " ^^ a_doc ^^ string " " ^^ b_doc ^^ string " -> "
    ^^ uncode (f (code a_doc) (code b_doc))
    ^^ string ")"

let lam3 (a : string) (b : string) (c : string) (f : 'a code -> 'b code -> 'c code -> 'd code) :
    ('a -> 'b -> 'c -> 'd) code =
  let a_doc = string (gensym a) in
  let b_doc = string (gensym b) in
  let c_doc = string (gensym c) in
  code
  $ string "(fun " ^^ a_doc ^^ string " " ^^ b_doc ^^ string " " ^^ c_doc ^^ string " -> "
    ^^ uncode (f (code a_doc) (code b_doc) (code c_doc))
    ^^ string ")"

let lam4 (a : string) (b : string) (c : string) (d : string) (f : 'a code -> 'b code -> 'c code -> 'd code -> 'e code) :
    ('a -> 'b -> 'c -> 'd -> 'e) code =
  let a_doc = string (gensym a) in
  let b_doc = string (gensym b) in
  let c_doc = string (gensym c) in
  let d_doc = string (gensym d) in
  code
  $ string "(fun " ^^ a_doc ^^ string " " ^^ b_doc ^^ string " " ^^ c_doc ^^ string " " ^^ d_doc ^^ string " -> "
    ^^ uncode (f (code a_doc) (code b_doc) (code c_doc) (code d_doc))
    ^^ string ")"

let app (f : ('a -> 'b) code) (a : 'a code) : 'b code = Code (Paren (App (to_ir f, [ to_ir a ])))
let app2 (f : ('a -> 'b -> 'c) code) (a : 'a code) (b : 'b code) : 'c code = app (app f a) b
let app3 (f : ('a -> 'b -> 'c -> 'd) code) (a : 'a code) (b : 'b code) (c : 'c code) : 'd code = app (app2 f a b) c

let app4 (f : ('a -> 'b -> 'c -> 'd -> 'e) code) (a : 'a code) (b : 'b code) (c : 'c code) (d : 'd code) : 'e code =
  app (app3 f a b c) d

let app5 (f : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) code) (a : 'a code) (b : 'b code) (c : 'c code) (d : 'd code)
    (e : 'e code) : 'f code =
  app (app4 f a b c d) e

let assert_env_length (w : world code) (e : int code) : unit code = app2 (code $ string "assert_env_length") w e
let return_n (w : world code) (n : int code) (exp : exp code) : unit code = app3 (code $ string "return_n") w n exp
let drop_n (w : world code) (e : int code) (n : int code) : unit code = app3 (code $ string "drop_n") w e n
let pc_to_exp (pc : int code) : exp code = app (from_ir (Function "pc_to_exp")) pc
let seq (x : unit code) (y : unit -> 'a code) : 'a code = from_ir (Seqs [ to_ir x; to_ir (y ()) ])
let seqs (xs : (unit -> unit code) list) : unit code = Stdlib.List.fold_left seq unit xs

let seq_b1 (x : unit code) (y : unit -> unit code) : unit code =
  code $ parens (group (uncode x ^^ string ";" ^^ break 1 ^^ uncode (y ())))

(*Indexing should start at 0*)
let zro (x : ('a * 'b) code) : 'a code = app (code $ string "fst") x
let pair_value (x : (Word.t * Value.seq) code) : Value.seq code = app (code $ string "snd") x
let add (x : int code) (y : int code) : int code = code $ parens (uncode x ^^ string " + " ^^ uncode y)
let dyn_array_get (arr : 'a Dynarray.t code) (i : int code) : 'a code = app2 (code $ string "Dynarray.get") arr i
let dyn_array_remove_last (arr : 'a Dynarray.t code) : unit code = app (code $ string "Dynarray.remove_last") arr
let world_state (w : world code) : state code = code $ parens (uncode w ^^ string ".state")
let state_env (s : state code) : env code = code $ parens (uncode s ^^ string ".e")
let world_env (w : world code) : env code = state_env @@ world_state w
let state_kont (s : state code) : kont code = code $ parens (uncode s ^^ string ".k")
let world_kont (w : world code) : kont code = state_kont @@ world_state w
let stepped (w : world code) : unit code = app (code $ string "stepped") w

let set_c (w : world code) (c : exp code) : unit code =
  code $ parens (uncode (world_state w) ^^ string ".c <- " ^^ uncode c)

let set_k (w : world code) (k : kont code) : unit code =
  code $ parens (uncode (world_state w) ^^ string ".k <- " ^^ uncode k)

let from_constructor (ctag : int code) : Value.seq code = app (code $ string "Memo.from_constructor") ctag
let to_unit (x : 'a code) : unit code = app (code $ string "ignore") x
let pop_env (w : world code) : Value.value code = app (code $ string "pop_env") w
let goto (w : world code) pc : unit code = seq (set_c w (pc_to_exp (int pc))) (fun _ -> stepped w)
let push_env (w : world code) (v : Value.seq code) : unit code = app2 (code $ string "push_env") w v
let get_env (w : world code) (i : int code) : Value.seq code = dyn_array_get (state_env @@ world_state w) i
let exec_done (w : world code) : unit code = app (code $ string "exec_done") w

let env_call (w : world code) (keep : int list code) (nargs : int code) : Value.seq code =
  app3 (code $ string "env_call") w keep nargs

let restore_env (w : world code) (n : int code) (seqs : Value.seq code) : unit code =
  app3 (code $ string "restore_env") w n seqs

let get_next_cont (seqs : Value.seq code) : Value.seq code = app (code $ string "get_next_cont") seqs

let resolve (w : world code) (src : Reference.source code) : (Word.t * Value.seq) option code =
  app2 (code $ string "resolve") w src

let memo_appends (xs : Value.seq code list) : Value.seq code =
  app
    (code $ string "Memo.appends")
    (code (string "[" ^^ separate (string ";") (Stdlib.List.map uncode xs) ^^ string "]"))

let memo_from_int (i : int code) : Value.seq code = app (code $ string "Memo.from_int") i
let int_from_word (w : Word.t code) : int code = app (code $ string "Word.to_int") w
let memo_splits (seq : Value.seq code) : Value.seq list code = app (code $ string "Memo.splits") seq
let word_get_value (w : Word.t code) : int code = app (code $ string "Word.get_value") w

let let_in (a : string) (value : 'a code) (body : 'a code -> 'b code) : 'b code =
  let name_doc = string (gensym a) in
  code $ string "let " ^^ name_doc ^^ string " = " ^^ uncode value ^^ string " in " ^^ uncode (body (code name_doc))

let list_literal (xs : 'a code list) : 'a list code =
  code $ string "[" ^^ separate (string ";") (Stdlib.List.map uncode xs) ^^ string "]"

let list_literal_of (f : 'a -> 'b code) (xs : 'a list) : 'b list code = list_literal (Stdlib.List.map f xs)

let match_option (x : 'a option code) (none : unit -> 'b code) (a : string) (some : 'a code -> 'b code) : 'b code =
  let name_doc = string (gensym a) in
  code
  $ string "match " ^^ uncode x ^^ string " with | None -> "
    ^^ uncode (none ())
    ^^ string " | Some " ^^ name_doc ^^ string " -> "
    ^^ uncode (some (code name_doc))

let src_E (i : int) : Reference.source code = code $ parens (string "Source.E " ^^ string (Int.to_string i))
