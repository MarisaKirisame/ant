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
let raw (s : string) = code $ string s
let paren : 'a code -> 'a code = fun c -> from_ir (Paren (to_ir c))

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
  from_ir (Paren (Lam (Raw a_doc, to_ir (f (code a_doc)))))

let lam2_ (a : string) (b : string) (f : 'a code -> 'b code -> 'c code) : ('a -> 'b -> 'c) code =
  let a_doc = string (gensym a) in
  let b_doc = string (gensym b) in
  from_ir (Paren (Lam (Raw (a_doc ^^ space ^^ b_doc), to_ir (f (code a_doc) (code b_doc)))))

let lam3_ (a : string) (b : string) (c : string) (f : 'a code -> 'b code -> 'c code -> 'd code) :
    ('a -> 'b -> 'c -> 'd) code =
  let a_doc = string (gensym a) in
  let b_doc = string (gensym b) in
  let c_doc = string (gensym c) in
  from_ir
    (Paren (Lam (Raw (a_doc ^^ space ^^ b_doc ^^ space ^^ c_doc), to_ir (f (code a_doc) (code b_doc) (code c_doc)))))

let lam4_ (a : string) (b : string) (c : string) (d : string) (f : 'a code -> 'b code -> 'c code -> 'd code -> 'e code)
    : ('a -> 'b -> 'c -> 'd -> 'e) code =
  let a_doc = string (gensym a) in
  let b_doc = string (gensym b) in
  let c_doc = string (gensym c) in
  let d_doc = string (gensym d) in
  from_ir
    (Paren
       (Lam
          ( Raw (a_doc ^^ space ^^ b_doc ^^ space ^^ c_doc ^^ space ^^ d_doc),
            to_ir (f (code a_doc) (code b_doc) (code c_doc) (code d_doc)) )))

let app_ (f : ('a -> 'b) code) (a : 'a code) : 'b code = from_ir (Paren (App (to_ir f, [ to_ir a ])))

let app2_ (f : ('a -> 'b -> 'c) code) (a : 'a code) (b : 'b code) : 'c code =
  from_ir (Paren (App (to_ir f, [ to_ir a; to_ir b ])))

let app3_ (f : ('a -> 'b -> 'c -> 'd) code) (a : 'a code) (b : 'b code) (c : 'c code) : 'd code =
  from_ir (Paren (App (to_ir f, [ to_ir a; to_ir b; to_ir c ])))

let app4_ (f : ('a -> 'b -> 'c -> 'd -> 'e) code) (a : 'a code) (b : 'b code) (c : 'c code) (d : 'd code) : 'e code =
  from_ir (Paren (App (to_ir f, [ to_ir a; to_ir b; to_ir c; to_ir d ])))

let app5_ (f : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) code) (a : 'a code) (b : 'b code) (c : 'c code) (d : 'd code)
    (e : 'e code) : 'f code =
  from_ir (Paren (App (to_ir f, [ to_ir a; to_ir b; to_ir c; to_ir d; to_ir e ])))

let assert_env_length_ (w : world code) (e : int code) : unit code = app2_ (from_ir $ Function "assert_env_length") w e

let return_n_ (w : world code) (n : int code) (exp : exp code) : unit code =
  app3_ (from_ir $ Function "return_n") w n exp

let drop_n_ (w : world code) (e : int code) (n : int code) : unit code = app3_ (from_ir $ Function "drop_n") w e n
let pc_to_int_ (pc : int code) : int code = app_ (from_ir $ Function "pc_to_int") pc
let int_to_pc_ (int : int code) : pc code = app_ (from_ir $ Function "int_to_pc") int
let pc_to_exp_ (pc : pc code) : exp code = app_ (from_ir $ Function "pc_to_exp") pc
let pc_ (Pc i : pc) : pc code = int_to_pc_ (int_ i)
let seq_ (x : unit code) (y : unit -> 'a code) : 'a code = from_ir (Seqs [ to_ir x; to_ir (y ()) ])
let seqs_ (xs : (unit -> unit code) list) : unit code = Stdlib.List.fold_left seq_ unit_ xs

let seq_b1_ (x : unit code) (y : unit -> unit code) : unit code =
  code $ parens (group (uncode x ^^ string ";" ^^ break 1 ^^ uncode (y ())))

(*Indexing should start at 0*)
let zro_ (x : ('a * 'b) code) : 'a code = app_ (from_ir $ Function "fst") x
let pair_value_ (x : (Word.t * Value.seq) code) : Value.seq code = app_ (from_ir $ Function "snd") x
let add_ (x : int code) (y : int code) : int code = code $ parens (uncode x ^^ string " + " ^^ uncode y)
let dyn_array_get_ (arr : 'a Dynarray.t code) (i : int code) : 'a code = app2_ (from_ir $ Function "Dynarray.get") arr i
let dyn_array_remove_last_ (arr : 'a Dynarray.t code) : unit code = app_ (from_ir $ Function "Dynarray.remove_last") arr
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

let from_constructor_ (ctag : int code) : Value.seq code = app_ (from_ir $ Function "Memo.from_constructor") ctag
let to_unit_ (x : 'a code) : unit code = app_ (from_ir $ Function "ignore") x
let pop_env_ (w : world code) : Value.value code = app_ (from_ir $ Function "pop_env") w

let goto_ (w : world code) (pc_value : pc) : unit code =
  seq_ (set_c_ w (pc_to_exp_ (pc_ pc_value))) (fun _ -> stepped_ w)

let push_env_ (w : world code) (v : Value.seq code) : unit code = app2_ (from_ir $ Function "push_env") w v
let get_env_ (w : world code) (i : int code) : Value.seq code = dyn_array_get_ (state_env_ @@ world_state_ w) i
let exec_done_ (w : world code) : unit code = app_ (from_ir $ Function "exec_done") w

let env_call_ (w : world code) (keep : int list code) (nargs : int code) : Value.seq code =
  app3_ (from_ir $ Function "env_call") w keep nargs

let restore_env_ (w : world code) (n : int code) (seqs : Value.seq code) : unit code =
  app3_ (from_ir $ Function "restore_env") w n seqs

let get_next_cont_ (seqs : Value.seq code) : Value.seq code = app_ (from_ir $ Function "get_next_cont") seqs

let resolve_ (w : world code) (src : Reference.source code) : (Word.t * Value.seq) option code =
  app2_ (from_ir $ Function "resolve") w src

let memo_appends_ (xs : Value.seq code list) : Value.seq code =
  app_ (from_ir $ Function "Memo.appends")
    (code (string "[" ^^ separate (string ";") (Stdlib.List.map uncode xs) ^^ string "]"))

let memo_from_int_ (i : int code) : Value.seq code = app_ (from_ir $ Function "Memo.from_int") i
let int_from_word_ (w : Word.t code) : int code = app_ (from_ir $ Function "Word.get_value") w
let memo_splits_ (seq : Value.seq code) : Value.seq list code = app_ (from_ir $ Function "Memo.splits") seq

(* NOTE: this number should be modified w.r.t the definition of Memo.splits_* in Memo.ml *)
let n_max_specialized_arity = 4

let memo_splits_specialized_ (seq : Value.seq code) (n : int) =
  if n > n_max_specialized_arity then memo_splits_ seq
  else app_ (from_ir $ Function ("Memo.splits_" ^ string_of_int n)) seq

let memo_list_match_ (seq : Value.seq code) : (Word.t * Value.seq) option code =
  app_ (from_ir $ Function "Memo.list_match") seq

let word_get_value_ (w : Word.t code) : int code = app_ (from_ir $ Function "Word.get_value") w
let option_get_ (c : 'a option code) : 'a code = app_ (from_ir $ Function "Option.get") c

let let_in_ (a : string) (value : 'a code) (body : 'a code -> 'b code) : 'b code =
  let name_doc = string (gensym a) in
  Code (LetIn (Raw name_doc, to_ir value, to_ir (body (code name_doc))))

let tuple2_ (a : 'a code) (b : 'b code) : ('a * 'b) code = code (parens (uncode a ^^ string ", " ^^ uncode b))
let pair_ (a : 'a code) (b : 'b code) : ('a * 'b) code = tuple2_ a b

let let_pat_in_ (pat : 'a code) (value : 'a code) (body : 'b code) : 'b code =
  Code (LetIn (to_ir pat, to_ir value, to_ir body))

let list_ (xs : 'a code list) : 'a list code = code (brackets (separate_map (string "; ") (fun x -> uncode x) xs))

let list_literal_ (xs : 'a code list) : 'a list code =
  code $ string "[" ^^ separate (string ";") (Stdlib.List.map uncode xs) ^^ string "]"

let list_literal_of_ (f : 'a -> 'b code) (xs : 'a list) : 'b list code = list_literal_ (Stdlib.List.map f xs)
let list_nth_ (xs : 'a list code) (i : int code) : 'a code = app2_ (from_ir $ Function "List.nth") xs i
let tuple_ (xs : 'a code list) : 'a list code = code (parens (separate_map (string ", ") (fun x -> uncode x) xs))
let memo_splits_pat_ (xs : 'a code list) = if List.length xs > n_max_specialized_arity then list_ xs else tuple_ xs

let match_option_ (x : 'a option code) (none : unit -> 'b code) (a : string) (some : 'a code -> 'b code) : 'b code =
  let name_doc = string (gensym a) in
  from_ir
    (Match
       ( to_ir x,
         [ (Raw (string "None"), to_ir (none ())); (Raw (string "Some " ^^ name_doc), to_ir (some (code name_doc))) ] ))

let src_E_ (i : int) : Reference.source code = code $ parens (string "Source.E " ^^ string (Int.to_string i))

let match_resolve_ (x : (Word.t * Value.seq) option code) (none : unit -> 'a code) (a : string)
    (some : (Word.t * Value.seq) code -> 'a code) : 'a code =
  let name = string (gensym a) in
  from_ir
    (Match
       (to_ir x, [ (Raw (string "None"), to_ir (none ())); (Raw (string "Some " ^^ name), to_ir (some (code name))) ]))

let match_resolve_destruct_ (x : (Word.t * Value.seq) option code) (none : unit -> 'a code) (hd : string) (tl : string)
    (some : Word.t code -> Value.seq code -> 'a code) : 'a code =
  let name_hd = string (gensym hd) in
  let name_tl = string (gensym tl) in
  from_ir
    (Match
       ( to_ir x,
         [
           (Raw (string "None"), to_ir (none ()));
           ( Raw (string "Some (" ^^ name_hd ^^ string ", " ^^ name_tl ^^ string ")"),
             to_ir (some (code name_hd) (code name_tl)) );
         ] ))

let match_raw_ (x : int code) (fs : (document * 'a code) list) : 'a code =
  let alts = Stdlib.List.map (fun (c, body) -> (Raw c, to_ir body)) fs in
  from_ir (Match (to_ir x, alts))

let match_int_ (x : int code) (fs : (int code * 'a code) list) : 'a code =
  let alts = Stdlib.List.map (fun (c, body) -> (to_ir c, to_ir body)) fs in
  from_ir (Match (to_ir x, alts))

let match_int_default_ (x : int code) (fs : (int * 'a code) list) (dflt : 'a code) : 'a code =
  let alts = Stdlib.List.map (fun (c, body) -> (int_ c, body)) fs in
  match_int_ x (alts @ [ (raw "_", dflt) ])

let unreachable_ : 'a code = code $ string "failwith \"unreachable\""

let match_ctor_tag_default_ (x : int code) (fs : (string * 'a code) list) (dflt : 'a code) : 'a code =
  let dummy_name = string (gensym "c") in
  let alts =
    Stdlib.List.map
      (fun (tag_name, body) ->
        (Raw (dummy_name ^^ string " when " ^^ dummy_name ^^ string " = " ^^ string tag_name), to_ir body))
      fs
  in
  from_ir (Match (to_ir x, alts @ [ (Raw (string "_"), to_ir dflt) ]))
