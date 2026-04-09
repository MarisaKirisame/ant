open Ant
open Word
open Memo
open Value
open Common

let tag_cont_done = 0
let tag_Z = 1
let tag_S = 2
let tag_Nil = 3
let tag_Cons = 4
let tag_None = 5
let tag_Some = 6
let tag_EInt = 7
let tag_EPlus = 8
let tag_ELt = 9
let tag_ELe = 10
let tag_EGt = 11
let tag_EGe = 12
let tag_EVar = 13
let tag_EAbs = 14
let tag_EApp = 15
let tag_ELet = 16
let tag_ETrue = 17
let tag_EFalse = 18
let tag_EIf = 19
let tag_ENil = 20
let tag_ECons = 21
let tag_EMatchList = 22
let tag_EPair = 23
let tag_EZro = 24
let tag_EFst = 25
let tag_EFix = 26
let tag_EHole = 27
let tag_EUnit = 28
let tag_VInt = 29
let tag_VAbs = 30
let tag_VUnit = 31
let tag_VTrue = 32
let tag_VFalse = 33
let tag_VNil = 34
let tag_VCons = 35
let tag_VPair = 36
let tag_VFix = 37
let tag_VStuck = 38
let tag_VTInt = 39
let tag_VTFunc = 40
let tag_VTBool = 41
let tag_VTList = 42
let tag_VTPair = 43
let tag_SHole = 44
let tag_STypeError = 45
let tag_SIndexError = 46
let tag_SApp = 47
let tag_SAdd0 = 48
let tag_SAdd1 = 49
let tag_SGt0 = 50
let tag_SGt1 = 51
let tag_SIf = 52
let tag_SMatchList = 53
let tag_SZro = 54
let tag_SFst = 55
let tag_cont_0 = 56
let tag_cont_1 = 57
let tag_cont_2 = 58
let tag_cont_3 = 59
let tag_cont_4 = 60
let tag_cont_5 = 61
let tag_cont_6 = 62
let tag_cont_7 = 63
let tag_cont_8 = 64
let tag_cont_9 = 65
let tag_cont_10 = 66
let tag_cont_11 = 67
let tag_cont_12 = 68
let tag_cont_13 = 69
let tag_cont_14 = 70
let tag_cont_15 = 71
let tag_cont_16 = 72
let tag_cont_17 = 73
let tag_cont_18 = 74
let tag_cont_19 = 75
let tag_cont_20 = 76
let tag_cont_21 = 77
let tag_cont_22 = 78

type nat = Z | S of nat

let rec from_ocaml_nat x =
  match x with
  | Z -> Memo.appends [ Memo.from_constructor tag_Z ]
  | S x0 -> Memo.appends [ Memo.from_constructor tag_S; from_ocaml_nat x0 ]

let rec to_ocaml_nat x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | 1 (* tag_Z *) -> Z
  | 2 (* tag_S *) ->
      let x0 = Memo.splits_1 t in
      S (to_ocaml_nat x0)
  | _ -> failwith "unreachable"

type 'a list = Nil | Cons of 'a * 'a list

let rec from_ocaml_list from_generic_a x =
  match x with
  | Nil -> Memo.appends [ Memo.from_constructor tag_Nil ]
  | Cons (x0, x1) ->
      Memo.appends [ Memo.from_constructor tag_Cons; from_generic_a x0; from_ocaml_list (fun x -> from_generic_a x) x1 ]

let rec to_ocaml_list to_generic_a x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | 3 (* tag_Nil *) -> Nil
  | 4 (* tag_Cons *) ->
      let x0, x1 = Memo.splits_2 t in
      Cons (to_generic_a x0, to_ocaml_list (fun x -> to_generic_a x) x1)
  | _ -> failwith "unreachable"

type 'a option = None | Some of 'a

let rec from_ocaml_option from_generic_a x =
  match x with
  | None -> Memo.appends [ Memo.from_constructor tag_None ]
  | Some x0 -> Memo.appends [ Memo.from_constructor tag_Some; from_generic_a x0 ]

let rec to_ocaml_option to_generic_a x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | 5 (* tag_None *) -> None
  | 6 (* tag_Some *) ->
      let x0 = Memo.splits_1 t in
      Some (to_generic_a x0)
  | _ -> failwith "unreachable"

type expr =
  | EInt of int
  | EPlus of expr * expr
  | ELt of expr * expr
  | ELe of expr * expr
  | EGt of expr * expr
  | EGe of expr * expr
  | EVar of nat
  | EAbs of expr
  | EApp of expr * expr
  | ELet of expr * expr
  | ETrue
  | EFalse
  | EIf of expr * expr * expr
  | ENil
  | ECons of expr * expr
  | EMatchList of expr * expr * expr
  | EPair of expr * expr
  | EZro of expr
  | EFst of expr
  | EFix of expr
  | EHole of int option
  | EUnit

let rec from_ocaml_expr x =
  match x with
  | EInt x0 -> Memo.appends [ Memo.from_constructor tag_EInt; Memo.from_int x0 ]
  | EPlus (x0, x1) -> Memo.appends [ Memo.from_constructor tag_EPlus; from_ocaml_expr x0; from_ocaml_expr x1 ]
  | ELt (x0, x1) -> Memo.appends [ Memo.from_constructor tag_ELt; from_ocaml_expr x0; from_ocaml_expr x1 ]
  | ELe (x0, x1) -> Memo.appends [ Memo.from_constructor tag_ELe; from_ocaml_expr x0; from_ocaml_expr x1 ]
  | EGt (x0, x1) -> Memo.appends [ Memo.from_constructor tag_EGt; from_ocaml_expr x0; from_ocaml_expr x1 ]
  | EGe (x0, x1) -> Memo.appends [ Memo.from_constructor tag_EGe; from_ocaml_expr x0; from_ocaml_expr x1 ]
  | EVar x0 -> Memo.appends [ Memo.from_constructor tag_EVar; from_ocaml_nat x0 ]
  | EAbs x0 -> Memo.appends [ Memo.from_constructor tag_EAbs; from_ocaml_expr x0 ]
  | EApp (x0, x1) -> Memo.appends [ Memo.from_constructor tag_EApp; from_ocaml_expr x0; from_ocaml_expr x1 ]
  | ELet (x0, x1) -> Memo.appends [ Memo.from_constructor tag_ELet; from_ocaml_expr x0; from_ocaml_expr x1 ]
  | ETrue -> Memo.appends [ Memo.from_constructor tag_ETrue ]
  | EFalse -> Memo.appends [ Memo.from_constructor tag_EFalse ]
  | EIf (x0, x1, x2) ->
      Memo.appends [ Memo.from_constructor tag_EIf; from_ocaml_expr x0; from_ocaml_expr x1; from_ocaml_expr x2 ]
  | ENil -> Memo.appends [ Memo.from_constructor tag_ENil ]
  | ECons (x0, x1) -> Memo.appends [ Memo.from_constructor tag_ECons; from_ocaml_expr x0; from_ocaml_expr x1 ]
  | EMatchList (x0, x1, x2) ->
      Memo.appends [ Memo.from_constructor tag_EMatchList; from_ocaml_expr x0; from_ocaml_expr x1; from_ocaml_expr x2 ]
  | EPair (x0, x1) -> Memo.appends [ Memo.from_constructor tag_EPair; from_ocaml_expr x0; from_ocaml_expr x1 ]
  | EZro x0 -> Memo.appends [ Memo.from_constructor tag_EZro; from_ocaml_expr x0 ]
  | EFst x0 -> Memo.appends [ Memo.from_constructor tag_EFst; from_ocaml_expr x0 ]
  | EFix x0 -> Memo.appends [ Memo.from_constructor tag_EFix; from_ocaml_expr x0 ]
  | EHole x0 -> Memo.appends [ Memo.from_constructor tag_EHole; from_ocaml_option (fun x -> Memo.from_int x) x0 ]
  | EUnit -> Memo.appends [ Memo.from_constructor tag_EUnit ]

let rec to_ocaml_expr x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | 7 (* tag_EInt *) ->
      let x0 = Memo.splits_1 t in
      EInt (Word.get_value (Memo.to_word x0))
  | 8 (* tag_EPlus *) ->
      let x0, x1 = Memo.splits_2 t in
      EPlus (to_ocaml_expr x0, to_ocaml_expr x1)
  | 9 (* tag_ELt *) ->
      let x0, x1 = Memo.splits_2 t in
      ELt (to_ocaml_expr x0, to_ocaml_expr x1)
  | 10 (* tag_ELe *) ->
      let x0, x1 = Memo.splits_2 t in
      ELe (to_ocaml_expr x0, to_ocaml_expr x1)
  | 11 (* tag_EGt *) ->
      let x0, x1 = Memo.splits_2 t in
      EGt (to_ocaml_expr x0, to_ocaml_expr x1)
  | 12 (* tag_EGe *) ->
      let x0, x1 = Memo.splits_2 t in
      EGe (to_ocaml_expr x0, to_ocaml_expr x1)
  | 13 (* tag_EVar *) ->
      let x0 = Memo.splits_1 t in
      EVar (to_ocaml_nat x0)
  | 14 (* tag_EAbs *) ->
      let x0 = Memo.splits_1 t in
      EAbs (to_ocaml_expr x0)
  | 15 (* tag_EApp *) ->
      let x0, x1 = Memo.splits_2 t in
      EApp (to_ocaml_expr x0, to_ocaml_expr x1)
  | 16 (* tag_ELet *) ->
      let x0, x1 = Memo.splits_2 t in
      ELet (to_ocaml_expr x0, to_ocaml_expr x1)
  | 17 (* tag_ETrue *) -> ETrue
  | 18 (* tag_EFalse *) -> EFalse
  | 19 (* tag_EIf *) ->
      let x0, x1, x2 = Memo.splits_3 t in
      EIf (to_ocaml_expr x0, to_ocaml_expr x1, to_ocaml_expr x2)
  | 20 (* tag_ENil *) -> ENil
  | 21 (* tag_ECons *) ->
      let x0, x1 = Memo.splits_2 t in
      ECons (to_ocaml_expr x0, to_ocaml_expr x1)
  | 22 (* tag_EMatchList *) ->
      let x0, x1, x2 = Memo.splits_3 t in
      EMatchList (to_ocaml_expr x0, to_ocaml_expr x1, to_ocaml_expr x2)
  | 23 (* tag_EPair *) ->
      let x0, x1 = Memo.splits_2 t in
      EPair (to_ocaml_expr x0, to_ocaml_expr x1)
  | 24 (* tag_EZro *) ->
      let x0 = Memo.splits_1 t in
      EZro (to_ocaml_expr x0)
  | 25 (* tag_EFst *) ->
      let x0 = Memo.splits_1 t in
      EFst (to_ocaml_expr x0)
  | 26 (* tag_EFix *) ->
      let x0 = Memo.splits_1 t in
      EFix (to_ocaml_expr x0)
  | 27 (* tag_EHole *) ->
      let x0 = Memo.splits_1 t in
      EHole (to_ocaml_option (fun x -> Word.get_value (Memo.to_word x)) x0)
  | 28 (* tag_EUnit *) -> EUnit
  | _ -> failwith "unreachable"

type value =
  | VInt of int
  | VAbs of expr * value list
  | VUnit
  | VTrue
  | VFalse
  | VNil
  | VCons of value * value
  | VPair of value * value
  | VFix of expr * value list
  | VStuck of stuck

and vtype = VTInt | VTFunc | VTBool | VTList | VTPair

and stuck =
  | SHole of int option
  | STypeError of value * vtype
  | SIndexError
  | SApp of stuck * expr
  | SAdd0 of stuck * expr
  | SAdd1 of value * stuck
  | SGt0 of stuck * expr
  | SGt1 of value * stuck
  | SIf of stuck * expr * expr
  | SMatchList of stuck * expr * expr
  | SZro of stuck
  | SFst of stuck

let rec from_ocaml_value x =
  match x with
  | VInt x0 -> Memo.appends [ Memo.from_constructor tag_VInt; Memo.from_int x0 ]
  | VAbs (x0, x1) ->
      Memo.appends
        [ Memo.from_constructor tag_VAbs; from_ocaml_expr x0; from_ocaml_list (fun x -> from_ocaml_value x) x1 ]
  | VUnit -> Memo.appends [ Memo.from_constructor tag_VUnit ]
  | VTrue -> Memo.appends [ Memo.from_constructor tag_VTrue ]
  | VFalse -> Memo.appends [ Memo.from_constructor tag_VFalse ]
  | VNil -> Memo.appends [ Memo.from_constructor tag_VNil ]
  | VCons (x0, x1) -> Memo.appends [ Memo.from_constructor tag_VCons; from_ocaml_value x0; from_ocaml_value x1 ]
  | VPair (x0, x1) -> Memo.appends [ Memo.from_constructor tag_VPair; from_ocaml_value x0; from_ocaml_value x1 ]
  | VFix (x0, x1) ->
      Memo.appends
        [ Memo.from_constructor tag_VFix; from_ocaml_expr x0; from_ocaml_list (fun x -> from_ocaml_value x) x1 ]
  | VStuck x0 -> Memo.appends [ Memo.from_constructor tag_VStuck; from_ocaml_stuck x0 ]

and from_ocaml_vtype x =
  match x with
  | VTInt -> Memo.appends [ Memo.from_constructor tag_VTInt ]
  | VTFunc -> Memo.appends [ Memo.from_constructor tag_VTFunc ]
  | VTBool -> Memo.appends [ Memo.from_constructor tag_VTBool ]
  | VTList -> Memo.appends [ Memo.from_constructor tag_VTList ]
  | VTPair -> Memo.appends [ Memo.from_constructor tag_VTPair ]

and from_ocaml_stuck x =
  match x with
  | SHole x0 -> Memo.appends [ Memo.from_constructor tag_SHole; from_ocaml_option (fun x -> Memo.from_int x) x0 ]
  | STypeError (x0, x1) ->
      Memo.appends [ Memo.from_constructor tag_STypeError; from_ocaml_value x0; from_ocaml_vtype x1 ]
  | SIndexError -> Memo.appends [ Memo.from_constructor tag_SIndexError ]
  | SApp (x0, x1) -> Memo.appends [ Memo.from_constructor tag_SApp; from_ocaml_stuck x0; from_ocaml_expr x1 ]
  | SAdd0 (x0, x1) -> Memo.appends [ Memo.from_constructor tag_SAdd0; from_ocaml_stuck x0; from_ocaml_expr x1 ]
  | SAdd1 (x0, x1) -> Memo.appends [ Memo.from_constructor tag_SAdd1; from_ocaml_value x0; from_ocaml_stuck x1 ]
  | SGt0 (x0, x1) -> Memo.appends [ Memo.from_constructor tag_SGt0; from_ocaml_stuck x0; from_ocaml_expr x1 ]
  | SGt1 (x0, x1) -> Memo.appends [ Memo.from_constructor tag_SGt1; from_ocaml_value x0; from_ocaml_stuck x1 ]
  | SIf (x0, x1, x2) ->
      Memo.appends [ Memo.from_constructor tag_SIf; from_ocaml_stuck x0; from_ocaml_expr x1; from_ocaml_expr x2 ]
  | SMatchList (x0, x1, x2) ->
      Memo.appends [ Memo.from_constructor tag_SMatchList; from_ocaml_stuck x0; from_ocaml_expr x1; from_ocaml_expr x2 ]
  | SZro x0 -> Memo.appends [ Memo.from_constructor tag_SZro; from_ocaml_stuck x0 ]
  | SFst x0 -> Memo.appends [ Memo.from_constructor tag_SFst; from_ocaml_stuck x0 ]

let rec to_ocaml_value x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | 29 (* tag_VInt *) ->
      let x0 = Memo.splits_1 t in
      VInt (Word.get_value (Memo.to_word x0))
  | 30 (* tag_VAbs *) ->
      let x0, x1 = Memo.splits_2 t in
      VAbs (to_ocaml_expr x0, to_ocaml_list (fun x -> to_ocaml_value x) x1)
  | 31 (* tag_VUnit *) -> VUnit
  | 32 (* tag_VTrue *) -> VTrue
  | 33 (* tag_VFalse *) -> VFalse
  | 34 (* tag_VNil *) -> VNil
  | 35 (* tag_VCons *) ->
      let x0, x1 = Memo.splits_2 t in
      VCons (to_ocaml_value x0, to_ocaml_value x1)
  | 36 (* tag_VPair *) ->
      let x0, x1 = Memo.splits_2 t in
      VPair (to_ocaml_value x0, to_ocaml_value x1)
  | 37 (* tag_VFix *) ->
      let x0, x1 = Memo.splits_2 t in
      VFix (to_ocaml_expr x0, to_ocaml_list (fun x -> to_ocaml_value x) x1)
  | 38 (* tag_VStuck *) ->
      let x0 = Memo.splits_1 t in
      VStuck (to_ocaml_stuck x0)
  | _ -> failwith "unreachable"

and to_ocaml_vtype x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | 39 (* tag_VTInt *) -> VTInt
  | 40 (* tag_VTFunc *) -> VTFunc
  | 41 (* tag_VTBool *) -> VTBool
  | 42 (* tag_VTList *) -> VTList
  | 43 (* tag_VTPair *) -> VTPair
  | _ -> failwith "unreachable"

and to_ocaml_stuck x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | 44 (* tag_SHole *) ->
      let x0 = Memo.splits_1 t in
      SHole (to_ocaml_option (fun x -> Word.get_value (Memo.to_word x)) x0)
  | 45 (* tag_STypeError *) ->
      let x0, x1 = Memo.splits_2 t in
      STypeError (to_ocaml_value x0, to_ocaml_vtype x1)
  | 46 (* tag_SIndexError *) -> SIndexError
  | 47 (* tag_SApp *) ->
      let x0, x1 = Memo.splits_2 t in
      SApp (to_ocaml_stuck x0, to_ocaml_expr x1)
  | 48 (* tag_SAdd0 *) ->
      let x0, x1 = Memo.splits_2 t in
      SAdd0 (to_ocaml_stuck x0, to_ocaml_expr x1)
  | 49 (* tag_SAdd1 *) ->
      let x0, x1 = Memo.splits_2 t in
      SAdd1 (to_ocaml_value x0, to_ocaml_stuck x1)
  | 50 (* tag_SGt0 *) ->
      let x0, x1 = Memo.splits_2 t in
      SGt0 (to_ocaml_stuck x0, to_ocaml_expr x1)
  | 51 (* tag_SGt1 *) ->
      let x0, x1 = Memo.splits_2 t in
      SGt1 (to_ocaml_value x0, to_ocaml_stuck x1)
  | 52 (* tag_SIf *) ->
      let x0, x1, x2 = Memo.splits_3 t in
      SIf (to_ocaml_stuck x0, to_ocaml_expr x1, to_ocaml_expr x2)
  | 53 (* tag_SMatchList *) ->
      let x0, x1, x2 = Memo.splits_3 t in
      SMatchList (to_ocaml_stuck x0, to_ocaml_expr x1, to_ocaml_expr x2)
  | 54 (* tag_SZro *) ->
      let x0 = Memo.splits_1 t in
      SZro (to_ocaml_stuck x0)
  | 55 (* tag_SFst *) ->
      let x0 = Memo.splits_1 t in
      SFst (to_ocaml_stuck x0)
  | _ -> failwith "unreachable"

let index memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 2 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  Dynarray.set initial_env 1 x1;
  exec_cek (pc_to_exp (int_to_pc 5)) initial_env (Memo.from_constructor tag_cont_done) memo

let eval memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 2 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  Dynarray.set initial_env 1 x1;
  exec_cek (pc_to_exp (int_to_pc 86)) initial_env (Memo.from_constructor tag_cont_done) memo

let populate_state () =
  Memo.reset ();
  Words.reset ();
  add_exp
    (fun w_109 ->
      assert_env_length w_109 1;
      let hd_0, tl_0 = resolve w_109 K in
      match Word.get_value hd_0 with
      | 56 (* tag_cont_0 *) ->
          let ret_0 = get_env_slot w_109 0 in
          assert_env_length w_109 1;
          w_109.state.k <- get_next_cont tl_0;
          init_frame w_109 4 (Memo.from_int 0);
          restore_env_slots w_109 [ 0; 1 ] tl_0;
          set_env_slot w_109 2 ret_0;
          w_109.state.c <- pc_to_exp (int_to_pc 87)
      | 57 (* tag_cont_1 *) ->
          let ret_1 = get_env_slot w_109 0 in
          assert_env_length w_109 1;
          w_109.state.k <- get_next_cont tl_0;
          init_frame w_109 3 (Memo.from_int 0);
          restore_env_slots w_109 [ 0; 1 ] tl_0;
          set_env_slot w_109 2 ret_1;
          w_109.state.c <- pc_to_exp (int_to_pc 88)
      | 58 (* tag_cont_2 *) ->
          let ret_2 = get_env_slot w_109 0 in
          assert_env_length w_109 1;
          w_109.state.k <- get_next_cont tl_0;
          init_frame w_109 4 (Memo.from_int 0);
          restore_env_slots w_109 [ 0; 1 ] tl_0;
          set_env_slot w_109 2 ret_2;
          w_109.state.c <- pc_to_exp (int_to_pc 89)
      | 59 (* tag_cont_3 *) ->
          let ret_3 = get_env_slot w_109 0 in
          assert_env_length w_109 1;
          w_109.state.k <- get_next_cont tl_0;
          init_frame w_109 3 (Memo.from_int 0);
          restore_env_slots w_109 [ 0; 1 ] tl_0;
          set_env_slot w_109 2 ret_3;
          w_109.state.c <- pc_to_exp (int_to_pc 90)
      | 60 (* tag_cont_4 *) ->
          let ret_4 = get_env_slot w_109 0 in
          assert_env_length w_109 1;
          w_109.state.k <- get_next_cont tl_0;
          init_frame w_109 4 (Memo.from_int 0);
          restore_env_slots w_109 [ 0; 1 ] tl_0;
          set_env_slot w_109 2 ret_4;
          w_109.state.c <- pc_to_exp (int_to_pc 91)
      | 61 (* tag_cont_5 *) ->
          let ret_5 = get_env_slot w_109 0 in
          assert_env_length w_109 1;
          w_109.state.k <- get_next_cont tl_0;
          init_frame w_109 3 (Memo.from_int 0);
          restore_env_slots w_109 [ 0; 1 ] tl_0;
          set_env_slot w_109 2 ret_5;
          w_109.state.c <- pc_to_exp (int_to_pc 92)
      | 62 (* tag_cont_6 *) ->
          let ret_6 = get_env_slot w_109 0 in
          assert_env_length w_109 1;
          w_109.state.k <- get_next_cont tl_0;
          init_frame w_109 4 (Memo.from_int 0);
          restore_env_slots w_109 [ 0; 1 ] tl_0;
          set_env_slot w_109 2 ret_6;
          w_109.state.c <- pc_to_exp (int_to_pc 93)
      | 63 (* tag_cont_7 *) ->
          let ret_7 = get_env_slot w_109 0 in
          assert_env_length w_109 1;
          w_109.state.k <- get_next_cont tl_0;
          init_frame w_109 3 (Memo.from_int 0);
          restore_env_slots w_109 [ 0; 1 ] tl_0;
          set_env_slot w_109 2 ret_7;
          w_109.state.c <- pc_to_exp (int_to_pc 94)
      | 64 (* tag_cont_8 *) ->
          let ret_8 = get_env_slot w_109 0 in
          assert_env_length w_109 1;
          w_109.state.k <- get_next_cont tl_0;
          init_frame w_109 4 (Memo.from_int 0);
          restore_env_slots w_109 [ 0; 1 ] tl_0;
          set_env_slot w_109 2 ret_8;
          w_109.state.c <- pc_to_exp (int_to_pc 95)
      | 65 (* tag_cont_9 *) ->
          let ret_9 = get_env_slot w_109 0 in
          assert_env_length w_109 1;
          w_109.state.k <- get_next_cont tl_0;
          init_frame w_109 3 (Memo.from_int 0);
          restore_env_slots w_109 [ 0; 1 ] tl_0;
          set_env_slot w_109 2 ret_9;
          w_109.state.c <- pc_to_exp (int_to_pc 96)
      | 66 (* tag_cont_10 *) ->
          let ret_10 = get_env_slot w_109 0 in
          assert_env_length w_109 1;
          w_109.state.k <- get_next_cont tl_0;
          init_frame w_109 2 (Memo.from_int 0);
          restore_env_slots w_109 [] tl_0;
          set_env_slot w_109 0 ret_10;
          w_109.state.c <- pc_to_exp (int_to_pc 97)
      | 67 (* tag_cont_11 *) ->
          let ret_11 = get_env_slot w_109 0 in
          assert_env_length w_109 1;
          w_109.state.k <- get_next_cont tl_0;
          init_frame w_109 3 (Memo.from_int 0);
          restore_env_slots w_109 [ 0; 1 ] tl_0;
          set_env_slot w_109 2 ret_11;
          w_109.state.c <- pc_to_exp (int_to_pc 98)
      | 68 (* tag_cont_12 *) ->
          let ret_12 = get_env_slot w_109 0 in
          assert_env_length w_109 1;
          w_109.state.k <- get_next_cont tl_0;
          init_frame w_109 4 (Memo.from_int 0);
          restore_env_slots w_109 [ 0; 1 ] tl_0;
          set_env_slot w_109 2 ret_12;
          w_109.state.c <- pc_to_exp (int_to_pc 99)
      | 69 (* tag_cont_13 *) ->
          let ret_13 = get_env_slot w_109 0 in
          assert_env_length w_109 1;
          w_109.state.k <- get_next_cont tl_0;
          init_frame w_109 5 (Memo.from_int 0);
          restore_env_slots w_109 [ 0; 1; 2 ] tl_0;
          set_env_slot w_109 3 ret_13;
          w_109.state.c <- pc_to_exp (int_to_pc 100)
      | 70 (* tag_cont_14 *) ->
          let ret_14 = get_env_slot w_109 0 in
          assert_env_length w_109 1;
          w_109.state.k <- get_next_cont tl_0;
          init_frame w_109 3 (Memo.from_int 0);
          restore_env_slots w_109 [ 0; 1 ] tl_0;
          set_env_slot w_109 2 ret_14;
          w_109.state.c <- pc_to_exp (int_to_pc 101)
      | 71 (* tag_cont_15 *) ->
          let ret_15 = get_env_slot w_109 0 in
          assert_env_length w_109 1;
          w_109.state.k <- get_next_cont tl_0;
          init_frame w_109 4 (Memo.from_int 0);
          restore_env_slots w_109 [ 0; 1; 2 ] tl_0;
          set_env_slot w_109 3 ret_15;
          w_109.state.c <- pc_to_exp (int_to_pc 102)
      | 72 (* tag_cont_16 *) ->
          let ret_16 = get_env_slot w_109 0 in
          assert_env_length w_109 1;
          w_109.state.k <- get_next_cont tl_0;
          init_frame w_109 3 (Memo.from_int 0);
          restore_env_slots w_109 [ 2 ] tl_0;
          set_env_slot w_109 0 ret_16;
          w_109.state.c <- pc_to_exp (int_to_pc 104)
      | 73 (* tag_cont_17 *) ->
          let ret_17 = get_env_slot w_109 0 in
          assert_env_length w_109 1;
          w_109.state.k <- get_next_cont tl_0;
          init_frame w_109 3 (Memo.from_int 0);
          restore_env_slots w_109 [ 0; 1 ] tl_0;
          set_env_slot w_109 2 ret_17;
          w_109.state.c <- pc_to_exp (int_to_pc 103)
      | 74 (* tag_cont_18 *) ->
          let ret_18 = get_env_slot w_109 0 in
          assert_env_length w_109 1;
          w_109.state.k <- get_next_cont tl_0;
          init_frame w_109 3 (Memo.from_int 0);
          restore_env_slots w_109 [ 2 ] tl_0;
          set_env_slot w_109 0 ret_18;
          w_109.state.c <- pc_to_exp (int_to_pc 106)
      | 75 (* tag_cont_19 *) ->
          let ret_19 = get_env_slot w_109 0 in
          assert_env_length w_109 1;
          w_109.state.k <- get_next_cont tl_0;
          init_frame w_109 3 (Memo.from_int 0);
          restore_env_slots w_109 [ 0; 1 ] tl_0;
          set_env_slot w_109 2 ret_19;
          w_109.state.c <- pc_to_exp (int_to_pc 105)
      | 76 (* tag_cont_20 *) ->
          let ret_20 = get_env_slot w_109 0 in
          assert_env_length w_109 1;
          w_109.state.k <- get_next_cont tl_0;
          init_frame w_109 2 (Memo.from_int 0);
          restore_env_slots w_109 [] tl_0;
          set_env_slot w_109 0 ret_20;
          w_109.state.c <- pc_to_exp (int_to_pc 107)
      | 77 (* tag_cont_21 *) ->
          let ret_21 = get_env_slot w_109 0 in
          assert_env_length w_109 1;
          w_109.state.k <- get_next_cont tl_0;
          init_frame w_109 2 (Memo.from_int 0);
          restore_env_slots w_109 [] tl_0;
          set_env_slot w_109 0 ret_21;
          w_109.state.c <- pc_to_exp (int_to_pc 108)
      | 78 (* tag_cont_22 *) ->
          let ret_22 = get_env_slot w_109 0 in
          assert_env_length w_109 1;
          w_109.state.k <- get_next_cont tl_0;
          init_frame w_109 4 (Memo.from_int 0);
          restore_env_slots w_109 [ 0; 1; 2 ] tl_0;
          set_env_slot w_109 3 ret_22;
          w_109.state.c <- pc_to_exp (int_to_pc 109)
      | _ -> failwith "unreachable (0)")
    0;
  add_exp
    (fun w_104 ->
      assert_env_length w_104 1;
      assert_env_length w_104 1;
      set_env_slot w_104 0 (Memo.appends [ Memo.from_constructor tag_Some; get_env_slot w_104 0 ]);
      return_value w_104 (get_env_slot w_104 0) (pc_to_exp (int_to_pc 0)))
    1;
  add_exp
    (fun w_105 ->
      assert_env_length w_105 2;
      let arg0_30 = get_env_slot w_105 0 in
      let arg1_30 = get_env_slot w_105 1 in
      assert_env_length w_105 2;
      init_frame w_105 2 (Memo.from_int 0);
      set_env_slot w_105 0 arg0_30;
      set_env_slot w_105 1 arg1_30;
      w_105.state.c <- pc_to_exp (int_to_pc 5))
    2;
  add_exp
    (fun w_106 ->
      assert_env_length w_106 3;
      assert_env_length w_106 3;
      let resolved_31 = resolve w_106 (Source.E 0) in
      let tag_17 = Word.get_value (fst resolved_31) in
      match tag_17 with
      | 1 (* tag_Z *) ->
          let edge0_67 = get_env_slot w_106 1 in
          init_frame w_106 1 (Memo.from_int 0);
          set_env_slot w_106 0 edge0_67;
          w_106.state.c <- pc_to_exp (int_to_pc 1)
      | 2 (* tag_S *) ->
          let parts_49 = Memo.splits (snd resolved_31) in
          if List.length parts_49 = 1 then (
            let part0_49 = List.nth parts_49 0 in
            let edge0_68 = get_env_slot w_106 2 in
            let edge1_47 = part0_49 in
            init_frame w_106 2 (Memo.from_int 0);
            set_env_slot w_106 0 edge0_68;
            set_env_slot w_106 1 edge1_47;
            w_106.state.c <- pc_to_exp (int_to_pc 2))
          else failwith "unreachable (3)"
      | _ -> failwith "unreachable (3)")
    3;
  add_exp
    (fun w_107 ->
      assert_env_length w_107 0;
      return_value w_107 (Memo.from_constructor tag_None) (pc_to_exp (int_to_pc 0)))
    4;
  add_exp
    (fun w_108 ->
      assert_env_length w_108 2;
      assert_env_length w_108 2;
      let resolved_32 = resolve w_108 (Source.E 0) in
      let tag_18 = Word.get_value (fst resolved_32) in
      match tag_18 with
      | 4 (* tag_Cons *) ->
          let parts_50 = Memo.splits (snd resolved_32) in
          if List.length parts_50 = 2 then (
            let part0_50 = List.nth parts_50 0 in
            let part1_16 = List.nth parts_50 1 in
            let edge0_69 = get_env_slot w_108 1 in
            let edge1_48 = part0_50 in
            let edge2_21 = part1_16 in
            init_frame w_108 3 (Memo.from_int 0);
            set_env_slot w_108 0 edge0_69;
            set_env_slot w_108 1 edge1_48;
            set_env_slot w_108 2 edge2_21;
            w_108.state.c <- pc_to_exp (int_to_pc 3))
          else (
            init_frame w_108 0 (Memo.from_int 0);
            w_108.state.c <- pc_to_exp (int_to_pc 4))
      | _ ->
          init_frame w_108 0 (Memo.from_int 0);
          w_108.state.c <- pc_to_exp (int_to_pc 4))
    5;
  add_exp
    (fun w_0 ->
      assert_env_length w_0 1;
      assert_env_length w_0 1;
      set_env_slot w_0 0 (Memo.appends [ Memo.from_constructor tag_VInt; get_env_slot w_0 0 ]);
      return_value w_0 (get_env_slot w_0 0) (pc_to_exp (int_to_pc 0)))
    6;
  add_exp
    (fun w_1 ->
      assert_env_length w_1 2;
      assert_env_length w_1 2;
      let resolved_0 = resolve w_1 (Source.E 0) in
      let resolved_1 = resolve w_1 (Source.E 1) in
      set_env_slot w_1 0 (Memo.from_int (Word.get_value (fst resolved_0) + Word.get_value (fst resolved_1)));
      assert_env_length w_1 2;
      set_env_slot w_1 0 (Memo.appends [ Memo.from_constructor tag_VInt; get_env_slot w_1 0 ]);
      return_value w_1 (get_env_slot w_1 0) (pc_to_exp (int_to_pc 0)))
    7;
  add_exp
    (fun w_2 ->
      assert_env_length w_2 2;
      assert_env_length w_2 2;
      set_env_slot w_2 0 (Memo.appends [ Memo.from_constructor tag_SAdd1; get_env_slot w_2 1; get_env_slot w_2 0 ]);
      assert_env_length w_2 2;
      set_env_slot w_2 0 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_2 0 ]);
      return_value w_2 (get_env_slot w_2 0) (pc_to_exp (int_to_pc 0)))
    8;
  add_exp
    (fun w_3 ->
      assert_env_length w_3 1;
      assert_env_length w_3 1;
      set_env_slot w_3 0
        (Memo.appends [ Memo.from_constructor tag_STypeError; get_env_slot w_3 0; Memo.from_constructor tag_VTInt ]);
      assert_env_length w_3 1;
      set_env_slot w_3 0 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_3 0 ]);
      return_value w_3 (get_env_slot w_3 0) (pc_to_exp (int_to_pc 0)))
    9;
  add_exp
    (fun w_4 ->
      assert_env_length w_4 4;
      let arg0_0 = get_env_slot w_4 3 in
      let arg1_0 = get_env_slot w_4 2 in
      assert_env_length w_4 4;
      w_4.state.k <- Memo.appends [ Memo.from_constructor tag_cont_0; collect_env_slots w_4 [ 0; 1 ]; w_4.state.k ];
      init_frame w_4 2 (Memo.from_int 0);
      set_env_slot w_4 0 arg0_0;
      set_env_slot w_4 1 arg1_0;
      w_4.state.c <- pc_to_exp (int_to_pc 86))
    10;
  add_exp
    (fun w_6 ->
      assert_env_length w_6 2;
      assert_env_length w_6 2;
      set_env_slot w_6 0 (Memo.appends [ Memo.from_constructor tag_SAdd0; get_env_slot w_6 1; get_env_slot w_6 0 ]);
      assert_env_length w_6 2;
      set_env_slot w_6 0 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_6 0 ]);
      return_value w_6 (get_env_slot w_6 0) (pc_to_exp (int_to_pc 0)))
    11;
  add_exp
    (fun w_7 ->
      assert_env_length w_7 1;
      assert_env_length w_7 1;
      set_env_slot w_7 0
        (Memo.appends [ Memo.from_constructor tag_STypeError; get_env_slot w_7 0; Memo.from_constructor tag_VTInt ]);
      assert_env_length w_7 1;
      set_env_slot w_7 0 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_7 0 ]);
      return_value w_7 (get_env_slot w_7 0) (pc_to_exp (int_to_pc 0)))
    12;
  add_exp
    (fun w_8 ->
      assert_env_length w_8 3;
      let arg0_1 = get_env_slot w_8 2 in
      let arg1_1 = get_env_slot w_8 0 in
      assert_env_length w_8 3;
      w_8.state.k <- Memo.appends [ Memo.from_constructor tag_cont_1; collect_env_slots w_8 [ 0; 1 ]; w_8.state.k ];
      init_frame w_8 2 (Memo.from_int 0);
      set_env_slot w_8 0 arg0_1;
      set_env_slot w_8 1 arg1_1;
      w_8.state.c <- pc_to_exp (int_to_pc 86))
    13;
  add_exp
    (fun w_10 ->
      assert_env_length w_10 2;
      assert_env_length w_10 2;
      let resolved_4 = resolve w_10 (Source.E 0) in
      let resolved_5 = resolve w_10 (Source.E 1) in
      set_env_slot w_10 0
        (Memo.from_int (if Word.get_value (fst resolved_4) < Word.get_value (fst resolved_5) then 1 else 0));
      let resolved_6 = resolve w_10 (Source.E 0) in
      if Word.get_value (fst resolved_6) <> 0 then (
        init_frame w_10 0 (Memo.from_int 0);
        w_10.state.c <- pc_to_exp (int_to_pc 15))
      else (
        init_frame w_10 0 (Memo.from_int 0);
        w_10.state.c <- pc_to_exp (int_to_pc 16)))
    14;
  add_exp
    (fun w_11 ->
      assert_env_length w_11 0;
      return_value w_11 (Memo.from_constructor tag_VTrue) (pc_to_exp (int_to_pc 0)))
    15;
  add_exp
    (fun w_12 ->
      assert_env_length w_12 0;
      return_value w_12 (Memo.from_constructor tag_VFalse) (pc_to_exp (int_to_pc 0)))
    16;
  add_exp
    (fun w_13 ->
      assert_env_length w_13 2;
      assert_env_length w_13 2;
      set_env_slot w_13 0 (Memo.appends [ Memo.from_constructor tag_SGt1; get_env_slot w_13 1; get_env_slot w_13 0 ]);
      assert_env_length w_13 2;
      set_env_slot w_13 0 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_13 0 ]);
      return_value w_13 (get_env_slot w_13 0) (pc_to_exp (int_to_pc 0)))
    17;
  add_exp
    (fun w_14 ->
      assert_env_length w_14 1;
      assert_env_length w_14 1;
      set_env_slot w_14 0
        (Memo.appends [ Memo.from_constructor tag_STypeError; get_env_slot w_14 0; Memo.from_constructor tag_VTInt ]);
      assert_env_length w_14 1;
      set_env_slot w_14 0 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_14 0 ]);
      return_value w_14 (get_env_slot w_14 0) (pc_to_exp (int_to_pc 0)))
    18;
  add_exp
    (fun w_15 ->
      assert_env_length w_15 4;
      let arg0_2 = get_env_slot w_15 3 in
      let arg1_2 = get_env_slot w_15 2 in
      assert_env_length w_15 4;
      w_15.state.k <- Memo.appends [ Memo.from_constructor tag_cont_2; collect_env_slots w_15 [ 0; 1 ]; w_15.state.k ];
      init_frame w_15 2 (Memo.from_int 0);
      set_env_slot w_15 0 arg0_2;
      set_env_slot w_15 1 arg1_2;
      w_15.state.c <- pc_to_exp (int_to_pc 86))
    19;
  add_exp
    (fun w_17 ->
      assert_env_length w_17 2;
      assert_env_length w_17 2;
      set_env_slot w_17 0 (Memo.appends [ Memo.from_constructor tag_SGt0; get_env_slot w_17 1; get_env_slot w_17 0 ]);
      assert_env_length w_17 2;
      set_env_slot w_17 0 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_17 0 ]);
      return_value w_17 (get_env_slot w_17 0) (pc_to_exp (int_to_pc 0)))
    20;
  add_exp
    (fun w_18 ->
      assert_env_length w_18 1;
      assert_env_length w_18 1;
      set_env_slot w_18 0
        (Memo.appends [ Memo.from_constructor tag_STypeError; get_env_slot w_18 0; Memo.from_constructor tag_VTInt ]);
      assert_env_length w_18 1;
      set_env_slot w_18 0 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_18 0 ]);
      return_value w_18 (get_env_slot w_18 0) (pc_to_exp (int_to_pc 0)))
    21;
  add_exp
    (fun w_19 ->
      assert_env_length w_19 3;
      let arg0_3 = get_env_slot w_19 2 in
      let arg1_3 = get_env_slot w_19 0 in
      assert_env_length w_19 3;
      w_19.state.k <- Memo.appends [ Memo.from_constructor tag_cont_3; collect_env_slots w_19 [ 0; 1 ]; w_19.state.k ];
      init_frame w_19 2 (Memo.from_int 0);
      set_env_slot w_19 0 arg0_3;
      set_env_slot w_19 1 arg1_3;
      w_19.state.c <- pc_to_exp (int_to_pc 86))
    22;
  add_exp
    (fun w_21 ->
      assert_env_length w_21 2;
      assert_env_length w_21 2;
      let resolved_9 = resolve w_21 (Source.E 0) in
      let resolved_10 = resolve w_21 (Source.E 1) in
      set_env_slot w_21 0
        (Memo.from_int (if Word.get_value (fst resolved_9) <= Word.get_value (fst resolved_10) then 1 else 0));
      let resolved_11 = resolve w_21 (Source.E 0) in
      if Word.get_value (fst resolved_11) <> 0 then (
        init_frame w_21 0 (Memo.from_int 0);
        w_21.state.c <- pc_to_exp (int_to_pc 24))
      else (
        init_frame w_21 0 (Memo.from_int 0);
        w_21.state.c <- pc_to_exp (int_to_pc 25)))
    23;
  add_exp
    (fun w_22 ->
      assert_env_length w_22 0;
      return_value w_22 (Memo.from_constructor tag_VTrue) (pc_to_exp (int_to_pc 0)))
    24;
  add_exp
    (fun w_23 ->
      assert_env_length w_23 0;
      return_value w_23 (Memo.from_constructor tag_VFalse) (pc_to_exp (int_to_pc 0)))
    25;
  add_exp
    (fun w_24 ->
      assert_env_length w_24 2;
      assert_env_length w_24 2;
      set_env_slot w_24 0 (Memo.appends [ Memo.from_constructor tag_SGt1; get_env_slot w_24 1; get_env_slot w_24 0 ]);
      assert_env_length w_24 2;
      set_env_slot w_24 0 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_24 0 ]);
      return_value w_24 (get_env_slot w_24 0) (pc_to_exp (int_to_pc 0)))
    26;
  add_exp
    (fun w_25 ->
      assert_env_length w_25 1;
      assert_env_length w_25 1;
      set_env_slot w_25 0
        (Memo.appends [ Memo.from_constructor tag_STypeError; get_env_slot w_25 0; Memo.from_constructor tag_VTInt ]);
      assert_env_length w_25 1;
      set_env_slot w_25 0 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_25 0 ]);
      return_value w_25 (get_env_slot w_25 0) (pc_to_exp (int_to_pc 0)))
    27;
  add_exp
    (fun w_26 ->
      assert_env_length w_26 4;
      let arg0_4 = get_env_slot w_26 3 in
      let arg1_4 = get_env_slot w_26 2 in
      assert_env_length w_26 4;
      w_26.state.k <- Memo.appends [ Memo.from_constructor tag_cont_4; collect_env_slots w_26 [ 0; 1 ]; w_26.state.k ];
      init_frame w_26 2 (Memo.from_int 0);
      set_env_slot w_26 0 arg0_4;
      set_env_slot w_26 1 arg1_4;
      w_26.state.c <- pc_to_exp (int_to_pc 86))
    28;
  add_exp
    (fun w_28 ->
      assert_env_length w_28 2;
      assert_env_length w_28 2;
      set_env_slot w_28 0 (Memo.appends [ Memo.from_constructor tag_SGt0; get_env_slot w_28 1; get_env_slot w_28 0 ]);
      assert_env_length w_28 2;
      set_env_slot w_28 0 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_28 0 ]);
      return_value w_28 (get_env_slot w_28 0) (pc_to_exp (int_to_pc 0)))
    29;
  add_exp
    (fun w_29 ->
      assert_env_length w_29 1;
      assert_env_length w_29 1;
      set_env_slot w_29 0
        (Memo.appends [ Memo.from_constructor tag_STypeError; get_env_slot w_29 0; Memo.from_constructor tag_VTInt ]);
      assert_env_length w_29 1;
      set_env_slot w_29 0 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_29 0 ]);
      return_value w_29 (get_env_slot w_29 0) (pc_to_exp (int_to_pc 0)))
    30;
  add_exp
    (fun w_30 ->
      assert_env_length w_30 3;
      let arg0_5 = get_env_slot w_30 2 in
      let arg1_5 = get_env_slot w_30 0 in
      assert_env_length w_30 3;
      w_30.state.k <- Memo.appends [ Memo.from_constructor tag_cont_5; collect_env_slots w_30 [ 0; 1 ]; w_30.state.k ];
      init_frame w_30 2 (Memo.from_int 0);
      set_env_slot w_30 0 arg0_5;
      set_env_slot w_30 1 arg1_5;
      w_30.state.c <- pc_to_exp (int_to_pc 86))
    31;
  add_exp
    (fun w_32 ->
      assert_env_length w_32 2;
      assert_env_length w_32 2;
      let resolved_14 = resolve w_32 (Source.E 0) in
      let resolved_15 = resolve w_32 (Source.E 1) in
      set_env_slot w_32 0
        (Memo.from_int (if Word.get_value (fst resolved_14) > Word.get_value (fst resolved_15) then 1 else 0));
      let resolved_16 = resolve w_32 (Source.E 0) in
      if Word.get_value (fst resolved_16) <> 0 then (
        init_frame w_32 0 (Memo.from_int 0);
        w_32.state.c <- pc_to_exp (int_to_pc 33))
      else (
        init_frame w_32 0 (Memo.from_int 0);
        w_32.state.c <- pc_to_exp (int_to_pc 34)))
    32;
  add_exp
    (fun w_33 ->
      assert_env_length w_33 0;
      return_value w_33 (Memo.from_constructor tag_VTrue) (pc_to_exp (int_to_pc 0)))
    33;
  add_exp
    (fun w_34 ->
      assert_env_length w_34 0;
      return_value w_34 (Memo.from_constructor tag_VFalse) (pc_to_exp (int_to_pc 0)))
    34;
  add_exp
    (fun w_35 ->
      assert_env_length w_35 2;
      assert_env_length w_35 2;
      set_env_slot w_35 0 (Memo.appends [ Memo.from_constructor tag_SGt1; get_env_slot w_35 1; get_env_slot w_35 0 ]);
      assert_env_length w_35 2;
      set_env_slot w_35 0 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_35 0 ]);
      return_value w_35 (get_env_slot w_35 0) (pc_to_exp (int_to_pc 0)))
    35;
  add_exp
    (fun w_36 ->
      assert_env_length w_36 1;
      assert_env_length w_36 1;
      set_env_slot w_36 0
        (Memo.appends [ Memo.from_constructor tag_STypeError; get_env_slot w_36 0; Memo.from_constructor tag_VTInt ]);
      assert_env_length w_36 1;
      set_env_slot w_36 0 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_36 0 ]);
      return_value w_36 (get_env_slot w_36 0) (pc_to_exp (int_to_pc 0)))
    36;
  add_exp
    (fun w_37 ->
      assert_env_length w_37 4;
      let arg0_6 = get_env_slot w_37 3 in
      let arg1_6 = get_env_slot w_37 2 in
      assert_env_length w_37 4;
      w_37.state.k <- Memo.appends [ Memo.from_constructor tag_cont_6; collect_env_slots w_37 [ 0; 1 ]; w_37.state.k ];
      init_frame w_37 2 (Memo.from_int 0);
      set_env_slot w_37 0 arg0_6;
      set_env_slot w_37 1 arg1_6;
      w_37.state.c <- pc_to_exp (int_to_pc 86))
    37;
  add_exp
    (fun w_39 ->
      assert_env_length w_39 2;
      assert_env_length w_39 2;
      set_env_slot w_39 0 (Memo.appends [ Memo.from_constructor tag_SGt0; get_env_slot w_39 1; get_env_slot w_39 0 ]);
      assert_env_length w_39 2;
      set_env_slot w_39 0 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_39 0 ]);
      return_value w_39 (get_env_slot w_39 0) (pc_to_exp (int_to_pc 0)))
    38;
  add_exp
    (fun w_40 ->
      assert_env_length w_40 1;
      assert_env_length w_40 1;
      set_env_slot w_40 0
        (Memo.appends [ Memo.from_constructor tag_STypeError; get_env_slot w_40 0; Memo.from_constructor tag_VTInt ]);
      assert_env_length w_40 1;
      set_env_slot w_40 0 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_40 0 ]);
      return_value w_40 (get_env_slot w_40 0) (pc_to_exp (int_to_pc 0)))
    39;
  add_exp
    (fun w_41 ->
      assert_env_length w_41 3;
      let arg0_7 = get_env_slot w_41 2 in
      let arg1_7 = get_env_slot w_41 0 in
      assert_env_length w_41 3;
      w_41.state.k <- Memo.appends [ Memo.from_constructor tag_cont_7; collect_env_slots w_41 [ 0; 1 ]; w_41.state.k ];
      init_frame w_41 2 (Memo.from_int 0);
      set_env_slot w_41 0 arg0_7;
      set_env_slot w_41 1 arg1_7;
      w_41.state.c <- pc_to_exp (int_to_pc 86))
    40;
  add_exp
    (fun w_43 ->
      assert_env_length w_43 2;
      assert_env_length w_43 2;
      let resolved_19 = resolve w_43 (Source.E 0) in
      let resolved_20 = resolve w_43 (Source.E 1) in
      set_env_slot w_43 0
        (Memo.from_int (if Word.get_value (fst resolved_19) >= Word.get_value (fst resolved_20) then 1 else 0));
      let resolved_21 = resolve w_43 (Source.E 0) in
      if Word.get_value (fst resolved_21) <> 0 then (
        init_frame w_43 0 (Memo.from_int 0);
        w_43.state.c <- pc_to_exp (int_to_pc 42))
      else (
        init_frame w_43 0 (Memo.from_int 0);
        w_43.state.c <- pc_to_exp (int_to_pc 43)))
    41;
  add_exp
    (fun w_44 ->
      assert_env_length w_44 0;
      return_value w_44 (Memo.from_constructor tag_VTrue) (pc_to_exp (int_to_pc 0)))
    42;
  add_exp
    (fun w_45 ->
      assert_env_length w_45 0;
      return_value w_45 (Memo.from_constructor tag_VFalse) (pc_to_exp (int_to_pc 0)))
    43;
  add_exp
    (fun w_46 ->
      assert_env_length w_46 2;
      assert_env_length w_46 2;
      set_env_slot w_46 0 (Memo.appends [ Memo.from_constructor tag_SGt1; get_env_slot w_46 1; get_env_slot w_46 0 ]);
      assert_env_length w_46 2;
      set_env_slot w_46 0 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_46 0 ]);
      return_value w_46 (get_env_slot w_46 0) (pc_to_exp (int_to_pc 0)))
    44;
  add_exp
    (fun w_47 ->
      assert_env_length w_47 1;
      assert_env_length w_47 1;
      set_env_slot w_47 0
        (Memo.appends [ Memo.from_constructor tag_STypeError; get_env_slot w_47 0; Memo.from_constructor tag_VTInt ]);
      assert_env_length w_47 1;
      set_env_slot w_47 0 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_47 0 ]);
      return_value w_47 (get_env_slot w_47 0) (pc_to_exp (int_to_pc 0)))
    45;
  add_exp
    (fun w_48 ->
      assert_env_length w_48 4;
      let arg0_8 = get_env_slot w_48 3 in
      let arg1_8 = get_env_slot w_48 2 in
      assert_env_length w_48 4;
      w_48.state.k <- Memo.appends [ Memo.from_constructor tag_cont_8; collect_env_slots w_48 [ 0; 1 ]; w_48.state.k ];
      init_frame w_48 2 (Memo.from_int 0);
      set_env_slot w_48 0 arg0_8;
      set_env_slot w_48 1 arg1_8;
      w_48.state.c <- pc_to_exp (int_to_pc 86))
    46;
  add_exp
    (fun w_50 ->
      assert_env_length w_50 2;
      assert_env_length w_50 2;
      set_env_slot w_50 0 (Memo.appends [ Memo.from_constructor tag_SGt0; get_env_slot w_50 1; get_env_slot w_50 0 ]);
      assert_env_length w_50 2;
      set_env_slot w_50 0 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_50 0 ]);
      return_value w_50 (get_env_slot w_50 0) (pc_to_exp (int_to_pc 0)))
    47;
  add_exp
    (fun w_51 ->
      assert_env_length w_51 1;
      assert_env_length w_51 1;
      set_env_slot w_51 0
        (Memo.appends [ Memo.from_constructor tag_STypeError; get_env_slot w_51 0; Memo.from_constructor tag_VTInt ]);
      assert_env_length w_51 1;
      set_env_slot w_51 0 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_51 0 ]);
      return_value w_51 (get_env_slot w_51 0) (pc_to_exp (int_to_pc 0)))
    48;
  add_exp
    (fun w_52 ->
      assert_env_length w_52 3;
      let arg0_9 = get_env_slot w_52 2 in
      let arg1_9 = get_env_slot w_52 0 in
      assert_env_length w_52 3;
      w_52.state.k <- Memo.appends [ Memo.from_constructor tag_cont_9; collect_env_slots w_52 [ 0; 1 ]; w_52.state.k ];
      init_frame w_52 2 (Memo.from_int 0);
      set_env_slot w_52 0 arg0_9;
      set_env_slot w_52 1 arg1_9;
      w_52.state.c <- pc_to_exp (int_to_pc 86))
    49;
  add_exp
    (fun w_54 ->
      assert_env_length w_54 1;
      return_value w_54 (get_env_slot w_54 0) (pc_to_exp (int_to_pc 0)))
    50;
  add_exp
    (fun w_55 ->
      assert_env_length w_55 1;
      assert_env_length w_55 1;
      set_env_slot w_55 0 (Memo.appends [ Memo.from_constructor tag_VStuck; Memo.from_constructor tag_SIndexError ]);
      return_value w_55 (get_env_slot w_55 0) (pc_to_exp (int_to_pc 0)))
    51;
  add_exp
    (fun w_56 ->
      assert_env_length w_56 2;
      let arg0_10 = get_env_slot w_56 0 in
      let arg1_10 = get_env_slot w_56 1 in
      assert_env_length w_56 2;
      w_56.state.k <- Memo.appends [ Memo.from_constructor tag_cont_10; collect_env_slots w_56 []; w_56.state.k ];
      init_frame w_56 2 (Memo.from_int 0);
      set_env_slot w_56 0 arg0_10;
      set_env_slot w_56 1 arg1_10;
      w_56.state.c <- pc_to_exp (int_to_pc 5))
    52;
  add_exp
    (fun w_58 ->
      assert_env_length w_58 2;
      assert_env_length w_58 2;
      set_env_slot w_58 0 (Memo.appends [ Memo.from_constructor tag_VAbs; get_env_slot w_58 1; get_env_slot w_58 0 ]);
      return_value w_58 (get_env_slot w_58 0) (pc_to_exp (int_to_pc 0)))
    53;
  add_exp
    (fun w_59 ->
      assert_env_length w_59 3;
      let arg0_11 = get_env_slot w_59 2 in
      let arg1_11 = get_env_slot w_59 1 in
      assert_env_length w_59 3;
      w_59.state.k <- Memo.appends [ Memo.from_constructor tag_cont_11; collect_env_slots w_59 [ 0; 1 ]; w_59.state.k ];
      init_frame w_59 2 (Memo.from_int 0);
      set_env_slot w_59 0 arg0_11;
      set_env_slot w_59 1 arg1_11;
      w_59.state.c <- pc_to_exp (int_to_pc 86))
    54;
  add_exp
    (fun w_61 ->
      assert_env_length w_61 2;
      assert_env_length w_61 2;
      set_env_slot w_61 0 (Memo.appends [ Memo.from_constructor tag_VFix; get_env_slot w_61 1; get_env_slot w_61 0 ]);
      return_value w_61 (get_env_slot w_61 0) (pc_to_exp (int_to_pc 0)))
    55;
  add_exp
    (fun w_62 ->
      assert_env_length w_62 4;
      let arg0_13 = get_env_slot w_62 3 in
      let arg1_13 = get_env_slot w_62 2 in
      assert_env_length w_62 4;
      w_62.state.k <- Memo.appends [ Memo.from_constructor tag_cont_12; collect_env_slots w_62 [ 0; 1 ]; w_62.state.k ];
      init_frame w_62 2 (Memo.from_int 0);
      set_env_slot w_62 0 arg0_13;
      set_env_slot w_62 1 arg1_13;
      w_62.state.c <- pc_to_exp (int_to_pc 86))
    56;
  add_exp
    (fun w_64 ->
      assert_env_length w_64 5;
      let arg0_15 = get_env_slot w_64 4 in
      let arg1_15 = get_env_slot w_64 3 in
      assert_env_length w_64 5;
      w_64.state.k <-
        Memo.appends [ Memo.from_constructor tag_cont_13; collect_env_slots w_64 [ 0; 1; 2 ]; w_64.state.k ];
      init_frame w_64 2 (Memo.from_int 0);
      set_env_slot w_64 0 arg0_15;
      set_env_slot w_64 1 arg1_15;
      w_64.state.c <- pc_to_exp (int_to_pc 86))
    57;
  add_exp
    (fun w_66 ->
      assert_env_length w_66 2;
      assert_env_length w_66 2;
      set_env_slot w_66 0 (Memo.appends [ Memo.from_constructor tag_SApp; get_env_slot w_66 1; get_env_slot w_66 0 ]);
      assert_env_length w_66 2;
      set_env_slot w_66 0 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_66 0 ]);
      return_value w_66 (get_env_slot w_66 0) (pc_to_exp (int_to_pc 0)))
    58;
  add_exp
    (fun w_67 ->
      assert_env_length w_67 1;
      assert_env_length w_67 1;
      set_env_slot w_67 0
        (Memo.appends [ Memo.from_constructor tag_STypeError; get_env_slot w_67 0; Memo.from_constructor tag_VTFunc ]);
      assert_env_length w_67 1;
      set_env_slot w_67 0 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_67 0 ]);
      return_value w_67 (get_env_slot w_67 0) (pc_to_exp (int_to_pc 0)))
    59;
  add_exp
    (fun w_68 ->
      assert_env_length w_68 3;
      let arg0_17 = get_env_slot w_68 2 in
      let arg1_17 = get_env_slot w_68 0 in
      assert_env_length w_68 3;
      w_68.state.k <- Memo.appends [ Memo.from_constructor tag_cont_14; collect_env_slots w_68 [ 0; 1 ]; w_68.state.k ];
      init_frame w_68 2 (Memo.from_int 0);
      set_env_slot w_68 0 arg0_17;
      set_env_slot w_68 1 arg1_17;
      w_68.state.c <- pc_to_exp (int_to_pc 86))
    60;
  add_exp
    (fun w_70 ->
      assert_env_length w_70 1;
      assert_env_length w_70 1;
      set_env_slot w_70 0 (Memo.appends [ Memo.from_constructor tag_SHole; get_env_slot w_70 0 ]);
      assert_env_length w_70 1;
      set_env_slot w_70 0 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_70 0 ]);
      return_value w_70 (get_env_slot w_70 0) (pc_to_exp (int_to_pc 0)))
    61;
  add_exp
    (fun w_71 ->
      assert_env_length w_71 0;
      return_value w_71 (Memo.from_constructor tag_VTrue) (pc_to_exp (int_to_pc 0)))
    62;
  add_exp
    (fun w_72 ->
      assert_env_length w_72 0;
      return_value w_72 (Memo.from_constructor tag_VFalse) (pc_to_exp (int_to_pc 0)))
    63;
  add_exp
    (fun w_73 ->
      assert_env_length w_73 2;
      let arg0_18 = get_env_slot w_73 1 in
      let arg1_18 = get_env_slot w_73 0 in
      assert_env_length w_73 2;
      init_frame w_73 2 (Memo.from_int 0);
      set_env_slot w_73 0 arg0_18;
      set_env_slot w_73 1 arg1_18;
      w_73.state.c <- pc_to_exp (int_to_pc 86))
    64;
  add_exp
    (fun w_74 ->
      assert_env_length w_74 2;
      let arg0_19 = get_env_slot w_74 1 in
      let arg1_19 = get_env_slot w_74 0 in
      assert_env_length w_74 2;
      init_frame w_74 2 (Memo.from_int 0);
      set_env_slot w_74 0 arg0_19;
      set_env_slot w_74 1 arg1_19;
      w_74.state.c <- pc_to_exp (int_to_pc 86))
    65;
  add_exp
    (fun w_75 ->
      assert_env_length w_75 3;
      assert_env_length w_75 3;
      set_env_slot w_75 0
        (Memo.appends [ Memo.from_constructor tag_SIf; get_env_slot w_75 2; get_env_slot w_75 0; get_env_slot w_75 1 ]);
      assert_env_length w_75 3;
      set_env_slot w_75 0 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_75 0 ]);
      return_value w_75 (get_env_slot w_75 0) (pc_to_exp (int_to_pc 0)))
    66;
  add_exp
    (fun w_76 ->
      assert_env_length w_76 1;
      assert_env_length w_76 1;
      set_env_slot w_76 0
        (Memo.appends [ Memo.from_constructor tag_STypeError; get_env_slot w_76 0; Memo.from_constructor tag_VTBool ]);
      assert_env_length w_76 1;
      set_env_slot w_76 0 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_76 0 ]);
      return_value w_76 (get_env_slot w_76 0) (pc_to_exp (int_to_pc 0)))
    67;
  add_exp
    (fun w_77 ->
      assert_env_length w_77 4;
      let arg0_20 = get_env_slot w_77 3 in
      let arg1_20 = get_env_slot w_77 0 in
      assert_env_length w_77 4;
      w_77.state.k <-
        Memo.appends [ Memo.from_constructor tag_cont_15; collect_env_slots w_77 [ 0; 1; 2 ]; w_77.state.k ];
      init_frame w_77 2 (Memo.from_int 0);
      set_env_slot w_77 0 arg0_20;
      set_env_slot w_77 1 arg1_20;
      w_77.state.c <- pc_to_exp (int_to_pc 86))
    68;
  add_exp
    (fun w_79 ->
      assert_env_length w_79 0;
      return_value w_79 (Memo.from_constructor tag_VNil) (pc_to_exp (int_to_pc 0)))
    69;
  add_exp
    (fun w_80 ->
      assert_env_length w_80 3;
      let arg0_21 = get_env_slot w_80 2 in
      let arg1_21 = get_env_slot w_80 0 in
      assert_env_length w_80 3;
      w_80.state.k <- Memo.appends [ Memo.from_constructor tag_cont_17; collect_env_slots w_80 [ 0; 1 ]; w_80.state.k ];
      init_frame w_80 2 (Memo.from_int 0);
      set_env_slot w_80 0 arg0_21;
      set_env_slot w_80 1 arg1_21;
      w_80.state.c <- pc_to_exp (int_to_pc 86))
    70;
  add_exp
    (fun w_83 ->
      assert_env_length w_83 3;
      let arg0_23 = get_env_slot w_83 2 in
      let arg1_23 = get_env_slot w_83 0 in
      assert_env_length w_83 3;
      w_83.state.k <- Memo.appends [ Memo.from_constructor tag_cont_19; collect_env_slots w_83 [ 0; 1 ]; w_83.state.k ];
      init_frame w_83 2 (Memo.from_int 0);
      set_env_slot w_83 0 arg0_23;
      set_env_slot w_83 1 arg1_23;
      w_83.state.c <- pc_to_exp (int_to_pc 86))
    71;
  add_exp
    (fun w_86 ->
      assert_env_length w_86 1;
      return_value w_86 (get_env_slot w_86 0) (pc_to_exp (int_to_pc 0)))
    72;
  add_exp
    (fun w_87 ->
      assert_env_length w_87 1;
      assert_env_length w_87 1;
      set_env_slot w_87 0 (Memo.appends [ Memo.from_constructor tag_SZro; get_env_slot w_87 0 ]);
      assert_env_length w_87 1;
      set_env_slot w_87 0 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_87 0 ]);
      return_value w_87 (get_env_slot w_87 0) (pc_to_exp (int_to_pc 0)))
    73;
  add_exp
    (fun w_88 ->
      assert_env_length w_88 1;
      assert_env_length w_88 1;
      set_env_slot w_88 0
        (Memo.appends [ Memo.from_constructor tag_STypeError; get_env_slot w_88 0; Memo.from_constructor tag_VTPair ]);
      assert_env_length w_88 1;
      set_env_slot w_88 0 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_88 0 ]);
      return_value w_88 (get_env_slot w_88 0) (pc_to_exp (int_to_pc 0)))
    74;
  add_exp
    (fun w_89 ->
      assert_env_length w_89 2;
      let arg0_25 = get_env_slot w_89 1 in
      let arg1_25 = get_env_slot w_89 0 in
      assert_env_length w_89 2;
      w_89.state.k <- Memo.appends [ Memo.from_constructor tag_cont_20; collect_env_slots w_89 []; w_89.state.k ];
      init_frame w_89 2 (Memo.from_int 0);
      set_env_slot w_89 0 arg0_25;
      set_env_slot w_89 1 arg1_25;
      w_89.state.c <- pc_to_exp (int_to_pc 86))
    75;
  add_exp
    (fun w_91 ->
      assert_env_length w_91 1;
      return_value w_91 (get_env_slot w_91 0) (pc_to_exp (int_to_pc 0)))
    76;
  add_exp
    (fun w_92 ->
      assert_env_length w_92 1;
      assert_env_length w_92 1;
      set_env_slot w_92 0 (Memo.appends [ Memo.from_constructor tag_SFst; get_env_slot w_92 0 ]);
      assert_env_length w_92 1;
      set_env_slot w_92 0 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_92 0 ]);
      return_value w_92 (get_env_slot w_92 0) (pc_to_exp (int_to_pc 0)))
    77;
  add_exp
    (fun w_93 ->
      assert_env_length w_93 1;
      assert_env_length w_93 1;
      set_env_slot w_93 0
        (Memo.appends [ Memo.from_constructor tag_STypeError; get_env_slot w_93 0; Memo.from_constructor tag_VTPair ]);
      assert_env_length w_93 1;
      set_env_slot w_93 0 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_93 0 ]);
      return_value w_93 (get_env_slot w_93 0) (pc_to_exp (int_to_pc 0)))
    78;
  add_exp
    (fun w_94 ->
      assert_env_length w_94 2;
      let arg0_26 = get_env_slot w_94 1 in
      let arg1_26 = get_env_slot w_94 0 in
      assert_env_length w_94 2;
      w_94.state.k <- Memo.appends [ Memo.from_constructor tag_cont_21; collect_env_slots w_94 []; w_94.state.k ];
      init_frame w_94 2 (Memo.from_int 0);
      set_env_slot w_94 0 arg0_26;
      set_env_slot w_94 1 arg1_26;
      w_94.state.c <- pc_to_exp (int_to_pc 86))
    79;
  add_exp
    (fun w_96 ->
      assert_env_length w_96 2;
      let arg0_27 = get_env_slot w_96 1 in
      let arg1_27 = get_env_slot w_96 0 in
      assert_env_length w_96 2;
      init_frame w_96 2 (Memo.from_int 0);
      set_env_slot w_96 0 arg0_27;
      set_env_slot w_96 1 arg1_27;
      w_96.state.c <- pc_to_exp (int_to_pc 86))
    80;
  add_exp
    (fun w_97 ->
      assert_env_length w_97 4;
      assert_env_length w_97 4;
      set_env_slot w_97 2 (Memo.appends [ Memo.from_constructor tag_Cons; get_env_slot w_97 3; get_env_slot w_97 2 ]);
      assert_env_length w_97 4;
      set_env_slot w_97 1 (Memo.appends [ Memo.from_constructor tag_Cons; get_env_slot w_97 1; get_env_slot w_97 2 ]);
      let arg0_28 = get_env_slot w_97 0 in
      let arg1_28 = get_env_slot w_97 1 in
      assert_env_length w_97 4;
      init_frame w_97 2 (Memo.from_int 0);
      set_env_slot w_97 0 arg0_28;
      set_env_slot w_97 1 arg1_28;
      w_97.state.c <- pc_to_exp (int_to_pc 86))
    81;
  add_exp
    (fun w_98 ->
      assert_env_length w_98 3;
      assert_env_length w_98 3;
      set_env_slot w_98 0
        (Memo.appends
           [ Memo.from_constructor tag_SMatchList; get_env_slot w_98 2; get_env_slot w_98 0; get_env_slot w_98 1 ]);
      assert_env_length w_98 3;
      set_env_slot w_98 0 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_98 0 ]);
      return_value w_98 (get_env_slot w_98 0) (pc_to_exp (int_to_pc 0)))
    82;
  add_exp
    (fun w_99 ->
      assert_env_length w_99 1;
      assert_env_length w_99 1;
      set_env_slot w_99 0
        (Memo.appends [ Memo.from_constructor tag_STypeError; get_env_slot w_99 0; Memo.from_constructor tag_VTList ]);
      assert_env_length w_99 1;
      set_env_slot w_99 0 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_99 0 ]);
      return_value w_99 (get_env_slot w_99 0) (pc_to_exp (int_to_pc 0)))
    83;
  add_exp
    (fun w_100 ->
      assert_env_length w_100 4;
      let arg0_29 = get_env_slot w_100 3 in
      let arg1_29 = get_env_slot w_100 0 in
      assert_env_length w_100 4;
      w_100.state.k <-
        Memo.appends [ Memo.from_constructor tag_cont_22; collect_env_slots w_100 [ 0; 1; 2 ]; w_100.state.k ];
      init_frame w_100 2 (Memo.from_int 0);
      set_env_slot w_100 0 arg0_29;
      set_env_slot w_100 1 arg1_29;
      w_100.state.c <- pc_to_exp (int_to_pc 86))
    84;
  add_exp
    (fun w_102 ->
      assert_env_length w_102 0;
      return_value w_102 (Memo.from_constructor tag_VUnit) (pc_to_exp (int_to_pc 0)))
    85;
  add_exp
    (fun w_103 ->
      assert_env_length w_103 2;
      assert_env_length w_103 2;
      let resolved_30 = resolve w_103 (Source.E 0) in
      let tag_16 = Word.get_value (fst resolved_30) in
      match tag_16 with
      | 7 (* tag_EInt *) ->
          let parts_31 = Memo.splits (snd resolved_30) in
          if List.length parts_31 = 1 then (
            let part0_31 = List.nth parts_31 0 in
            let edge0_49 = part0_31 in
            init_frame w_103 1 (Memo.from_int 0);
            set_env_slot w_103 0 edge0_49;
            w_103.state.c <- pc_to_exp (int_to_pc 6))
          else failwith "unreachable (86)"
      | 8 (* tag_EPlus *) ->
          let parts_32 = Memo.splits (snd resolved_30) in
          if List.length parts_32 = 2 then (
            let part0_32 = List.nth parts_32 0 in
            let part1_5 = List.nth parts_32 1 in
            let edge0_50 = get_env_slot w_103 1 in
            let edge1_31 = part0_32 in
            let edge2_10 = part1_5 in
            init_frame w_103 3 (Memo.from_int 0);
            set_env_slot w_103 0 edge0_50;
            set_env_slot w_103 2 edge1_31;
            set_env_slot w_103 1 edge2_10;
            w_103.state.c <- pc_to_exp (int_to_pc 13))
          else failwith "unreachable (86)"
      | 9 (* tag_ELt *) ->
          let parts_33 = Memo.splits (snd resolved_30) in
          if List.length parts_33 = 2 then (
            let part0_33 = List.nth parts_33 0 in
            let part1_6 = List.nth parts_33 1 in
            let edge0_51 = get_env_slot w_103 1 in
            let edge1_32 = part0_33 in
            let edge2_11 = part1_6 in
            init_frame w_103 3 (Memo.from_int 0);
            set_env_slot w_103 0 edge0_51;
            set_env_slot w_103 2 edge1_32;
            set_env_slot w_103 1 edge2_11;
            w_103.state.c <- pc_to_exp (int_to_pc 22))
          else failwith "unreachable (86)"
      | 10 (* tag_ELe *) ->
          let parts_34 = Memo.splits (snd resolved_30) in
          if List.length parts_34 = 2 then (
            let part0_34 = List.nth parts_34 0 in
            let part1_7 = List.nth parts_34 1 in
            let edge0_52 = get_env_slot w_103 1 in
            let edge1_33 = part0_34 in
            let edge2_12 = part1_7 in
            init_frame w_103 3 (Memo.from_int 0);
            set_env_slot w_103 0 edge0_52;
            set_env_slot w_103 2 edge1_33;
            set_env_slot w_103 1 edge2_12;
            w_103.state.c <- pc_to_exp (int_to_pc 31))
          else failwith "unreachable (86)"
      | 11 (* tag_EGt *) ->
          let parts_35 = Memo.splits (snd resolved_30) in
          if List.length parts_35 = 2 then (
            let part0_35 = List.nth parts_35 0 in
            let part1_8 = List.nth parts_35 1 in
            let edge0_53 = get_env_slot w_103 1 in
            let edge1_34 = part0_35 in
            let edge2_13 = part1_8 in
            init_frame w_103 3 (Memo.from_int 0);
            set_env_slot w_103 0 edge0_53;
            set_env_slot w_103 2 edge1_34;
            set_env_slot w_103 1 edge2_13;
            w_103.state.c <- pc_to_exp (int_to_pc 40))
          else failwith "unreachable (86)"
      | 12 (* tag_EGe *) ->
          let parts_36 = Memo.splits (snd resolved_30) in
          if List.length parts_36 = 2 then (
            let part0_36 = List.nth parts_36 0 in
            let part1_9 = List.nth parts_36 1 in
            let edge0_54 = get_env_slot w_103 1 in
            let edge1_35 = part0_36 in
            let edge2_14 = part1_9 in
            init_frame w_103 3 (Memo.from_int 0);
            set_env_slot w_103 0 edge0_54;
            set_env_slot w_103 2 edge1_35;
            set_env_slot w_103 1 edge2_14;
            w_103.state.c <- pc_to_exp (int_to_pc 49))
          else failwith "unreachable (86)"
      | 13 (* tag_EVar *) ->
          let parts_37 = Memo.splits (snd resolved_30) in
          if List.length parts_37 = 1 then (
            let part0_37 = List.nth parts_37 0 in
            let edge0_55 = get_env_slot w_103 1 in
            let edge1_36 = part0_37 in
            init_frame w_103 2 (Memo.from_int 0);
            set_env_slot w_103 0 edge0_55;
            set_env_slot w_103 1 edge1_36;
            w_103.state.c <- pc_to_exp (int_to_pc 52))
          else failwith "unreachable (86)"
      | 14 (* tag_EAbs *) ->
          let parts_38 = Memo.splits (snd resolved_30) in
          if List.length parts_38 = 1 then (
            let part0_38 = List.nth parts_38 0 in
            let edge0_56 = get_env_slot w_103 1 in
            let edge1_37 = part0_38 in
            init_frame w_103 2 (Memo.from_int 0);
            set_env_slot w_103 0 edge0_56;
            set_env_slot w_103 1 edge1_37;
            w_103.state.c <- pc_to_exp (int_to_pc 53))
          else failwith "unreachable (86)"
      | 16 (* tag_ELet *) ->
          let parts_39 = Memo.splits (snd resolved_30) in
          if List.length parts_39 = 2 then (
            let part0_39 = List.nth parts_39 0 in
            let part1_10 = List.nth parts_39 1 in
            let edge0_57 = get_env_slot w_103 1 in
            let edge1_38 = part0_39 in
            let edge2_15 = part1_10 in
            init_frame w_103 3 (Memo.from_int 0);
            set_env_slot w_103 1 edge0_57;
            set_env_slot w_103 2 edge1_38;
            set_env_slot w_103 0 edge2_15;
            w_103.state.c <- pc_to_exp (int_to_pc 54))
          else failwith "unreachable (86)"
      | 26 (* tag_EFix *) ->
          let parts_40 = Memo.splits (snd resolved_30) in
          if List.length parts_40 = 1 then (
            let part0_40 = List.nth parts_40 0 in
            let edge0_58 = get_env_slot w_103 1 in
            let edge1_39 = part0_40 in
            init_frame w_103 2 (Memo.from_int 0);
            set_env_slot w_103 0 edge0_58;
            set_env_slot w_103 1 edge1_39;
            w_103.state.c <- pc_to_exp (int_to_pc 55))
          else failwith "unreachable (86)"
      | 15 (* tag_EApp *) ->
          let parts_41 = Memo.splits (snd resolved_30) in
          if List.length parts_41 = 2 then (
            let part0_41 = List.nth parts_41 0 in
            let part1_11 = List.nth parts_41 1 in
            let edge0_59 = get_env_slot w_103 1 in
            let edge1_40 = part0_41 in
            let edge2_16 = part1_11 in
            init_frame w_103 3 (Memo.from_int 0);
            set_env_slot w_103 0 edge0_59;
            set_env_slot w_103 2 edge1_40;
            set_env_slot w_103 1 edge2_16;
            w_103.state.c <- pc_to_exp (int_to_pc 60))
          else failwith "unreachable (86)"
      | 27 (* tag_EHole *) ->
          let parts_42 = Memo.splits (snd resolved_30) in
          if List.length parts_42 = 1 then (
            let part0_42 = List.nth parts_42 0 in
            let edge0_60 = part0_42 in
            init_frame w_103 1 (Memo.from_int 0);
            set_env_slot w_103 0 edge0_60;
            w_103.state.c <- pc_to_exp (int_to_pc 61))
          else failwith "unreachable (86)"
      | 17 (* tag_ETrue *) ->
          init_frame w_103 0 (Memo.from_int 0);
          w_103.state.c <- pc_to_exp (int_to_pc 62)
      | 18 (* tag_EFalse *) ->
          init_frame w_103 0 (Memo.from_int 0);
          w_103.state.c <- pc_to_exp (int_to_pc 63)
      | 19 (* tag_EIf *) ->
          let parts_43 = Memo.splits (snd resolved_30) in
          if List.length parts_43 = 3 then (
            let part0_43 = List.nth parts_43 0 in
            let part1_12 = List.nth parts_43 1 in
            let part2_0 = List.nth parts_43 2 in
            let edge0_61 = get_env_slot w_103 1 in
            let edge1_41 = part0_43 in
            let edge2_17 = part1_12 in
            let edge3_8 = part2_0 in
            init_frame w_103 4 (Memo.from_int 0);
            set_env_slot w_103 0 edge0_61;
            set_env_slot w_103 3 edge1_41;
            set_env_slot w_103 1 edge2_17;
            set_env_slot w_103 2 edge3_8;
            w_103.state.c <- pc_to_exp (int_to_pc 68))
          else failwith "unreachable (86)"
      | 20 (* tag_ENil *) ->
          init_frame w_103 0 (Memo.from_int 0);
          w_103.state.c <- pc_to_exp (int_to_pc 69)
      | 21 (* tag_ECons *) ->
          let parts_44 = Memo.splits (snd resolved_30) in
          if List.length parts_44 = 2 then (
            let part0_44 = List.nth parts_44 0 in
            let part1_13 = List.nth parts_44 1 in
            let edge0_62 = get_env_slot w_103 1 in
            let edge1_42 = part0_44 in
            let edge2_18 = part1_13 in
            init_frame w_103 3 (Memo.from_int 0);
            set_env_slot w_103 0 edge0_62;
            set_env_slot w_103 2 edge1_42;
            set_env_slot w_103 1 edge2_18;
            w_103.state.c <- pc_to_exp (int_to_pc 70))
          else failwith "unreachable (86)"
      | 23 (* tag_EPair *) ->
          let parts_45 = Memo.splits (snd resolved_30) in
          if List.length parts_45 = 2 then (
            let part0_45 = List.nth parts_45 0 in
            let part1_14 = List.nth parts_45 1 in
            let edge0_63 = get_env_slot w_103 1 in
            let edge1_43 = part0_45 in
            let edge2_19 = part1_14 in
            init_frame w_103 3 (Memo.from_int 0);
            set_env_slot w_103 0 edge0_63;
            set_env_slot w_103 2 edge1_43;
            set_env_slot w_103 1 edge2_19;
            w_103.state.c <- pc_to_exp (int_to_pc 71))
          else failwith "unreachable (86)"
      | 24 (* tag_EZro *) ->
          let parts_46 = Memo.splits (snd resolved_30) in
          if List.length parts_46 = 1 then (
            let part0_46 = List.nth parts_46 0 in
            let edge0_64 = get_env_slot w_103 1 in
            let edge1_44 = part0_46 in
            init_frame w_103 2 (Memo.from_int 0);
            set_env_slot w_103 0 edge0_64;
            set_env_slot w_103 1 edge1_44;
            w_103.state.c <- pc_to_exp (int_to_pc 75))
          else failwith "unreachable (86)"
      | 25 (* tag_EFst *) ->
          let parts_47 = Memo.splits (snd resolved_30) in
          if List.length parts_47 = 1 then (
            let part0_47 = List.nth parts_47 0 in
            let edge0_65 = get_env_slot w_103 1 in
            let edge1_45 = part0_47 in
            init_frame w_103 2 (Memo.from_int 0);
            set_env_slot w_103 0 edge0_65;
            set_env_slot w_103 1 edge1_45;
            w_103.state.c <- pc_to_exp (int_to_pc 79))
          else failwith "unreachable (86)"
      | 22 (* tag_EMatchList *) ->
          let parts_48 = Memo.splits (snd resolved_30) in
          if List.length parts_48 = 3 then (
            let part0_48 = List.nth parts_48 0 in
            let part1_15 = List.nth parts_48 1 in
            let part2_1 = List.nth parts_48 2 in
            let edge0_66 = get_env_slot w_103 1 in
            let edge1_46 = part0_48 in
            let edge2_20 = part1_15 in
            let edge3_9 = part2_1 in
            init_frame w_103 4 (Memo.from_int 0);
            set_env_slot w_103 0 edge0_66;
            set_env_slot w_103 3 edge1_46;
            set_env_slot w_103 1 edge2_20;
            set_env_slot w_103 2 edge3_9;
            w_103.state.c <- pc_to_exp (int_to_pc 84))
          else failwith "unreachable (86)"
      | 28 (* tag_EUnit *) ->
          init_frame w_103 0 (Memo.from_int 0);
          w_103.state.c <- pc_to_exp (int_to_pc 85)
      | _ -> failwith "unreachable (86)")
    86;
  add_exp
    (fun w_5 ->
      assert_env_length w_5 4;
      assert_env_length w_5 4;
      let resolved_2 = resolve w_5 (Source.E 2) in
      let tag_0 = Word.get_value (fst resolved_2) in
      match tag_0 with
      | 29 (* tag_VInt *) ->
          let parts_0 = Memo.splits (snd resolved_2) in
          if List.length parts_0 = 1 then (
            let part0_0 = List.nth parts_0 0 in
            let edge0_1 = get_env_slot w_5 0 in
            let edge1_0 = part0_0 in
            init_frame w_5 2 (Memo.from_int 0);
            set_env_slot w_5 0 edge0_1;
            set_env_slot w_5 1 edge1_0;
            w_5.state.c <- pc_to_exp (int_to_pc 7))
          else
            let edge0_0 = get_env_slot w_5 2 in
            init_frame w_5 1 (Memo.from_int 0);
            set_env_slot w_5 0 edge0_0;
            w_5.state.c <- pc_to_exp (int_to_pc 9)
      | 38 (* tag_VStuck *) ->
          let parts_1 = Memo.splits (snd resolved_2) in
          if List.length parts_1 = 1 then (
            let part0_1 = List.nth parts_1 0 in
            let edge0_2 = get_env_slot w_5 1 in
            let edge1_1 = part0_1 in
            init_frame w_5 2 (Memo.from_int 0);
            set_env_slot w_5 1 edge0_2;
            set_env_slot w_5 0 edge1_1;
            w_5.state.c <- pc_to_exp (int_to_pc 8))
          else
            let edge0_0 = get_env_slot w_5 2 in
            init_frame w_5 1 (Memo.from_int 0);
            set_env_slot w_5 0 edge0_0;
            w_5.state.c <- pc_to_exp (int_to_pc 9)
      | _ ->
          let edge0_0 = get_env_slot w_5 2 in
          init_frame w_5 1 (Memo.from_int 0);
          set_env_slot w_5 0 edge0_0;
          w_5.state.c <- pc_to_exp (int_to_pc 9))
    87;
  add_exp
    (fun w_9 ->
      assert_env_length w_9 3;
      assert_env_length w_9 3;
      let resolved_3 = resolve w_9 (Source.E 2) in
      let tag_1 = Word.get_value (fst resolved_3) in
      match tag_1 with
      | 29 (* tag_VInt *) ->
          let parts_2 = Memo.splits (snd resolved_3) in
          if List.length parts_2 = 1 then (
            let part0_2 = List.nth parts_2 0 in
            let edge0_4 = get_env_slot w_9 0 in
            let edge1_2 = get_env_slot w_9 1 in
            let edge2_0 = get_env_slot w_9 2 in
            let edge3_0 = part0_2 in
            init_frame w_9 4 (Memo.from_int 0);
            set_env_slot w_9 2 edge0_4;
            set_env_slot w_9 3 edge1_2;
            set_env_slot w_9 1 edge2_0;
            set_env_slot w_9 0 edge3_0;
            w_9.state.c <- pc_to_exp (int_to_pc 10))
          else
            let edge0_3 = get_env_slot w_9 2 in
            init_frame w_9 1 (Memo.from_int 0);
            set_env_slot w_9 0 edge0_3;
            w_9.state.c <- pc_to_exp (int_to_pc 12)
      | 38 (* tag_VStuck *) ->
          let parts_3 = Memo.splits (snd resolved_3) in
          if List.length parts_3 = 1 then (
            let part0_3 = List.nth parts_3 0 in
            let edge0_5 = get_env_slot w_9 1 in
            let edge1_3 = part0_3 in
            init_frame w_9 2 (Memo.from_int 0);
            set_env_slot w_9 0 edge0_5;
            set_env_slot w_9 1 edge1_3;
            w_9.state.c <- pc_to_exp (int_to_pc 11))
          else
            let edge0_3 = get_env_slot w_9 2 in
            init_frame w_9 1 (Memo.from_int 0);
            set_env_slot w_9 0 edge0_3;
            w_9.state.c <- pc_to_exp (int_to_pc 12)
      | _ ->
          let edge0_3 = get_env_slot w_9 2 in
          init_frame w_9 1 (Memo.from_int 0);
          set_env_slot w_9 0 edge0_3;
          w_9.state.c <- pc_to_exp (int_to_pc 12))
    88;
  add_exp
    (fun w_16 ->
      assert_env_length w_16 4;
      assert_env_length w_16 4;
      let resolved_7 = resolve w_16 (Source.E 2) in
      let tag_2 = Word.get_value (fst resolved_7) in
      match tag_2 with
      | 29 (* tag_VInt *) ->
          let parts_4 = Memo.splits (snd resolved_7) in
          if List.length parts_4 = 1 then (
            let part0_4 = List.nth parts_4 0 in
            let edge0_7 = get_env_slot w_16 0 in
            let edge1_4 = part0_4 in
            init_frame w_16 2 (Memo.from_int 0);
            set_env_slot w_16 0 edge0_7;
            set_env_slot w_16 1 edge1_4;
            w_16.state.c <- pc_to_exp (int_to_pc 14))
          else
            let edge0_6 = get_env_slot w_16 2 in
            init_frame w_16 1 (Memo.from_int 0);
            set_env_slot w_16 0 edge0_6;
            w_16.state.c <- pc_to_exp (int_to_pc 18)
      | 38 (* tag_VStuck *) ->
          let parts_5 = Memo.splits (snd resolved_7) in
          if List.length parts_5 = 1 then (
            let part0_5 = List.nth parts_5 0 in
            let edge0_8 = get_env_slot w_16 1 in
            let edge1_5 = part0_5 in
            init_frame w_16 2 (Memo.from_int 0);
            set_env_slot w_16 1 edge0_8;
            set_env_slot w_16 0 edge1_5;
            w_16.state.c <- pc_to_exp (int_to_pc 17))
          else
            let edge0_6 = get_env_slot w_16 2 in
            init_frame w_16 1 (Memo.from_int 0);
            set_env_slot w_16 0 edge0_6;
            w_16.state.c <- pc_to_exp (int_to_pc 18)
      | _ ->
          let edge0_6 = get_env_slot w_16 2 in
          init_frame w_16 1 (Memo.from_int 0);
          set_env_slot w_16 0 edge0_6;
          w_16.state.c <- pc_to_exp (int_to_pc 18))
    89;
  add_exp
    (fun w_20 ->
      assert_env_length w_20 3;
      assert_env_length w_20 3;
      let resolved_8 = resolve w_20 (Source.E 2) in
      let tag_3 = Word.get_value (fst resolved_8) in
      match tag_3 with
      | 29 (* tag_VInt *) ->
          let parts_6 = Memo.splits (snd resolved_8) in
          if List.length parts_6 = 1 then (
            let part0_6 = List.nth parts_6 0 in
            let edge0_10 = get_env_slot w_20 0 in
            let edge1_6 = get_env_slot w_20 1 in
            let edge2_1 = get_env_slot w_20 2 in
            let edge3_1 = part0_6 in
            init_frame w_20 4 (Memo.from_int 0);
            set_env_slot w_20 2 edge0_10;
            set_env_slot w_20 3 edge1_6;
            set_env_slot w_20 1 edge2_1;
            set_env_slot w_20 0 edge3_1;
            w_20.state.c <- pc_to_exp (int_to_pc 19))
          else
            let edge0_9 = get_env_slot w_20 2 in
            init_frame w_20 1 (Memo.from_int 0);
            set_env_slot w_20 0 edge0_9;
            w_20.state.c <- pc_to_exp (int_to_pc 21)
      | 38 (* tag_VStuck *) ->
          let parts_7 = Memo.splits (snd resolved_8) in
          if List.length parts_7 = 1 then (
            let part0_7 = List.nth parts_7 0 in
            let edge0_11 = get_env_slot w_20 1 in
            let edge1_7 = part0_7 in
            init_frame w_20 2 (Memo.from_int 0);
            set_env_slot w_20 0 edge0_11;
            set_env_slot w_20 1 edge1_7;
            w_20.state.c <- pc_to_exp (int_to_pc 20))
          else
            let edge0_9 = get_env_slot w_20 2 in
            init_frame w_20 1 (Memo.from_int 0);
            set_env_slot w_20 0 edge0_9;
            w_20.state.c <- pc_to_exp (int_to_pc 21)
      | _ ->
          let edge0_9 = get_env_slot w_20 2 in
          init_frame w_20 1 (Memo.from_int 0);
          set_env_slot w_20 0 edge0_9;
          w_20.state.c <- pc_to_exp (int_to_pc 21))
    90;
  add_exp
    (fun w_27 ->
      assert_env_length w_27 4;
      assert_env_length w_27 4;
      let resolved_12 = resolve w_27 (Source.E 2) in
      let tag_4 = Word.get_value (fst resolved_12) in
      match tag_4 with
      | 29 (* tag_VInt *) ->
          let parts_8 = Memo.splits (snd resolved_12) in
          if List.length parts_8 = 1 then (
            let part0_8 = List.nth parts_8 0 in
            let edge0_13 = get_env_slot w_27 0 in
            let edge1_8 = part0_8 in
            init_frame w_27 2 (Memo.from_int 0);
            set_env_slot w_27 0 edge0_13;
            set_env_slot w_27 1 edge1_8;
            w_27.state.c <- pc_to_exp (int_to_pc 23))
          else
            let edge0_12 = get_env_slot w_27 2 in
            init_frame w_27 1 (Memo.from_int 0);
            set_env_slot w_27 0 edge0_12;
            w_27.state.c <- pc_to_exp (int_to_pc 27)
      | 38 (* tag_VStuck *) ->
          let parts_9 = Memo.splits (snd resolved_12) in
          if List.length parts_9 = 1 then (
            let part0_9 = List.nth parts_9 0 in
            let edge0_14 = get_env_slot w_27 1 in
            let edge1_9 = part0_9 in
            init_frame w_27 2 (Memo.from_int 0);
            set_env_slot w_27 1 edge0_14;
            set_env_slot w_27 0 edge1_9;
            w_27.state.c <- pc_to_exp (int_to_pc 26))
          else
            let edge0_12 = get_env_slot w_27 2 in
            init_frame w_27 1 (Memo.from_int 0);
            set_env_slot w_27 0 edge0_12;
            w_27.state.c <- pc_to_exp (int_to_pc 27)
      | _ ->
          let edge0_12 = get_env_slot w_27 2 in
          init_frame w_27 1 (Memo.from_int 0);
          set_env_slot w_27 0 edge0_12;
          w_27.state.c <- pc_to_exp (int_to_pc 27))
    91;
  add_exp
    (fun w_31 ->
      assert_env_length w_31 3;
      assert_env_length w_31 3;
      let resolved_13 = resolve w_31 (Source.E 2) in
      let tag_5 = Word.get_value (fst resolved_13) in
      match tag_5 with
      | 29 (* tag_VInt *) ->
          let parts_10 = Memo.splits (snd resolved_13) in
          if List.length parts_10 = 1 then (
            let part0_10 = List.nth parts_10 0 in
            let edge0_16 = get_env_slot w_31 0 in
            let edge1_10 = get_env_slot w_31 1 in
            let edge2_2 = get_env_slot w_31 2 in
            let edge3_2 = part0_10 in
            init_frame w_31 4 (Memo.from_int 0);
            set_env_slot w_31 2 edge0_16;
            set_env_slot w_31 3 edge1_10;
            set_env_slot w_31 1 edge2_2;
            set_env_slot w_31 0 edge3_2;
            w_31.state.c <- pc_to_exp (int_to_pc 28))
          else
            let edge0_15 = get_env_slot w_31 2 in
            init_frame w_31 1 (Memo.from_int 0);
            set_env_slot w_31 0 edge0_15;
            w_31.state.c <- pc_to_exp (int_to_pc 30)
      | 38 (* tag_VStuck *) ->
          let parts_11 = Memo.splits (snd resolved_13) in
          if List.length parts_11 = 1 then (
            let part0_11 = List.nth parts_11 0 in
            let edge0_17 = get_env_slot w_31 1 in
            let edge1_11 = part0_11 in
            init_frame w_31 2 (Memo.from_int 0);
            set_env_slot w_31 0 edge0_17;
            set_env_slot w_31 1 edge1_11;
            w_31.state.c <- pc_to_exp (int_to_pc 29))
          else
            let edge0_15 = get_env_slot w_31 2 in
            init_frame w_31 1 (Memo.from_int 0);
            set_env_slot w_31 0 edge0_15;
            w_31.state.c <- pc_to_exp (int_to_pc 30)
      | _ ->
          let edge0_15 = get_env_slot w_31 2 in
          init_frame w_31 1 (Memo.from_int 0);
          set_env_slot w_31 0 edge0_15;
          w_31.state.c <- pc_to_exp (int_to_pc 30))
    92;
  add_exp
    (fun w_38 ->
      assert_env_length w_38 4;
      assert_env_length w_38 4;
      let resolved_17 = resolve w_38 (Source.E 2) in
      let tag_6 = Word.get_value (fst resolved_17) in
      match tag_6 with
      | 29 (* tag_VInt *) ->
          let parts_12 = Memo.splits (snd resolved_17) in
          if List.length parts_12 = 1 then (
            let part0_12 = List.nth parts_12 0 in
            let edge0_19 = get_env_slot w_38 0 in
            let edge1_12 = part0_12 in
            init_frame w_38 2 (Memo.from_int 0);
            set_env_slot w_38 0 edge0_19;
            set_env_slot w_38 1 edge1_12;
            w_38.state.c <- pc_to_exp (int_to_pc 32))
          else
            let edge0_18 = get_env_slot w_38 2 in
            init_frame w_38 1 (Memo.from_int 0);
            set_env_slot w_38 0 edge0_18;
            w_38.state.c <- pc_to_exp (int_to_pc 36)
      | 38 (* tag_VStuck *) ->
          let parts_13 = Memo.splits (snd resolved_17) in
          if List.length parts_13 = 1 then (
            let part0_13 = List.nth parts_13 0 in
            let edge0_20 = get_env_slot w_38 1 in
            let edge1_13 = part0_13 in
            init_frame w_38 2 (Memo.from_int 0);
            set_env_slot w_38 1 edge0_20;
            set_env_slot w_38 0 edge1_13;
            w_38.state.c <- pc_to_exp (int_to_pc 35))
          else
            let edge0_18 = get_env_slot w_38 2 in
            init_frame w_38 1 (Memo.from_int 0);
            set_env_slot w_38 0 edge0_18;
            w_38.state.c <- pc_to_exp (int_to_pc 36)
      | _ ->
          let edge0_18 = get_env_slot w_38 2 in
          init_frame w_38 1 (Memo.from_int 0);
          set_env_slot w_38 0 edge0_18;
          w_38.state.c <- pc_to_exp (int_to_pc 36))
    93;
  add_exp
    (fun w_42 ->
      assert_env_length w_42 3;
      assert_env_length w_42 3;
      let resolved_18 = resolve w_42 (Source.E 2) in
      let tag_7 = Word.get_value (fst resolved_18) in
      match tag_7 with
      | 29 (* tag_VInt *) ->
          let parts_14 = Memo.splits (snd resolved_18) in
          if List.length parts_14 = 1 then (
            let part0_14 = List.nth parts_14 0 in
            let edge0_22 = get_env_slot w_42 0 in
            let edge1_14 = get_env_slot w_42 1 in
            let edge2_3 = get_env_slot w_42 2 in
            let edge3_3 = part0_14 in
            init_frame w_42 4 (Memo.from_int 0);
            set_env_slot w_42 2 edge0_22;
            set_env_slot w_42 3 edge1_14;
            set_env_slot w_42 1 edge2_3;
            set_env_slot w_42 0 edge3_3;
            w_42.state.c <- pc_to_exp (int_to_pc 37))
          else
            let edge0_21 = get_env_slot w_42 2 in
            init_frame w_42 1 (Memo.from_int 0);
            set_env_slot w_42 0 edge0_21;
            w_42.state.c <- pc_to_exp (int_to_pc 39)
      | 38 (* tag_VStuck *) ->
          let parts_15 = Memo.splits (snd resolved_18) in
          if List.length parts_15 = 1 then (
            let part0_15 = List.nth parts_15 0 in
            let edge0_23 = get_env_slot w_42 1 in
            let edge1_15 = part0_15 in
            init_frame w_42 2 (Memo.from_int 0);
            set_env_slot w_42 0 edge0_23;
            set_env_slot w_42 1 edge1_15;
            w_42.state.c <- pc_to_exp (int_to_pc 38))
          else
            let edge0_21 = get_env_slot w_42 2 in
            init_frame w_42 1 (Memo.from_int 0);
            set_env_slot w_42 0 edge0_21;
            w_42.state.c <- pc_to_exp (int_to_pc 39)
      | _ ->
          let edge0_21 = get_env_slot w_42 2 in
          init_frame w_42 1 (Memo.from_int 0);
          set_env_slot w_42 0 edge0_21;
          w_42.state.c <- pc_to_exp (int_to_pc 39))
    94;
  add_exp
    (fun w_49 ->
      assert_env_length w_49 4;
      assert_env_length w_49 4;
      let resolved_22 = resolve w_49 (Source.E 2) in
      let tag_8 = Word.get_value (fst resolved_22) in
      match tag_8 with
      | 29 (* tag_VInt *) ->
          let parts_16 = Memo.splits (snd resolved_22) in
          if List.length parts_16 = 1 then (
            let part0_16 = List.nth parts_16 0 in
            let edge0_25 = get_env_slot w_49 0 in
            let edge1_16 = part0_16 in
            init_frame w_49 2 (Memo.from_int 0);
            set_env_slot w_49 0 edge0_25;
            set_env_slot w_49 1 edge1_16;
            w_49.state.c <- pc_to_exp (int_to_pc 41))
          else
            let edge0_24 = get_env_slot w_49 2 in
            init_frame w_49 1 (Memo.from_int 0);
            set_env_slot w_49 0 edge0_24;
            w_49.state.c <- pc_to_exp (int_to_pc 45)
      | 38 (* tag_VStuck *) ->
          let parts_17 = Memo.splits (snd resolved_22) in
          if List.length parts_17 = 1 then (
            let part0_17 = List.nth parts_17 0 in
            let edge0_26 = get_env_slot w_49 1 in
            let edge1_17 = part0_17 in
            init_frame w_49 2 (Memo.from_int 0);
            set_env_slot w_49 1 edge0_26;
            set_env_slot w_49 0 edge1_17;
            w_49.state.c <- pc_to_exp (int_to_pc 44))
          else
            let edge0_24 = get_env_slot w_49 2 in
            init_frame w_49 1 (Memo.from_int 0);
            set_env_slot w_49 0 edge0_24;
            w_49.state.c <- pc_to_exp (int_to_pc 45)
      | _ ->
          let edge0_24 = get_env_slot w_49 2 in
          init_frame w_49 1 (Memo.from_int 0);
          set_env_slot w_49 0 edge0_24;
          w_49.state.c <- pc_to_exp (int_to_pc 45))
    95;
  add_exp
    (fun w_53 ->
      assert_env_length w_53 3;
      assert_env_length w_53 3;
      let resolved_23 = resolve w_53 (Source.E 2) in
      let tag_9 = Word.get_value (fst resolved_23) in
      match tag_9 with
      | 29 (* tag_VInt *) ->
          let parts_18 = Memo.splits (snd resolved_23) in
          if List.length parts_18 = 1 then (
            let part0_18 = List.nth parts_18 0 in
            let edge0_28 = get_env_slot w_53 0 in
            let edge1_18 = get_env_slot w_53 1 in
            let edge2_4 = get_env_slot w_53 2 in
            let edge3_4 = part0_18 in
            init_frame w_53 4 (Memo.from_int 0);
            set_env_slot w_53 2 edge0_28;
            set_env_slot w_53 3 edge1_18;
            set_env_slot w_53 1 edge2_4;
            set_env_slot w_53 0 edge3_4;
            w_53.state.c <- pc_to_exp (int_to_pc 46))
          else
            let edge0_27 = get_env_slot w_53 2 in
            init_frame w_53 1 (Memo.from_int 0);
            set_env_slot w_53 0 edge0_27;
            w_53.state.c <- pc_to_exp (int_to_pc 48)
      | 38 (* tag_VStuck *) ->
          let parts_19 = Memo.splits (snd resolved_23) in
          if List.length parts_19 = 1 then (
            let part0_19 = List.nth parts_19 0 in
            let edge0_29 = get_env_slot w_53 1 in
            let edge1_19 = part0_19 in
            init_frame w_53 2 (Memo.from_int 0);
            set_env_slot w_53 0 edge0_29;
            set_env_slot w_53 1 edge1_19;
            w_53.state.c <- pc_to_exp (int_to_pc 47))
          else
            let edge0_27 = get_env_slot w_53 2 in
            init_frame w_53 1 (Memo.from_int 0);
            set_env_slot w_53 0 edge0_27;
            w_53.state.c <- pc_to_exp (int_to_pc 48)
      | _ ->
          let edge0_27 = get_env_slot w_53 2 in
          init_frame w_53 1 (Memo.from_int 0);
          set_env_slot w_53 0 edge0_27;
          w_53.state.c <- pc_to_exp (int_to_pc 48))
    96;
  add_exp
    (fun w_57 ->
      assert_env_length w_57 2;
      assert_env_length w_57 2;
      let resolved_24 = resolve w_57 (Source.E 0) in
      let tag_10 = Word.get_value (fst resolved_24) in
      match tag_10 with
      | 6 (* tag_Some *) ->
          let parts_20 = Memo.splits (snd resolved_24) in
          if List.length parts_20 = 1 then (
            let part0_20 = List.nth parts_20 0 in
            let edge0_30 = part0_20 in
            init_frame w_57 1 (Memo.from_int 0);
            set_env_slot w_57 0 edge0_30;
            w_57.state.c <- pc_to_exp (int_to_pc 50))
          else failwith "unreachable (97)"
      | 5 (* tag_None *) ->
          init_frame w_57 1 (Memo.from_int 0);
          w_57.state.c <- pc_to_exp (int_to_pc 51)
      | _ -> failwith "unreachable (97)")
    97;
  add_exp
    (fun w_60 ->
      assert_env_length w_60 3;
      assert_env_length w_60 3;
      set_env_slot w_60 1 (Memo.appends [ Memo.from_constructor tag_Cons; get_env_slot w_60 2; get_env_slot w_60 1 ]);
      let arg0_12 = get_env_slot w_60 0 in
      let arg1_12 = get_env_slot w_60 1 in
      assert_env_length w_60 3;
      init_frame w_60 2 (Memo.from_int 0);
      set_env_slot w_60 0 arg0_12;
      set_env_slot w_60 1 arg1_12;
      w_60.state.c <- pc_to_exp (int_to_pc 86))
    98;
  add_exp
    (fun w_63 ->
      assert_env_length w_63 4;
      assert_env_length w_63 4;
      set_env_slot w_63 1 (Memo.appends [ Memo.from_constructor tag_Cons; get_env_slot w_63 2; get_env_slot w_63 1 ]);
      let arg0_14 = get_env_slot w_63 0 in
      let arg1_14 = get_env_slot w_63 1 in
      assert_env_length w_63 4;
      init_frame w_63 2 (Memo.from_int 0);
      set_env_slot w_63 0 arg0_14;
      set_env_slot w_63 1 arg1_14;
      w_63.state.c <- pc_to_exp (int_to_pc 86))
    99;
  add_exp
    (fun w_65 ->
      assert_env_length w_65 5;
      assert_env_length w_65 5;
      set_env_slot w_65 1 (Memo.appends [ Memo.from_constructor tag_Cons; get_env_slot w_65 2; get_env_slot w_65 1 ]);
      assert_env_length w_65 5;
      set_env_slot w_65 1 (Memo.appends [ Memo.from_constructor tag_Cons; get_env_slot w_65 3; get_env_slot w_65 1 ]);
      let arg0_16 = get_env_slot w_65 0 in
      let arg1_16 = get_env_slot w_65 1 in
      assert_env_length w_65 5;
      init_frame w_65 2 (Memo.from_int 0);
      set_env_slot w_65 0 arg0_16;
      set_env_slot w_65 1 arg1_16;
      w_65.state.c <- pc_to_exp (int_to_pc 86))
    100;
  add_exp
    (fun w_69 ->
      assert_env_length w_69 3;
      assert_env_length w_69 3;
      let resolved_25 = resolve w_69 (Source.E 2) in
      let tag_11 = Word.get_value (fst resolved_25) in
      match tag_11 with
      | 30 (* tag_VAbs *) ->
          let parts_21 = Memo.splits (snd resolved_25) in
          if List.length parts_21 = 2 then (
            let part0_21 = List.nth parts_21 0 in
            let part1_0 = List.nth parts_21 1 in
            let edge0_32 = get_env_slot w_69 0 in
            let edge1_20 = get_env_slot w_69 1 in
            let edge2_5 = part0_21 in
            let edge3_5 = part1_0 in
            init_frame w_69 4 (Memo.from_int 0);
            set_env_slot w_69 2 edge0_32;
            set_env_slot w_69 3 edge1_20;
            set_env_slot w_69 0 edge2_5;
            set_env_slot w_69 1 edge3_5;
            w_69.state.c <- pc_to_exp (int_to_pc 56))
          else
            let edge0_31 = get_env_slot w_69 2 in
            init_frame w_69 1 (Memo.from_int 0);
            set_env_slot w_69 0 edge0_31;
            w_69.state.c <- pc_to_exp (int_to_pc 59)
      | 37 (* tag_VFix *) ->
          let parts_22 = Memo.splits (snd resolved_25) in
          if List.length parts_22 = 2 then (
            let part0_22 = List.nth parts_22 0 in
            let part1_1 = List.nth parts_22 1 in
            let edge0_33 = get_env_slot w_69 0 in
            let edge1_21 = get_env_slot w_69 1 in
            let edge2_6 = get_env_slot w_69 2 in
            let edge3_6 = part0_22 in
            let edge4_0 = part1_1 in
            init_frame w_69 5 (Memo.from_int 0);
            set_env_slot w_69 3 edge0_33;
            set_env_slot w_69 4 edge1_21;
            set_env_slot w_69 2 edge2_6;
            set_env_slot w_69 0 edge3_6;
            set_env_slot w_69 1 edge4_0;
            w_69.state.c <- pc_to_exp (int_to_pc 57))
          else
            let edge0_31 = get_env_slot w_69 2 in
            init_frame w_69 1 (Memo.from_int 0);
            set_env_slot w_69 0 edge0_31;
            w_69.state.c <- pc_to_exp (int_to_pc 59)
      | 38 (* tag_VStuck *) ->
          let parts_23 = Memo.splits (snd resolved_25) in
          if List.length parts_23 = 1 then (
            let part0_23 = List.nth parts_23 0 in
            let edge0_34 = get_env_slot w_69 1 in
            let edge1_22 = part0_23 in
            init_frame w_69 2 (Memo.from_int 0);
            set_env_slot w_69 0 edge0_34;
            set_env_slot w_69 1 edge1_22;
            w_69.state.c <- pc_to_exp (int_to_pc 58))
          else
            let edge0_31 = get_env_slot w_69 2 in
            init_frame w_69 1 (Memo.from_int 0);
            set_env_slot w_69 0 edge0_31;
            w_69.state.c <- pc_to_exp (int_to_pc 59)
      | _ ->
          let edge0_31 = get_env_slot w_69 2 in
          init_frame w_69 1 (Memo.from_int 0);
          set_env_slot w_69 0 edge0_31;
          w_69.state.c <- pc_to_exp (int_to_pc 59))
    101;
  add_exp
    (fun w_78 ->
      assert_env_length w_78 4;
      assert_env_length w_78 4;
      let resolved_26 = resolve w_78 (Source.E 3) in
      let tag_12 = Word.get_value (fst resolved_26) in
      match tag_12 with
      | 32 (* tag_VTrue *) ->
          let edge0_36 = get_env_slot w_78 0 in
          let edge1_23 = get_env_slot w_78 1 in
          init_frame w_78 2 (Memo.from_int 0);
          set_env_slot w_78 0 edge0_36;
          set_env_slot w_78 1 edge1_23;
          w_78.state.c <- pc_to_exp (int_to_pc 64)
      | 33 (* tag_VFalse *) ->
          let edge0_37 = get_env_slot w_78 0 in
          let edge1_24 = get_env_slot w_78 2 in
          init_frame w_78 2 (Memo.from_int 0);
          set_env_slot w_78 0 edge0_37;
          set_env_slot w_78 1 edge1_24;
          w_78.state.c <- pc_to_exp (int_to_pc 65)
      | 38 (* tag_VStuck *) ->
          let parts_24 = Memo.splits (snd resolved_26) in
          if List.length parts_24 = 1 then (
            let part0_24 = List.nth parts_24 0 in
            let edge0_38 = get_env_slot w_78 1 in
            let edge1_25 = get_env_slot w_78 2 in
            let edge2_7 = part0_24 in
            init_frame w_78 3 (Memo.from_int 0);
            set_env_slot w_78 0 edge0_38;
            set_env_slot w_78 1 edge1_25;
            set_env_slot w_78 2 edge2_7;
            w_78.state.c <- pc_to_exp (int_to_pc 66))
          else
            let edge0_35 = get_env_slot w_78 3 in
            init_frame w_78 1 (Memo.from_int 0);
            set_env_slot w_78 0 edge0_35;
            w_78.state.c <- pc_to_exp (int_to_pc 67)
      | _ ->
          let edge0_35 = get_env_slot w_78 3 in
          init_frame w_78 1 (Memo.from_int 0);
          set_env_slot w_78 0 edge0_35;
          w_78.state.c <- pc_to_exp (int_to_pc 67))
    102;
  add_exp
    (fun w_81 ->
      assert_env_length w_81 3;
      let arg0_22 = get_env_slot w_81 1 in
      let arg1_22 = get_env_slot w_81 0 in
      assert_env_length w_81 3;
      w_81.state.k <- Memo.appends [ Memo.from_constructor tag_cont_16; collect_env_slots w_81 [ 2 ]; w_81.state.k ];
      init_frame w_81 2 (Memo.from_int 0);
      set_env_slot w_81 0 arg0_22;
      set_env_slot w_81 1 arg1_22;
      w_81.state.c <- pc_to_exp (int_to_pc 86))
    103;
  add_exp
    (fun w_82 ->
      assert_env_length w_82 3;
      assert_env_length w_82 3;
      set_env_slot w_82 0 (Memo.appends [ Memo.from_constructor tag_VCons; get_env_slot w_82 2; get_env_slot w_82 0 ]);
      return_value w_82 (get_env_slot w_82 0) (pc_to_exp (int_to_pc 0)))
    104;
  add_exp
    (fun w_84 ->
      assert_env_length w_84 3;
      let arg0_24 = get_env_slot w_84 1 in
      let arg1_24 = get_env_slot w_84 0 in
      assert_env_length w_84 3;
      w_84.state.k <- Memo.appends [ Memo.from_constructor tag_cont_18; collect_env_slots w_84 [ 2 ]; w_84.state.k ];
      init_frame w_84 2 (Memo.from_int 0);
      set_env_slot w_84 0 arg0_24;
      set_env_slot w_84 1 arg1_24;
      w_84.state.c <- pc_to_exp (int_to_pc 86))
    105;
  add_exp
    (fun w_85 ->
      assert_env_length w_85 3;
      assert_env_length w_85 3;
      set_env_slot w_85 0 (Memo.appends [ Memo.from_constructor tag_VPair; get_env_slot w_85 2; get_env_slot w_85 0 ]);
      return_value w_85 (get_env_slot w_85 0) (pc_to_exp (int_to_pc 0)))
    106;
  add_exp
    (fun w_90 ->
      assert_env_length w_90 2;
      assert_env_length w_90 2;
      let resolved_27 = resolve w_90 (Source.E 0) in
      let tag_13 = Word.get_value (fst resolved_27) in
      match tag_13 with
      | 36 (* tag_VPair *) ->
          let parts_25 = Memo.splits (snd resolved_27) in
          if List.length parts_25 = 2 then (
            let part0_25 = List.nth parts_25 0 in
            let part1_2 = List.nth parts_25 1 in
            let edge0_40 = part0_25 in
            let edge1_26 = part1_2 in
            init_frame w_90 1 (Memo.from_int 0);
            set_env_slot w_90 0 edge0_40;
            set_env_slot w_90 0 edge1_26;
            w_90.state.c <- pc_to_exp (int_to_pc 72))
          else
            let edge0_39 = get_env_slot w_90 0 in
            init_frame w_90 1 (Memo.from_int 0);
            set_env_slot w_90 0 edge0_39;
            w_90.state.c <- pc_to_exp (int_to_pc 74)
      | 38 (* tag_VStuck *) ->
          let parts_26 = Memo.splits (snd resolved_27) in
          if List.length parts_26 = 1 then (
            let part0_26 = List.nth parts_26 0 in
            let edge0_41 = part0_26 in
            init_frame w_90 1 (Memo.from_int 0);
            set_env_slot w_90 0 edge0_41;
            w_90.state.c <- pc_to_exp (int_to_pc 73))
          else
            let edge0_39 = get_env_slot w_90 0 in
            init_frame w_90 1 (Memo.from_int 0);
            set_env_slot w_90 0 edge0_39;
            w_90.state.c <- pc_to_exp (int_to_pc 74)
      | _ ->
          let edge0_39 = get_env_slot w_90 0 in
          init_frame w_90 1 (Memo.from_int 0);
          set_env_slot w_90 0 edge0_39;
          w_90.state.c <- pc_to_exp (int_to_pc 74))
    107;
  add_exp
    (fun w_95 ->
      assert_env_length w_95 2;
      assert_env_length w_95 2;
      let resolved_28 = resolve w_95 (Source.E 0) in
      let tag_14 = Word.get_value (fst resolved_28) in
      match tag_14 with
      | 36 (* tag_VPair *) ->
          let parts_27 = Memo.splits (snd resolved_28) in
          if List.length parts_27 = 2 then (
            let part0_27 = List.nth parts_27 0 in
            let part1_3 = List.nth parts_27 1 in
            let edge0_43 = part0_27 in
            let edge1_27 = part1_3 in
            init_frame w_95 1 (Memo.from_int 0);
            set_env_slot w_95 0 edge0_43;
            set_env_slot w_95 0 edge1_27;
            w_95.state.c <- pc_to_exp (int_to_pc 76))
          else
            let edge0_42 = get_env_slot w_95 0 in
            init_frame w_95 1 (Memo.from_int 0);
            set_env_slot w_95 0 edge0_42;
            w_95.state.c <- pc_to_exp (int_to_pc 78)
      | 38 (* tag_VStuck *) ->
          let parts_28 = Memo.splits (snd resolved_28) in
          if List.length parts_28 = 1 then (
            let part0_28 = List.nth parts_28 0 in
            let edge0_44 = part0_28 in
            init_frame w_95 1 (Memo.from_int 0);
            set_env_slot w_95 0 edge0_44;
            w_95.state.c <- pc_to_exp (int_to_pc 77))
          else
            let edge0_42 = get_env_slot w_95 0 in
            init_frame w_95 1 (Memo.from_int 0);
            set_env_slot w_95 0 edge0_42;
            w_95.state.c <- pc_to_exp (int_to_pc 78)
      | _ ->
          let edge0_42 = get_env_slot w_95 0 in
          init_frame w_95 1 (Memo.from_int 0);
          set_env_slot w_95 0 edge0_42;
          w_95.state.c <- pc_to_exp (int_to_pc 78))
    108;
  add_exp
    (fun w_101 ->
      assert_env_length w_101 4;
      assert_env_length w_101 4;
      let resolved_29 = resolve w_101 (Source.E 3) in
      let tag_15 = Word.get_value (fst resolved_29) in
      match tag_15 with
      | 34 (* tag_VNil *) ->
          let edge0_46 = get_env_slot w_101 0 in
          let edge1_28 = get_env_slot w_101 1 in
          init_frame w_101 2 (Memo.from_int 0);
          set_env_slot w_101 0 edge0_46;
          set_env_slot w_101 1 edge1_28;
          w_101.state.c <- pc_to_exp (int_to_pc 80)
      | 35 (* tag_VCons *) ->
          let parts_29 = Memo.splits (snd resolved_29) in
          if List.length parts_29 = 2 then (
            let part0_29 = List.nth parts_29 0 in
            let part1_4 = List.nth parts_29 1 in
            let edge0_47 = get_env_slot w_101 0 in
            let edge1_29 = get_env_slot w_101 2 in
            let edge2_8 = part0_29 in
            let edge3_7 = part1_4 in
            init_frame w_101 4 (Memo.from_int 0);
            set_env_slot w_101 2 edge0_47;
            set_env_slot w_101 0 edge1_29;
            set_env_slot w_101 3 edge2_8;
            set_env_slot w_101 1 edge3_7;
            w_101.state.c <- pc_to_exp (int_to_pc 81))
          else
            let edge0_45 = get_env_slot w_101 3 in
            init_frame w_101 1 (Memo.from_int 0);
            set_env_slot w_101 0 edge0_45;
            w_101.state.c <- pc_to_exp (int_to_pc 83)
      | 38 (* tag_VStuck *) ->
          let parts_30 = Memo.splits (snd resolved_29) in
          if List.length parts_30 = 1 then (
            let part0_30 = List.nth parts_30 0 in
            let edge0_48 = get_env_slot w_101 1 in
            let edge1_30 = get_env_slot w_101 2 in
            let edge2_9 = part0_30 in
            init_frame w_101 3 (Memo.from_int 0);
            set_env_slot w_101 0 edge0_48;
            set_env_slot w_101 1 edge1_30;
            set_env_slot w_101 2 edge2_9;
            w_101.state.c <- pc_to_exp (int_to_pc 82))
          else
            let edge0_45 = get_env_slot w_101 3 in
            init_frame w_101 1 (Memo.from_int 0);
            set_env_slot w_101 0 edge0_45;
            w_101.state.c <- pc_to_exp (int_to_pc 83)
      | _ ->
          let edge0_45 = get_env_slot w_101 3 in
          init_frame w_101 1 (Memo.from_int 0);
          set_env_slot w_101 0 edge0_45;
          w_101.state.c <- pc_to_exp (int_to_pc 83))
    109;
  Words.set_constructor_degree 0 1;
  Words.set_constructor_degree 1 1;
  Words.set_constructor_degree 2 0;
  Words.set_constructor_degree 3 1;
  Words.set_constructor_degree 4 (-1);
  Words.set_constructor_degree 5 1;
  Words.set_constructor_degree 6 0;
  Words.set_constructor_degree 7 0;
  Words.set_constructor_degree 8 (-1);
  Words.set_constructor_degree 9 (-1);
  Words.set_constructor_degree 10 (-1);
  Words.set_constructor_degree 11 (-1);
  Words.set_constructor_degree 12 (-1);
  Words.set_constructor_degree 13 0;
  Words.set_constructor_degree 14 0;
  Words.set_constructor_degree 15 (-1);
  Words.set_constructor_degree 16 (-1);
  Words.set_constructor_degree 17 1;
  Words.set_constructor_degree 18 1;
  Words.set_constructor_degree 19 (-2);
  Words.set_constructor_degree 20 1;
  Words.set_constructor_degree 21 (-1);
  Words.set_constructor_degree 22 (-2);
  Words.set_constructor_degree 23 (-1);
  Words.set_constructor_degree 24 0;
  Words.set_constructor_degree 25 0;
  Words.set_constructor_degree 26 0;
  Words.set_constructor_degree 27 0;
  Words.set_constructor_degree 28 1;
  Words.set_constructor_degree 29 0;
  Words.set_constructor_degree 30 (-1);
  Words.set_constructor_degree 31 1;
  Words.set_constructor_degree 32 1;
  Words.set_constructor_degree 33 1;
  Words.set_constructor_degree 34 1;
  Words.set_constructor_degree 35 (-1);
  Words.set_constructor_degree 36 (-1);
  Words.set_constructor_degree 37 (-1);
  Words.set_constructor_degree 38 0;
  Words.set_constructor_degree 39 1;
  Words.set_constructor_degree 40 1;
  Words.set_constructor_degree 41 1;
  Words.set_constructor_degree 42 1;
  Words.set_constructor_degree 43 1;
  Words.set_constructor_degree 44 0;
  Words.set_constructor_degree 45 (-1);
  Words.set_constructor_degree 46 1;
  Words.set_constructor_degree 47 (-1);
  Words.set_constructor_degree 48 (-1);
  Words.set_constructor_degree 49 (-1);
  Words.set_constructor_degree 50 (-1);
  Words.set_constructor_degree 51 (-1);
  Words.set_constructor_degree 52 (-2);
  Words.set_constructor_degree 53 (-2);
  Words.set_constructor_degree 54 0;
  Words.set_constructor_degree 55 0;
  Words.set_constructor_degree 56 (-2);
  Words.set_constructor_degree 57 (-2);
  Words.set_constructor_degree 58 (-2);
  Words.set_constructor_degree 59 (-2);
  Words.set_constructor_degree 60 (-2);
  Words.set_constructor_degree 61 (-2);
  Words.set_constructor_degree 62 (-2);
  Words.set_constructor_degree 63 (-2);
  Words.set_constructor_degree 64 (-2);
  Words.set_constructor_degree 65 (-2);
  Words.set_constructor_degree 66 0;
  Words.set_constructor_degree 67 (-2);
  Words.set_constructor_degree 68 (-2);
  Words.set_constructor_degree 69 (-3);
  Words.set_constructor_degree 70 (-2);
  Words.set_constructor_degree 71 (-3);
  Words.set_constructor_degree 72 (-1);
  Words.set_constructor_degree 73 (-2);
  Words.set_constructor_degree 74 (-1);
  Words.set_constructor_degree 75 (-2);
  Words.set_constructor_degree 76 0;
  Words.set_constructor_degree 77 0;
  Words.set_constructor_degree 78 (-3)
