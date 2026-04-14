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
  exec_cek (pc_to_exp (int_to_pc 1)) initial_env (Memo.from_constructor tag_cont_done) memo

let eval memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 2 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  Dynarray.set initial_env 1 x1;
  exec_cek (pc_to_exp (int_to_pc 7)) initial_env (Memo.from_constructor tag_cont_done) memo

let populate_state () =
  Memo.reset ();
  Words.reset ();
  add_exp
    (fun w_30 ->
      assert_env_length w_30 1;
      let hd_0, tl_0 = resolve w_30 K in
      match Word.get_value hd_0 with
      | 56 (* tag_cont_0 *) ->
          let ret_0 = get_env_slot w_30 0 in
          assert_env_length w_30 1;
          w_30.state.k <- get_next_cont tl_0;
          init_frame w_30 4 (Memo.from_int 0);
          restore_env_slots w_30 [ 0; 1; 2 ] tl_0;
          set_env_slot w_30 3 ret_0;
          w_30.state.c <- pc_to_exp (int_to_pc 9)
      | 57 (* tag_cont_1 *) ->
          let ret_1 = get_env_slot w_30 0 in
          assert_env_length w_30 1;
          w_30.state.k <- get_next_cont tl_0;
          init_frame w_30 6 (Memo.from_int 0);
          restore_env_slots w_30 [ 0; 1; 2; 3; 4 ] tl_0;
          set_env_slot w_30 5 ret_1;
          w_30.state.c <- pc_to_exp (int_to_pc 8)
      | 58 (* tag_cont_2 *) ->
          let ret_2 = get_env_slot w_30 0 in
          assert_env_length w_30 1;
          w_30.state.k <- get_next_cont tl_0;
          init_frame w_30 4 (Memo.from_int 0);
          restore_env_slots w_30 [ 0; 1; 2 ] tl_0;
          set_env_slot w_30 3 ret_2;
          w_30.state.c <- pc_to_exp (int_to_pc 11)
      | 59 (* tag_cont_3 *) ->
          let ret_3 = get_env_slot w_30 0 in
          assert_env_length w_30 1;
          w_30.state.k <- get_next_cont tl_0;
          init_frame w_30 6 (Memo.from_int 0);
          restore_env_slots w_30 [ 0; 1; 2; 3; 4 ] tl_0;
          set_env_slot w_30 5 ret_3;
          w_30.state.c <- pc_to_exp (int_to_pc 10)
      | 60 (* tag_cont_4 *) ->
          let ret_4 = get_env_slot w_30 0 in
          assert_env_length w_30 1;
          w_30.state.k <- get_next_cont tl_0;
          init_frame w_30 4 (Memo.from_int 0);
          restore_env_slots w_30 [ 0; 1; 2 ] tl_0;
          set_env_slot w_30 3 ret_4;
          w_30.state.c <- pc_to_exp (int_to_pc 13)
      | 61 (* tag_cont_5 *) ->
          let ret_5 = get_env_slot w_30 0 in
          assert_env_length w_30 1;
          w_30.state.k <- get_next_cont tl_0;
          init_frame w_30 6 (Memo.from_int 0);
          restore_env_slots w_30 [ 0; 1; 2; 3; 4 ] tl_0;
          set_env_slot w_30 5 ret_5;
          w_30.state.c <- pc_to_exp (int_to_pc 12)
      | 62 (* tag_cont_6 *) ->
          let ret_6 = get_env_slot w_30 0 in
          assert_env_length w_30 1;
          w_30.state.k <- get_next_cont tl_0;
          init_frame w_30 4 (Memo.from_int 0);
          restore_env_slots w_30 [ 0; 1; 2 ] tl_0;
          set_env_slot w_30 3 ret_6;
          w_30.state.c <- pc_to_exp (int_to_pc 15)
      | 63 (* tag_cont_7 *) ->
          let ret_7 = get_env_slot w_30 0 in
          assert_env_length w_30 1;
          w_30.state.k <- get_next_cont tl_0;
          init_frame w_30 6 (Memo.from_int 0);
          restore_env_slots w_30 [ 0; 1; 2; 3; 4 ] tl_0;
          set_env_slot w_30 5 ret_7;
          w_30.state.c <- pc_to_exp (int_to_pc 14)
      | 64 (* tag_cont_8 *) ->
          let ret_8 = get_env_slot w_30 0 in
          assert_env_length w_30 1;
          w_30.state.k <- get_next_cont tl_0;
          init_frame w_30 4 (Memo.from_int 0);
          restore_env_slots w_30 [ 0; 1; 2 ] tl_0;
          set_env_slot w_30 3 ret_8;
          w_30.state.c <- pc_to_exp (int_to_pc 17)
      | 65 (* tag_cont_9 *) ->
          let ret_9 = get_env_slot w_30 0 in
          assert_env_length w_30 1;
          w_30.state.k <- get_next_cont tl_0;
          init_frame w_30 6 (Memo.from_int 0);
          restore_env_slots w_30 [ 0; 1; 2; 3; 4 ] tl_0;
          set_env_slot w_30 5 ret_9;
          w_30.state.c <- pc_to_exp (int_to_pc 16)
      | 66 (* tag_cont_10 *) ->
          let ret_10 = get_env_slot w_30 0 in
          assert_env_length w_30 1;
          w_30.state.k <- get_next_cont tl_0;
          init_frame w_30 2 (Memo.from_int 0);
          restore_env_slots w_30 [ 0 ] tl_0;
          set_env_slot w_30 1 ret_10;
          w_30.state.c <- pc_to_exp (int_to_pc 18)
      | 67 (* tag_cont_11 *) ->
          let ret_11 = get_env_slot w_30 0 in
          assert_env_length w_30 1;
          w_30.state.k <- get_next_cont tl_0;
          init_frame w_30 3 (Memo.from_int 0);
          restore_env_slots w_30 [ 0; 1 ] tl_0;
          set_env_slot w_30 2 ret_11;
          w_30.state.c <- pc_to_exp (int_to_pc 19)
      | 68 (* tag_cont_12 *) ->
          let ret_12 = get_env_slot w_30 0 in
          assert_env_length w_30 1;
          w_30.state.k <- get_next_cont tl_0;
          init_frame w_30 3 (Memo.from_int 0);
          restore_env_slots w_30 [ 0; 1 ] tl_0;
          set_env_slot w_30 2 ret_12;
          w_30.state.c <- pc_to_exp (int_to_pc 21)
      | 69 (* tag_cont_13 *) ->
          let ret_13 = get_env_slot w_30 0 in
          assert_env_length w_30 1;
          w_30.state.k <- get_next_cont tl_0;
          init_frame w_30 4 (Memo.from_int 0);
          restore_env_slots w_30 [ 0; 1; 2 ] tl_0;
          set_env_slot w_30 3 ret_13;
          w_30.state.c <- pc_to_exp (int_to_pc 22)
      | 70 (* tag_cont_14 *) ->
          let ret_14 = get_env_slot w_30 0 in
          assert_env_length w_30 1;
          w_30.state.k <- get_next_cont tl_0;
          init_frame w_30 8 (Memo.from_int 0);
          restore_env_slots w_30 [ 0; 1; 2; 3; 4; 5; 6 ] tl_0;
          set_env_slot w_30 7 ret_14;
          w_30.state.c <- pc_to_exp (int_to_pc 20)
      | 71 (* tag_cont_15 *) ->
          let ret_15 = get_env_slot w_30 0 in
          assert_env_length w_30 1;
          w_30.state.k <- get_next_cont tl_0;
          init_frame w_30 6 (Memo.from_int 0);
          restore_env_slots w_30 [ 0; 1; 2; 3; 4 ] tl_0;
          set_env_slot w_30 5 ret_15;
          w_30.state.c <- pc_to_exp (int_to_pc 23)
      | 72 (* tag_cont_16 *) ->
          let ret_16 = get_env_slot w_30 0 in
          assert_env_length w_30 1;
          w_30.state.k <- get_next_cont tl_0;
          init_frame w_30 2 (Memo.from_int 0);
          restore_env_slots w_30 [ 0 ] tl_0;
          set_env_slot w_30 1 ret_16;
          w_30.state.c <- pc_to_exp (int_to_pc 25)
      | 73 (* tag_cont_17 *) ->
          let ret_17 = get_env_slot w_30 0 in
          assert_env_length w_30 1;
          w_30.state.k <- get_next_cont tl_0;
          init_frame w_30 3 (Memo.from_int 0);
          restore_env_slots w_30 [ 0; 1 ] tl_0;
          set_env_slot w_30 2 ret_17;
          w_30.state.c <- pc_to_exp (int_to_pc 24)
      | 74 (* tag_cont_18 *) ->
          let ret_18 = get_env_slot w_30 0 in
          assert_env_length w_30 1;
          w_30.state.k <- get_next_cont tl_0;
          init_frame w_30 2 (Memo.from_int 0);
          restore_env_slots w_30 [ 0 ] tl_0;
          set_env_slot w_30 1 ret_18;
          w_30.state.c <- pc_to_exp (int_to_pc 27)
      | 75 (* tag_cont_19 *) ->
          let ret_19 = get_env_slot w_30 0 in
          assert_env_length w_30 1;
          w_30.state.k <- get_next_cont tl_0;
          init_frame w_30 3 (Memo.from_int 0);
          restore_env_slots w_30 [ 0; 1 ] tl_0;
          set_env_slot w_30 2 ret_19;
          w_30.state.c <- pc_to_exp (int_to_pc 26)
      | 76 (* tag_cont_20 *) ->
          let ret_20 = get_env_slot w_30 0 in
          assert_env_length w_30 1;
          w_30.state.k <- get_next_cont tl_0;
          init_frame w_30 4 (Memo.from_int 0);
          restore_env_slots w_30 [ 0; 1; 2 ] tl_0;
          set_env_slot w_30 3 ret_20;
          w_30.state.c <- pc_to_exp (int_to_pc 28)
      | 77 (* tag_cont_21 *) ->
          let ret_21 = get_env_slot w_30 0 in
          assert_env_length w_30 1;
          w_30.state.k <- get_next_cont tl_0;
          init_frame w_30 4 (Memo.from_int 0);
          restore_env_slots w_30 [ 0; 1; 2 ] tl_0;
          set_env_slot w_30 3 ret_21;
          w_30.state.c <- pc_to_exp (int_to_pc 29)
      | 78 (* tag_cont_22 *) ->
          let ret_22 = get_env_slot w_30 0 in
          assert_env_length w_30 1;
          w_30.state.k <- get_next_cont tl_0;
          init_frame w_30 8 (Memo.from_int 0);
          restore_env_slots w_30 [ 0; 1; 2; 3; 4; 5; 6 ] tl_0;
          set_env_slot w_30 7 ret_22;
          w_30.state.c <- pc_to_exp (int_to_pc 30)
      | _ -> failwith "unreachable (0)")
    0;
  add_exp
    (fun w_29 ->
      assert_env_length w_29 2;
      resize_frame w_29 7 (Memo.from_int 0);
      assert_env_length w_29 7;
      let resolved_31 = resolve w_29 (Source.E 0) in
      let tag_17 = Word.get_value (fst resolved_31) in
      match tag_17 with
      | 4 (* tag_Cons *) ->
          let parts_49 = Memo.splits (snd resolved_31) in
          if List.length parts_49 = 2 then (
            let part0_49 = List.nth parts_49 0 in
            let part1_16 = List.nth parts_49 1 in
            set_env_slot w_29 2 part0_49;
            set_env_slot w_29 3 part1_16;
            assert_env_length w_29 7;
            let resolved_32 = resolve w_29 (Source.E 1) in
            let tag_18 = Word.get_value (fst resolved_32) in
            match tag_18 with
            | 1 (* tag_Z *) ->
                assert_env_length w_29 7;
                set_env_slot w_29 5 (Memo.appends [ Memo.from_constructor tag_Some; get_env_slot w_29 2 ]);
                trim_resolved w_29 2;
                return_value w_29 (get_env_slot w_29 5) (pc_to_exp (int_to_pc 0))
            | 2 (* tag_S *) ->
                let parts_50 = Memo.splits (snd resolved_32) in
                if List.length parts_50 = 1 then (
                  let part0_50 = List.nth parts_50 0 in
                  set_env_slot w_29 4 part0_50;
                  let arg0_30 = get_env_slot w_29 3 in
                  let arg1_30 = get_env_slot w_29 4 in
                  assert_env_length w_29 7;
                  trim_resolved w_29 2;
                  init_frame w_29 2 (Memo.from_int 0);
                  set_env_slot w_29 0 arg0_30;
                  set_env_slot w_29 1 arg1_30;
                  w_29.state.c <- pc_to_exp (int_to_pc 1))
                else failwith "unreachable (1)"
            | _ -> failwith "unreachable (1)")
          else (
            trim_resolved w_29 2;
            return_value w_29 (Memo.from_constructor tag_None) (pc_to_exp (int_to_pc 0)))
      | _ ->
          trim_resolved w_29 2;
          return_value w_29 (Memo.from_constructor tag_None) (pc_to_exp (int_to_pc 0)))
    1;
  add_exp
    (fun w_0 ->
      assert_env_length w_0 2;
      assert_env_length w_0 2;
      let resolved_0 = resolve w_0 (Source.E 1) in
      let resolved_1 = resolve w_0 (Source.E 0) in
      set_env_slot w_0 0 (Memo.from_int (Word.get_value (fst resolved_0) + Word.get_value (fst resolved_1)));
      assert_env_length w_0 2;
      set_env_slot w_0 0 (Memo.appends [ Memo.from_constructor tag_VInt; get_env_slot w_0 0 ]);
      return_value w_0 (get_env_slot w_0 0) (pc_to_exp (int_to_pc 0)))
    2;
  add_exp
    (fun w_1 ->
      assert_env_length w_1 2;
      assert_env_length w_1 2;
      let resolved_2 = resolve w_1 (Source.E 1) in
      let resolved_3 = resolve w_1 (Source.E 0) in
      set_env_slot w_1 0
        (Memo.from_int (if Word.get_value (fst resolved_2) < Word.get_value (fst resolved_3) then 1 else 0));
      let resolved_4 = resolve w_1 (Source.E 0) in
      if Word.get_value (fst resolved_4) <> 0 then
        return_value w_1 (Memo.from_constructor tag_VTrue) (pc_to_exp (int_to_pc 0))
      else return_value w_1 (Memo.from_constructor tag_VFalse) (pc_to_exp (int_to_pc 0)))
    3;
  add_exp
    (fun w_2 ->
      assert_env_length w_2 2;
      assert_env_length w_2 2;
      let resolved_5 = resolve w_2 (Source.E 1) in
      let resolved_6 = resolve w_2 (Source.E 0) in
      set_env_slot w_2 0
        (Memo.from_int (if Word.get_value (fst resolved_5) <= Word.get_value (fst resolved_6) then 1 else 0));
      let resolved_7 = resolve w_2 (Source.E 0) in
      if Word.get_value (fst resolved_7) <> 0 then
        return_value w_2 (Memo.from_constructor tag_VTrue) (pc_to_exp (int_to_pc 0))
      else return_value w_2 (Memo.from_constructor tag_VFalse) (pc_to_exp (int_to_pc 0)))
    4;
  add_exp
    (fun w_3 ->
      assert_env_length w_3 2;
      assert_env_length w_3 2;
      let resolved_8 = resolve w_3 (Source.E 1) in
      let resolved_9 = resolve w_3 (Source.E 0) in
      set_env_slot w_3 0
        (Memo.from_int (if Word.get_value (fst resolved_8) > Word.get_value (fst resolved_9) then 1 else 0));
      let resolved_10 = resolve w_3 (Source.E 0) in
      if Word.get_value (fst resolved_10) <> 0 then
        return_value w_3 (Memo.from_constructor tag_VTrue) (pc_to_exp (int_to_pc 0))
      else return_value w_3 (Memo.from_constructor tag_VFalse) (pc_to_exp (int_to_pc 0)))
    5;
  add_exp
    (fun w_4 ->
      assert_env_length w_4 2;
      assert_env_length w_4 2;
      let resolved_11 = resolve w_4 (Source.E 1) in
      let resolved_12 = resolve w_4 (Source.E 0) in
      set_env_slot w_4 0
        (Memo.from_int (if Word.get_value (fst resolved_11) >= Word.get_value (fst resolved_12) then 1 else 0));
      let resolved_13 = resolve w_4 (Source.E 0) in
      if Word.get_value (fst resolved_13) <> 0 then
        return_value w_4 (Memo.from_constructor tag_VTrue) (pc_to_exp (int_to_pc 0))
      else return_value w_4 (Memo.from_constructor tag_VFalse) (pc_to_exp (int_to_pc 0)))
    6;
  add_exp
    (fun w_5 ->
      assert_env_length w_5 2;
      resize_frame w_5 172 (Memo.from_int 0);
      assert_env_length w_5 172;
      let resolved_14 = resolve w_5 (Source.E 0) in
      let tag_0 = Word.get_value (fst resolved_14) in
      match tag_0 with
      | 7 (* tag_EInt *) ->
          let parts_0 = Memo.splits (snd resolved_14) in
          if List.length parts_0 = 1 then (
            let part0_0 = List.nth parts_0 0 in
            set_env_slot w_5 2 part0_0;
            assert_env_length w_5 172;
            set_env_slot w_5 68 (Memo.appends [ Memo.from_constructor tag_VInt; get_env_slot w_5 2 ]);
            trim_resolved w_5 2;
            return_value w_5 (get_env_slot w_5 68) (pc_to_exp (int_to_pc 0)))
          else failwith "unreachable (7)"
      | 8 (* tag_EPlus *) ->
          let parts_1 = Memo.splits (snd resolved_14) in
          if List.length parts_1 = 2 then (
            let part0_1 = List.nth parts_1 0 in
            let part1_0 = List.nth parts_1 1 in
            set_env_slot w_5 3 part0_1;
            set_env_slot w_5 4 part1_0;
            let arg0_0 = get_env_slot w_5 3 in
            let arg1_0 = get_env_slot w_5 1 in
            assert_env_length w_5 172;
            w_5.state.k <-
              Memo.appends [ Memo.from_constructor tag_cont_1; collect_env_slots w_5 [ 1; 4; 5; 6; 7 ]; w_5.state.k ];
            trim_resolved w_5 2;
            init_frame w_5 2 (Memo.from_int 0);
            set_env_slot w_5 0 arg0_0;
            set_env_slot w_5 1 arg1_0;
            w_5.state.c <- pc_to_exp (int_to_pc 7))
          else failwith "unreachable (7)"
      | 9 (* tag_ELt *) ->
          let parts_6 = Memo.splits (snd resolved_14) in
          if List.length parts_6 = 2 then (
            let part0_6 = List.nth parts_6 0 in
            let part1_1 = List.nth parts_6 1 in
            set_env_slot w_5 8 part0_6;
            set_env_slot w_5 9 part1_1;
            let arg0_2 = get_env_slot w_5 8 in
            let arg1_2 = get_env_slot w_5 1 in
            assert_env_length w_5 172;
            w_5.state.k <-
              Memo.appends [ Memo.from_constructor tag_cont_3; collect_env_slots w_5 [ 1; 9; 10; 11; 12 ]; w_5.state.k ];
            trim_resolved w_5 2;
            init_frame w_5 2 (Memo.from_int 0);
            set_env_slot w_5 0 arg0_2;
            set_env_slot w_5 1 arg1_2;
            w_5.state.c <- pc_to_exp (int_to_pc 7))
          else failwith "unreachable (7)"
      | 10 (* tag_ELe *) ->
          let parts_11 = Memo.splits (snd resolved_14) in
          if List.length parts_11 = 2 then (
            let part0_11 = List.nth parts_11 0 in
            let part1_2 = List.nth parts_11 1 in
            set_env_slot w_5 13 part0_11;
            set_env_slot w_5 14 part1_2;
            let arg0_4 = get_env_slot w_5 13 in
            let arg1_4 = get_env_slot w_5 1 in
            assert_env_length w_5 172;
            w_5.state.k <-
              Memo.appends
                [ Memo.from_constructor tag_cont_5; collect_env_slots w_5 [ 1; 14; 15; 16; 17 ]; w_5.state.k ];
            trim_resolved w_5 2;
            init_frame w_5 2 (Memo.from_int 0);
            set_env_slot w_5 0 arg0_4;
            set_env_slot w_5 1 arg1_4;
            w_5.state.c <- pc_to_exp (int_to_pc 7))
          else failwith "unreachable (7)"
      | 11 (* tag_EGt *) ->
          let parts_16 = Memo.splits (snd resolved_14) in
          if List.length parts_16 = 2 then (
            let part0_16 = List.nth parts_16 0 in
            let part1_3 = List.nth parts_16 1 in
            set_env_slot w_5 18 part0_16;
            set_env_slot w_5 19 part1_3;
            let arg0_6 = get_env_slot w_5 18 in
            let arg1_6 = get_env_slot w_5 1 in
            assert_env_length w_5 172;
            w_5.state.k <-
              Memo.appends
                [ Memo.from_constructor tag_cont_7; collect_env_slots w_5 [ 1; 19; 20; 21; 22 ]; w_5.state.k ];
            trim_resolved w_5 2;
            init_frame w_5 2 (Memo.from_int 0);
            set_env_slot w_5 0 arg0_6;
            set_env_slot w_5 1 arg1_6;
            w_5.state.c <- pc_to_exp (int_to_pc 7))
          else failwith "unreachable (7)"
      | 12 (* tag_EGe *) ->
          let parts_21 = Memo.splits (snd resolved_14) in
          if List.length parts_21 = 2 then (
            let part0_21 = List.nth parts_21 0 in
            let part1_4 = List.nth parts_21 1 in
            set_env_slot w_5 23 part0_21;
            set_env_slot w_5 24 part1_4;
            let arg0_8 = get_env_slot w_5 23 in
            let arg1_8 = get_env_slot w_5 1 in
            assert_env_length w_5 172;
            w_5.state.k <-
              Memo.appends
                [ Memo.from_constructor tag_cont_9; collect_env_slots w_5 [ 1; 24; 25; 26; 27 ]; w_5.state.k ];
            trim_resolved w_5 2;
            init_frame w_5 2 (Memo.from_int 0);
            set_env_slot w_5 0 arg0_8;
            set_env_slot w_5 1 arg1_8;
            w_5.state.c <- pc_to_exp (int_to_pc 7))
          else failwith "unreachable (7)"
      | 13 (* tag_EVar *) ->
          let parts_26 = Memo.splits (snd resolved_14) in
          if List.length parts_26 = 1 then (
            let part0_26 = List.nth parts_26 0 in
            set_env_slot w_5 28 part0_26;
            let arg0_10 = get_env_slot w_5 1 in
            let arg1_10 = get_env_slot w_5 28 in
            assert_env_length w_5 172;
            w_5.state.k <- Memo.appends [ Memo.from_constructor tag_cont_10; collect_env_slots w_5 [ 29 ]; w_5.state.k ];
            trim_resolved w_5 2;
            init_frame w_5 2 (Memo.from_int 0);
            set_env_slot w_5 0 arg0_10;
            set_env_slot w_5 1 arg1_10;
            w_5.state.c <- pc_to_exp (int_to_pc 1))
          else failwith "unreachable (7)"
      | 14 (* tag_EAbs *) ->
          let parts_28 = Memo.splits (snd resolved_14) in
          if List.length parts_28 = 1 then (
            let part0_28 = List.nth parts_28 0 in
            set_env_slot w_5 30 part0_28;
            assert_env_length w_5 172;
            set_env_slot w_5 121
              (Memo.appends [ Memo.from_constructor tag_VAbs; get_env_slot w_5 30; get_env_slot w_5 1 ]);
            trim_resolved w_5 2;
            return_value w_5 (get_env_slot w_5 121) (pc_to_exp (int_to_pc 0)))
          else failwith "unreachable (7)"
      | 16 (* tag_ELet *) ->
          let parts_29 = Memo.splits (snd resolved_14) in
          if List.length parts_29 = 2 then (
            let part0_29 = List.nth parts_29 0 in
            let part1_5 = List.nth parts_29 1 in
            set_env_slot w_5 31 part0_29;
            set_env_slot w_5 32 part1_5;
            let arg0_11 = get_env_slot w_5 31 in
            let arg1_11 = get_env_slot w_5 1 in
            assert_env_length w_5 172;
            w_5.state.k <-
              Memo.appends [ Memo.from_constructor tag_cont_11; collect_env_slots w_5 [ 1; 32 ]; w_5.state.k ];
            trim_resolved w_5 2;
            init_frame w_5 2 (Memo.from_int 0);
            set_env_slot w_5 0 arg0_11;
            set_env_slot w_5 1 arg1_11;
            w_5.state.c <- pc_to_exp (int_to_pc 7))
          else failwith "unreachable (7)"
      | 26 (* tag_EFix *) ->
          let parts_30 = Memo.splits (snd resolved_14) in
          if List.length parts_30 = 1 then (
            let part0_30 = List.nth parts_30 0 in
            set_env_slot w_5 33 part0_30;
            assert_env_length w_5 172;
            set_env_slot w_5 125
              (Memo.appends [ Memo.from_constructor tag_VFix; get_env_slot w_5 33; get_env_slot w_5 1 ]);
            trim_resolved w_5 2;
            return_value w_5 (get_env_slot w_5 125) (pc_to_exp (int_to_pc 0)))
          else failwith "unreachable (7)"
      | 15 (* tag_EApp *) ->
          let parts_31 = Memo.splits (snd resolved_14) in
          if List.length parts_31 = 2 then (
            let part0_31 = List.nth parts_31 0 in
            let part1_6 = List.nth parts_31 1 in
            set_env_slot w_5 34 part0_31;
            set_env_slot w_5 35 part1_6;
            let arg0_13 = get_env_slot w_5 34 in
            let arg1_13 = get_env_slot w_5 1 in
            assert_env_length w_5 172;
            w_5.state.k <-
              Memo.appends
                [ Memo.from_constructor tag_cont_14; collect_env_slots w_5 [ 1; 35; 36; 37; 38; 39; 40 ]; w_5.state.k ];
            trim_resolved w_5 2;
            init_frame w_5 2 (Memo.from_int 0);
            set_env_slot w_5 0 arg0_13;
            set_env_slot w_5 1 arg1_13;
            w_5.state.c <- pc_to_exp (int_to_pc 7))
          else failwith "unreachable (7)"
      | 27 (* tag_EHole *) ->
          let parts_35 = Memo.splits (snd resolved_14) in
          if List.length parts_35 = 1 then (
            let part0_35 = List.nth parts_35 0 in
            set_env_slot w_5 41 part0_35;
            assert_env_length w_5 172;
            set_env_slot w_5 138 (Memo.appends [ Memo.from_constructor tag_SHole; get_env_slot w_5 41 ]);
            assert_env_length w_5 172;
            set_env_slot w_5 139 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_5 138 ]);
            trim_resolved w_5 2;
            return_value w_5 (get_env_slot w_5 139) (pc_to_exp (int_to_pc 0)))
          else failwith "unreachable (7)"
      | 17 (* tag_ETrue *) ->
          trim_resolved w_5 2;
          return_value w_5 (Memo.from_constructor tag_VTrue) (pc_to_exp (int_to_pc 0))
      | 18 (* tag_EFalse *) ->
          trim_resolved w_5 2;
          return_value w_5 (Memo.from_constructor tag_VFalse) (pc_to_exp (int_to_pc 0))
      | 19 (* tag_EIf *) ->
          let parts_36 = Memo.splits (snd resolved_14) in
          if List.length parts_36 = 3 then (
            let part0_36 = List.nth parts_36 0 in
            let part1_9 = List.nth parts_36 1 in
            let part2_0 = List.nth parts_36 2 in
            set_env_slot w_5 42 part0_36;
            set_env_slot w_5 43 part1_9;
            set_env_slot w_5 44 part2_0;
            let arg0_18 = get_env_slot w_5 42 in
            let arg1_18 = get_env_slot w_5 1 in
            assert_env_length w_5 172;
            w_5.state.k <-
              Memo.appends
                [ Memo.from_constructor tag_cont_15; collect_env_slots w_5 [ 1; 43; 44; 45; 46 ]; w_5.state.k ];
            trim_resolved w_5 2;
            init_frame w_5 2 (Memo.from_int 0);
            set_env_slot w_5 0 arg0_18;
            set_env_slot w_5 1 arg1_18;
            w_5.state.c <- pc_to_exp (int_to_pc 7))
          else failwith "unreachable (7)"
      | 20 (* tag_ENil *) ->
          trim_resolved w_5 2;
          return_value w_5 (Memo.from_constructor tag_VNil) (pc_to_exp (int_to_pc 0))
      | 21 (* tag_ECons *) ->
          let parts_38 = Memo.splits (snd resolved_14) in
          if List.length parts_38 = 2 then (
            let part0_38 = List.nth parts_38 0 in
            let part1_10 = List.nth parts_38 1 in
            set_env_slot w_5 47 part0_38;
            set_env_slot w_5 48 part1_10;
            let arg0_21 = get_env_slot w_5 47 in
            let arg1_21 = get_env_slot w_5 1 in
            assert_env_length w_5 172;
            w_5.state.k <-
              Memo.appends [ Memo.from_constructor tag_cont_17; collect_env_slots w_5 [ 1; 48 ]; w_5.state.k ];
            trim_resolved w_5 2;
            init_frame w_5 2 (Memo.from_int 0);
            set_env_slot w_5 0 arg0_21;
            set_env_slot w_5 1 arg1_21;
            w_5.state.c <- pc_to_exp (int_to_pc 7))
          else failwith "unreachable (7)"
      | 23 (* tag_EPair *) ->
          let parts_39 = Memo.splits (snd resolved_14) in
          if List.length parts_39 = 2 then (
            let part0_39 = List.nth parts_39 0 in
            let part1_11 = List.nth parts_39 1 in
            set_env_slot w_5 49 part0_39;
            set_env_slot w_5 50 part1_11;
            let arg0_23 = get_env_slot w_5 49 in
            let arg1_23 = get_env_slot w_5 1 in
            assert_env_length w_5 172;
            w_5.state.k <-
              Memo.appends [ Memo.from_constructor tag_cont_19; collect_env_slots w_5 [ 1; 50 ]; w_5.state.k ];
            trim_resolved w_5 2;
            init_frame w_5 2 (Memo.from_int 0);
            set_env_slot w_5 0 arg0_23;
            set_env_slot w_5 1 arg1_23;
            w_5.state.c <- pc_to_exp (int_to_pc 7))
          else failwith "unreachable (7)"
      | 24 (* tag_EZro *) ->
          let parts_40 = Memo.splits (snd resolved_14) in
          if List.length parts_40 = 1 then (
            let part0_40 = List.nth parts_40 0 in
            set_env_slot w_5 51 part0_40;
            let arg0_25 = get_env_slot w_5 51 in
            let arg1_25 = get_env_slot w_5 1 in
            assert_env_length w_5 172;
            w_5.state.k <-
              Memo.appends [ Memo.from_constructor tag_cont_20; collect_env_slots w_5 [ 52; 54; 55 ]; w_5.state.k ];
            trim_resolved w_5 2;
            init_frame w_5 2 (Memo.from_int 0);
            set_env_slot w_5 0 arg0_25;
            set_env_slot w_5 1 arg1_25;
            w_5.state.c <- pc_to_exp (int_to_pc 7))
          else failwith "unreachable (7)"
      | 25 (* tag_EFst *) ->
          let parts_43 = Memo.splits (snd resolved_14) in
          if List.length parts_43 = 1 then (
            let part0_43 = List.nth parts_43 0 in
            set_env_slot w_5 56 part0_43;
            let arg0_26 = get_env_slot w_5 56 in
            let arg1_26 = get_env_slot w_5 1 in
            assert_env_length w_5 172;
            w_5.state.k <-
              Memo.appends [ Memo.from_constructor tag_cont_21; collect_env_slots w_5 [ 58; 59; 60 ]; w_5.state.k ];
            trim_resolved w_5 2;
            init_frame w_5 2 (Memo.from_int 0);
            set_env_slot w_5 0 arg0_26;
            set_env_slot w_5 1 arg1_26;
            w_5.state.c <- pc_to_exp (int_to_pc 7))
          else failwith "unreachable (7)"
      | 22 (* tag_EMatchList *) ->
          let parts_46 = Memo.splits (snd resolved_14) in
          if List.length parts_46 = 3 then (
            let part0_46 = List.nth parts_46 0 in
            let part1_14 = List.nth parts_46 1 in
            let part2_1 = List.nth parts_46 2 in
            set_env_slot w_5 61 part0_46;
            set_env_slot w_5 62 part1_14;
            set_env_slot w_5 63 part2_1;
            let arg0_27 = get_env_slot w_5 61 in
            let arg1_27 = get_env_slot w_5 1 in
            assert_env_length w_5 172;
            w_5.state.k <-
              Memo.appends
                [ Memo.from_constructor tag_cont_22; collect_env_slots w_5 [ 1; 62; 63; 64; 65; 66; 67 ]; w_5.state.k ];
            trim_resolved w_5 2;
            init_frame w_5 2 (Memo.from_int 0);
            set_env_slot w_5 0 arg0_27;
            set_env_slot w_5 1 arg1_27;
            w_5.state.c <- pc_to_exp (int_to_pc 7))
          else failwith "unreachable (7)"
      | 28 (* tag_EUnit *) ->
          trim_resolved w_5 2;
          return_value w_5 (Memo.from_constructor tag_VUnit) (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (7)")
    7;
  add_exp
    (fun w_6 ->
      assert_env_length w_6 6;
      resize_frame w_6 15 (Memo.from_int 0);
      assert_env_length w_6 15;
      let resolved_15 = resolve w_6 (Source.E 5) in
      let tag_1 = Word.get_value (fst resolved_15) in
      match tag_1 with
      | 29 (* tag_VInt *) ->
          let parts_2 = Memo.splits (snd resolved_15) in
          if List.length parts_2 = 1 then (
            let part0_2 = List.nth parts_2 0 in
            set_env_slot w_6 2 part0_2;
            let arg0_1 = get_env_slot w_6 1 in
            let arg1_1 = get_env_slot w_6 0 in
            assert_env_length w_6 15;
            w_6.state.k <-
              Memo.appends [ Memo.from_constructor tag_cont_0; collect_env_slots w_6 [ 2; 3; 5 ]; w_6.state.k ];
            trim_resolved w_6 6;
            init_frame w_6 2 (Memo.from_int 0);
            set_env_slot w_6 0 arg0_1;
            set_env_slot w_6 1 arg1_1;
            w_6.state.c <- pc_to_exp (int_to_pc 7))
          else (
            assert_env_length w_6 15;
            set_env_slot w_6 13
              (Memo.appends
                 [ Memo.from_constructor tag_STypeError; get_env_slot w_6 5; Memo.from_constructor tag_VTInt ]);
            assert_env_length w_6 15;
            set_env_slot w_6 14 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_6 13 ]);
            trim_resolved w_6 6;
            return_value w_6 (get_env_slot w_6 14) (pc_to_exp (int_to_pc 0)))
      | 38 (* tag_VStuck *) ->
          let parts_5 = Memo.splits (snd resolved_15) in
          if List.length parts_5 = 1 then (
            let part0_5 = List.nth parts_5 0 in
            set_env_slot w_6 4 part0_5;
            assert_env_length w_6 15;
            set_env_slot w_6 11
              (Memo.appends [ Memo.from_constructor tag_SAdd0; get_env_slot w_6 4; get_env_slot w_6 1 ]);
            assert_env_length w_6 15;
            set_env_slot w_6 12 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_6 11 ]);
            trim_resolved w_6 6;
            return_value w_6 (get_env_slot w_6 12) (pc_to_exp (int_to_pc 0)))
          else (
            assert_env_length w_6 15;
            set_env_slot w_6 13
              (Memo.appends
                 [ Memo.from_constructor tag_STypeError; get_env_slot w_6 5; Memo.from_constructor tag_VTInt ]);
            assert_env_length w_6 15;
            set_env_slot w_6 14 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_6 13 ]);
            trim_resolved w_6 6;
            return_value w_6 (get_env_slot w_6 14) (pc_to_exp (int_to_pc 0)))
      | _ ->
          assert_env_length w_6 15;
          set_env_slot w_6 13
            (Memo.appends [ Memo.from_constructor tag_STypeError; get_env_slot w_6 5; Memo.from_constructor tag_VTInt ]);
          assert_env_length w_6 15;
          set_env_slot w_6 14 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_6 13 ]);
          trim_resolved w_6 6;
          return_value w_6 (get_env_slot w_6 14) (pc_to_exp (int_to_pc 0)))
    8;
  add_exp
    (fun w_7 ->
      assert_env_length w_7 4;
      resize_frame w_7 8 (Memo.from_int 0);
      assert_env_length w_7 8;
      let resolved_16 = resolve w_7 (Source.E 3) in
      let tag_2 = Word.get_value (fst resolved_16) in
      match tag_2 with
      | 29 (* tag_VInt *) ->
          let parts_3 = Memo.splits (snd resolved_16) in
          if List.length parts_3 = 1 then (
            let part0_3 = List.nth parts_3 0 in
            trim_resolved w_7 4;
            shuffle_frame w_7 [| NewValue part0_3; OldSlot 0 |] (Memo.from_int 0);
            w_7.state.c <- pc_to_exp (int_to_pc 2))
          else (
            assert_env_length w_7 8;
            set_env_slot w_7 6
              (Memo.appends
                 [ Memo.from_constructor tag_STypeError; get_env_slot w_7 3; Memo.from_constructor tag_VTInt ]);
            assert_env_length w_7 8;
            set_env_slot w_7 7 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_7 6 ]);
            trim_resolved w_7 4;
            return_value w_7 (get_env_slot w_7 7) (pc_to_exp (int_to_pc 0)))
      | 38 (* tag_VStuck *) ->
          let parts_4 = Memo.splits (snd resolved_16) in
          if List.length parts_4 = 1 then (
            let part0_4 = List.nth parts_4 0 in
            set_env_slot w_7 1 part0_4;
            assert_env_length w_7 8;
            set_env_slot w_7 4
              (Memo.appends [ Memo.from_constructor tag_SAdd1; get_env_slot w_7 2; get_env_slot w_7 1 ]);
            assert_env_length w_7 8;
            set_env_slot w_7 5 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_7 4 ]);
            trim_resolved w_7 4;
            return_value w_7 (get_env_slot w_7 5) (pc_to_exp (int_to_pc 0)))
          else (
            assert_env_length w_7 8;
            set_env_slot w_7 6
              (Memo.appends
                 [ Memo.from_constructor tag_STypeError; get_env_slot w_7 3; Memo.from_constructor tag_VTInt ]);
            assert_env_length w_7 8;
            set_env_slot w_7 7 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_7 6 ]);
            trim_resolved w_7 4;
            return_value w_7 (get_env_slot w_7 7) (pc_to_exp (int_to_pc 0)))
      | _ ->
          assert_env_length w_7 8;
          set_env_slot w_7 6
            (Memo.appends [ Memo.from_constructor tag_STypeError; get_env_slot w_7 3; Memo.from_constructor tag_VTInt ]);
          assert_env_length w_7 8;
          set_env_slot w_7 7 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_7 6 ]);
          trim_resolved w_7 4;
          return_value w_7 (get_env_slot w_7 7) (pc_to_exp (int_to_pc 0)))
    9;
  add_exp
    (fun w_8 ->
      assert_env_length w_8 6;
      resize_frame w_8 15 (Memo.from_int 0);
      assert_env_length w_8 15;
      let resolved_17 = resolve w_8 (Source.E 5) in
      let tag_3 = Word.get_value (fst resolved_17) in
      match tag_3 with
      | 29 (* tag_VInt *) ->
          let parts_7 = Memo.splits (snd resolved_17) in
          if List.length parts_7 = 1 then (
            let part0_7 = List.nth parts_7 0 in
            set_env_slot w_8 2 part0_7;
            let arg0_3 = get_env_slot w_8 1 in
            let arg1_3 = get_env_slot w_8 0 in
            assert_env_length w_8 15;
            w_8.state.k <-
              Memo.appends [ Memo.from_constructor tag_cont_2; collect_env_slots w_8 [ 2; 3; 5 ]; w_8.state.k ];
            trim_resolved w_8 6;
            init_frame w_8 2 (Memo.from_int 0);
            set_env_slot w_8 0 arg0_3;
            set_env_slot w_8 1 arg1_3;
            w_8.state.c <- pc_to_exp (int_to_pc 7))
          else (
            assert_env_length w_8 15;
            set_env_slot w_8 13
              (Memo.appends
                 [ Memo.from_constructor tag_STypeError; get_env_slot w_8 5; Memo.from_constructor tag_VTInt ]);
            assert_env_length w_8 15;
            set_env_slot w_8 14 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_8 13 ]);
            trim_resolved w_8 6;
            return_value w_8 (get_env_slot w_8 14) (pc_to_exp (int_to_pc 0)))
      | 38 (* tag_VStuck *) ->
          let parts_10 = Memo.splits (snd resolved_17) in
          if List.length parts_10 = 1 then (
            let part0_10 = List.nth parts_10 0 in
            set_env_slot w_8 4 part0_10;
            assert_env_length w_8 15;
            set_env_slot w_8 11
              (Memo.appends [ Memo.from_constructor tag_SGt0; get_env_slot w_8 4; get_env_slot w_8 1 ]);
            assert_env_length w_8 15;
            set_env_slot w_8 12 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_8 11 ]);
            trim_resolved w_8 6;
            return_value w_8 (get_env_slot w_8 12) (pc_to_exp (int_to_pc 0)))
          else (
            assert_env_length w_8 15;
            set_env_slot w_8 13
              (Memo.appends
                 [ Memo.from_constructor tag_STypeError; get_env_slot w_8 5; Memo.from_constructor tag_VTInt ]);
            assert_env_length w_8 15;
            set_env_slot w_8 14 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_8 13 ]);
            trim_resolved w_8 6;
            return_value w_8 (get_env_slot w_8 14) (pc_to_exp (int_to_pc 0)))
      | _ ->
          assert_env_length w_8 15;
          set_env_slot w_8 13
            (Memo.appends [ Memo.from_constructor tag_STypeError; get_env_slot w_8 5; Memo.from_constructor tag_VTInt ]);
          assert_env_length w_8 15;
          set_env_slot w_8 14 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_8 13 ]);
          trim_resolved w_8 6;
          return_value w_8 (get_env_slot w_8 14) (pc_to_exp (int_to_pc 0)))
    10;
  add_exp
    (fun w_9 ->
      assert_env_length w_9 4;
      resize_frame w_9 8 (Memo.from_int 0);
      assert_env_length w_9 8;
      let resolved_18 = resolve w_9 (Source.E 3) in
      let tag_4 = Word.get_value (fst resolved_18) in
      match tag_4 with
      | 29 (* tag_VInt *) ->
          let parts_8 = Memo.splits (snd resolved_18) in
          if List.length parts_8 = 1 then (
            let part0_8 = List.nth parts_8 0 in
            trim_resolved w_9 4;
            shuffle_frame w_9 [| NewValue part0_8; OldSlot 0 |] (Memo.from_int 0);
            w_9.state.c <- pc_to_exp (int_to_pc 3))
          else (
            assert_env_length w_9 8;
            set_env_slot w_9 6
              (Memo.appends
                 [ Memo.from_constructor tag_STypeError; get_env_slot w_9 3; Memo.from_constructor tag_VTInt ]);
            assert_env_length w_9 8;
            set_env_slot w_9 7 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_9 6 ]);
            trim_resolved w_9 4;
            return_value w_9 (get_env_slot w_9 7) (pc_to_exp (int_to_pc 0)))
      | 38 (* tag_VStuck *) ->
          let parts_9 = Memo.splits (snd resolved_18) in
          if List.length parts_9 = 1 then (
            let part0_9 = List.nth parts_9 0 in
            set_env_slot w_9 1 part0_9;
            assert_env_length w_9 8;
            set_env_slot w_9 4 (Memo.appends [ Memo.from_constructor tag_SGt1; get_env_slot w_9 2; get_env_slot w_9 1 ]);
            assert_env_length w_9 8;
            set_env_slot w_9 5 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_9 4 ]);
            trim_resolved w_9 4;
            return_value w_9 (get_env_slot w_9 5) (pc_to_exp (int_to_pc 0)))
          else (
            assert_env_length w_9 8;
            set_env_slot w_9 6
              (Memo.appends
                 [ Memo.from_constructor tag_STypeError; get_env_slot w_9 3; Memo.from_constructor tag_VTInt ]);
            assert_env_length w_9 8;
            set_env_slot w_9 7 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_9 6 ]);
            trim_resolved w_9 4;
            return_value w_9 (get_env_slot w_9 7) (pc_to_exp (int_to_pc 0)))
      | _ ->
          assert_env_length w_9 8;
          set_env_slot w_9 6
            (Memo.appends [ Memo.from_constructor tag_STypeError; get_env_slot w_9 3; Memo.from_constructor tag_VTInt ]);
          assert_env_length w_9 8;
          set_env_slot w_9 7 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_9 6 ]);
          trim_resolved w_9 4;
          return_value w_9 (get_env_slot w_9 7) (pc_to_exp (int_to_pc 0)))
    11;
  add_exp
    (fun w_10 ->
      assert_env_length w_10 6;
      resize_frame w_10 15 (Memo.from_int 0);
      assert_env_length w_10 15;
      let resolved_19 = resolve w_10 (Source.E 5) in
      let tag_5 = Word.get_value (fst resolved_19) in
      match tag_5 with
      | 29 (* tag_VInt *) ->
          let parts_12 = Memo.splits (snd resolved_19) in
          if List.length parts_12 = 1 then (
            let part0_12 = List.nth parts_12 0 in
            set_env_slot w_10 2 part0_12;
            let arg0_5 = get_env_slot w_10 1 in
            let arg1_5 = get_env_slot w_10 0 in
            assert_env_length w_10 15;
            w_10.state.k <-
              Memo.appends [ Memo.from_constructor tag_cont_4; collect_env_slots w_10 [ 2; 3; 5 ]; w_10.state.k ];
            trim_resolved w_10 6;
            init_frame w_10 2 (Memo.from_int 0);
            set_env_slot w_10 0 arg0_5;
            set_env_slot w_10 1 arg1_5;
            w_10.state.c <- pc_to_exp (int_to_pc 7))
          else (
            assert_env_length w_10 15;
            set_env_slot w_10 13
              (Memo.appends
                 [ Memo.from_constructor tag_STypeError; get_env_slot w_10 5; Memo.from_constructor tag_VTInt ]);
            assert_env_length w_10 15;
            set_env_slot w_10 14 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_10 13 ]);
            trim_resolved w_10 6;
            return_value w_10 (get_env_slot w_10 14) (pc_to_exp (int_to_pc 0)))
      | 38 (* tag_VStuck *) ->
          let parts_15 = Memo.splits (snd resolved_19) in
          if List.length parts_15 = 1 then (
            let part0_15 = List.nth parts_15 0 in
            set_env_slot w_10 4 part0_15;
            assert_env_length w_10 15;
            set_env_slot w_10 11
              (Memo.appends [ Memo.from_constructor tag_SGt0; get_env_slot w_10 4; get_env_slot w_10 1 ]);
            assert_env_length w_10 15;
            set_env_slot w_10 12 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_10 11 ]);
            trim_resolved w_10 6;
            return_value w_10 (get_env_slot w_10 12) (pc_to_exp (int_to_pc 0)))
          else (
            assert_env_length w_10 15;
            set_env_slot w_10 13
              (Memo.appends
                 [ Memo.from_constructor tag_STypeError; get_env_slot w_10 5; Memo.from_constructor tag_VTInt ]);
            assert_env_length w_10 15;
            set_env_slot w_10 14 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_10 13 ]);
            trim_resolved w_10 6;
            return_value w_10 (get_env_slot w_10 14) (pc_to_exp (int_to_pc 0)))
      | _ ->
          assert_env_length w_10 15;
          set_env_slot w_10 13
            (Memo.appends
               [ Memo.from_constructor tag_STypeError; get_env_slot w_10 5; Memo.from_constructor tag_VTInt ]);
          assert_env_length w_10 15;
          set_env_slot w_10 14 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_10 13 ]);
          trim_resolved w_10 6;
          return_value w_10 (get_env_slot w_10 14) (pc_to_exp (int_to_pc 0)))
    12;
  add_exp
    (fun w_11 ->
      assert_env_length w_11 4;
      resize_frame w_11 8 (Memo.from_int 0);
      assert_env_length w_11 8;
      let resolved_20 = resolve w_11 (Source.E 3) in
      let tag_6 = Word.get_value (fst resolved_20) in
      match tag_6 with
      | 29 (* tag_VInt *) ->
          let parts_13 = Memo.splits (snd resolved_20) in
          if List.length parts_13 = 1 then (
            let part0_13 = List.nth parts_13 0 in
            trim_resolved w_11 4;
            shuffle_frame w_11 [| NewValue part0_13; OldSlot 0 |] (Memo.from_int 0);
            w_11.state.c <- pc_to_exp (int_to_pc 4))
          else (
            assert_env_length w_11 8;
            set_env_slot w_11 6
              (Memo.appends
                 [ Memo.from_constructor tag_STypeError; get_env_slot w_11 3; Memo.from_constructor tag_VTInt ]);
            assert_env_length w_11 8;
            set_env_slot w_11 7 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_11 6 ]);
            trim_resolved w_11 4;
            return_value w_11 (get_env_slot w_11 7) (pc_to_exp (int_to_pc 0)))
      | 38 (* tag_VStuck *) ->
          let parts_14 = Memo.splits (snd resolved_20) in
          if List.length parts_14 = 1 then (
            let part0_14 = List.nth parts_14 0 in
            set_env_slot w_11 1 part0_14;
            assert_env_length w_11 8;
            set_env_slot w_11 4
              (Memo.appends [ Memo.from_constructor tag_SGt1; get_env_slot w_11 2; get_env_slot w_11 1 ]);
            assert_env_length w_11 8;
            set_env_slot w_11 5 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_11 4 ]);
            trim_resolved w_11 4;
            return_value w_11 (get_env_slot w_11 5) (pc_to_exp (int_to_pc 0)))
          else (
            assert_env_length w_11 8;
            set_env_slot w_11 6
              (Memo.appends
                 [ Memo.from_constructor tag_STypeError; get_env_slot w_11 3; Memo.from_constructor tag_VTInt ]);
            assert_env_length w_11 8;
            set_env_slot w_11 7 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_11 6 ]);
            trim_resolved w_11 4;
            return_value w_11 (get_env_slot w_11 7) (pc_to_exp (int_to_pc 0)))
      | _ ->
          assert_env_length w_11 8;
          set_env_slot w_11 6
            (Memo.appends
               [ Memo.from_constructor tag_STypeError; get_env_slot w_11 3; Memo.from_constructor tag_VTInt ]);
          assert_env_length w_11 8;
          set_env_slot w_11 7 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_11 6 ]);
          trim_resolved w_11 4;
          return_value w_11 (get_env_slot w_11 7) (pc_to_exp (int_to_pc 0)))
    13;
  add_exp
    (fun w_12 ->
      assert_env_length w_12 6;
      resize_frame w_12 15 (Memo.from_int 0);
      assert_env_length w_12 15;
      let resolved_21 = resolve w_12 (Source.E 5) in
      let tag_7 = Word.get_value (fst resolved_21) in
      match tag_7 with
      | 29 (* tag_VInt *) ->
          let parts_17 = Memo.splits (snd resolved_21) in
          if List.length parts_17 = 1 then (
            let part0_17 = List.nth parts_17 0 in
            set_env_slot w_12 2 part0_17;
            let arg0_7 = get_env_slot w_12 1 in
            let arg1_7 = get_env_slot w_12 0 in
            assert_env_length w_12 15;
            w_12.state.k <-
              Memo.appends [ Memo.from_constructor tag_cont_6; collect_env_slots w_12 [ 2; 3; 5 ]; w_12.state.k ];
            trim_resolved w_12 6;
            init_frame w_12 2 (Memo.from_int 0);
            set_env_slot w_12 0 arg0_7;
            set_env_slot w_12 1 arg1_7;
            w_12.state.c <- pc_to_exp (int_to_pc 7))
          else (
            assert_env_length w_12 15;
            set_env_slot w_12 13
              (Memo.appends
                 [ Memo.from_constructor tag_STypeError; get_env_slot w_12 5; Memo.from_constructor tag_VTInt ]);
            assert_env_length w_12 15;
            set_env_slot w_12 14 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_12 13 ]);
            trim_resolved w_12 6;
            return_value w_12 (get_env_slot w_12 14) (pc_to_exp (int_to_pc 0)))
      | 38 (* tag_VStuck *) ->
          let parts_20 = Memo.splits (snd resolved_21) in
          if List.length parts_20 = 1 then (
            let part0_20 = List.nth parts_20 0 in
            set_env_slot w_12 4 part0_20;
            assert_env_length w_12 15;
            set_env_slot w_12 11
              (Memo.appends [ Memo.from_constructor tag_SGt0; get_env_slot w_12 4; get_env_slot w_12 1 ]);
            assert_env_length w_12 15;
            set_env_slot w_12 12 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_12 11 ]);
            trim_resolved w_12 6;
            return_value w_12 (get_env_slot w_12 12) (pc_to_exp (int_to_pc 0)))
          else (
            assert_env_length w_12 15;
            set_env_slot w_12 13
              (Memo.appends
                 [ Memo.from_constructor tag_STypeError; get_env_slot w_12 5; Memo.from_constructor tag_VTInt ]);
            assert_env_length w_12 15;
            set_env_slot w_12 14 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_12 13 ]);
            trim_resolved w_12 6;
            return_value w_12 (get_env_slot w_12 14) (pc_to_exp (int_to_pc 0)))
      | _ ->
          assert_env_length w_12 15;
          set_env_slot w_12 13
            (Memo.appends
               [ Memo.from_constructor tag_STypeError; get_env_slot w_12 5; Memo.from_constructor tag_VTInt ]);
          assert_env_length w_12 15;
          set_env_slot w_12 14 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_12 13 ]);
          trim_resolved w_12 6;
          return_value w_12 (get_env_slot w_12 14) (pc_to_exp (int_to_pc 0)))
    14;
  add_exp
    (fun w_13 ->
      assert_env_length w_13 4;
      resize_frame w_13 8 (Memo.from_int 0);
      assert_env_length w_13 8;
      let resolved_22 = resolve w_13 (Source.E 3) in
      let tag_8 = Word.get_value (fst resolved_22) in
      match tag_8 with
      | 29 (* tag_VInt *) ->
          let parts_18 = Memo.splits (snd resolved_22) in
          if List.length parts_18 = 1 then (
            let part0_18 = List.nth parts_18 0 in
            trim_resolved w_13 4;
            shuffle_frame w_13 [| NewValue part0_18; OldSlot 0 |] (Memo.from_int 0);
            w_13.state.c <- pc_to_exp (int_to_pc 5))
          else (
            assert_env_length w_13 8;
            set_env_slot w_13 6
              (Memo.appends
                 [ Memo.from_constructor tag_STypeError; get_env_slot w_13 3; Memo.from_constructor tag_VTInt ]);
            assert_env_length w_13 8;
            set_env_slot w_13 7 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_13 6 ]);
            trim_resolved w_13 4;
            return_value w_13 (get_env_slot w_13 7) (pc_to_exp (int_to_pc 0)))
      | 38 (* tag_VStuck *) ->
          let parts_19 = Memo.splits (snd resolved_22) in
          if List.length parts_19 = 1 then (
            let part0_19 = List.nth parts_19 0 in
            set_env_slot w_13 1 part0_19;
            assert_env_length w_13 8;
            set_env_slot w_13 4
              (Memo.appends [ Memo.from_constructor tag_SGt1; get_env_slot w_13 2; get_env_slot w_13 1 ]);
            assert_env_length w_13 8;
            set_env_slot w_13 5 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_13 4 ]);
            trim_resolved w_13 4;
            return_value w_13 (get_env_slot w_13 5) (pc_to_exp (int_to_pc 0)))
          else (
            assert_env_length w_13 8;
            set_env_slot w_13 6
              (Memo.appends
                 [ Memo.from_constructor tag_STypeError; get_env_slot w_13 3; Memo.from_constructor tag_VTInt ]);
            assert_env_length w_13 8;
            set_env_slot w_13 7 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_13 6 ]);
            trim_resolved w_13 4;
            return_value w_13 (get_env_slot w_13 7) (pc_to_exp (int_to_pc 0)))
      | _ ->
          assert_env_length w_13 8;
          set_env_slot w_13 6
            (Memo.appends
               [ Memo.from_constructor tag_STypeError; get_env_slot w_13 3; Memo.from_constructor tag_VTInt ]);
          assert_env_length w_13 8;
          set_env_slot w_13 7 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_13 6 ]);
          trim_resolved w_13 4;
          return_value w_13 (get_env_slot w_13 7) (pc_to_exp (int_to_pc 0)))
    15;
  add_exp
    (fun w_14 ->
      assert_env_length w_14 6;
      resize_frame w_14 15 (Memo.from_int 0);
      assert_env_length w_14 15;
      let resolved_23 = resolve w_14 (Source.E 5) in
      let tag_9 = Word.get_value (fst resolved_23) in
      match tag_9 with
      | 29 (* tag_VInt *) ->
          let parts_22 = Memo.splits (snd resolved_23) in
          if List.length parts_22 = 1 then (
            let part0_22 = List.nth parts_22 0 in
            set_env_slot w_14 2 part0_22;
            let arg0_9 = get_env_slot w_14 1 in
            let arg1_9 = get_env_slot w_14 0 in
            assert_env_length w_14 15;
            w_14.state.k <-
              Memo.appends [ Memo.from_constructor tag_cont_8; collect_env_slots w_14 [ 2; 3; 5 ]; w_14.state.k ];
            trim_resolved w_14 6;
            init_frame w_14 2 (Memo.from_int 0);
            set_env_slot w_14 0 arg0_9;
            set_env_slot w_14 1 arg1_9;
            w_14.state.c <- pc_to_exp (int_to_pc 7))
          else (
            assert_env_length w_14 15;
            set_env_slot w_14 13
              (Memo.appends
                 [ Memo.from_constructor tag_STypeError; get_env_slot w_14 5; Memo.from_constructor tag_VTInt ]);
            assert_env_length w_14 15;
            set_env_slot w_14 14 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_14 13 ]);
            trim_resolved w_14 6;
            return_value w_14 (get_env_slot w_14 14) (pc_to_exp (int_to_pc 0)))
      | 38 (* tag_VStuck *) ->
          let parts_25 = Memo.splits (snd resolved_23) in
          if List.length parts_25 = 1 then (
            let part0_25 = List.nth parts_25 0 in
            set_env_slot w_14 4 part0_25;
            assert_env_length w_14 15;
            set_env_slot w_14 11
              (Memo.appends [ Memo.from_constructor tag_SGt0; get_env_slot w_14 4; get_env_slot w_14 1 ]);
            assert_env_length w_14 15;
            set_env_slot w_14 12 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_14 11 ]);
            trim_resolved w_14 6;
            return_value w_14 (get_env_slot w_14 12) (pc_to_exp (int_to_pc 0)))
          else (
            assert_env_length w_14 15;
            set_env_slot w_14 13
              (Memo.appends
                 [ Memo.from_constructor tag_STypeError; get_env_slot w_14 5; Memo.from_constructor tag_VTInt ]);
            assert_env_length w_14 15;
            set_env_slot w_14 14 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_14 13 ]);
            trim_resolved w_14 6;
            return_value w_14 (get_env_slot w_14 14) (pc_to_exp (int_to_pc 0)))
      | _ ->
          assert_env_length w_14 15;
          set_env_slot w_14 13
            (Memo.appends
               [ Memo.from_constructor tag_STypeError; get_env_slot w_14 5; Memo.from_constructor tag_VTInt ]);
          assert_env_length w_14 15;
          set_env_slot w_14 14 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_14 13 ]);
          trim_resolved w_14 6;
          return_value w_14 (get_env_slot w_14 14) (pc_to_exp (int_to_pc 0)))
    16;
  add_exp
    (fun w_15 ->
      assert_env_length w_15 4;
      resize_frame w_15 8 (Memo.from_int 0);
      assert_env_length w_15 8;
      let resolved_24 = resolve w_15 (Source.E 3) in
      let tag_10 = Word.get_value (fst resolved_24) in
      match tag_10 with
      | 29 (* tag_VInt *) ->
          let parts_23 = Memo.splits (snd resolved_24) in
          if List.length parts_23 = 1 then (
            let part0_23 = List.nth parts_23 0 in
            trim_resolved w_15 4;
            shuffle_frame w_15 [| NewValue part0_23; OldSlot 0 |] (Memo.from_int 0);
            w_15.state.c <- pc_to_exp (int_to_pc 6))
          else (
            assert_env_length w_15 8;
            set_env_slot w_15 6
              (Memo.appends
                 [ Memo.from_constructor tag_STypeError; get_env_slot w_15 3; Memo.from_constructor tag_VTInt ]);
            assert_env_length w_15 8;
            set_env_slot w_15 7 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_15 6 ]);
            trim_resolved w_15 4;
            return_value w_15 (get_env_slot w_15 7) (pc_to_exp (int_to_pc 0)))
      | 38 (* tag_VStuck *) ->
          let parts_24 = Memo.splits (snd resolved_24) in
          if List.length parts_24 = 1 then (
            let part0_24 = List.nth parts_24 0 in
            set_env_slot w_15 1 part0_24;
            assert_env_length w_15 8;
            set_env_slot w_15 4
              (Memo.appends [ Memo.from_constructor tag_SGt1; get_env_slot w_15 2; get_env_slot w_15 1 ]);
            assert_env_length w_15 8;
            set_env_slot w_15 5 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_15 4 ]);
            trim_resolved w_15 4;
            return_value w_15 (get_env_slot w_15 5) (pc_to_exp (int_to_pc 0)))
          else (
            assert_env_length w_15 8;
            set_env_slot w_15 6
              (Memo.appends
                 [ Memo.from_constructor tag_STypeError; get_env_slot w_15 3; Memo.from_constructor tag_VTInt ]);
            assert_env_length w_15 8;
            set_env_slot w_15 7 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_15 6 ]);
            trim_resolved w_15 4;
            return_value w_15 (get_env_slot w_15 7) (pc_to_exp (int_to_pc 0)))
      | _ ->
          assert_env_length w_15 8;
          set_env_slot w_15 6
            (Memo.appends
               [ Memo.from_constructor tag_STypeError; get_env_slot w_15 3; Memo.from_constructor tag_VTInt ]);
          assert_env_length w_15 8;
          set_env_slot w_15 7 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_15 6 ]);
          trim_resolved w_15 4;
          return_value w_15 (get_env_slot w_15 7) (pc_to_exp (int_to_pc 0)))
    17;
  add_exp
    (fun w_16 ->
      assert_env_length w_16 2;
      resize_frame w_16 3 (Memo.from_int 0);
      assert_env_length w_16 3;
      let resolved_25 = resolve w_16 (Source.E 1) in
      let tag_11 = Word.get_value (fst resolved_25) in
      match tag_11 with
      | 6 (* tag_Some *) ->
          let parts_27 = Memo.splits (snd resolved_25) in
          if List.length parts_27 = 1 then (
            let part0_27 = List.nth parts_27 0 in
            set_env_slot w_16 0 part0_27;
            trim_resolved w_16 2;
            return_value w_16 (get_env_slot w_16 0) (pc_to_exp (int_to_pc 0)))
          else failwith "unreachable (18)"
      | 5 (* tag_None *) ->
          assert_env_length w_16 3;
          set_env_slot w_16 2 (Memo.appends [ Memo.from_constructor tag_VStuck; Memo.from_constructor tag_SIndexError ]);
          trim_resolved w_16 2;
          return_value w_16 (get_env_slot w_16 2) (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (18)")
    18;
  add_exp
    (fun w_17 ->
      assert_env_length w_17 3;
      resize_frame w_17 5 (Memo.from_int 0);
      assert_env_length w_17 5;
      set_env_slot w_17 3 (Memo.appends [ Memo.from_constructor tag_Cons; get_env_slot w_17 2; get_env_slot w_17 0 ]);
      let arg0_12 = get_env_slot w_17 1 in
      let arg1_12 = get_env_slot w_17 3 in
      assert_env_length w_17 5;
      trim_resolved w_17 3;
      init_frame w_17 2 (Memo.from_int 0);
      set_env_slot w_17 0 arg0_12;
      set_env_slot w_17 1 arg1_12;
      w_17.state.c <- pc_to_exp (int_to_pc 7))
    19;
  add_exp
    (fun w_18 ->
      assert_env_length w_18 8;
      resize_frame w_18 19 (Memo.from_int 0);
      assert_env_length w_18 19;
      let resolved_26 = resolve w_18 (Source.E 7) in
      let tag_12 = Word.get_value (fst resolved_26) in
      match tag_12 with
      | 30 (* tag_VAbs *) ->
          let parts_32 = Memo.splits (snd resolved_26) in
          if List.length parts_32 = 2 then (
            let part0_32 = List.nth parts_32 0 in
            let part1_7 = List.nth parts_32 1 in
            set_env_slot w_18 2 part0_32;
            set_env_slot w_18 3 part1_7;
            let arg0_14 = get_env_slot w_18 1 in
            let arg1_14 = get_env_slot w_18 0 in
            assert_env_length w_18 19;
            w_18.state.k <-
              Memo.appends [ Memo.from_constructor tag_cont_12; collect_env_slots w_18 [ 2; 3 ]; w_18.state.k ];
            trim_resolved w_18 8;
            init_frame w_18 2 (Memo.from_int 0);
            set_env_slot w_18 0 arg0_14;
            set_env_slot w_18 1 arg1_14;
            w_18.state.c <- pc_to_exp (int_to_pc 7))
          else (
            assert_env_length w_18 19;
            set_env_slot w_18 17
              (Memo.appends
                 [ Memo.from_constructor tag_STypeError; get_env_slot w_18 7; Memo.from_constructor tag_VTFunc ]);
            assert_env_length w_18 19;
            set_env_slot w_18 18 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_18 17 ]);
            trim_resolved w_18 8;
            return_value w_18 (get_env_slot w_18 18) (pc_to_exp (int_to_pc 0)))
      | 37 (* tag_VFix *) ->
          let parts_33 = Memo.splits (snd resolved_26) in
          if List.length parts_33 = 2 then (
            let part0_33 = List.nth parts_33 0 in
            let part1_8 = List.nth parts_33 1 in
            set_env_slot w_18 4 part0_33;
            set_env_slot w_18 5 part1_8;
            let arg0_16 = get_env_slot w_18 1 in
            let arg1_16 = get_env_slot w_18 0 in
            assert_env_length w_18 19;
            w_18.state.k <-
              Memo.appends [ Memo.from_constructor tag_cont_13; collect_env_slots w_18 [ 4; 5; 7 ]; w_18.state.k ];
            trim_resolved w_18 8;
            init_frame w_18 2 (Memo.from_int 0);
            set_env_slot w_18 0 arg0_16;
            set_env_slot w_18 1 arg1_16;
            w_18.state.c <- pc_to_exp (int_to_pc 7))
          else (
            assert_env_length w_18 19;
            set_env_slot w_18 17
              (Memo.appends
                 [ Memo.from_constructor tag_STypeError; get_env_slot w_18 7; Memo.from_constructor tag_VTFunc ]);
            assert_env_length w_18 19;
            set_env_slot w_18 18 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_18 17 ]);
            trim_resolved w_18 8;
            return_value w_18 (get_env_slot w_18 18) (pc_to_exp (int_to_pc 0)))
      | 38 (* tag_VStuck *) ->
          let parts_34 = Memo.splits (snd resolved_26) in
          if List.length parts_34 = 1 then (
            let part0_34 = List.nth parts_34 0 in
            set_env_slot w_18 6 part0_34;
            assert_env_length w_18 19;
            set_env_slot w_18 15
              (Memo.appends [ Memo.from_constructor tag_SApp; get_env_slot w_18 6; get_env_slot w_18 1 ]);
            assert_env_length w_18 19;
            set_env_slot w_18 16 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_18 15 ]);
            trim_resolved w_18 8;
            return_value w_18 (get_env_slot w_18 16) (pc_to_exp (int_to_pc 0)))
          else (
            assert_env_length w_18 19;
            set_env_slot w_18 17
              (Memo.appends
                 [ Memo.from_constructor tag_STypeError; get_env_slot w_18 7; Memo.from_constructor tag_VTFunc ]);
            assert_env_length w_18 19;
            set_env_slot w_18 18 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_18 17 ]);
            trim_resolved w_18 8;
            return_value w_18 (get_env_slot w_18 18) (pc_to_exp (int_to_pc 0)))
      | _ ->
          assert_env_length w_18 19;
          set_env_slot w_18 17
            (Memo.appends
               [ Memo.from_constructor tag_STypeError; get_env_slot w_18 7; Memo.from_constructor tag_VTFunc ]);
          assert_env_length w_18 19;
          set_env_slot w_18 18 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_18 17 ]);
          trim_resolved w_18 8;
          return_value w_18 (get_env_slot w_18 18) (pc_to_exp (int_to_pc 0)))
    20;
  add_exp
    (fun w_19 ->
      assert_env_length w_19 3;
      resize_frame w_19 5 (Memo.from_int 0);
      assert_env_length w_19 5;
      set_env_slot w_19 3 (Memo.appends [ Memo.from_constructor tag_Cons; get_env_slot w_19 2; get_env_slot w_19 1 ]);
      let arg0_15 = get_env_slot w_19 0 in
      let arg1_15 = get_env_slot w_19 3 in
      assert_env_length w_19 5;
      trim_resolved w_19 3;
      init_frame w_19 2 (Memo.from_int 0);
      set_env_slot w_19 0 arg0_15;
      set_env_slot w_19 1 arg1_15;
      w_19.state.c <- pc_to_exp (int_to_pc 7))
    21;
  add_exp
    (fun w_20 ->
      assert_env_length w_20 4;
      resize_frame w_20 7 (Memo.from_int 0);
      assert_env_length w_20 7;
      set_env_slot w_20 4 (Memo.appends [ Memo.from_constructor tag_Cons; get_env_slot w_20 2; get_env_slot w_20 1 ]);
      assert_env_length w_20 7;
      set_env_slot w_20 5 (Memo.appends [ Memo.from_constructor tag_Cons; get_env_slot w_20 3; get_env_slot w_20 4 ]);
      let arg0_17 = get_env_slot w_20 0 in
      let arg1_17 = get_env_slot w_20 5 in
      assert_env_length w_20 7;
      trim_resolved w_20 4;
      init_frame w_20 2 (Memo.from_int 0);
      set_env_slot w_20 0 arg0_17;
      set_env_slot w_20 1 arg1_17;
      w_20.state.c <- pc_to_exp (int_to_pc 7))
    22;
  add_exp
    (fun w_21 ->
      assert_env_length w_21 6;
      resize_frame w_21 12 (Memo.from_int 0);
      assert_env_length w_21 12;
      let resolved_27 = resolve w_21 (Source.E 5) in
      let tag_13 = Word.get_value (fst resolved_27) in
      match tag_13 with
      | 32 (* tag_VTrue *) ->
          let arg0_19 = get_env_slot w_21 1 in
          let arg1_19 = get_env_slot w_21 0 in
          assert_env_length w_21 12;
          trim_resolved w_21 6;
          init_frame w_21 2 (Memo.from_int 0);
          set_env_slot w_21 0 arg0_19;
          set_env_slot w_21 1 arg1_19;
          w_21.state.c <- pc_to_exp (int_to_pc 7)
      | 33 (* tag_VFalse *) ->
          let arg0_20 = get_env_slot w_21 2 in
          let arg1_20 = get_env_slot w_21 0 in
          assert_env_length w_21 12;
          trim_resolved w_21 6;
          init_frame w_21 2 (Memo.from_int 0);
          set_env_slot w_21 0 arg0_20;
          set_env_slot w_21 1 arg1_20;
          w_21.state.c <- pc_to_exp (int_to_pc 7)
      | 38 (* tag_VStuck *) ->
          let parts_37 = Memo.splits (snd resolved_27) in
          if List.length parts_37 = 1 then (
            let part0_37 = List.nth parts_37 0 in
            set_env_slot w_21 3 part0_37;
            assert_env_length w_21 12;
            set_env_slot w_21 8
              (Memo.appends
                 [ Memo.from_constructor tag_SIf; get_env_slot w_21 3; get_env_slot w_21 1; get_env_slot w_21 2 ]);
            assert_env_length w_21 12;
            set_env_slot w_21 9 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_21 8 ]);
            trim_resolved w_21 6;
            return_value w_21 (get_env_slot w_21 9) (pc_to_exp (int_to_pc 0)))
          else (
            set_env_slot w_21 4 (get_env_slot w_21 5);
            assert_env_length w_21 12;
            set_env_slot w_21 10
              (Memo.appends
                 [ Memo.from_constructor tag_STypeError; get_env_slot w_21 4; Memo.from_constructor tag_VTBool ]);
            assert_env_length w_21 12;
            set_env_slot w_21 11 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_21 10 ]);
            trim_resolved w_21 6;
            return_value w_21 (get_env_slot w_21 11) (pc_to_exp (int_to_pc 0)))
      | _ ->
          set_env_slot w_21 4 (get_env_slot w_21 5);
          assert_env_length w_21 12;
          set_env_slot w_21 10
            (Memo.appends
               [ Memo.from_constructor tag_STypeError; get_env_slot w_21 4; Memo.from_constructor tag_VTBool ]);
          assert_env_length w_21 12;
          set_env_slot w_21 11 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_21 10 ]);
          trim_resolved w_21 6;
          return_value w_21 (get_env_slot w_21 11) (pc_to_exp (int_to_pc 0)))
    23;
  add_exp
    (fun w_22 ->
      assert_env_length w_22 3;
      let arg0_22 = get_env_slot w_22 1 in
      let arg1_22 = get_env_slot w_22 0 in
      assert_env_length w_22 3;
      w_22.state.k <- Memo.appends [ Memo.from_constructor tag_cont_16; collect_env_slots w_22 [ 2 ]; w_22.state.k ];
      init_frame w_22 2 (Memo.from_int 0);
      set_env_slot w_22 0 arg0_22;
      set_env_slot w_22 1 arg1_22;
      w_22.state.c <- pc_to_exp (int_to_pc 7))
    24;
  add_exp
    (fun w_23 ->
      assert_env_length w_23 2;
      resize_frame w_23 3 (Memo.from_int 0);
      assert_env_length w_23 3;
      set_env_slot w_23 2 (Memo.appends [ Memo.from_constructor tag_VCons; get_env_slot w_23 0; get_env_slot w_23 1 ]);
      trim_resolved w_23 2;
      return_value w_23 (get_env_slot w_23 2) (pc_to_exp (int_to_pc 0)))
    25;
  add_exp
    (fun w_24 ->
      assert_env_length w_24 3;
      let arg0_24 = get_env_slot w_24 1 in
      let arg1_24 = get_env_slot w_24 0 in
      assert_env_length w_24 3;
      w_24.state.k <- Memo.appends [ Memo.from_constructor tag_cont_18; collect_env_slots w_24 [ 2 ]; w_24.state.k ];
      init_frame w_24 2 (Memo.from_int 0);
      set_env_slot w_24 0 arg0_24;
      set_env_slot w_24 1 arg1_24;
      w_24.state.c <- pc_to_exp (int_to_pc 7))
    26;
  add_exp
    (fun w_25 ->
      assert_env_length w_25 2;
      resize_frame w_25 3 (Memo.from_int 0);
      assert_env_length w_25 3;
      set_env_slot w_25 2 (Memo.appends [ Memo.from_constructor tag_VPair; get_env_slot w_25 0; get_env_slot w_25 1 ]);
      trim_resolved w_25 2;
      return_value w_25 (get_env_slot w_25 2) (pc_to_exp (int_to_pc 0)))
    27;
  add_exp
    (fun w_26 ->
      assert_env_length w_26 4;
      resize_frame w_26 9 (Memo.from_int 0);
      assert_env_length w_26 9;
      let resolved_28 = resolve w_26 (Source.E 3) in
      let tag_14 = Word.get_value (fst resolved_28) in
      match tag_14 with
      | 36 (* tag_VPair *) ->
          let parts_41 = Memo.splits (snd resolved_28) in
          if List.length parts_41 = 2 then (
            let part0_41 = List.nth parts_41 0 in
            let part1_12 = List.nth parts_41 1 in
            set_env_slot w_26 0 part0_41;
            set_env_slot w_26 4 part1_12;
            trim_resolved w_26 4;
            return_value w_26 (get_env_slot w_26 0) (pc_to_exp (int_to_pc 0)))
          else (
            set_env_slot w_26 2 (get_env_slot w_26 3);
            assert_env_length w_26 9;
            set_env_slot w_26 7
              (Memo.appends
                 [ Memo.from_constructor tag_STypeError; get_env_slot w_26 2; Memo.from_constructor tag_VTPair ]);
            assert_env_length w_26 9;
            set_env_slot w_26 8 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_26 7 ]);
            trim_resolved w_26 4;
            return_value w_26 (get_env_slot w_26 8) (pc_to_exp (int_to_pc 0)))
      | 38 (* tag_VStuck *) ->
          let parts_42 = Memo.splits (snd resolved_28) in
          if List.length parts_42 = 1 then (
            let part0_42 = List.nth parts_42 0 in
            set_env_slot w_26 1 part0_42;
            assert_env_length w_26 9;
            set_env_slot w_26 5 (Memo.appends [ Memo.from_constructor tag_SZro; get_env_slot w_26 1 ]);
            assert_env_length w_26 9;
            set_env_slot w_26 6 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_26 5 ]);
            trim_resolved w_26 4;
            return_value w_26 (get_env_slot w_26 6) (pc_to_exp (int_to_pc 0)))
          else (
            set_env_slot w_26 2 (get_env_slot w_26 3);
            assert_env_length w_26 9;
            set_env_slot w_26 7
              (Memo.appends
                 [ Memo.from_constructor tag_STypeError; get_env_slot w_26 2; Memo.from_constructor tag_VTPair ]);
            assert_env_length w_26 9;
            set_env_slot w_26 8 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_26 7 ]);
            trim_resolved w_26 4;
            return_value w_26 (get_env_slot w_26 8) (pc_to_exp (int_to_pc 0)))
      | _ ->
          set_env_slot w_26 2 (get_env_slot w_26 3);
          assert_env_length w_26 9;
          set_env_slot w_26 7
            (Memo.appends
               [ Memo.from_constructor tag_STypeError; get_env_slot w_26 2; Memo.from_constructor tag_VTPair ]);
          assert_env_length w_26 9;
          set_env_slot w_26 8 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_26 7 ]);
          trim_resolved w_26 4;
          return_value w_26 (get_env_slot w_26 8) (pc_to_exp (int_to_pc 0)))
    28;
  add_exp
    (fun w_27 ->
      assert_env_length w_27 4;
      resize_frame w_27 9 (Memo.from_int 0);
      assert_env_length w_27 9;
      let resolved_29 = resolve w_27 (Source.E 3) in
      let tag_15 = Word.get_value (fst resolved_29) in
      match tag_15 with
      | 36 (* tag_VPair *) ->
          let parts_44 = Memo.splits (snd resolved_29) in
          if List.length parts_44 = 2 then (
            let part0_44 = List.nth parts_44 0 in
            let part1_13 = List.nth parts_44 1 in
            set_env_slot w_27 4 part0_44;
            set_env_slot w_27 0 part1_13;
            trim_resolved w_27 4;
            return_value w_27 (get_env_slot w_27 0) (pc_to_exp (int_to_pc 0)))
          else (
            set_env_slot w_27 2 (get_env_slot w_27 3);
            assert_env_length w_27 9;
            set_env_slot w_27 7
              (Memo.appends
                 [ Memo.from_constructor tag_STypeError; get_env_slot w_27 2; Memo.from_constructor tag_VTPair ]);
            assert_env_length w_27 9;
            set_env_slot w_27 8 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_27 7 ]);
            trim_resolved w_27 4;
            return_value w_27 (get_env_slot w_27 8) (pc_to_exp (int_to_pc 0)))
      | 38 (* tag_VStuck *) ->
          let parts_45 = Memo.splits (snd resolved_29) in
          if List.length parts_45 = 1 then (
            let part0_45 = List.nth parts_45 0 in
            set_env_slot w_27 1 part0_45;
            assert_env_length w_27 9;
            set_env_slot w_27 5 (Memo.appends [ Memo.from_constructor tag_SFst; get_env_slot w_27 1 ]);
            assert_env_length w_27 9;
            set_env_slot w_27 6 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_27 5 ]);
            trim_resolved w_27 4;
            return_value w_27 (get_env_slot w_27 6) (pc_to_exp (int_to_pc 0)))
          else (
            set_env_slot w_27 2 (get_env_slot w_27 3);
            assert_env_length w_27 9;
            set_env_slot w_27 7
              (Memo.appends
                 [ Memo.from_constructor tag_STypeError; get_env_slot w_27 2; Memo.from_constructor tag_VTPair ]);
            assert_env_length w_27 9;
            set_env_slot w_27 8 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_27 7 ]);
            trim_resolved w_27 4;
            return_value w_27 (get_env_slot w_27 8) (pc_to_exp (int_to_pc 0)))
      | _ ->
          set_env_slot w_27 2 (get_env_slot w_27 3);
          assert_env_length w_27 9;
          set_env_slot w_27 7
            (Memo.appends
               [ Memo.from_constructor tag_STypeError; get_env_slot w_27 2; Memo.from_constructor tag_VTPair ]);
          assert_env_length w_27 9;
          set_env_slot w_27 8 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_27 7 ]);
          trim_resolved w_27 4;
          return_value w_27 (get_env_slot w_27 8) (pc_to_exp (int_to_pc 0)))
    29;
  add_exp
    (fun w_28 ->
      assert_env_length w_28 8;
      resize_frame w_28 16 (Memo.from_int 0);
      assert_env_length w_28 16;
      let resolved_30 = resolve w_28 (Source.E 7) in
      let tag_16 = Word.get_value (fst resolved_30) in
      match tag_16 with
      | 34 (* tag_VNil *) ->
          let arg0_28 = get_env_slot w_28 1 in
          let arg1_28 = get_env_slot w_28 0 in
          assert_env_length w_28 16;
          trim_resolved w_28 8;
          init_frame w_28 2 (Memo.from_int 0);
          set_env_slot w_28 0 arg0_28;
          set_env_slot w_28 1 arg1_28;
          w_28.state.c <- pc_to_exp (int_to_pc 7)
      | 35 (* tag_VCons *) ->
          let parts_47 = Memo.splits (snd resolved_30) in
          if List.length parts_47 = 2 then (
            let part0_47 = List.nth parts_47 0 in
            let part1_15 = List.nth parts_47 1 in
            set_env_slot w_28 3 part0_47;
            set_env_slot w_28 4 part1_15;
            assert_env_length w_28 16;
            set_env_slot w_28 9
              (Memo.appends [ Memo.from_constructor tag_Cons; get_env_slot w_28 3; get_env_slot w_28 0 ]);
            assert_env_length w_28 16;
            set_env_slot w_28 10
              (Memo.appends [ Memo.from_constructor tag_Cons; get_env_slot w_28 4; get_env_slot w_28 9 ]);
            let arg0_29 = get_env_slot w_28 2 in
            let arg1_29 = get_env_slot w_28 10 in
            assert_env_length w_28 16;
            trim_resolved w_28 8;
            init_frame w_28 2 (Memo.from_int 0);
            set_env_slot w_28 0 arg0_29;
            set_env_slot w_28 1 arg1_29;
            w_28.state.c <- pc_to_exp (int_to_pc 7))
          else (
            set_env_slot w_28 6 (get_env_slot w_28 7);
            assert_env_length w_28 16;
            set_env_slot w_28 14
              (Memo.appends
                 [ Memo.from_constructor tag_STypeError; get_env_slot w_28 6; Memo.from_constructor tag_VTList ]);
            assert_env_length w_28 16;
            set_env_slot w_28 15 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_28 14 ]);
            trim_resolved w_28 8;
            return_value w_28 (get_env_slot w_28 15) (pc_to_exp (int_to_pc 0)))
      | 38 (* tag_VStuck *) ->
          let parts_48 = Memo.splits (snd resolved_30) in
          if List.length parts_48 = 1 then (
            let part0_48 = List.nth parts_48 0 in
            set_env_slot w_28 5 part0_48;
            assert_env_length w_28 16;
            set_env_slot w_28 12
              (Memo.appends
                 [ Memo.from_constructor tag_SMatchList; get_env_slot w_28 5; get_env_slot w_28 1; get_env_slot w_28 2 ]);
            assert_env_length w_28 16;
            set_env_slot w_28 13 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_28 12 ]);
            trim_resolved w_28 8;
            return_value w_28 (get_env_slot w_28 13) (pc_to_exp (int_to_pc 0)))
          else (
            set_env_slot w_28 6 (get_env_slot w_28 7);
            assert_env_length w_28 16;
            set_env_slot w_28 14
              (Memo.appends
                 [ Memo.from_constructor tag_STypeError; get_env_slot w_28 6; Memo.from_constructor tag_VTList ]);
            assert_env_length w_28 16;
            set_env_slot w_28 15 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_28 14 ]);
            trim_resolved w_28 8;
            return_value w_28 (get_env_slot w_28 15) (pc_to_exp (int_to_pc 0)))
      | _ ->
          set_env_slot w_28 6 (get_env_slot w_28 7);
          assert_env_length w_28 16;
          set_env_slot w_28 14
            (Memo.appends
               [ Memo.from_constructor tag_STypeError; get_env_slot w_28 6; Memo.from_constructor tag_VTList ]);
          assert_env_length w_28 16;
          set_env_slot w_28 15 (Memo.appends [ Memo.from_constructor tag_VStuck; get_env_slot w_28 14 ]);
          trim_resolved w_28 8;
          return_value w_28 (get_env_slot w_28 15) (pc_to_exp (int_to_pc 0)))
    30;
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
  Words.set_constructor_degree 56 (-3);
  Words.set_constructor_degree 57 (-5);
  Words.set_constructor_degree 58 (-3);
  Words.set_constructor_degree 59 (-5);
  Words.set_constructor_degree 60 (-3);
  Words.set_constructor_degree 61 (-5);
  Words.set_constructor_degree 62 (-3);
  Words.set_constructor_degree 63 (-5);
  Words.set_constructor_degree 64 (-3);
  Words.set_constructor_degree 65 (-5);
  Words.set_constructor_degree 66 (-1);
  Words.set_constructor_degree 67 (-2);
  Words.set_constructor_degree 68 (-2);
  Words.set_constructor_degree 69 (-3);
  Words.set_constructor_degree 70 (-7);
  Words.set_constructor_degree 71 (-5);
  Words.set_constructor_degree 72 (-1);
  Words.set_constructor_degree 73 (-2);
  Words.set_constructor_degree 74 (-1);
  Words.set_constructor_degree 75 (-2);
  Words.set_constructor_degree 76 (-3);
  Words.set_constructor_degree 77 (-3);
  Words.set_constructor_degree 78 (-7)
