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
let tag_cont_1 = 56
let tag_cont_2 = 57
let tag_cont_3 = 58
let tag_cont_4 = 59
let tag_cont_5 = 60
let tag_cont_6 = 61
let tag_cont_7 = 62
let tag_cont_8 = 63
let tag_cont_9 = 64
let tag_cont_10 = 65
let tag_cont_11 = 66
let tag_cont_12 = 67
let tag_cont_13 = 68
let tag_cont_14 = 69
let tag_cont_15 = 70
let tag_cont_16 = 71
let tag_cont_17 = 72
let tag_cont_18 = 73
let tag_cont_19 = 74
let tag_cont_20 = 75
let tag_cont_21 = 76
let tag_cont_22 = 77
let tag_cont_23 = 78

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
  | SHole of int option * value list
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
  | SHole (x0, x1) ->
      Memo.appends
        [
          Memo.from_constructor tag_SHole;
          from_ocaml_option (fun x -> Memo.from_int x) x0;
          from_ocaml_list (fun x -> from_ocaml_value x) x1;
        ]
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
      let x0, x1 = Memo.splits_2 t in
      SHole (to_ocaml_option (fun x -> Word.get_value (Memo.to_word x)) x0, to_ocaml_list (fun x -> to_ocaml_value x) x1)
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

let rec index memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 1)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec eval memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 4)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let populate_state () =
  Memo.reset ();
  Words.reset ();
  add_exp
    (fun w_5 ->
      assert_env_length w_5 1;
      let hd_0, tl_0 = resolve w_5 K in
      match Word.get_value hd_0 with
      | c_19 when c_19 = tag_cont_done -> exec_done w_5
      | c_19 when c_19 = tag_cont_1 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 2 tl_0;
          assert_env_length w_5 3;
          push_env w_5 (Dynarray.get w_5.state.e 2);
          w_5.state.c <- pc_to_exp (int_to_pc 6)
      | c_19 when c_19 = tag_cont_2 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 2 tl_0;
          assert_env_length w_5 3;
          push_env w_5 (Dynarray.get w_5.state.e 2);
          w_5.state.c <- pc_to_exp (int_to_pc 7)
      | c_19 when c_19 = tag_cont_3 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 2 tl_0;
          assert_env_length w_5 3;
          push_env w_5 (Dynarray.get w_5.state.e 2);
          w_5.state.c <- pc_to_exp (int_to_pc 8)
      | c_19 when c_19 = tag_cont_4 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 2 tl_0;
          assert_env_length w_5 3;
          push_env w_5 (Dynarray.get w_5.state.e 2);
          w_5.state.c <- pc_to_exp (int_to_pc 9)
      | c_19 when c_19 = tag_cont_5 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 2 tl_0;
          assert_env_length w_5 3;
          push_env w_5 (Dynarray.get w_5.state.e 2);
          w_5.state.c <- pc_to_exp (int_to_pc 10)
      | c_19 when c_19 = tag_cont_6 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 0 tl_0;
          w_5.state.c <- pc_to_exp (int_to_pc 11)
      | c_19 when c_19 = tag_cont_7 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 2 tl_0;
          assert_env_length w_5 3;
          push_env w_5 (Dynarray.get w_5.state.e 0);
          assert_env_length w_5 4;
          let ctor_arg_40 = pop_env w_5 in
          let ctor_arg_41 = pop_env w_5 in
          push_env w_5 (Memo.appends [ Memo.from_constructor tag_Cons; ctor_arg_41; ctor_arg_40 ]);
          assert_env_length w_5 3;
          ignore (env_call w_5 [] 2);
          w_5.state.c <- pc_to_exp (int_to_pc 4)
      | c_19 when c_19 = tag_cont_8 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 2 tl_0;
          assert_env_length w_5 3;
          push_env w_5 (Dynarray.get w_5.state.e 2);
          w_5.state.c <- pc_to_exp (int_to_pc 12)
      | c_19 when c_19 = tag_cont_9 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 3 tl_0;
          w_5.state.c <- pc_to_exp (int_to_pc 13)
      | c_19 when c_19 = tag_cont_10 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 2 tl_0;
          assert_env_length w_5 3;
          push_env w_5 (Dynarray.get w_5.state.e 1);
          assert_env_length w_5 4;
          push_env w_5 (Dynarray.get w_5.state.e 0);
          assert_env_length w_5 5;
          let keep_21 = env_call w_5 [ 2 ] 2 in
          w_5.state.k <- Memo.appends [ Memo.from_constructor tag_cont_22; keep_21; w_5.state.k ];
          w_5.state.c <- pc_to_exp (int_to_pc 4)
      | c_19 when c_19 = tag_cont_11 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 2 tl_0;
          assert_env_length w_5 3;
          push_env w_5 (Dynarray.get w_5.state.e 1);
          assert_env_length w_5 4;
          push_env w_5 (Dynarray.get w_5.state.e 0);
          assert_env_length w_5 5;
          let keep_22 = env_call w_5 [ 2 ] 2 in
          w_5.state.k <- Memo.appends [ Memo.from_constructor tag_cont_23; keep_22; w_5.state.k ];
          w_5.state.c <- pc_to_exp (int_to_pc 4)
      | c_19 when c_19 = tag_cont_12 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 0 tl_0;
          w_5.state.c <- pc_to_exp (int_to_pc 14)
      | c_19 when c_19 = tag_cont_13 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 0 tl_0;
          w_5.state.c <- pc_to_exp (int_to_pc 15)
      | c_19 when c_19 = tag_cont_14 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 3 tl_0;
          w_5.state.c <- pc_to_exp (int_to_pc 16)
      | c_19 when c_19 = tag_cont_15 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 2 tl_0;
          assert_env_length w_5 3;
          push_env w_5 (Dynarray.get w_5.state.e 2);
          w_5.state.c <- pc_to_exp (int_to_pc 18)
      | c_19 when c_19 = tag_cont_16 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 2 tl_0;
          assert_env_length w_5 3;
          push_env w_5 (Dynarray.get w_5.state.e 2);
          w_5.state.c <- pc_to_exp (int_to_pc 20)
      | c_19 when c_19 = tag_cont_17 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 2 tl_0;
          assert_env_length w_5 3;
          push_env w_5 (Dynarray.get w_5.state.e 2);
          w_5.state.c <- pc_to_exp (int_to_pc 22)
      | c_19 when c_19 = tag_cont_18 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 2 tl_0;
          assert_env_length w_5 3;
          push_env w_5 (Dynarray.get w_5.state.e 2);
          w_5.state.c <- pc_to_exp (int_to_pc 24)
      | c_19 when c_19 = tag_cont_19 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 2 tl_0;
          assert_env_length w_5 3;
          push_env w_5 (Dynarray.get w_5.state.e 2);
          w_5.state.c <- pc_to_exp (int_to_pc 26)
      | c_19 when c_19 = tag_cont_20 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 2 tl_0;
          assert_env_length w_5 3;
          push_env w_5 (Dynarray.get w_5.state.e 0);
          assert_env_length w_5 4;
          let ctor_arg_107 = pop_env w_5 in
          let ctor_arg_108 = pop_env w_5 in
          push_env w_5 (Memo.appends [ Memo.from_constructor tag_Cons; ctor_arg_108; ctor_arg_107 ]);
          assert_env_length w_5 3;
          ignore (env_call w_5 [] 2);
          w_5.state.c <- pc_to_exp (int_to_pc 4)
      | c_19 when c_19 = tag_cont_21 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 3 tl_0;
          assert_env_length w_5 4;
          push_env w_5 (Dynarray.get w_5.state.e 0);
          assert_env_length w_5 5;
          push_env w_5 (Dynarray.get w_5.state.e 1);
          assert_env_length w_5 6;
          let ctor_arg_109 = pop_env w_5 in
          let ctor_arg_110 = pop_env w_5 in
          push_env w_5 (Memo.appends [ Memo.from_constructor tag_Cons; ctor_arg_110; ctor_arg_109 ]);
          assert_env_length w_5 5;
          let ctor_arg_111 = pop_env w_5 in
          let ctor_arg_112 = pop_env w_5 in
          push_env w_5 (Memo.appends [ Memo.from_constructor tag_Cons; ctor_arg_112; ctor_arg_111 ]);
          assert_env_length w_5 4;
          ignore (env_call w_5 [] 2);
          w_5.state.c <- pc_to_exp (int_to_pc 4)
      | c_19 when c_19 = tag_cont_22 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 1 tl_0;
          assert_env_length w_5 2;
          let ctor_arg_113 = pop_env w_5 in
          let ctor_arg_114 = pop_env w_5 in
          push_env w_5 (Memo.appends [ Memo.from_constructor tag_VCons; ctor_arg_114; ctor_arg_113 ]);
          assert_env_length w_5 1;
          drop_n w_5 1 0;
          assert_env_length w_5 1;
          return_n w_5 1 (pc_to_exp (int_to_pc 0))
      | c_19 when c_19 = tag_cont_23 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 1 tl_0;
          assert_env_length w_5 2;
          let ctor_arg_115 = pop_env w_5 in
          let ctor_arg_116 = pop_env w_5 in
          push_env w_5 (Memo.appends [ Memo.from_constructor tag_VPair; ctor_arg_116; ctor_arg_115 ]);
          assert_env_length w_5 1;
          drop_n w_5 1 0;
          assert_env_length w_5 1;
          return_n w_5 1 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (0)")
    0;
  add_exp
    (fun w_0 ->
      assert_env_length w_0 2;
      push_env w_0 (Dynarray.get w_0.state.e 0);
      w_0.state.c <- pc_to_exp (int_to_pc 3))
    1;
  add_exp
    (fun w_2 ->
      assert_env_length w_2 5;
      let last_1 = Source.E 4 in
      let x_1 = resolve w_2 last_1 in
      match Word.get_value (fst x_1) with
      | c_1 when c_1 = tag_Z ->
          ignore (pop_env w_2);
          assert_env_length w_2 4;
          push_env w_2 (Dynarray.get w_2.state.e 2);
          assert_env_length w_2 5;
          let ctor_arg_0 = pop_env w_2 in
          push_env w_2 (Memo.appends [ Memo.from_constructor tag_Some; ctor_arg_0 ]);
          assert_env_length w_2 5;
          drop_n w_2 5 2;
          assert_env_length w_2 3;
          return_n w_2 3 (pc_to_exp (int_to_pc 0))
      | c_1 when c_1 = tag_S ->
          let splits_1 = Memo.splits (snd x_1) in
          let split0_1 = List.nth splits_1 0 in
          ignore (pop_env w_2);
          push_env w_2 split0_1;
          assert_env_length w_2 5;
          push_env w_2 (Dynarray.get w_2.state.e 3);
          assert_env_length w_2 6;
          push_env w_2 (Dynarray.get w_2.state.e 4);
          assert_env_length w_2 7;
          ignore (env_call w_2 [] 2);
          w_2.state.c <- pc_to_exp (int_to_pc 1)
      | _ -> failwith "unreachable (2)")
    2;
  add_exp
    (fun w_1 ->
      assert_env_length w_1 3;
      let last_0 = Source.E 2 in
      let x_0 = resolve w_1 last_0 in
      match Word.get_value (fst x_0) with
      | c_0 when c_0 = tag_Cons ->
          let splits_0 = Memo.splits (snd x_0) in
          let split0_0 = List.nth splits_0 0 in
          let split1_0 = List.nth splits_0 1 in
          ignore (pop_env w_1);
          push_env w_1 split0_0;
          push_env w_1 split1_0;
          assert_env_length w_1 4;
          push_env w_1 (Dynarray.get w_1.state.e 1);
          w_1.state.c <- pc_to_exp (int_to_pc 2)
      | _ ->
          ignore (pop_env w_1);
          assert_env_length w_1 2;
          push_env w_1 (Memo.from_constructor tag_None);
          assert_env_length w_1 3;
          return_n w_1 3 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (3)")
    3;
  add_exp
    (fun w_3 ->
      assert_env_length w_3 2;
      push_env w_3 (Dynarray.get w_3.state.e 0);
      w_3.state.c <- pc_to_exp (int_to_pc 5))
    4;
  add_exp
    (fun w_4 ->
      assert_env_length w_4 3;
      let last_2 = Source.E 2 in
      let x_2 = resolve w_4 last_2 in
      match Word.get_value (fst x_2) with
      | c_2 when c_2 = tag_EInt ->
          let splits_2 = Memo.splits (snd x_2) in
          let split0_2 = List.nth splits_2 0 in
          ignore (pop_env w_4);
          push_env w_4 split0_2;
          assert_env_length w_4 3;
          push_env w_4 (Dynarray.get w_4.state.e 2);
          assert_env_length w_4 4;
          let ctor_arg_1 = pop_env w_4 in
          push_env w_4 (Memo.appends [ Memo.from_constructor tag_VInt; ctor_arg_1 ]);
          assert_env_length w_4 4;
          drop_n w_4 4 1;
          assert_env_length w_4 3;
          return_n w_4 3 (pc_to_exp (int_to_pc 0))
      | c_2 when c_2 = tag_EPlus ->
          let splits_3 = Memo.splits (snd x_2) in
          let split0_3 = List.nth splits_3 0 in
          let split1_1 = List.nth splits_3 1 in
          ignore (pop_env w_4);
          push_env w_4 split0_3;
          push_env w_4 split1_1;
          assert_env_length w_4 4;
          push_env w_4 (Dynarray.get w_4.state.e 2);
          assert_env_length w_4 5;
          push_env w_4 (Dynarray.get w_4.state.e 1);
          assert_env_length w_4 6;
          let keep_0 = env_call w_4 [ 1; 3 ] 2 in
          w_4.state.k <- Memo.appends [ Memo.from_constructor tag_cont_1; keep_0; w_4.state.k ];
          w_4.state.c <- pc_to_exp (int_to_pc 4)
      | c_2 when c_2 = tag_ELt ->
          let splits_4 = Memo.splits (snd x_2) in
          let split0_4 = List.nth splits_4 0 in
          let split1_2 = List.nth splits_4 1 in
          ignore (pop_env w_4);
          push_env w_4 split0_4;
          push_env w_4 split1_2;
          assert_env_length w_4 4;
          push_env w_4 (Dynarray.get w_4.state.e 2);
          assert_env_length w_4 5;
          push_env w_4 (Dynarray.get w_4.state.e 1);
          assert_env_length w_4 6;
          let keep_1 = env_call w_4 [ 1; 3 ] 2 in
          w_4.state.k <- Memo.appends [ Memo.from_constructor tag_cont_2; keep_1; w_4.state.k ];
          w_4.state.c <- pc_to_exp (int_to_pc 4)
      | c_2 when c_2 = tag_ELe ->
          let splits_5 = Memo.splits (snd x_2) in
          let split0_5 = List.nth splits_5 0 in
          let split1_3 = List.nth splits_5 1 in
          ignore (pop_env w_4);
          push_env w_4 split0_5;
          push_env w_4 split1_3;
          assert_env_length w_4 4;
          push_env w_4 (Dynarray.get w_4.state.e 2);
          assert_env_length w_4 5;
          push_env w_4 (Dynarray.get w_4.state.e 1);
          assert_env_length w_4 6;
          let keep_2 = env_call w_4 [ 1; 3 ] 2 in
          w_4.state.k <- Memo.appends [ Memo.from_constructor tag_cont_3; keep_2; w_4.state.k ];
          w_4.state.c <- pc_to_exp (int_to_pc 4)
      | c_2 when c_2 = tag_EGt ->
          let splits_6 = Memo.splits (snd x_2) in
          let split0_6 = List.nth splits_6 0 in
          let split1_4 = List.nth splits_6 1 in
          ignore (pop_env w_4);
          push_env w_4 split0_6;
          push_env w_4 split1_4;
          assert_env_length w_4 4;
          push_env w_4 (Dynarray.get w_4.state.e 2);
          assert_env_length w_4 5;
          push_env w_4 (Dynarray.get w_4.state.e 1);
          assert_env_length w_4 6;
          let keep_3 = env_call w_4 [ 1; 3 ] 2 in
          w_4.state.k <- Memo.appends [ Memo.from_constructor tag_cont_4; keep_3; w_4.state.k ];
          w_4.state.c <- pc_to_exp (int_to_pc 4)
      | c_2 when c_2 = tag_EGe ->
          let splits_7 = Memo.splits (snd x_2) in
          let split0_7 = List.nth splits_7 0 in
          let split1_5 = List.nth splits_7 1 in
          ignore (pop_env w_4);
          push_env w_4 split0_7;
          push_env w_4 split1_5;
          assert_env_length w_4 4;
          push_env w_4 (Dynarray.get w_4.state.e 2);
          assert_env_length w_4 5;
          push_env w_4 (Dynarray.get w_4.state.e 1);
          assert_env_length w_4 6;
          let keep_4 = env_call w_4 [ 1; 3 ] 2 in
          w_4.state.k <- Memo.appends [ Memo.from_constructor tag_cont_5; keep_4; w_4.state.k ];
          w_4.state.c <- pc_to_exp (int_to_pc 4)
      | c_2 when c_2 = tag_EVar ->
          let splits_8 = Memo.splits (snd x_2) in
          let split0_8 = List.nth splits_8 0 in
          ignore (pop_env w_4);
          push_env w_4 split0_8;
          assert_env_length w_4 3;
          push_env w_4 (Dynarray.get w_4.state.e 1);
          assert_env_length w_4 4;
          push_env w_4 (Dynarray.get w_4.state.e 2);
          assert_env_length w_4 5;
          let keep_5 = env_call w_4 [] 2 in
          w_4.state.k <- Memo.appends [ Memo.from_constructor tag_cont_6; keep_5; w_4.state.k ];
          w_4.state.c <- pc_to_exp (int_to_pc 1)
      | c_2 when c_2 = tag_EAbs ->
          let splits_9 = Memo.splits (snd x_2) in
          let split0_9 = List.nth splits_9 0 in
          ignore (pop_env w_4);
          push_env w_4 split0_9;
          assert_env_length w_4 3;
          push_env w_4 (Dynarray.get w_4.state.e 2);
          assert_env_length w_4 4;
          push_env w_4 (Dynarray.get w_4.state.e 1);
          assert_env_length w_4 5;
          let ctor_arg_2 = pop_env w_4 in
          let ctor_arg_3 = pop_env w_4 in
          push_env w_4 (Memo.appends [ Memo.from_constructor tag_VAbs; ctor_arg_3; ctor_arg_2 ]);
          assert_env_length w_4 4;
          drop_n w_4 4 1;
          assert_env_length w_4 3;
          return_n w_4 3 (pc_to_exp (int_to_pc 0))
      | c_2 when c_2 = tag_ELet ->
          let splits_10 = Memo.splits (snd x_2) in
          let split0_10 = List.nth splits_10 0 in
          let split1_6 = List.nth splits_10 1 in
          ignore (pop_env w_4);
          push_env w_4 split0_10;
          push_env w_4 split1_6;
          assert_env_length w_4 4;
          push_env w_4 (Dynarray.get w_4.state.e 3);
          assert_env_length w_4 5;
          push_env w_4 (Dynarray.get w_4.state.e 2);
          assert_env_length w_4 6;
          push_env w_4 (Dynarray.get w_4.state.e 1);
          assert_env_length w_4 7;
          let keep_6 = env_call w_4 [ 1; 4 ] 2 in
          w_4.state.k <- Memo.appends [ Memo.from_constructor tag_cont_7; keep_6; w_4.state.k ];
          w_4.state.c <- pc_to_exp (int_to_pc 4)
      | c_2 when c_2 = tag_EFix ->
          let splits_11 = Memo.splits (snd x_2) in
          let split0_11 = List.nth splits_11 0 in
          ignore (pop_env w_4);
          push_env w_4 split0_11;
          assert_env_length w_4 3;
          push_env w_4 (Dynarray.get w_4.state.e 2);
          assert_env_length w_4 4;
          push_env w_4 (Dynarray.get w_4.state.e 1);
          assert_env_length w_4 5;
          let ctor_arg_4 = pop_env w_4 in
          let ctor_arg_5 = pop_env w_4 in
          push_env w_4 (Memo.appends [ Memo.from_constructor tag_VFix; ctor_arg_5; ctor_arg_4 ]);
          assert_env_length w_4 4;
          drop_n w_4 4 1;
          assert_env_length w_4 3;
          return_n w_4 3 (pc_to_exp (int_to_pc 0))
      | c_2 when c_2 = tag_EApp ->
          let splits_12 = Memo.splits (snd x_2) in
          let split0_12 = List.nth splits_12 0 in
          let split1_7 = List.nth splits_12 1 in
          ignore (pop_env w_4);
          push_env w_4 split0_12;
          push_env w_4 split1_7;
          assert_env_length w_4 4;
          push_env w_4 (Dynarray.get w_4.state.e 2);
          assert_env_length w_4 5;
          push_env w_4 (Dynarray.get w_4.state.e 1);
          assert_env_length w_4 6;
          let keep_7 = env_call w_4 [ 1; 3 ] 2 in
          w_4.state.k <- Memo.appends [ Memo.from_constructor tag_cont_8; keep_7; w_4.state.k ];
          w_4.state.c <- pc_to_exp (int_to_pc 4)
      | c_2 when c_2 = tag_EHole ->
          let splits_13 = Memo.splits (snd x_2) in
          let split0_13 = List.nth splits_13 0 in
          ignore (pop_env w_4);
          push_env w_4 split0_13;
          assert_env_length w_4 3;
          push_env w_4 (Dynarray.get w_4.state.e 2);
          assert_env_length w_4 4;
          push_env w_4 (Dynarray.get w_4.state.e 1);
          assert_env_length w_4 5;
          let ctor_arg_6 = pop_env w_4 in
          let ctor_arg_7 = pop_env w_4 in
          push_env w_4 (Memo.appends [ Memo.from_constructor tag_SHole; ctor_arg_7; ctor_arg_6 ]);
          assert_env_length w_4 4;
          let ctor_arg_8 = pop_env w_4 in
          push_env w_4 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_8 ]);
          assert_env_length w_4 4;
          drop_n w_4 4 1;
          assert_env_length w_4 3;
          return_n w_4 3 (pc_to_exp (int_to_pc 0))
      | c_2 when c_2 = tag_ETrue ->
          ignore (pop_env w_4);
          assert_env_length w_4 2;
          push_env w_4 (Memo.from_constructor tag_VTrue);
          assert_env_length w_4 3;
          return_n w_4 3 (pc_to_exp (int_to_pc 0))
      | c_2 when c_2 = tag_EFalse ->
          ignore (pop_env w_4);
          assert_env_length w_4 2;
          push_env w_4 (Memo.from_constructor tag_VFalse);
          assert_env_length w_4 3;
          return_n w_4 3 (pc_to_exp (int_to_pc 0))
      | c_2 when c_2 = tag_EIf ->
          let splits_14 = Memo.splits (snd x_2) in
          let split0_14 = List.nth splits_14 0 in
          let split1_8 = List.nth splits_14 1 in
          let split2_0 = List.nth splits_14 2 in
          ignore (pop_env w_4);
          push_env w_4 split0_14;
          push_env w_4 split1_8;
          push_env w_4 split2_0;
          assert_env_length w_4 5;
          push_env w_4 (Dynarray.get w_4.state.e 2);
          assert_env_length w_4 6;
          push_env w_4 (Dynarray.get w_4.state.e 1);
          assert_env_length w_4 7;
          let keep_8 = env_call w_4 [ 1; 3; 4 ] 2 in
          w_4.state.k <- Memo.appends [ Memo.from_constructor tag_cont_9; keep_8; w_4.state.k ];
          w_4.state.c <- pc_to_exp (int_to_pc 4)
      | c_2 when c_2 = tag_ENil ->
          ignore (pop_env w_4);
          assert_env_length w_4 2;
          push_env w_4 (Memo.from_constructor tag_VNil);
          assert_env_length w_4 3;
          return_n w_4 3 (pc_to_exp (int_to_pc 0))
      | c_2 when c_2 = tag_ECons ->
          let splits_15 = Memo.splits (snd x_2) in
          let split0_15 = List.nth splits_15 0 in
          let split1_9 = List.nth splits_15 1 in
          ignore (pop_env w_4);
          push_env w_4 split0_15;
          push_env w_4 split1_9;
          assert_env_length w_4 4;
          push_env w_4 (Dynarray.get w_4.state.e 2);
          assert_env_length w_4 5;
          push_env w_4 (Dynarray.get w_4.state.e 1);
          assert_env_length w_4 6;
          let keep_9 = env_call w_4 [ 1; 3 ] 2 in
          w_4.state.k <- Memo.appends [ Memo.from_constructor tag_cont_10; keep_9; w_4.state.k ];
          w_4.state.c <- pc_to_exp (int_to_pc 4)
      | c_2 when c_2 = tag_EPair ->
          let splits_16 = Memo.splits (snd x_2) in
          let split0_16 = List.nth splits_16 0 in
          let split1_10 = List.nth splits_16 1 in
          ignore (pop_env w_4);
          push_env w_4 split0_16;
          push_env w_4 split1_10;
          assert_env_length w_4 4;
          push_env w_4 (Dynarray.get w_4.state.e 2);
          assert_env_length w_4 5;
          push_env w_4 (Dynarray.get w_4.state.e 1);
          assert_env_length w_4 6;
          let keep_10 = env_call w_4 [ 1; 3 ] 2 in
          w_4.state.k <- Memo.appends [ Memo.from_constructor tag_cont_11; keep_10; w_4.state.k ];
          w_4.state.c <- pc_to_exp (int_to_pc 4)
      | c_2 when c_2 = tag_EZro ->
          let splits_17 = Memo.splits (snd x_2) in
          let split0_17 = List.nth splits_17 0 in
          ignore (pop_env w_4);
          push_env w_4 split0_17;
          assert_env_length w_4 3;
          push_env w_4 (Dynarray.get w_4.state.e 2);
          assert_env_length w_4 4;
          push_env w_4 (Dynarray.get w_4.state.e 1);
          assert_env_length w_4 5;
          let keep_11 = env_call w_4 [] 2 in
          w_4.state.k <- Memo.appends [ Memo.from_constructor tag_cont_12; keep_11; w_4.state.k ];
          w_4.state.c <- pc_to_exp (int_to_pc 4)
      | c_2 when c_2 = tag_EFst ->
          let splits_18 = Memo.splits (snd x_2) in
          let split0_18 = List.nth splits_18 0 in
          ignore (pop_env w_4);
          push_env w_4 split0_18;
          assert_env_length w_4 3;
          push_env w_4 (Dynarray.get w_4.state.e 2);
          assert_env_length w_4 4;
          push_env w_4 (Dynarray.get w_4.state.e 1);
          assert_env_length w_4 5;
          let keep_12 = env_call w_4 [] 2 in
          w_4.state.k <- Memo.appends [ Memo.from_constructor tag_cont_13; keep_12; w_4.state.k ];
          w_4.state.c <- pc_to_exp (int_to_pc 4)
      | c_2 when c_2 = tag_EMatchList ->
          let splits_19 = Memo.splits (snd x_2) in
          let split0_19 = List.nth splits_19 0 in
          let split1_11 = List.nth splits_19 1 in
          let split2_1 = List.nth splits_19 2 in
          ignore (pop_env w_4);
          push_env w_4 split0_19;
          push_env w_4 split1_11;
          push_env w_4 split2_1;
          assert_env_length w_4 5;
          push_env w_4 (Dynarray.get w_4.state.e 2);
          assert_env_length w_4 6;
          push_env w_4 (Dynarray.get w_4.state.e 1);
          assert_env_length w_4 7;
          let keep_13 = env_call w_4 [ 1; 3; 4 ] 2 in
          w_4.state.k <- Memo.appends [ Memo.from_constructor tag_cont_14; keep_13; w_4.state.k ];
          w_4.state.c <- pc_to_exp (int_to_pc 4)
      | c_2 when c_2 = tag_EUnit ->
          ignore (pop_env w_4);
          assert_env_length w_4 2;
          push_env w_4 (Memo.from_constructor tag_VUnit);
          assert_env_length w_4 3;
          return_n w_4 3 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (5)")
    5;
  add_exp
    (fun w_6 ->
      assert_env_length w_6 4;
      let last_3 = Source.E 3 in
      let x_3 = resolve w_6 last_3 in
      match Word.get_value (fst x_3) with
      | c_3 when c_3 = tag_VInt ->
          let splits_20 = Memo.splits (snd x_3) in
          let split0_20 = List.nth splits_20 0 in
          ignore (pop_env w_6);
          push_env w_6 split0_20;
          assert_env_length w_6 4;
          push_env w_6 (Dynarray.get w_6.state.e 1);
          assert_env_length w_6 5;
          push_env w_6 (Dynarray.get w_6.state.e 0);
          assert_env_length w_6 6;
          let keep_14 = env_call w_6 [ 2; 3 ] 2 in
          w_6.state.k <- Memo.appends [ Memo.from_constructor tag_cont_15; keep_14; w_6.state.k ];
          w_6.state.c <- pc_to_exp (int_to_pc 4)
      | c_3 when c_3 = tag_VStuck ->
          let splits_21 = Memo.splits (snd x_3) in
          let split0_21 = List.nth splits_21 0 in
          ignore (pop_env w_6);
          push_env w_6 split0_21;
          assert_env_length w_6 4;
          push_env w_6 (Dynarray.get w_6.state.e 3);
          assert_env_length w_6 5;
          push_env w_6 (Dynarray.get w_6.state.e 1);
          assert_env_length w_6 6;
          let ctor_arg_9 = pop_env w_6 in
          let ctor_arg_10 = pop_env w_6 in
          push_env w_6 (Memo.appends [ Memo.from_constructor tag_SAdd0; ctor_arg_10; ctor_arg_9 ]);
          assert_env_length w_6 5;
          let ctor_arg_11 = pop_env w_6 in
          push_env w_6 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_11 ]);
          assert_env_length w_6 5;
          drop_n w_6 5 1;
          assert_env_length w_6 4;
          drop_n w_6 4 1;
          assert_env_length w_6 3;
          drop_n w_6 3 1;
          assert_env_length w_6 2;
          return_n w_6 2 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_6);
          assert_env_length w_6 3;
          push_env w_6 (Dynarray.get w_6.state.e 2);
          assert_env_length w_6 4;
          push_env w_6 (Memo.from_constructor tag_VTInt);
          assert_env_length w_6 5;
          let ctor_arg_12 = pop_env w_6 in
          let ctor_arg_13 = pop_env w_6 in
          push_env w_6 (Memo.appends [ Memo.from_constructor tag_STypeError; ctor_arg_13; ctor_arg_12 ]);
          assert_env_length w_6 4;
          let ctor_arg_14 = pop_env w_6 in
          push_env w_6 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_14 ]);
          assert_env_length w_6 4;
          drop_n w_6 4 1;
          assert_env_length w_6 3;
          drop_n w_6 3 1;
          assert_env_length w_6 2;
          return_n w_6 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (6)")
    6;
  add_exp
    (fun w_7 ->
      assert_env_length w_7 4;
      let last_4 = Source.E 3 in
      let x_4 = resolve w_7 last_4 in
      match Word.get_value (fst x_4) with
      | c_4 when c_4 = tag_VInt ->
          let splits_22 = Memo.splits (snd x_4) in
          let split0_22 = List.nth splits_22 0 in
          ignore (pop_env w_7);
          push_env w_7 split0_22;
          assert_env_length w_7 4;
          push_env w_7 (Dynarray.get w_7.state.e 1);
          assert_env_length w_7 5;
          push_env w_7 (Dynarray.get w_7.state.e 0);
          assert_env_length w_7 6;
          let keep_15 = env_call w_7 [ 2; 3 ] 2 in
          w_7.state.k <- Memo.appends [ Memo.from_constructor tag_cont_16; keep_15; w_7.state.k ];
          w_7.state.c <- pc_to_exp (int_to_pc 4)
      | c_4 when c_4 = tag_VStuck ->
          let splits_23 = Memo.splits (snd x_4) in
          let split0_23 = List.nth splits_23 0 in
          ignore (pop_env w_7);
          push_env w_7 split0_23;
          assert_env_length w_7 4;
          push_env w_7 (Dynarray.get w_7.state.e 3);
          assert_env_length w_7 5;
          push_env w_7 (Dynarray.get w_7.state.e 1);
          assert_env_length w_7 6;
          let ctor_arg_15 = pop_env w_7 in
          let ctor_arg_16 = pop_env w_7 in
          push_env w_7 (Memo.appends [ Memo.from_constructor tag_SGt0; ctor_arg_16; ctor_arg_15 ]);
          assert_env_length w_7 5;
          let ctor_arg_17 = pop_env w_7 in
          push_env w_7 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_17 ]);
          assert_env_length w_7 5;
          drop_n w_7 5 1;
          assert_env_length w_7 4;
          drop_n w_7 4 1;
          assert_env_length w_7 3;
          drop_n w_7 3 1;
          assert_env_length w_7 2;
          return_n w_7 2 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_7);
          assert_env_length w_7 3;
          push_env w_7 (Dynarray.get w_7.state.e 2);
          assert_env_length w_7 4;
          push_env w_7 (Memo.from_constructor tag_VTInt);
          assert_env_length w_7 5;
          let ctor_arg_18 = pop_env w_7 in
          let ctor_arg_19 = pop_env w_7 in
          push_env w_7 (Memo.appends [ Memo.from_constructor tag_STypeError; ctor_arg_19; ctor_arg_18 ]);
          assert_env_length w_7 4;
          let ctor_arg_20 = pop_env w_7 in
          push_env w_7 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_20 ]);
          assert_env_length w_7 4;
          drop_n w_7 4 1;
          assert_env_length w_7 3;
          drop_n w_7 3 1;
          assert_env_length w_7 2;
          return_n w_7 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (7)")
    7;
  add_exp
    (fun w_8 ->
      assert_env_length w_8 4;
      let last_5 = Source.E 3 in
      let x_5 = resolve w_8 last_5 in
      match Word.get_value (fst x_5) with
      | c_5 when c_5 = tag_VInt ->
          let splits_24 = Memo.splits (snd x_5) in
          let split0_24 = List.nth splits_24 0 in
          ignore (pop_env w_8);
          push_env w_8 split0_24;
          assert_env_length w_8 4;
          push_env w_8 (Dynarray.get w_8.state.e 1);
          assert_env_length w_8 5;
          push_env w_8 (Dynarray.get w_8.state.e 0);
          assert_env_length w_8 6;
          let keep_16 = env_call w_8 [ 2; 3 ] 2 in
          w_8.state.k <- Memo.appends [ Memo.from_constructor tag_cont_17; keep_16; w_8.state.k ];
          w_8.state.c <- pc_to_exp (int_to_pc 4)
      | c_5 when c_5 = tag_VStuck ->
          let splits_25 = Memo.splits (snd x_5) in
          let split0_25 = List.nth splits_25 0 in
          ignore (pop_env w_8);
          push_env w_8 split0_25;
          assert_env_length w_8 4;
          push_env w_8 (Dynarray.get w_8.state.e 3);
          assert_env_length w_8 5;
          push_env w_8 (Dynarray.get w_8.state.e 1);
          assert_env_length w_8 6;
          let ctor_arg_21 = pop_env w_8 in
          let ctor_arg_22 = pop_env w_8 in
          push_env w_8 (Memo.appends [ Memo.from_constructor tag_SGt0; ctor_arg_22; ctor_arg_21 ]);
          assert_env_length w_8 5;
          let ctor_arg_23 = pop_env w_8 in
          push_env w_8 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_23 ]);
          assert_env_length w_8 5;
          drop_n w_8 5 1;
          assert_env_length w_8 4;
          drop_n w_8 4 1;
          assert_env_length w_8 3;
          drop_n w_8 3 1;
          assert_env_length w_8 2;
          return_n w_8 2 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_8);
          assert_env_length w_8 3;
          push_env w_8 (Dynarray.get w_8.state.e 2);
          assert_env_length w_8 4;
          push_env w_8 (Memo.from_constructor tag_VTInt);
          assert_env_length w_8 5;
          let ctor_arg_24 = pop_env w_8 in
          let ctor_arg_25 = pop_env w_8 in
          push_env w_8 (Memo.appends [ Memo.from_constructor tag_STypeError; ctor_arg_25; ctor_arg_24 ]);
          assert_env_length w_8 4;
          let ctor_arg_26 = pop_env w_8 in
          push_env w_8 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_26 ]);
          assert_env_length w_8 4;
          drop_n w_8 4 1;
          assert_env_length w_8 3;
          drop_n w_8 3 1;
          assert_env_length w_8 2;
          return_n w_8 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (8)")
    8;
  add_exp
    (fun w_9 ->
      assert_env_length w_9 4;
      let last_6 = Source.E 3 in
      let x_6 = resolve w_9 last_6 in
      match Word.get_value (fst x_6) with
      | c_6 when c_6 = tag_VInt ->
          let splits_26 = Memo.splits (snd x_6) in
          let split0_26 = List.nth splits_26 0 in
          ignore (pop_env w_9);
          push_env w_9 split0_26;
          assert_env_length w_9 4;
          push_env w_9 (Dynarray.get w_9.state.e 1);
          assert_env_length w_9 5;
          push_env w_9 (Dynarray.get w_9.state.e 0);
          assert_env_length w_9 6;
          let keep_17 = env_call w_9 [ 2; 3 ] 2 in
          w_9.state.k <- Memo.appends [ Memo.from_constructor tag_cont_18; keep_17; w_9.state.k ];
          w_9.state.c <- pc_to_exp (int_to_pc 4)
      | c_6 when c_6 = tag_VStuck ->
          let splits_27 = Memo.splits (snd x_6) in
          let split0_27 = List.nth splits_27 0 in
          ignore (pop_env w_9);
          push_env w_9 split0_27;
          assert_env_length w_9 4;
          push_env w_9 (Dynarray.get w_9.state.e 3);
          assert_env_length w_9 5;
          push_env w_9 (Dynarray.get w_9.state.e 1);
          assert_env_length w_9 6;
          let ctor_arg_27 = pop_env w_9 in
          let ctor_arg_28 = pop_env w_9 in
          push_env w_9 (Memo.appends [ Memo.from_constructor tag_SGt0; ctor_arg_28; ctor_arg_27 ]);
          assert_env_length w_9 5;
          let ctor_arg_29 = pop_env w_9 in
          push_env w_9 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_29 ]);
          assert_env_length w_9 5;
          drop_n w_9 5 1;
          assert_env_length w_9 4;
          drop_n w_9 4 1;
          assert_env_length w_9 3;
          drop_n w_9 3 1;
          assert_env_length w_9 2;
          return_n w_9 2 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_9);
          assert_env_length w_9 3;
          push_env w_9 (Dynarray.get w_9.state.e 2);
          assert_env_length w_9 4;
          push_env w_9 (Memo.from_constructor tag_VTInt);
          assert_env_length w_9 5;
          let ctor_arg_30 = pop_env w_9 in
          let ctor_arg_31 = pop_env w_9 in
          push_env w_9 (Memo.appends [ Memo.from_constructor tag_STypeError; ctor_arg_31; ctor_arg_30 ]);
          assert_env_length w_9 4;
          let ctor_arg_32 = pop_env w_9 in
          push_env w_9 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_32 ]);
          assert_env_length w_9 4;
          drop_n w_9 4 1;
          assert_env_length w_9 3;
          drop_n w_9 3 1;
          assert_env_length w_9 2;
          return_n w_9 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (9)")
    9;
  add_exp
    (fun w_10 ->
      assert_env_length w_10 4;
      let last_7 = Source.E 3 in
      let x_7 = resolve w_10 last_7 in
      match Word.get_value (fst x_7) with
      | c_7 when c_7 = tag_VInt ->
          let splits_28 = Memo.splits (snd x_7) in
          let split0_28 = List.nth splits_28 0 in
          ignore (pop_env w_10);
          push_env w_10 split0_28;
          assert_env_length w_10 4;
          push_env w_10 (Dynarray.get w_10.state.e 1);
          assert_env_length w_10 5;
          push_env w_10 (Dynarray.get w_10.state.e 0);
          assert_env_length w_10 6;
          let keep_18 = env_call w_10 [ 2; 3 ] 2 in
          w_10.state.k <- Memo.appends [ Memo.from_constructor tag_cont_19; keep_18; w_10.state.k ];
          w_10.state.c <- pc_to_exp (int_to_pc 4)
      | c_7 when c_7 = tag_VStuck ->
          let splits_29 = Memo.splits (snd x_7) in
          let split0_29 = List.nth splits_29 0 in
          ignore (pop_env w_10);
          push_env w_10 split0_29;
          assert_env_length w_10 4;
          push_env w_10 (Dynarray.get w_10.state.e 3);
          assert_env_length w_10 5;
          push_env w_10 (Dynarray.get w_10.state.e 1);
          assert_env_length w_10 6;
          let ctor_arg_33 = pop_env w_10 in
          let ctor_arg_34 = pop_env w_10 in
          push_env w_10 (Memo.appends [ Memo.from_constructor tag_SGt0; ctor_arg_34; ctor_arg_33 ]);
          assert_env_length w_10 5;
          let ctor_arg_35 = pop_env w_10 in
          push_env w_10 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_35 ]);
          assert_env_length w_10 5;
          drop_n w_10 5 1;
          assert_env_length w_10 4;
          drop_n w_10 4 1;
          assert_env_length w_10 3;
          drop_n w_10 3 1;
          assert_env_length w_10 2;
          return_n w_10 2 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_10);
          assert_env_length w_10 3;
          push_env w_10 (Dynarray.get w_10.state.e 2);
          assert_env_length w_10 4;
          push_env w_10 (Memo.from_constructor tag_VTInt);
          assert_env_length w_10 5;
          let ctor_arg_36 = pop_env w_10 in
          let ctor_arg_37 = pop_env w_10 in
          push_env w_10 (Memo.appends [ Memo.from_constructor tag_STypeError; ctor_arg_37; ctor_arg_36 ]);
          assert_env_length w_10 4;
          let ctor_arg_38 = pop_env w_10 in
          push_env w_10 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_38 ]);
          assert_env_length w_10 4;
          drop_n w_10 4 1;
          assert_env_length w_10 3;
          drop_n w_10 3 1;
          assert_env_length w_10 2;
          return_n w_10 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (10)")
    10;
  add_exp
    (fun w_11 ->
      assert_env_length w_11 1;
      let last_8 = Source.E 0 in
      let x_8 = resolve w_11 last_8 in
      match Word.get_value (fst x_8) with
      | c_8 when c_8 = tag_Some ->
          let splits_30 = Memo.splits (snd x_8) in
          let split0_30 = List.nth splits_30 0 in
          ignore (pop_env w_11);
          push_env w_11 split0_30;
          assert_env_length w_11 1;
          push_env w_11 (Dynarray.get w_11.state.e 0);
          assert_env_length w_11 2;
          drop_n w_11 2 1;
          assert_env_length w_11 1;
          drop_n w_11 1 0;
          assert_env_length w_11 1;
          return_n w_11 1 (pc_to_exp (int_to_pc 0))
      | c_8 when c_8 = tag_None ->
          ignore (pop_env w_11);
          assert_env_length w_11 0;
          push_env w_11 (Memo.from_constructor tag_SIndexError);
          assert_env_length w_11 1;
          let ctor_arg_39 = pop_env w_11 in
          push_env w_11 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_39 ]);
          assert_env_length w_11 1;
          drop_n w_11 1 0;
          assert_env_length w_11 1;
          return_n w_11 1 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (11)")
    11;
  add_exp
    (fun w_12 ->
      assert_env_length w_12 4;
      let last_9 = Source.E 3 in
      let x_9 = resolve w_12 last_9 in
      match Word.get_value (fst x_9) with
      | c_9 when c_9 = tag_VAbs ->
          let splits_31 = Memo.splits (snd x_9) in
          let split0_31 = List.nth splits_31 0 in
          let split1_12 = List.nth splits_31 1 in
          ignore (pop_env w_12);
          push_env w_12 split0_31;
          push_env w_12 split1_12;
          assert_env_length w_12 5;
          push_env w_12 (Dynarray.get w_12.state.e 3);
          assert_env_length w_12 6;
          push_env w_12 (Dynarray.get w_12.state.e 1);
          assert_env_length w_12 7;
          push_env w_12 (Dynarray.get w_12.state.e 0);
          assert_env_length w_12 8;
          let keep_19 = env_call w_12 [ 4; 5 ] 2 in
          w_12.state.k <- Memo.appends [ Memo.from_constructor tag_cont_20; keep_19; w_12.state.k ];
          w_12.state.c <- pc_to_exp (int_to_pc 4)
      | c_9 when c_9 = tag_VFix ->
          let splits_32 = Memo.splits (snd x_9) in
          let split0_32 = List.nth splits_32 0 in
          let split1_13 = List.nth splits_32 1 in
          ignore (pop_env w_12);
          push_env w_12 split0_32;
          push_env w_12 split1_13;
          assert_env_length w_12 5;
          push_env w_12 (Dynarray.get w_12.state.e 3);
          assert_env_length w_12 6;
          push_env w_12 (Dynarray.get w_12.state.e 1);
          assert_env_length w_12 7;
          push_env w_12 (Dynarray.get w_12.state.e 0);
          assert_env_length w_12 8;
          let keep_20 = env_call w_12 [ 2; 4; 5 ] 2 in
          w_12.state.k <- Memo.appends [ Memo.from_constructor tag_cont_21; keep_20; w_12.state.k ];
          w_12.state.c <- pc_to_exp (int_to_pc 4)
      | c_9 when c_9 = tag_VStuck ->
          let splits_33 = Memo.splits (snd x_9) in
          let split0_33 = List.nth splits_33 0 in
          ignore (pop_env w_12);
          push_env w_12 split0_33;
          assert_env_length w_12 4;
          push_env w_12 (Dynarray.get w_12.state.e 3);
          assert_env_length w_12 5;
          push_env w_12 (Dynarray.get w_12.state.e 1);
          assert_env_length w_12 6;
          let ctor_arg_42 = pop_env w_12 in
          let ctor_arg_43 = pop_env w_12 in
          push_env w_12 (Memo.appends [ Memo.from_constructor tag_SApp; ctor_arg_43; ctor_arg_42 ]);
          assert_env_length w_12 5;
          let ctor_arg_44 = pop_env w_12 in
          push_env w_12 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_44 ]);
          assert_env_length w_12 5;
          drop_n w_12 5 1;
          assert_env_length w_12 4;
          drop_n w_12 4 1;
          assert_env_length w_12 3;
          drop_n w_12 3 1;
          assert_env_length w_12 2;
          return_n w_12 2 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_12);
          assert_env_length w_12 3;
          push_env w_12 (Dynarray.get w_12.state.e 2);
          assert_env_length w_12 4;
          push_env w_12 (Memo.from_constructor tag_VTFunc);
          assert_env_length w_12 5;
          let ctor_arg_45 = pop_env w_12 in
          let ctor_arg_46 = pop_env w_12 in
          push_env w_12 (Memo.appends [ Memo.from_constructor tag_STypeError; ctor_arg_46; ctor_arg_45 ]);
          assert_env_length w_12 4;
          let ctor_arg_47 = pop_env w_12 in
          push_env w_12 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_47 ]);
          assert_env_length w_12 4;
          drop_n w_12 4 1;
          assert_env_length w_12 3;
          drop_n w_12 3 1;
          assert_env_length w_12 2;
          return_n w_12 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (12)")
    12;
  add_exp
    (fun w_13 ->
      assert_env_length w_13 4;
      let last_10 = Source.E 3 in
      let x_10 = resolve w_13 last_10 in
      match Word.get_value (fst x_10) with
      | c_10 when c_10 = tag_VTrue ->
          ignore (pop_env w_13);
          assert_env_length w_13 3;
          push_env w_13 (Dynarray.get w_13.state.e 1);
          assert_env_length w_13 4;
          push_env w_13 (Dynarray.get w_13.state.e 0);
          assert_env_length w_13 5;
          ignore (env_call w_13 [] 2);
          w_13.state.c <- pc_to_exp (int_to_pc 4)
      | c_10 when c_10 = tag_VFalse ->
          ignore (pop_env w_13);
          assert_env_length w_13 3;
          push_env w_13 (Dynarray.get w_13.state.e 2);
          assert_env_length w_13 4;
          push_env w_13 (Dynarray.get w_13.state.e 0);
          assert_env_length w_13 5;
          ignore (env_call w_13 [] 2);
          w_13.state.c <- pc_to_exp (int_to_pc 4)
      | c_10 when c_10 = tag_VStuck ->
          let splits_34 = Memo.splits (snd x_10) in
          let split0_34 = List.nth splits_34 0 in
          ignore (pop_env w_13);
          push_env w_13 split0_34;
          assert_env_length w_13 4;
          push_env w_13 (Dynarray.get w_13.state.e 3);
          assert_env_length w_13 5;
          push_env w_13 (Dynarray.get w_13.state.e 1);
          assert_env_length w_13 6;
          push_env w_13 (Dynarray.get w_13.state.e 2);
          assert_env_length w_13 7;
          let ctor_arg_48 = pop_env w_13 in
          let ctor_arg_49 = pop_env w_13 in
          let ctor_arg_50 = pop_env w_13 in
          push_env w_13 (Memo.appends [ Memo.from_constructor tag_SIf; ctor_arg_50; ctor_arg_49; ctor_arg_48 ]);
          assert_env_length w_13 5;
          let ctor_arg_51 = pop_env w_13 in
          push_env w_13 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_51 ]);
          assert_env_length w_13 5;
          drop_n w_13 5 1;
          assert_env_length w_13 4;
          drop_n w_13 4 2;
          assert_env_length w_13 2;
          return_n w_13 2 (pc_to_exp (int_to_pc 0))
      | iv ->
          push_env w_13 (Dynarray.get w_13.state.e 3);
          ignore (pop_env w_13);
          assert_env_length w_13 4;
          push_env w_13 (Dynarray.get w_13.state.e 3);
          assert_env_length w_13 5;
          push_env w_13 (Memo.from_constructor tag_VTBool);
          assert_env_length w_13 6;
          let ctor_arg_52 = pop_env w_13 in
          let ctor_arg_53 = pop_env w_13 in
          push_env w_13 (Memo.appends [ Memo.from_constructor tag_STypeError; ctor_arg_53; ctor_arg_52 ]);
          assert_env_length w_13 5;
          let ctor_arg_54 = pop_env w_13 in
          push_env w_13 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_54 ]);
          assert_env_length w_13 5;
          drop_n w_13 5 1;
          assert_env_length w_13 4;
          drop_n w_13 4 2;
          assert_env_length w_13 2;
          return_n w_13 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (13)")
    13;
  add_exp
    (fun w_14 ->
      assert_env_length w_14 1;
      let last_11 = Source.E 0 in
      let x_11 = resolve w_14 last_11 in
      match Word.get_value (fst x_11) with
      | c_11 when c_11 = tag_VPair ->
          let splits_35 = Memo.splits (snd x_11) in
          let split0_35 = List.nth splits_35 0 in
          let split1_14 = List.nth splits_35 1 in
          ignore (pop_env w_14);
          push_env w_14 split0_35;
          push_env w_14 split1_14;
          assert_env_length w_14 2;
          push_env w_14 (Dynarray.get w_14.state.e 0);
          assert_env_length w_14 3;
          drop_n w_14 3 2;
          assert_env_length w_14 1;
          drop_n w_14 1 0;
          assert_env_length w_14 1;
          return_n w_14 1 (pc_to_exp (int_to_pc 0))
      | c_11 when c_11 = tag_VStuck ->
          let splits_36 = Memo.splits (snd x_11) in
          let split0_36 = List.nth splits_36 0 in
          ignore (pop_env w_14);
          push_env w_14 split0_36;
          assert_env_length w_14 1;
          push_env w_14 (Dynarray.get w_14.state.e 0);
          assert_env_length w_14 2;
          let ctor_arg_55 = pop_env w_14 in
          push_env w_14 (Memo.appends [ Memo.from_constructor tag_SZro; ctor_arg_55 ]);
          assert_env_length w_14 2;
          let ctor_arg_56 = pop_env w_14 in
          push_env w_14 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_56 ]);
          assert_env_length w_14 2;
          drop_n w_14 2 1;
          assert_env_length w_14 1;
          drop_n w_14 1 0;
          assert_env_length w_14 1;
          return_n w_14 1 (pc_to_exp (int_to_pc 0))
      | pv ->
          push_env w_14 (Dynarray.get w_14.state.e 0);
          ignore (pop_env w_14);
          assert_env_length w_14 1;
          push_env w_14 (Dynarray.get w_14.state.e 0);
          assert_env_length w_14 2;
          push_env w_14 (Memo.from_constructor tag_VTPair);
          assert_env_length w_14 3;
          let ctor_arg_57 = pop_env w_14 in
          let ctor_arg_58 = pop_env w_14 in
          push_env w_14 (Memo.appends [ Memo.from_constructor tag_STypeError; ctor_arg_58; ctor_arg_57 ]);
          assert_env_length w_14 2;
          let ctor_arg_59 = pop_env w_14 in
          push_env w_14 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_59 ]);
          assert_env_length w_14 2;
          drop_n w_14 2 1;
          assert_env_length w_14 1;
          drop_n w_14 1 0;
          assert_env_length w_14 1;
          return_n w_14 1 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (14)")
    14;
  add_exp
    (fun w_15 ->
      assert_env_length w_15 1;
      let last_12 = Source.E 0 in
      let x_12 = resolve w_15 last_12 in
      match Word.get_value (fst x_12) with
      | c_12 when c_12 = tag_VPair ->
          let splits_37 = Memo.splits (snd x_12) in
          let split0_37 = List.nth splits_37 0 in
          let split1_15 = List.nth splits_37 1 in
          ignore (pop_env w_15);
          push_env w_15 split0_37;
          push_env w_15 split1_15;
          assert_env_length w_15 2;
          push_env w_15 (Dynarray.get w_15.state.e 1);
          assert_env_length w_15 3;
          drop_n w_15 3 2;
          assert_env_length w_15 1;
          drop_n w_15 1 0;
          assert_env_length w_15 1;
          return_n w_15 1 (pc_to_exp (int_to_pc 0))
      | c_12 when c_12 = tag_VStuck ->
          let splits_38 = Memo.splits (snd x_12) in
          let split0_38 = List.nth splits_38 0 in
          ignore (pop_env w_15);
          push_env w_15 split0_38;
          assert_env_length w_15 1;
          push_env w_15 (Dynarray.get w_15.state.e 0);
          assert_env_length w_15 2;
          let ctor_arg_60 = pop_env w_15 in
          push_env w_15 (Memo.appends [ Memo.from_constructor tag_SFst; ctor_arg_60 ]);
          assert_env_length w_15 2;
          let ctor_arg_61 = pop_env w_15 in
          push_env w_15 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_61 ]);
          assert_env_length w_15 2;
          drop_n w_15 2 1;
          assert_env_length w_15 1;
          drop_n w_15 1 0;
          assert_env_length w_15 1;
          return_n w_15 1 (pc_to_exp (int_to_pc 0))
      | pv ->
          push_env w_15 (Dynarray.get w_15.state.e 0);
          ignore (pop_env w_15);
          assert_env_length w_15 1;
          push_env w_15 (Dynarray.get w_15.state.e 0);
          assert_env_length w_15 2;
          push_env w_15 (Memo.from_constructor tag_VTPair);
          assert_env_length w_15 3;
          let ctor_arg_62 = pop_env w_15 in
          let ctor_arg_63 = pop_env w_15 in
          push_env w_15 (Memo.appends [ Memo.from_constructor tag_STypeError; ctor_arg_63; ctor_arg_62 ]);
          assert_env_length w_15 2;
          let ctor_arg_64 = pop_env w_15 in
          push_env w_15 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_64 ]);
          assert_env_length w_15 2;
          drop_n w_15 2 1;
          assert_env_length w_15 1;
          drop_n w_15 1 0;
          assert_env_length w_15 1;
          return_n w_15 1 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (15)")
    15;
  add_exp
    (fun w_16 ->
      assert_env_length w_16 4;
      let last_13 = Source.E 3 in
      let x_13 = resolve w_16 last_13 in
      match Word.get_value (fst x_13) with
      | c_13 when c_13 = tag_VNil ->
          ignore (pop_env w_16);
          assert_env_length w_16 3;
          push_env w_16 (Dynarray.get w_16.state.e 1);
          assert_env_length w_16 4;
          push_env w_16 (Dynarray.get w_16.state.e 0);
          assert_env_length w_16 5;
          ignore (env_call w_16 [] 2);
          w_16.state.c <- pc_to_exp (int_to_pc 4)
      | c_13 when c_13 = tag_VCons ->
          let splits_39 = Memo.splits (snd x_13) in
          let split0_39 = List.nth splits_39 0 in
          let split1_16 = List.nth splits_39 1 in
          ignore (pop_env w_16);
          push_env w_16 split0_39;
          push_env w_16 split1_16;
          assert_env_length w_16 5;
          push_env w_16 (Dynarray.get w_16.state.e 2);
          assert_env_length w_16 6;
          push_env w_16 (Dynarray.get w_16.state.e 4);
          assert_env_length w_16 7;
          push_env w_16 (Dynarray.get w_16.state.e 3);
          assert_env_length w_16 8;
          push_env w_16 (Dynarray.get w_16.state.e 0);
          assert_env_length w_16 9;
          let ctor_arg_65 = pop_env w_16 in
          let ctor_arg_66 = pop_env w_16 in
          push_env w_16 (Memo.appends [ Memo.from_constructor tag_Cons; ctor_arg_66; ctor_arg_65 ]);
          assert_env_length w_16 8;
          let ctor_arg_67 = pop_env w_16 in
          let ctor_arg_68 = pop_env w_16 in
          push_env w_16 (Memo.appends [ Memo.from_constructor tag_Cons; ctor_arg_68; ctor_arg_67 ]);
          assert_env_length w_16 7;
          ignore (env_call w_16 [] 2);
          w_16.state.c <- pc_to_exp (int_to_pc 4)
      | c_13 when c_13 = tag_VStuck ->
          let splits_40 = Memo.splits (snd x_13) in
          let split0_40 = List.nth splits_40 0 in
          ignore (pop_env w_16);
          push_env w_16 split0_40;
          assert_env_length w_16 4;
          push_env w_16 (Dynarray.get w_16.state.e 3);
          assert_env_length w_16 5;
          push_env w_16 (Dynarray.get w_16.state.e 1);
          assert_env_length w_16 6;
          push_env w_16 (Dynarray.get w_16.state.e 2);
          assert_env_length w_16 7;
          let ctor_arg_69 = pop_env w_16 in
          let ctor_arg_70 = pop_env w_16 in
          let ctor_arg_71 = pop_env w_16 in
          push_env w_16 (Memo.appends [ Memo.from_constructor tag_SMatchList; ctor_arg_71; ctor_arg_70; ctor_arg_69 ]);
          assert_env_length w_16 5;
          let ctor_arg_72 = pop_env w_16 in
          push_env w_16 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_72 ]);
          assert_env_length w_16 5;
          drop_n w_16 5 1;
          assert_env_length w_16 4;
          drop_n w_16 4 2;
          assert_env_length w_16 2;
          return_n w_16 2 (pc_to_exp (int_to_pc 0))
      | vv ->
          push_env w_16 (Dynarray.get w_16.state.e 3);
          ignore (pop_env w_16);
          assert_env_length w_16 4;
          push_env w_16 (Dynarray.get w_16.state.e 3);
          assert_env_length w_16 5;
          push_env w_16 (Memo.from_constructor tag_VTList);
          assert_env_length w_16 6;
          let ctor_arg_73 = pop_env w_16 in
          let ctor_arg_74 = pop_env w_16 in
          push_env w_16 (Memo.appends [ Memo.from_constructor tag_STypeError; ctor_arg_74; ctor_arg_73 ]);
          assert_env_length w_16 5;
          let ctor_arg_75 = pop_env w_16 in
          push_env w_16 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_75 ]);
          assert_env_length w_16 5;
          drop_n w_16 5 1;
          assert_env_length w_16 4;
          drop_n w_16 4 2;
          assert_env_length w_16 2;
          return_n w_16 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (16)")
    16;
  add_exp
    (fun w_18 ->
      assert_env_length w_18 6;
      let x0_0 = resolve w_18 (Source.E 4) in
      let x1_0 = resolve w_18 (Source.E 5) in
      ignore (pop_env w_18);
      ignore (pop_env w_18);
      push_env w_18 (Memo.from_int (Word.get_value (fst x0_0) + Word.get_value (fst x1_0)));
      assert_env_length w_18 5;
      let ctor_arg_76 = pop_env w_18 in
      push_env w_18 (Memo.appends [ Memo.from_constructor tag_VInt; ctor_arg_76 ]);
      assert_env_length w_18 5;
      drop_n w_18 5 1;
      assert_env_length w_18 4;
      drop_n w_18 4 1;
      assert_env_length w_18 3;
      drop_n w_18 3 1;
      assert_env_length w_18 2;
      drop_n w_18 2 1;
      assert_env_length w_18 1;
      drop_n w_18 1 0;
      assert_env_length w_18 1;
      return_n w_18 1 (pc_to_exp (int_to_pc 0)))
    17;
  add_exp
    (fun w_17 ->
      assert_env_length w_17 4;
      let last_14 = Source.E 3 in
      let x_14 = resolve w_17 last_14 in
      match Word.get_value (fst x_14) with
      | c_14 when c_14 = tag_VInt ->
          let splits_41 = Memo.splits (snd x_14) in
          let split0_41 = List.nth splits_41 0 in
          ignore (pop_env w_17);
          push_env w_17 split0_41;
          assert_env_length w_17 4;
          push_env w_17 (Dynarray.get w_17.state.e 1);
          assert_env_length w_17 5;
          push_env w_17 (Dynarray.get w_17.state.e 3);
          w_17.state.c <- pc_to_exp (int_to_pc 17)
      | c_14 when c_14 = tag_VStuck ->
          let splits_42 = Memo.splits (snd x_14) in
          let split0_42 = List.nth splits_42 0 in
          ignore (pop_env w_17);
          push_env w_17 split0_42;
          assert_env_length w_17 4;
          push_env w_17 (Dynarray.get w_17.state.e 0);
          assert_env_length w_17 5;
          push_env w_17 (Dynarray.get w_17.state.e 3);
          assert_env_length w_17 6;
          let ctor_arg_77 = pop_env w_17 in
          let ctor_arg_78 = pop_env w_17 in
          push_env w_17 (Memo.appends [ Memo.from_constructor tag_SAdd1; ctor_arg_78; ctor_arg_77 ]);
          assert_env_length w_17 5;
          let ctor_arg_79 = pop_env w_17 in
          push_env w_17 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_79 ]);
          assert_env_length w_17 5;
          drop_n w_17 5 1;
          assert_env_length w_17 4;
          drop_n w_17 4 1;
          assert_env_length w_17 3;
          drop_n w_17 3 1;
          assert_env_length w_17 2;
          drop_n w_17 2 1;
          assert_env_length w_17 1;
          drop_n w_17 1 0;
          assert_env_length w_17 1;
          return_n w_17 1 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_17);
          assert_env_length w_17 3;
          push_env w_17 (Dynarray.get w_17.state.e 2);
          assert_env_length w_17 4;
          push_env w_17 (Memo.from_constructor tag_VTInt);
          assert_env_length w_17 5;
          let ctor_arg_80 = pop_env w_17 in
          let ctor_arg_81 = pop_env w_17 in
          push_env w_17 (Memo.appends [ Memo.from_constructor tag_STypeError; ctor_arg_81; ctor_arg_80 ]);
          assert_env_length w_17 4;
          let ctor_arg_82 = pop_env w_17 in
          push_env w_17 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_82 ]);
          assert_env_length w_17 4;
          drop_n w_17 4 1;
          assert_env_length w_17 3;
          drop_n w_17 3 1;
          assert_env_length w_17 2;
          drop_n w_17 2 1;
          assert_env_length w_17 1;
          drop_n w_17 1 0;
          assert_env_length w_17 1;
          return_n w_17 1 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (18)")
    18;
  add_exp
    (fun w_20 ->
      assert_env_length w_20 6;
      let x0_1 = resolve w_20 (Source.E 4) in
      let x1_1 = resolve w_20 (Source.E 5) in
      ignore (pop_env w_20);
      ignore (pop_env w_20);
      push_env w_20 (Memo.from_int (if Word.get_value (fst x0_1) < Word.get_value (fst x1_1) then 1 else 0));
      assert_env_length w_20 5;
      let cond_0 = resolve w_20 (Source.E 4) in
      ignore (pop_env w_20);
      let if_kont_0 =
       fun _ ->
        assert_env_length w_20 5;
        drop_n w_20 5 1;
        assert_env_length w_20 4;
        drop_n w_20 4 1;
        assert_env_length w_20 3;
        drop_n w_20 3 1;
        assert_env_length w_20 2;
        drop_n w_20 2 1;
        assert_env_length w_20 1;
        drop_n w_20 1 0;
        assert_env_length w_20 1;
        return_n w_20 1 (pc_to_exp (int_to_pc 0))
      in
      if Word.get_value (fst cond_0) <> 0 then (
        assert_env_length w_20 4;
        push_env w_20 (Memo.from_constructor tag_VTrue);
        if_kont_0 ())
      else (
        assert_env_length w_20 4;
        push_env w_20 (Memo.from_constructor tag_VFalse);
        if_kont_0 ()))
    19;
  add_exp
    (fun w_19 ->
      assert_env_length w_19 4;
      let last_15 = Source.E 3 in
      let x_15 = resolve w_19 last_15 in
      match Word.get_value (fst x_15) with
      | c_15 when c_15 = tag_VInt ->
          let splits_43 = Memo.splits (snd x_15) in
          let split0_43 = List.nth splits_43 0 in
          ignore (pop_env w_19);
          push_env w_19 split0_43;
          assert_env_length w_19 4;
          push_env w_19 (Dynarray.get w_19.state.e 1);
          assert_env_length w_19 5;
          push_env w_19 (Dynarray.get w_19.state.e 3);
          w_19.state.c <- pc_to_exp (int_to_pc 19)
      | c_15 when c_15 = tag_VStuck ->
          let splits_44 = Memo.splits (snd x_15) in
          let split0_44 = List.nth splits_44 0 in
          ignore (pop_env w_19);
          push_env w_19 split0_44;
          assert_env_length w_19 4;
          push_env w_19 (Dynarray.get w_19.state.e 0);
          assert_env_length w_19 5;
          push_env w_19 (Dynarray.get w_19.state.e 3);
          assert_env_length w_19 6;
          let ctor_arg_83 = pop_env w_19 in
          let ctor_arg_84 = pop_env w_19 in
          push_env w_19 (Memo.appends [ Memo.from_constructor tag_SGt1; ctor_arg_84; ctor_arg_83 ]);
          assert_env_length w_19 5;
          let ctor_arg_85 = pop_env w_19 in
          push_env w_19 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_85 ]);
          assert_env_length w_19 5;
          drop_n w_19 5 1;
          assert_env_length w_19 4;
          drop_n w_19 4 1;
          assert_env_length w_19 3;
          drop_n w_19 3 1;
          assert_env_length w_19 2;
          drop_n w_19 2 1;
          assert_env_length w_19 1;
          drop_n w_19 1 0;
          assert_env_length w_19 1;
          return_n w_19 1 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_19);
          assert_env_length w_19 3;
          push_env w_19 (Dynarray.get w_19.state.e 2);
          assert_env_length w_19 4;
          push_env w_19 (Memo.from_constructor tag_VTInt);
          assert_env_length w_19 5;
          let ctor_arg_86 = pop_env w_19 in
          let ctor_arg_87 = pop_env w_19 in
          push_env w_19 (Memo.appends [ Memo.from_constructor tag_STypeError; ctor_arg_87; ctor_arg_86 ]);
          assert_env_length w_19 4;
          let ctor_arg_88 = pop_env w_19 in
          push_env w_19 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_88 ]);
          assert_env_length w_19 4;
          drop_n w_19 4 1;
          assert_env_length w_19 3;
          drop_n w_19 3 1;
          assert_env_length w_19 2;
          drop_n w_19 2 1;
          assert_env_length w_19 1;
          drop_n w_19 1 0;
          assert_env_length w_19 1;
          return_n w_19 1 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (20)")
    20;
  add_exp
    (fun w_22 ->
      assert_env_length w_22 6;
      let x0_2 = resolve w_22 (Source.E 4) in
      let x1_2 = resolve w_22 (Source.E 5) in
      ignore (pop_env w_22);
      ignore (pop_env w_22);
      push_env w_22 (Memo.from_int (if Word.get_value (fst x0_2) <= Word.get_value (fst x1_2) then 1 else 0));
      assert_env_length w_22 5;
      let cond_1 = resolve w_22 (Source.E 4) in
      ignore (pop_env w_22);
      let if_kont_1 =
       fun _ ->
        assert_env_length w_22 5;
        drop_n w_22 5 1;
        assert_env_length w_22 4;
        drop_n w_22 4 1;
        assert_env_length w_22 3;
        drop_n w_22 3 1;
        assert_env_length w_22 2;
        drop_n w_22 2 1;
        assert_env_length w_22 1;
        drop_n w_22 1 0;
        assert_env_length w_22 1;
        return_n w_22 1 (pc_to_exp (int_to_pc 0))
      in
      if Word.get_value (fst cond_1) <> 0 then (
        assert_env_length w_22 4;
        push_env w_22 (Memo.from_constructor tag_VTrue);
        if_kont_1 ())
      else (
        assert_env_length w_22 4;
        push_env w_22 (Memo.from_constructor tag_VFalse);
        if_kont_1 ()))
    21;
  add_exp
    (fun w_21 ->
      assert_env_length w_21 4;
      let last_16 = Source.E 3 in
      let x_16 = resolve w_21 last_16 in
      match Word.get_value (fst x_16) with
      | c_16 when c_16 = tag_VInt ->
          let splits_45 = Memo.splits (snd x_16) in
          let split0_45 = List.nth splits_45 0 in
          ignore (pop_env w_21);
          push_env w_21 split0_45;
          assert_env_length w_21 4;
          push_env w_21 (Dynarray.get w_21.state.e 1);
          assert_env_length w_21 5;
          push_env w_21 (Dynarray.get w_21.state.e 3);
          w_21.state.c <- pc_to_exp (int_to_pc 21)
      | c_16 when c_16 = tag_VStuck ->
          let splits_46 = Memo.splits (snd x_16) in
          let split0_46 = List.nth splits_46 0 in
          ignore (pop_env w_21);
          push_env w_21 split0_46;
          assert_env_length w_21 4;
          push_env w_21 (Dynarray.get w_21.state.e 0);
          assert_env_length w_21 5;
          push_env w_21 (Dynarray.get w_21.state.e 3);
          assert_env_length w_21 6;
          let ctor_arg_89 = pop_env w_21 in
          let ctor_arg_90 = pop_env w_21 in
          push_env w_21 (Memo.appends [ Memo.from_constructor tag_SGt1; ctor_arg_90; ctor_arg_89 ]);
          assert_env_length w_21 5;
          let ctor_arg_91 = pop_env w_21 in
          push_env w_21 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_91 ]);
          assert_env_length w_21 5;
          drop_n w_21 5 1;
          assert_env_length w_21 4;
          drop_n w_21 4 1;
          assert_env_length w_21 3;
          drop_n w_21 3 1;
          assert_env_length w_21 2;
          drop_n w_21 2 1;
          assert_env_length w_21 1;
          drop_n w_21 1 0;
          assert_env_length w_21 1;
          return_n w_21 1 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_21);
          assert_env_length w_21 3;
          push_env w_21 (Dynarray.get w_21.state.e 2);
          assert_env_length w_21 4;
          push_env w_21 (Memo.from_constructor tag_VTInt);
          assert_env_length w_21 5;
          let ctor_arg_92 = pop_env w_21 in
          let ctor_arg_93 = pop_env w_21 in
          push_env w_21 (Memo.appends [ Memo.from_constructor tag_STypeError; ctor_arg_93; ctor_arg_92 ]);
          assert_env_length w_21 4;
          let ctor_arg_94 = pop_env w_21 in
          push_env w_21 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_94 ]);
          assert_env_length w_21 4;
          drop_n w_21 4 1;
          assert_env_length w_21 3;
          drop_n w_21 3 1;
          assert_env_length w_21 2;
          drop_n w_21 2 1;
          assert_env_length w_21 1;
          drop_n w_21 1 0;
          assert_env_length w_21 1;
          return_n w_21 1 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (22)")
    22;
  add_exp
    (fun w_24 ->
      assert_env_length w_24 6;
      let x0_3 = resolve w_24 (Source.E 4) in
      let x1_3 = resolve w_24 (Source.E 5) in
      ignore (pop_env w_24);
      ignore (pop_env w_24);
      push_env w_24 (Memo.from_int (if Word.get_value (fst x0_3) > Word.get_value (fst x1_3) then 1 else 0));
      assert_env_length w_24 5;
      let cond_2 = resolve w_24 (Source.E 4) in
      ignore (pop_env w_24);
      let if_kont_2 =
       fun _ ->
        assert_env_length w_24 5;
        drop_n w_24 5 1;
        assert_env_length w_24 4;
        drop_n w_24 4 1;
        assert_env_length w_24 3;
        drop_n w_24 3 1;
        assert_env_length w_24 2;
        drop_n w_24 2 1;
        assert_env_length w_24 1;
        drop_n w_24 1 0;
        assert_env_length w_24 1;
        return_n w_24 1 (pc_to_exp (int_to_pc 0))
      in
      if Word.get_value (fst cond_2) <> 0 then (
        assert_env_length w_24 4;
        push_env w_24 (Memo.from_constructor tag_VTrue);
        if_kont_2 ())
      else (
        assert_env_length w_24 4;
        push_env w_24 (Memo.from_constructor tag_VFalse);
        if_kont_2 ()))
    23;
  add_exp
    (fun w_23 ->
      assert_env_length w_23 4;
      let last_17 = Source.E 3 in
      let x_17 = resolve w_23 last_17 in
      match Word.get_value (fst x_17) with
      | c_17 when c_17 = tag_VInt ->
          let splits_47 = Memo.splits (snd x_17) in
          let split0_47 = List.nth splits_47 0 in
          ignore (pop_env w_23);
          push_env w_23 split0_47;
          assert_env_length w_23 4;
          push_env w_23 (Dynarray.get w_23.state.e 1);
          assert_env_length w_23 5;
          push_env w_23 (Dynarray.get w_23.state.e 3);
          w_23.state.c <- pc_to_exp (int_to_pc 23)
      | c_17 when c_17 = tag_VStuck ->
          let splits_48 = Memo.splits (snd x_17) in
          let split0_48 = List.nth splits_48 0 in
          ignore (pop_env w_23);
          push_env w_23 split0_48;
          assert_env_length w_23 4;
          push_env w_23 (Dynarray.get w_23.state.e 0);
          assert_env_length w_23 5;
          push_env w_23 (Dynarray.get w_23.state.e 3);
          assert_env_length w_23 6;
          let ctor_arg_95 = pop_env w_23 in
          let ctor_arg_96 = pop_env w_23 in
          push_env w_23 (Memo.appends [ Memo.from_constructor tag_SGt1; ctor_arg_96; ctor_arg_95 ]);
          assert_env_length w_23 5;
          let ctor_arg_97 = pop_env w_23 in
          push_env w_23 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_97 ]);
          assert_env_length w_23 5;
          drop_n w_23 5 1;
          assert_env_length w_23 4;
          drop_n w_23 4 1;
          assert_env_length w_23 3;
          drop_n w_23 3 1;
          assert_env_length w_23 2;
          drop_n w_23 2 1;
          assert_env_length w_23 1;
          drop_n w_23 1 0;
          assert_env_length w_23 1;
          return_n w_23 1 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_23);
          assert_env_length w_23 3;
          push_env w_23 (Dynarray.get w_23.state.e 2);
          assert_env_length w_23 4;
          push_env w_23 (Memo.from_constructor tag_VTInt);
          assert_env_length w_23 5;
          let ctor_arg_98 = pop_env w_23 in
          let ctor_arg_99 = pop_env w_23 in
          push_env w_23 (Memo.appends [ Memo.from_constructor tag_STypeError; ctor_arg_99; ctor_arg_98 ]);
          assert_env_length w_23 4;
          let ctor_arg_100 = pop_env w_23 in
          push_env w_23 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_100 ]);
          assert_env_length w_23 4;
          drop_n w_23 4 1;
          assert_env_length w_23 3;
          drop_n w_23 3 1;
          assert_env_length w_23 2;
          drop_n w_23 2 1;
          assert_env_length w_23 1;
          drop_n w_23 1 0;
          assert_env_length w_23 1;
          return_n w_23 1 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (24)")
    24;
  add_exp
    (fun w_26 ->
      assert_env_length w_26 6;
      let x0_4 = resolve w_26 (Source.E 4) in
      let x1_4 = resolve w_26 (Source.E 5) in
      ignore (pop_env w_26);
      ignore (pop_env w_26);
      push_env w_26 (Memo.from_int (if Word.get_value (fst x0_4) >= Word.get_value (fst x1_4) then 1 else 0));
      assert_env_length w_26 5;
      let cond_3 = resolve w_26 (Source.E 4) in
      ignore (pop_env w_26);
      let if_kont_3 =
       fun _ ->
        assert_env_length w_26 5;
        drop_n w_26 5 1;
        assert_env_length w_26 4;
        drop_n w_26 4 1;
        assert_env_length w_26 3;
        drop_n w_26 3 1;
        assert_env_length w_26 2;
        drop_n w_26 2 1;
        assert_env_length w_26 1;
        drop_n w_26 1 0;
        assert_env_length w_26 1;
        return_n w_26 1 (pc_to_exp (int_to_pc 0))
      in
      if Word.get_value (fst cond_3) <> 0 then (
        assert_env_length w_26 4;
        push_env w_26 (Memo.from_constructor tag_VTrue);
        if_kont_3 ())
      else (
        assert_env_length w_26 4;
        push_env w_26 (Memo.from_constructor tag_VFalse);
        if_kont_3 ()))
    25;
  add_exp
    (fun w_25 ->
      assert_env_length w_25 4;
      let last_18 = Source.E 3 in
      let x_18 = resolve w_25 last_18 in
      match Word.get_value (fst x_18) with
      | c_18 when c_18 = tag_VInt ->
          let splits_49 = Memo.splits (snd x_18) in
          let split0_49 = List.nth splits_49 0 in
          ignore (pop_env w_25);
          push_env w_25 split0_49;
          assert_env_length w_25 4;
          push_env w_25 (Dynarray.get w_25.state.e 1);
          assert_env_length w_25 5;
          push_env w_25 (Dynarray.get w_25.state.e 3);
          w_25.state.c <- pc_to_exp (int_to_pc 25)
      | c_18 when c_18 = tag_VStuck ->
          let splits_50 = Memo.splits (snd x_18) in
          let split0_50 = List.nth splits_50 0 in
          ignore (pop_env w_25);
          push_env w_25 split0_50;
          assert_env_length w_25 4;
          push_env w_25 (Dynarray.get w_25.state.e 0);
          assert_env_length w_25 5;
          push_env w_25 (Dynarray.get w_25.state.e 3);
          assert_env_length w_25 6;
          let ctor_arg_101 = pop_env w_25 in
          let ctor_arg_102 = pop_env w_25 in
          push_env w_25 (Memo.appends [ Memo.from_constructor tag_SGt1; ctor_arg_102; ctor_arg_101 ]);
          assert_env_length w_25 5;
          let ctor_arg_103 = pop_env w_25 in
          push_env w_25 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_103 ]);
          assert_env_length w_25 5;
          drop_n w_25 5 1;
          assert_env_length w_25 4;
          drop_n w_25 4 1;
          assert_env_length w_25 3;
          drop_n w_25 3 1;
          assert_env_length w_25 2;
          drop_n w_25 2 1;
          assert_env_length w_25 1;
          drop_n w_25 1 0;
          assert_env_length w_25 1;
          return_n w_25 1 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_25);
          assert_env_length w_25 3;
          push_env w_25 (Dynarray.get w_25.state.e 2);
          assert_env_length w_25 4;
          push_env w_25 (Memo.from_constructor tag_VTInt);
          assert_env_length w_25 5;
          let ctor_arg_104 = pop_env w_25 in
          let ctor_arg_105 = pop_env w_25 in
          push_env w_25 (Memo.appends [ Memo.from_constructor tag_STypeError; ctor_arg_105; ctor_arg_104 ]);
          assert_env_length w_25 4;
          let ctor_arg_106 = pop_env w_25 in
          push_env w_25 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_106 ]);
          assert_env_length w_25 4;
          drop_n w_25 4 1;
          assert_env_length w_25 3;
          drop_n w_25 3 1;
          assert_env_length w_25 2;
          drop_n w_25 2 1;
          assert_env_length w_25 1;
          drop_n w_25 1 0;
          assert_env_length w_25 1;
          return_n w_25 1 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (26)")
    26;
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
  Words.set_constructor_degree 44 (-1);
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
  Words.set_constructor_degree 61 0;
  Words.set_constructor_degree 62 (-2);
  Words.set_constructor_degree 63 (-2);
  Words.set_constructor_degree 64 (-3);
  Words.set_constructor_degree 65 (-2);
  Words.set_constructor_degree 66 (-2);
  Words.set_constructor_degree 67 0;
  Words.set_constructor_degree 68 0;
  Words.set_constructor_degree 69 (-3);
  Words.set_constructor_degree 70 (-2);
  Words.set_constructor_degree 71 (-2);
  Words.set_constructor_degree 72 (-2);
  Words.set_constructor_degree 73 (-2);
  Words.set_constructor_degree 74 (-2);
  Words.set_constructor_degree 75 (-2);
  Words.set_constructor_degree 76 (-3);
  Words.set_constructor_degree 77 (-1);
  Words.set_constructor_degree 78 (-1)
