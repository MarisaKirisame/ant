open Ant
open NamedExpr
module LC = LiveCEK

let base_names =
  [|
    "a";
    "b";
    "c";
    "d";
    "e";
    "f";
    "g";
    "h";
    "i";
    "j";
    "k";
    "l";
    "m";
    "n";
    "o";
    "p";
    "q";
    "r";
    "s";
    "t";
    "u";
    "v";
    "w";
    "x";
    "y";
    "z";
  |]

let make_name_generator () =
  let used = Hashtbl.create 16 in
  let counter = ref 0 in
  let fresh ?hint () =
    let base =
      match hint with
      | Some h -> h
      | None ->
          let n = !counter in
          incr counter;
          let candidate = base_names.(n mod Array.length base_names) in
          let suffix = n / Array.length base_names in
          if suffix = 0 then candidate else candidate ^ string_of_int suffix
    in
    let count = match Hashtbl.find_opt used base with Some c -> c | None -> 0 in
    Hashtbl.replace used base (count + 1);
    if count = 0 then base else base ^ string_of_int count
  in
  fresh

let rec lookup_name ctx idx =
  match (ctx, idx) with
  | name :: _, 0 -> name
  | _ :: rest, n when n > 0 -> lookup_name rest (n - 1)
  | _ -> Printf.sprintf "free%d" idx

let rec index_of_name (ctx : string list) (target : string) (offset : int) : int option =
  match ctx with
  | [] -> None
  | name :: rest -> if String.equal name target then Some offset else index_of_name rest target (offset + 1)

let rec nat_from_int i =
  assert (i >= 0);
  if i == 0 then LC.Z else LC.S (nat_from_int (i - 1))

let rec int_of_nat = function LC.Z -> 0 | LC.S n -> 1 + int_of_nat n

let[@warning "-32"] expr_of_nexpr ?(ctx = []) nexpr =
  let rec aux ctx = function
    | NEInt i -> LC.EInt i
    | NEPlus (lhs, rhs) -> LC.EPlus (aux ctx lhs, aux ctx rhs)
    | NELt (lhs, rhs) -> LC.ELt (aux ctx lhs, aux ctx rhs)
    | NELe (lhs, rhs) -> LC.ELe (aux ctx lhs, aux ctx rhs)
    | NEGt (lhs, rhs) -> LC.EGt (aux ctx lhs, aux ctx rhs)
    | NEGe (lhs, rhs) -> LC.EGe (aux ctx lhs, aux ctx rhs)
    | NEVar name -> (
        match index_of_name ctx name 0 with
        | Some idx -> LC.EVar (nat_from_int idx)
        | None -> invalid_arg (Printf.sprintf "expr_of_nexpr: unbound variable %S" name))
    | NEAbs (param, body) -> LC.EAbs (aux (param :: ctx) body)
    | NEApp (fn, arg) -> LC.EApp (aux ctx fn, aux ctx arg)
    | NELet (name, bound, body) -> LC.ELet (aux ctx bound, aux (name :: ctx) body)
    | NETrue -> LC.ETrue
    | NEFalse -> LC.EFalse
    | NEIf (cond, thn, els) -> LC.EIf (aux ctx cond, aux ctx thn, aux ctx els)
    | NENil -> LC.ENil
    | NECons (hd, tl) -> LC.ECons (aux ctx hd, aux ctx tl)
    | NEMatchList (target, nil_case, head_name, tail_name, cons_case) ->
        LC.EMatchList (aux ctx target, aux ctx nil_case, aux (tail_name :: head_name :: ctx) cons_case)
    | NEFix (func_name, arg_name, body) -> LC.EFix (aux (arg_name :: func_name :: ctx) body)
    | NEHole -> LC.EHole None
  in
  aux ctx nexpr

let nexpr_of_expr ?(ctx = []) expr =
  let fresh_name = make_name_generator () in
  let rec aux ctx expr =
    match expr with
    | LC.EInt i -> NEInt i
    | LC.EPlus (lhs, rhs) -> NEPlus (aux ctx lhs, aux ctx rhs)
    | LC.ELt (lhs, rhs) -> NELt (aux ctx lhs, aux ctx rhs)
    | LC.ELe (lhs, rhs) -> NELe (aux ctx lhs, aux ctx rhs)
    | LC.EGt (lhs, rhs) -> NEGt (aux ctx lhs, aux ctx rhs)
    | LC.EGe (lhs, rhs) -> NEGe (aux ctx lhs, aux ctx rhs)
    | LC.EVar idx -> NEVar (lookup_name ctx (int_of_nat idx))
    | LC.EAbs body ->
        let param = fresh_name ~hint:"x" () in
        NEAbs (param, aux (param :: ctx) body)
    | LC.EApp (fn, arg) -> NEApp (aux ctx fn, aux ctx arg)
    | LC.ELet (bound, body) ->
        let name = fresh_name ~hint:"x" () in
        NELet (name, aux ctx bound, aux (name :: ctx) body)
    | LC.ETrue -> NETrue
    | LC.EFalse -> NEFalse
    | LC.EIf (cond, thn, els) -> NEIf (aux ctx cond, aux ctx thn, aux ctx els)
    | LC.ENil -> NENil
    | LC.ECons (hd, tl) -> NECons (aux ctx hd, aux ctx tl)
    | LC.EMatchList (target, nil_case, cons_case) ->
        let head_name = fresh_name ~hint:"hd" () in
        let tail_name = fresh_name ~hint:"tl" () in
        NEMatchList
          (aux ctx target, aux ctx nil_case, head_name, tail_name, aux (tail_name :: head_name :: ctx) cons_case)
    | LC.EFix body ->
        let func_name = fresh_name ~hint:"f" () in
        let arg_name = fresh_name ~hint:"xs" () in
        NEFix (func_name, arg_name, aux (arg_name :: func_name :: ctx) body)
    | LC.EHole _ -> NEHole
  in
  aux ctx expr

let parse_nexpr input =
  let lexbuf = Lexing.from_string input in
  let report_position pos =
    let open Lexing in
    (pos.pos_lnum, pos.pos_cnum - pos.pos_bol + 1)
  in
  try LiveParser.nexpr LiveLexer.token lexbuf with
  | LiveLexer.Error (msg, pos) ->
      let line, col = report_position pos in
      invalid_arg (Printf.sprintf "Lexer error at line %d, column %d: %s" line col msg)
  | LiveParser.Error ->
      let line, col = report_position lexbuf.Lexing.lex_curr_p in
      invalid_arg (Printf.sprintf "Parse error at line %d, column %d" line col)

let pp_nexpr fmt nexpr =
  let rec aux fmt expr =
    match expr with
    | NEInt i -> Format.pp_print_int fmt i
    | NEPlus (lhs, rhs) -> Format.fprintf fmt "(%a + %a)" aux lhs aux rhs
    | NELt (lhs, rhs) -> Format.fprintf fmt "(%a < %a)" aux lhs aux rhs
    | NELe (lhs, rhs) -> Format.fprintf fmt "(%a <= %a)" aux lhs aux rhs
    | NEGt (lhs, rhs) -> Format.fprintf fmt "(%a > %a)" aux lhs aux rhs
    | NEGe (lhs, rhs) -> Format.fprintf fmt "(%a >= %a)" aux lhs aux rhs
    | NEVar name -> Format.pp_print_string fmt name
    | NEAbs (param, body) -> Format.fprintf fmt "(fun %s -> %a)" param aux body
    | NEApp (fn, arg) -> Format.fprintf fmt "(%a %a)" aux fn aux arg
    | NELet (name, bound, body) -> Format.fprintf fmt "(let %s = %a in %a)" name aux bound aux body
    | NETrue -> Format.pp_print_string fmt "true"
    | NEFalse -> Format.pp_print_string fmt "false"
    | NEIf (cond, thn, els) -> Format.fprintf fmt "(if %a then %a else %a)" aux cond aux thn aux els
    | NENil -> Format.pp_print_string fmt "[]"
    | NECons (hd, tl) -> Format.fprintf fmt "(%a :: %a)" aux hd aux tl
    | NEMatchList (target, nil_case, head_name, tail_name, cons_case) ->
        Format.fprintf fmt "(match %a with [] -> %a | %s :: %s -> %a)" aux target aux nil_case head_name tail_name aux
          cons_case
    | NEFix (func_name, arg_name, body) -> Format.fprintf fmt "(fix %s %s. %a)" func_name arg_name aux body
    | NEHole -> Format.pp_print_string fmt "hole"
  in
  aux fmt nexpr

let pp_expr fmt expr = pp_nexpr fmt (nexpr_of_expr expr)
let expr_to_string expr = Format.asprintf "%a" pp_expr expr
let rec len_live_list = function LC.Nil -> 0 | LC.Cons (_, tl) -> 1 + len_live_list tl

(* Produce a sequence of partial expressions that reveal subexpressions from left to right. The
   list always starts with a single hole and ends with the fully constructed input expression. *)
let left_to_right (expr : LC.expr) : LC.expr list =
  let tail = function [] -> failwith "left_to_right: not expecting empty list" | _ :: t -> t in
  let rec build e =
    match e with
    | LC.EHole x -> [ LC.EHole x ]
    | LC.EInt _ | LC.EVar _ | LC.ETrue | LC.EFalse | LC.ENil -> [ LC.EHole None; e ]
    | LC.EAbs body ->
        let steps_body = build body in
        LC.EHole None :: LC.EAbs (LC.EHole None) :: List.map (fun s -> LC.EAbs s) (tail steps_body)
    | LC.EFix body ->
        let steps_body = build body in
        LC.EHole None :: LC.EFix (LC.EHole None) :: List.map (fun s -> LC.EFix s) (tail steps_body)
    | LC.EPlus (l, r) ->
        let sl = build l in
        let sr = build r in
        LC.EHole None
        :: LC.EPlus (LC.EHole None, LC.EHole None)
        :: List.map (fun s -> LC.EPlus (s, LC.EHole None)) (tail sl)
        @ List.map (fun s -> LC.EPlus (l, s)) (tail sr)
    | LC.ELt (l, r) ->
        let sl = build l in
        let sr = build r in
        LC.EHole None
        :: LC.ELt (LC.EHole None, LC.EHole None)
        :: List.map (fun s -> LC.ELt (s, LC.EHole None)) (tail sl)
        @ List.map (fun s -> LC.ELt (l, s)) (tail sr)
    | LC.ELe (l, r) ->
        let sl = build l in
        let sr = build r in
        LC.EHole None
        :: LC.ELe (LC.EHole None, LC.EHole None)
        :: List.map (fun s -> LC.ELe (s, LC.EHole None)) (tail sl)
        @ List.map (fun s -> LC.ELe (l, s)) (tail sr)
    | LC.EGt (l, r) ->
        let sl = build l in
        let sr = build r in
        LC.EHole None
        :: LC.EGt (LC.EHole None, LC.EHole None)
        :: List.map (fun s -> LC.EGt (s, LC.EHole None)) (tail sl)
        @ List.map (fun s -> LC.EGt (l, s)) (tail sr)
    | LC.EGe (l, r) ->
        let sl = build l in
        let sr = build r in
        LC.EHole None
        :: LC.EGe (LC.EHole None, LC.EHole None)
        :: List.map (fun s -> LC.EGe (s, LC.EHole None)) (tail sl)
        @ List.map (fun s -> LC.EGe (l, s)) (tail sr)
    | LC.EApp (fn, arg) ->
        let sfn = build fn in
        let sarg = build arg in
        LC.EHole None
        :: LC.EApp (LC.EHole None, LC.EHole None)
        :: List.map (fun s -> LC.EApp (s, LC.EHole None)) (tail sfn)
        @ List.map (fun s -> LC.EApp (fn, s)) (tail sarg)
    | LC.ELet (bound, body) ->
        let sbound = build bound in
        let sbody = build body in
        LC.EHole None
        :: LC.ELet (LC.EHole None, LC.EHole None)
        :: List.map (fun s -> LC.ELet (s, LC.EHole None)) (tail sbound)
        @ List.map (fun s -> LC.ELet (bound, s)) (tail sbody)
    | LC.EIf (cond, thn, els) ->
        let scond = build cond in
        let sthn = build thn in
        let sels = build els in
        LC.EHole None
        :: LC.EIf (LC.EHole None, LC.EHole None, LC.EHole None)
        :: List.map (fun s -> LC.EIf (s, LC.EHole None, LC.EHole None)) (tail scond)
        @ List.map (fun s -> LC.EIf (cond, s, LC.EHole None)) (tail sthn)
        @ List.map (fun s -> LC.EIf (cond, thn, s)) (tail sels)
    | LC.ECons (hd, tl) ->
        let shd = build hd in
        let stl = build tl in
        LC.EHole None
        :: LC.ECons (LC.EHole None, LC.EHole None)
        :: List.map (fun s -> LC.ECons (s, LC.EHole None)) (tail shd)
        @ List.map (fun s -> LC.ECons (hd, s)) (tail stl)
    | LC.EMatchList (target, nil_case, cons_case) ->
        let starget = build target in
        let snil = build nil_case in
        let scons = build cons_case in
        LC.EHole None
        :: LC.EMatchList (LC.EHole None, LC.EHole None, LC.EHole None)
        :: List.map (fun s -> LC.EMatchList (s, LC.EHole None, LC.EHole None)) (tail starget)
        @ List.map (fun s -> LC.EMatchList (target, s, LC.EHole None)) (tail snil)
        @ List.map (fun s -> LC.EMatchList (target, nil_case, s)) (tail scons)
  in
  build expr

module DemandedExpansion = struct
  open LiveCEK

  let id_counter = ref 0

  let fresh_id () =
    incr id_counter;
    !id_counter

  let oracle : (int, expr) Hashtbl.t = Hashtbl.create 32

  let rec get_blocking_id_value = function
    | VStuck s -> get_blocking_id_stuck s
    | VCons (h, t) -> ( match get_blocking_id_value h with Some id -> Some id | None -> get_blocking_id_value t)
    | VAbs (_, env) | VFix (_, env) -> get_blocking_id_env env
    | _ -> None

  and get_blocking_id_env = function
    | LC.Nil -> None
    | LC.Cons (v, vs) -> ( match get_blocking_id_value v with Some id -> Some id | None -> get_blocking_id_env vs)

  and get_blocking_id_stuck stuck =
    match stuck with
    | SHole (id, _) -> id
    | STypeError _ -> None
    | SIndexError -> None
    | SApp (s, _) | SAdd0 (s, _) | SGt0 (s, _) | SIf (s, _, _) | SMatchList (s, _, _) -> get_blocking_id_stuck s
    | SAdd1 (_, s) | SGt1 (_, s) -> get_blocking_id_stuck s

  let reveal_shallow target_expr =
    match target_expr with
    | EPlus (l, r) ->
        let id1, id2 = (fresh_id (), fresh_id ()) in
        Hashtbl.add oracle id1 l;
        Hashtbl.add oracle id2 r;
        EPlus (EHole (Some id1), EHole (Some id2))
    | ELt (l, r) ->
        let id1, id2 = (fresh_id (), fresh_id ()) in
        Hashtbl.add oracle id1 l;
        Hashtbl.add oracle id2 r;
        ELt (EHole (Some id1), EHole (Some id2))
    | ELe (l, r) ->
        let id1, id2 = (fresh_id (), fresh_id ()) in
        Hashtbl.add oracle id1 l;
        Hashtbl.add oracle id2 r;
        ELe (EHole (Some id1), EHole (Some id2))
    | EGt (l, r) ->
        let id1, id2 = (fresh_id (), fresh_id ()) in
        Hashtbl.add oracle id1 l;
        Hashtbl.add oracle id2 r;
        EGt (EHole (Some id1), EHole (Some id2))
    | EGe (l, r) ->
        let id1, id2 = (fresh_id (), fresh_id ()) in
        Hashtbl.add oracle id1 l;
        Hashtbl.add oracle id2 r;
        EGe (EHole (Some id1), EHole (Some id2))
    | EIf (c, t, e) ->
        let id1, id2, id3 = (fresh_id (), fresh_id (), fresh_id ()) in
        Hashtbl.add oracle id1 c;
        Hashtbl.add oracle id2 t;
        Hashtbl.add oracle id3 e;
        EIf (EHole (Some id1), EHole (Some id2), EHole (Some id3))
    | ELet (l, r) ->
        let id1, id2 = (fresh_id (), fresh_id ()) in
        Hashtbl.add oracle id1 l;
        Hashtbl.add oracle id2 r;
        ELet (EHole (Some id1), EHole (Some id2))
    | EApp (f, x) ->
        let id1, id2 = (fresh_id (), fresh_id ()) in
        Hashtbl.add oracle id1 f;
        Hashtbl.add oracle id2 x;
        EApp (EHole (Some id1), EHole (Some id2))
    | EAbs e ->
        let id1 = fresh_id () in
        Hashtbl.add oracle id1 e;
        EAbs (EHole (Some id1))
    | ECons (h, t) ->
        let id1, id2 = (fresh_id (), fresh_id ()) in
        Hashtbl.add oracle id1 h;
        Hashtbl.add oracle id2 t;
        ECons (EHole (Some id1), EHole (Some id2))
    | EMatchList (v, n, c) ->
        let id1, id2, id3 = (fresh_id (), fresh_id (), fresh_id ()) in
        Hashtbl.add oracle id1 v;
        Hashtbl.add oracle id2 n;
        Hashtbl.add oracle id3 c;
        EMatchList (EHole (Some id1), EHole (Some id2), EHole (Some id3))
    | EFix e ->
        let id1 = fresh_id () in
        Hashtbl.add oracle id1 e;
        EFix (EHole (Some id1))
    | EInt _ | EVar _ | ETrue | EFalse | ENil | EHole _ -> target_expr

  let rec apply_expansion expr target_id expansion =
    match expr with
    | EHole (Some id) when id = target_id -> expansion
    | EHole id -> EHole id
    | EPlus (l, r) -> EPlus (apply_expansion l target_id expansion, apply_expansion r target_id expansion)
    | ELt (l, r) -> ELt (apply_expansion l target_id expansion, apply_expansion r target_id expansion)
    | ELe (l, r) -> ELe (apply_expansion l target_id expansion, apply_expansion r target_id expansion)
    | EGt (l, r) -> EGt (apply_expansion l target_id expansion, apply_expansion r target_id expansion)
    | EGe (l, r) -> EGe (apply_expansion l target_id expansion, apply_expansion r target_id expansion)
    | EAbs e -> EAbs (apply_expansion e target_id expansion)
    | EApp (f, x) -> EApp (apply_expansion f target_id expansion, apply_expansion x target_id expansion)
    | ELet (l, r) -> ELet (apply_expansion l target_id expansion, apply_expansion r target_id expansion)
    | EIf (c, t, e) ->
        EIf
          ( apply_expansion c target_id expansion,
            apply_expansion t target_id expansion,
            apply_expansion e target_id expansion )
    | ECons (h, t) -> ECons (apply_expansion h target_id expansion, apply_expansion t target_id expansion)
    | EMatchList (v, n, c) ->
        EMatchList
          ( apply_expansion v target_id expansion,
            apply_expansion n target_id expansion,
            apply_expansion c target_id expansion )
    | EFix e -> EFix (apply_expansion e target_id expansion)
    | EInt _ | EVar _ | ETrue | EFalse | ENil -> expr

  let interactive prog use =
    Hashtbl.clear oracle;
    id_counter := 0;
    let start_prog = EHole (Some 0) in
    Hashtbl.add oracle 0 prog;
    let rec loop iter current_prog =
      match get_blocking_id_value (use iter current_prog) with
      | None -> ()
      | Some blocking_id ->
          let target_subtree = Hashtbl.find oracle blocking_id in
          let expansion = reveal_shallow target_subtree in
          let next_prog = apply_expansion current_prog blocking_id expansion in
          loop (iter + 1) next_prog
    in
    loop 0 start_prog
end

let demanded_interactive = DemandedExpansion.interactive

let rec pp_value fmt value =
  match value with
  | LC.VInt i -> Format.pp_print_int fmt i
  | LC.VTrue -> Format.pp_print_string fmt "true"
  | LC.VFalse -> Format.pp_print_string fmt "false"
  | LC.VNil -> Format.pp_print_string fmt "[]"
  | LC.VCons _ as cons -> (
      let rec gather acc = function
        | LC.VCons (h, t) -> gather (h :: acc) t
        | LC.VNil -> `List (List.rev acc)
        | tail -> `Improper (List.rev acc, tail)
      in
      let render_list fmt elems =
        Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "; ") pp_value fmt elems
      in
      match gather [] cons with
      | `List elems -> Format.fprintf fmt "[%a]" render_list elems
      | `Improper (elems, tail) -> Format.fprintf fmt "[%a | %a]" render_list elems pp_value tail)
  | LC.VAbs (body, env) -> Format.fprintf fmt "<fun %a | env=%d>" pp_expr body (len_live_list env)
  | LC.VFix (body, env) -> Format.fprintf fmt "<fix %a | env=%d>" pp_expr body (len_live_list env)
  | LC.VStuck stuck -> pp_stuck fmt stuck

and pp_stuck fmt = function
  | LC.SHole (_, env) -> Format.fprintf fmt "<hole env=%d>" (len_live_list env)
  | LC.STypeError (value, ty) -> Format.fprintf fmt "<type-error %a : %a>" pp_value value pp_vtype ty
  | LC.SIndexError -> Format.pp_print_string fmt "<index-error>"
  | LC.SApp (stuck, expr) -> Format.fprintf fmt "<stuck app %a %a>" pp_stuck stuck pp_expr expr
  | LC.SAdd0 (stuck, expr) -> Format.fprintf fmt "<stuck add0 %a %a>" pp_stuck stuck pp_expr expr
  | LC.SAdd1 (value, stuck) -> Format.fprintf fmt "<stuck add1 %a %a>" pp_value value pp_stuck stuck
  | LC.SGt0 (stuck, expr) -> Format.fprintf fmt "<stuck gt0 %a %a>" pp_stuck stuck pp_expr expr
  | LC.SGt1 (value, stuck) -> Format.fprintf fmt "<stuck gt1 %a %a>" pp_value value pp_stuck stuck
  | LC.SIf (stuck, thn, els) -> Format.fprintf fmt "<stuck if %a %a %a>" pp_stuck stuck pp_expr thn pp_expr els
  | LC.SMatchList (stuck, nil_case, cons_case) ->
      Format.fprintf fmt "<stuck match %a %a %a>" pp_stuck stuck pp_expr nil_case pp_expr cons_case

and pp_vtype fmt = function
  | LC.VTInt -> Format.pp_print_string fmt "int"
  | LC.VTFunc -> Format.pp_print_string fmt "func"
  | LC.VTBool -> Format.pp_print_string fmt "bool"
  | LC.VTList -> Format.pp_print_string fmt "list"

let value_to_string value = Format.asprintf "%a" pp_value value

type step_writer = Memo.exec_result -> unit

let with_steps_writer steps_path f =
  let oc = open_out_gen [ Open_creat; Open_trunc; Open_text; Open_wronly ] 0o644 steps_path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () ->
      let write_steps_json (r : Memo.exec_result) : unit =
        Printf.fprintf oc "{\"step\":%d,\"without_memo_step\":%d}\n" r.step r.without_memo_step;
        flush oc
      in
      f write_steps_json)

let eval_expression ~write_steps x =
  let exec_res = LC.eval (LC.from_ocaml_expr x) (LC.from_ocaml_list LC.from_ocaml_value LC.Nil) in
  write_steps exec_res;
  LC.to_ocaml_value exec_res.words

let quicksort_nexpr : nexpr =
  parse_nexpr
    "let append = fix append xs. fun ys -> match xs with [] -> ys | h :: t -> h :: (append t ys) in let filter = fun p \
     -> fix filter xs. match xs with [] -> [] | h :: t -> if p h then h :: (filter t) else (filter t) in fix quicksort \
     xs. match xs with [] -> [] | pivot :: rest -> let smaller = quicksort (filter (fun x -> x < pivot) rest) in let \
     greater = quicksort (filter (fun x -> x >= pivot) rest) in append smaller (pivot :: greater)"

let quicksort_expr = expr_of_nexpr quicksort_nexpr

let mapinc =
  LC.EFix
    (LC.EMatchList
       ( LC.EVar (nat_from_int 0),
         LC.EVar (nat_from_int 0),
         LC.ECons
           (LC.EPlus (LC.EInt 1, LC.EVar (nat_from_int 1)), LC.EApp (LC.EVar (nat_from_int 3), LC.EVar (nat_from_int 0)))
       ))

let random_list =
  [
    17;
    81;
    91;
    31;
    90;
    70;
    14;
    62;
    9;
    20;
    70;
    75;
    23;
    25;
    8;
    70;
    88;
    53;
    95;
    50;
    36;
    47;
    94;
    23;
    18;
    55;
    57;
    44;
    22;
    97;
    96;
    74;
    48;
    76;
    17;
    8;
    31;
    67;
    69;
    80;
    29;
    16;
    75;
    13;
    54;
    97;
    65;
    85;
    45;
    58;
  ]

let random_list_expr = List.fold_right (fun n acc -> LC.ECons (LC.EInt n, acc)) random_list LC.ENil

let run_simple_benchmark ~write_steps () =
  let eval expr = eval_expression ~write_steps expr in
  print_endline "mapinc:";
  print_endline (expr_to_string mapinc);
  print_endline (value_to_string (eval (LC.EInt 42)));
  let repeat_list x =
    let rec build n acc = if n == 0 then acc else build (n - 1) (LC.ECons (LC.EInt 1, acc)) in
    build x LC.ENil
  in
  let nats x acc =
    let rec build n = if n == x then acc else LC.ECons (LC.EInt n, build (n + 1)) in
    build 0
  in
  print_endline (value_to_string (eval (LC.EApp (mapinc, repeat_list 2))));
  print_endline (value_to_string (eval (LC.EApp (mapinc, repeat_list 40))));
  print_endline (value_to_string (eval (LC.EApp (mapinc, repeat_list 45))));
  print_endline (value_to_string (eval (LC.EApp (mapinc, nats 40 LC.ENil))));
  print_endline (value_to_string (eval (LC.EApp (mapinc, nats 45 LC.ENil))));
  print_endline (value_to_string (eval (LC.EApp (mapinc, nats 46 LC.ENil))));
  print_endline (value_to_string (eval (LC.EApp (mapinc, nats 45 (nats 45 LC.ENil)))));
  print_endline (value_to_string (eval (LC.ELet (mapinc, LC.EApp (LC.EVar (nat_from_int 0), nats 45 LC.ENil)))))

let run_left_to_right_benchmark ~write_steps () =
  let eval expr = eval_expression ~write_steps expr in
  print_endline "left_to_right quicksort (list fixed):";
  left_to_right quicksort_expr
  |> List.iteri (fun i e ->
      let applied = LC.EApp (e, random_list_expr) in
      Printf.printf "step %d value: %s\n" i (value_to_string (eval applied)))

let run_demanded_benchmark ~write_steps () =
  let eval expr = eval_expression ~write_steps expr in
  print_endline "demanded_interactive quicksort (list fixed):";
  demanded_interactive quicksort_expr (fun i e ->
      Printf.printf "step %d ast: %s\n" i (expr_to_string e);
      let applied = LC.EApp (e, random_list_expr) in
      let value = eval applied in
      Printf.printf "step %d value: %s\n" i (value_to_string value);
      value)
