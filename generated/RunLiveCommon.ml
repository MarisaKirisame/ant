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
    | NEAnd (lhs, rhs) -> LC.EIf (aux ctx lhs, aux ctx rhs, LC.EFalse)
    | NEUnit -> LC.EUnit
    | x -> failwith ("expr_of_nexpr not implemente for expr: " ^ Format.asprintf "%a" pp_nexpr x)
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
    | EUnit -> NEUnit
    | EPair (lhs, rhs) -> NEPair (aux ctx lhs, aux ctx rhs)
    | EZro x -> NEZro (aux ctx x)
    | EFst x -> NEFst (aux ctx x)
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

let pp_expr fmt expr = pp_nexpr fmt (nexpr_of_expr expr)
let expr_to_string expr = Format.asprintf "%a" pp_expr expr
let rec len_live_list = function LC.Nil -> 0 | LC.Cons (_, tl) -> 1 + len_live_list tl

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

let eval_expression ~memo ~write_steps x =
  let exec_res = LC.eval memo (LC.from_ocaml_expr x) (LC.from_ocaml_list LC.from_ocaml_value LC.Nil) in
  write_steps exec_res;
  LC.to_ocaml_value exec_res.words

let quicksort_nexpr =
  parse_nexpr
    "let append = fix append xs. fun ys -> match xs with [] -> ys | h :: t -> h :: (append t ys) in let filter = fun p \
     -> fix filter xs. match xs with [] -> [] | h :: t -> if p h then h :: (filter t) else (filter t) in fix quicksort \
     xs. match xs with [] -> [] | pivot :: rest -> let smaller = quicksort (filter (fun x -> x < pivot) rest) in let \
     greater = quicksort (filter (fun x -> x >= pivot) rest) in append smaller (pivot :: greater)"

let quicksort_expr = expr_of_nexpr quicksort_nexpr

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
