open Ant
open Common
open Word
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
  let rec fresh ?hint () =
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
    | NEHole -> LC.EHole
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
    | LC.EHole -> NEHole
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
  | LC.SHole env -> Format.fprintf fmt "<hole env=%d>" (len_live_list env)
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
let steps_output_path = "eval_steps.json"

let steps_out : out_channel Lazy.t =
  lazy (open_out_gen [ Open_creat; Open_trunc; Open_text; Open_wronly ] 0o644 steps_output_path)

let () = at_exit (fun () -> if Lazy.is_val steps_out then close_out_noerr (Lazy.force steps_out))

let write_steps_json (r : Memo.exec_result) : unit =
  let oc = Lazy.force steps_out in
  Printf.fprintf oc "{\"step\":%d,\"without_memo_step\":%d}\n" r.step r.without_memo_step;
  flush oc

let quicksort_nexpr : nexpr =
  parse_nexpr
    "let append = fix append xs. fun ys -> match xs with [] -> ys | h :: t -> h :: (append t ys) in let filter = fun p \
     -> fix filter xs. match xs with [] -> [] | h :: t -> if p h then h :: (filter p t) else (filter p t) in let lt = \
     _ in let ge = _ in fix quicksort xs. match xs with [] -> [] | pivot :: rest -> let smaller = filter (fun x -> lt \
     x pivot) rest in let greater = filter (fun x -> ge x pivot) rest in append (quicksort smaller) (pivot :: \
     quicksort greater)"

let eval_expression x =
  let exec_res = LC.eval (LC.from_ocaml_expr x) (LC.from_ocaml_list LC.from_ocaml_value LC.Nil) in
  write_steps_json exec_res;
  LC.to_ocaml_value exec_res.words

let mapinc =
  LC.EFix
    (LC.EMatchList
       ( LC.EVar (nat_from_int 0),
         LC.EVar (nat_from_int 0),
         LC.ECons
           (LC.EPlus (LC.EInt 1, LC.EVar (nat_from_int 1)), LC.EApp (LC.EVar (nat_from_int 3), LC.EVar (nat_from_int 0)))
       ))

let run () : unit =
  print_endline "mapinc:";
  print_endline (expr_to_string mapinc);
  print_endline (value_to_string (eval_expression (LC.EInt 42)));
  let repeat_list x =
    let rec build n acc = if n == 0 then acc else build (n - 1) (LC.ECons (LC.EInt 1, acc)) in
    build x LC.ENil
  in
  let nats x acc =
    let rec build n = if n == x then acc else LC.ECons (LC.EInt n, build (n + 1)) in
    build 0
  in
  let lt_demo = LC.EIf (LC.ELt (LC.EInt 1, LC.EInt 2), LC.EInt 42, LC.EInt 0) in
  print_endline "lt demo:";
  print_endline (expr_to_string lt_demo);
  print_endline (value_to_string (eval_expression lt_demo));
  print_endline (value_to_string (eval_expression (LC.EApp (mapinc, repeat_list 2))));
  print_endline (value_to_string (eval_expression (LC.EApp (mapinc, repeat_list 40))));
  print_endline (value_to_string (eval_expression (LC.EApp (mapinc, repeat_list 45))));
  print_endline (value_to_string (eval_expression (LC.EApp (mapinc, nats 40 LC.ENil))));
  print_endline (value_to_string (eval_expression (LC.EApp (mapinc, nats 45 LC.ENil))));
  print_endline (value_to_string (eval_expression (LC.EApp (mapinc, nats 46 LC.ENil))));
  print_endline (value_to_string (eval_expression (LC.EApp (mapinc, nats 45 (nats 45 LC.ENil)))));
  print_endline
    (value_to_string (eval_expression (LC.ELet (mapinc, LC.EApp (LC.EVar (nat_from_int 0), nats 45 LC.ENil)))));
  ()
