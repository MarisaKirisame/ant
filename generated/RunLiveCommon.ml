open Ant
open NamedExpr
module LC = LiveCEK
module LP = LivePlain

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

(*
   LivePlain defines its own (non-memoised) versions of the datatypes that LiveCEK
   uses.  To run the plain interpreter while reusing the existing printers and
   helpers (which are written for LiveCEK values), we convert back and forth.
*)

let rec lp_nat_of_lc = function LC.Z -> LP.Z | LC.S n -> LP.S (lp_nat_of_lc n)
let rec lc_nat_of_lp = function LP.Z -> LC.Z | LP.S n -> LC.S (lc_nat_of_lp n)
let lp_option_of_lc f = function LC.None -> LP.None | LC.Some x -> LP.Some (f x)
let lc_option_of_lp f = function LP.None -> LC.None | LP.Some x -> LC.Some (f x)
let rec lp_list_of_lc f = function LC.Nil -> LP.Nil | LC.Cons (h, t) -> LP.Cons (f h, lp_list_of_lc f t)
let rec lc_list_of_lp f = function LP.Nil -> LC.Nil | LP.Cons (h, t) -> LC.Cons (f h, lc_list_of_lp f t)

let rec lp_expr_of_lc = function
  | LC.EInt i -> LP.EInt i
  | LC.EPlus (l, r) -> LP.EPlus (lp_expr_of_lc l, lp_expr_of_lc r)
  | LC.ELt (l, r) -> LP.ELt (lp_expr_of_lc l, lp_expr_of_lc r)
  | LC.ELe (l, r) -> LP.ELe (lp_expr_of_lc l, lp_expr_of_lc r)
  | LC.EGt (l, r) -> LP.EGt (lp_expr_of_lc l, lp_expr_of_lc r)
  | LC.EGe (l, r) -> LP.EGe (lp_expr_of_lc l, lp_expr_of_lc r)
  | LC.EVar n -> LP.EVar (lp_nat_of_lc n)
  | LC.EAbs b -> LP.EAbs (lp_expr_of_lc b)
  | LC.EApp (f, x) -> LP.EApp (lp_expr_of_lc f, lp_expr_of_lc x)
  | LC.ELet (l, r) -> LP.ELet (lp_expr_of_lc l, lp_expr_of_lc r)
  | LC.ETrue -> LP.ETrue
  | LC.EFalse -> LP.EFalse
  | LC.EIf (c, t, e) -> LP.EIf (lp_expr_of_lc c, lp_expr_of_lc t, lp_expr_of_lc e)
  | LC.ENil -> LP.ENil
  | LC.ECons (h, t) -> LP.ECons (lp_expr_of_lc h, lp_expr_of_lc t)
  | LC.EMatchList (v, n, c) -> LP.EMatchList (lp_expr_of_lc v, lp_expr_of_lc n, lp_expr_of_lc c)
  | LC.EPair (a, b) -> LP.EPair (lp_expr_of_lc a, lp_expr_of_lc b)
  | LC.EZro p -> LP.EZro (lp_expr_of_lc p)
  | LC.EFst p -> LP.EFst (lp_expr_of_lc p)
  | LC.EFix b -> LP.EFix (lp_expr_of_lc b)
  | LC.EHole id -> LP.EHole (lp_option_of_lc (fun x -> x) id)
  | LC.EUnit -> LP.EUnit

let rec lc_expr_of_lp = function
  | LP.EInt i -> LC.EInt i
  | LP.EPlus (l, r) -> LC.EPlus (lc_expr_of_lp l, lc_expr_of_lp r)
  | LP.ELt (l, r) -> LC.ELt (lc_expr_of_lp l, lc_expr_of_lp r)
  | LP.ELe (l, r) -> LC.ELe (lc_expr_of_lp l, lc_expr_of_lp r)
  | LP.EGt (l, r) -> LC.EGt (lc_expr_of_lp l, lc_expr_of_lp r)
  | LP.EGe (l, r) -> LC.EGe (lc_expr_of_lp l, lc_expr_of_lp r)
  | LP.EVar n -> LC.EVar (lc_nat_of_lp n)
  | LP.EAbs b -> LC.EAbs (lc_expr_of_lp b)
  | LP.EApp (f, x) -> LC.EApp (lc_expr_of_lp f, lc_expr_of_lp x)
  | LP.ELet (l, r) -> LC.ELet (lc_expr_of_lp l, lc_expr_of_lp r)
  | LP.ETrue -> LC.ETrue
  | LP.EFalse -> LC.EFalse
  | LP.EIf (c, t, e) -> LC.EIf (lc_expr_of_lp c, lc_expr_of_lp t, lc_expr_of_lp e)
  | LP.ENil -> LC.ENil
  | LP.ECons (h, t) -> LC.ECons (lc_expr_of_lp h, lc_expr_of_lp t)
  | LP.EMatchList (v, n, c) -> LC.EMatchList (lc_expr_of_lp v, lc_expr_of_lp n, lc_expr_of_lp c)
  | LP.EPair (a, b) -> LC.EPair (lc_expr_of_lp a, lc_expr_of_lp b)
  | LP.EZro p -> LC.EZro (lc_expr_of_lp p)
  | LP.EFst p -> LC.EFst (lc_expr_of_lp p)
  | LP.EFix b -> LC.EFix (lc_expr_of_lp b)
  | LP.EHole id -> LC.EHole (lc_option_of_lp (fun x -> x) id)
  | LP.EUnit -> LC.EUnit

let rec lp_vtype_of_lc = function
  | LC.VTInt -> LP.VTInt
  | LC.VTFunc -> LP.VTFunc
  | LC.VTBool -> LP.VTBool
  | LC.VTList -> LP.VTList
  | LC.VTPair -> LP.VTPair

let rec lc_vtype_of_lp = function
  | LP.VTInt -> LC.VTInt
  | LP.VTFunc -> LC.VTFunc
  | LP.VTBool -> LC.VTBool
  | LP.VTList -> LC.VTList
  | LP.VTPair -> LC.VTPair

let rec lp_value_of_lc = function
  | LC.VInt i -> LP.VInt i
  | LC.VAbs (body, env) -> LP.VAbs (lp_expr_of_lc body, lp_list_of_lc lp_value_of_lc env)
  | LC.VUnit -> LP.VUnit
  | LC.VTrue -> LP.VTrue
  | LC.VFalse -> LP.VFalse
  | LC.VNil -> LP.VNil
  | LC.VCons (h, t) -> LP.VCons (lp_value_of_lc h, lp_value_of_lc t)
  | LC.VPair (a, b) -> LP.VPair (lp_value_of_lc a, lp_value_of_lc b)
  | LC.VFix (body, env) -> LP.VFix (lp_expr_of_lc body, lp_list_of_lc lp_value_of_lc env)
  | LC.VStuck s -> LP.VStuck (lp_stuck_of_lc s)

and lc_value_of_lp = function
  | LP.VInt i -> LC.VInt i
  | LP.VAbs (body, env) -> LC.VAbs (lc_expr_of_lp body, lc_list_of_lp lc_value_of_lp env)
  | LP.VUnit -> LC.VUnit
  | LP.VTrue -> LC.VTrue
  | LP.VFalse -> LC.VFalse
  | LP.VNil -> LC.VNil
  | LP.VCons (h, t) -> LC.VCons (lc_value_of_lp h, lc_value_of_lp t)
  | LP.VPair (a, b) -> LC.VPair (lc_value_of_lp a, lc_value_of_lp b)
  | LP.VFix (body, env) -> LC.VFix (lc_expr_of_lp body, lc_list_of_lp lc_value_of_lp env)
  | LP.VStuck s -> LC.VStuck (lc_stuck_of_lp s)

and lp_stuck_of_lc = function
  | LC.SHole (id, env) -> LP.SHole (lp_option_of_lc (fun x -> x) id, lp_list_of_lc lp_value_of_lc env)
  | LC.STypeError (v, ty) -> LP.STypeError (lp_value_of_lc v, lp_vtype_of_lc ty)
  | LC.SIndexError -> LP.SIndexError
  | LC.SApp (s, e) -> LP.SApp (lp_stuck_of_lc s, lp_expr_of_lc e)
  | LC.SAdd0 (s, e) -> LP.SAdd0 (lp_stuck_of_lc s, lp_expr_of_lc e)
  | LC.SAdd1 (v, s) -> LP.SAdd1 (lp_value_of_lc v, lp_stuck_of_lc s)
  | LC.SGt0 (s, e) -> LP.SGt0 (lp_stuck_of_lc s, lp_expr_of_lc e)
  | LC.SGt1 (v, s) -> LP.SGt1 (lp_value_of_lc v, lp_stuck_of_lc s)
  | LC.SIf (s, t, e) -> LP.SIf (lp_stuck_of_lc s, lp_expr_of_lc t, lp_expr_of_lc e)
  | LC.SMatchList (s, n, c) -> LP.SMatchList (lp_stuck_of_lc s, lp_expr_of_lc n, lp_expr_of_lc c)
  | LC.SZro s -> LP.SZro (lp_stuck_of_lc s)
  | LC.SFst s -> LP.SFst (lp_stuck_of_lc s)

and lc_stuck_of_lp = function
  | LP.SHole (id, env) -> LC.SHole (lc_option_of_lp (fun x -> x) id, lc_list_of_lp lc_value_of_lp env)
  | LP.STypeError (v, ty) -> LC.STypeError (lc_value_of_lp v, lc_vtype_of_lp ty)
  | LP.SIndexError -> LC.SIndexError
  | LP.SApp (s, e) -> LC.SApp (lc_stuck_of_lp s, lc_expr_of_lp e)
  | LP.SAdd0 (s, e) -> LC.SAdd0 (lc_stuck_of_lp s, lc_expr_of_lp e)
  | LP.SAdd1 (v, s) -> LC.SAdd1 (lc_value_of_lp v, lc_stuck_of_lp s)
  | LP.SGt0 (s, e) -> LC.SGt0 (lc_stuck_of_lp s, lc_expr_of_lp e)
  | LP.SGt1 (v, s) -> LC.SGt1 (lc_value_of_lp v, lc_stuck_of_lp s)
  | LP.SIf (s, t, e) -> LC.SIf (lc_stuck_of_lp s, lc_expr_of_lp t, lc_expr_of_lp e)
  | LP.SMatchList (s, n, c) -> LC.SMatchList (lc_stuck_of_lp s, lc_expr_of_lp n, lc_expr_of_lp c)
  | LP.SZro s -> LC.SZro (lc_stuck_of_lp s)
  | LP.SFst s -> LC.SFst (lc_stuck_of_lp s)

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

let with_outchannel steps_path f =
  let oc = open_out_gen [ Open_creat; Open_trunc; Open_text; Open_wronly ] 0o644 steps_path in
  Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () -> f oc)

let write_steps_json oc (r : Memo.exec_result) : unit =
  let escape_json s =
    let buf = Buffer.create (String.length s) in
    String.iter
      (function
        | '"' -> Buffer.add_string buf "\\\"" | '\\' -> Buffer.add_string buf "\\\\" | c -> Buffer.add_char buf c)
      s;
    Buffer.contents buf
  in
  let json_of_profile entries =
    let buf = Buffer.create 64 in
    Buffer.add_char buf '[';
    let rec loop first = function
      | [] -> ()
      | (name, time) :: rest ->
          if not first then Buffer.add_char buf ',';
          Buffer.add_char buf '[';
          Buffer.add_char buf '"';
          Buffer.add_string buf (escape_json name);
          Buffer.add_char buf '"';
          Buffer.add_char buf ',';
          Buffer.add_string buf (string_of_int time);
          Buffer.add_char buf ']';
          loop false rest
    in
    loop true entries;
    Buffer.add_char buf ']';
    Buffer.contents buf
  in
  let memo_profile = Profile.dump_profile Profile.memo_profile |> json_of_profile in
  let cek_profile = Profile.dump_profile Profile.cek_profile |> json_of_profile in
  let plain_profile = Profile.dump_profile Profile.plain_profile |> json_of_profile in
  Printf.fprintf oc
    "{\"name\":\"exec_time\",\"step\":%d,\"without_memo_step\":%d,\"memo_profile\":%s,\"plain_profile\":%s, \
     \"cek_profile\":%s}\n"
    r.step r.without_memo_step memo_profile plain_profile cek_profile;
  flush oc

let write_memo_stats_json oc (memo : State.memo) : unit =
  let stats = Memo.memo_stats memo in
  let escape_json s =
    let buf = Buffer.create (String.length s) in
    String.iter
      (function
        | '"' -> Buffer.add_string buf "\\\"" | '\\' -> Buffer.add_string buf "\\\\" | c -> Buffer.add_char buf c)
      s;
    Buffer.contents buf
  in
  let buf = Buffer.create 64 in
  Buffer.add_string buf "{\"name\":\"memo_stats\",\"depth_breakdown\":[";
  let len = Stdlib.Dynarray.length stats.by_depth in
  for i = 0 to len - 1 do
    let node = Stdlib.Dynarray.get stats.by_depth i in
    if i > 0 then Buffer.add_char buf ',';
    Buffer.add_string buf "{\"depth\":";
    Buffer.add_string buf (string_of_int node.depth);
    Buffer.add_string buf ",\"node_count\":";
    Buffer.add_string buf (string_of_int node.node_count);
    Buffer.add_char buf '}'
  done;
  Buffer.add_string buf "],\"rule_stat\":[";
  List.iteri
    (fun i (entry : Memo.rule_stat) ->
      if i > 0 then Buffer.add_char buf ',';
      Buffer.add_string buf "{\"size\":";
      Buffer.add_string buf (string_of_int entry.size);
      Buffer.add_string buf ",\"pvar_length\":";
      Buffer.add_string buf (string_of_int entry.pvar_length);
      Buffer.add_string buf ",\"sc\":";
      Buffer.add_string buf (string_of_int entry.sc);
      Buffer.add_string buf ",\"hit_count\":";
      Buffer.add_string buf (string_of_int entry.hit_count);
      Buffer.add_string buf ",\"insert_time\":";
      Buffer.add_string buf (string_of_int entry.insert_time);
      Buffer.add_string buf ",\"depth\":";
      Buffer.add_string buf (string_of_int entry.depth);
      Buffer.add_string buf ",\"rule\":\"";
      Buffer.add_string buf "" (*escape_json (Lazy.force entry.rule)*);
      Buffer.add_char buf '"';
      Buffer.add_char buf '}')
    stats.rule_stat;
  Buffer.add_string buf "]}\n";
  Buffer.output_buffer oc buf;
  flush oc

(* Evaluate using the direct (non-CEK) interpreter defined in LivePlain.  We expose
   the same LC.value interface by converting the input expression and environment
   to LivePlain, running it, then converting the resulting value back.  Timing is
   recorded via the shared profiler and consumed by write_steps_json. *)
let eval_plain_slot = Profile.register_slot Profile.plain_profile "eval_plain"
let eval_cek_slot = Profile.register_slot Profile.cek_profile "eval_cek"

let eval_plain (expr : LC.expr) : LC.value =
  let env = LC.Nil in
  let lp_expr = lp_expr_of_lc expr in
  let lp_env = lp_list_of_lc lp_value_of_lc env in
  Gc.full_major ();
  let lp_result = Profile.with_slot eval_plain_slot (fun () -> LP.eval lp_expr lp_env) in
  (*lc_value_of_lp lp_result*)
  Profile.with_slot eval_cek_slot (fun () ->
      LC.to_ocaml_value
        (Memo.exec_cek_raw
           (Memo.pc_to_exp (Common.int_to_pc 4))
           (Dynarray.of_list [ LC.from_ocaml_expr expr; LC.from_ocaml_list LC.from_ocaml_value env ])
           (Memo.from_constructor LC.tag_cont_done)))

let eval_expression ~memo ~write_steps x =
  let exec_res = LC.eval memo (LC.from_ocaml_expr x) (LC.from_ocaml_list LC.from_ocaml_value LC.Nil) in
  let _ = eval_plain x in
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

(*let random_list = random_list @ random_list @ random_list*)
let random_list =
  random_list @ random_list @ random_list @ random_list @ random_list @ random_list @ random_list @ random_list

let random_list_expr = List.fold_right (fun n acc -> LC.ECons (LC.EInt n, acc)) random_list LC.ENil
