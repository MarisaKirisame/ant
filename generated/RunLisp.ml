module LC = LispCEK
module Plain = LispPlain
module Memo = Ant.Memo
module Word = Ant.Word.Word
module Frontend = LispFrontend
module Json = Yojson.Safe
module State = Ant.State

let steps_file = "eval_steps_lisp.json"

let with_memo f =
  let memo = Memo.init_memo () in
  f memo

let rec lisp_list_of_list = function [] -> LC.Nil | x :: xs -> LC.Cons (x, lisp_list_of_list xs)
let rec list_of_lisp = function LC.Nil -> [] | LC.Cons (hd, tl) -> hd :: list_of_lisp tl
let env_values_of_seq seq = list_of_lisp (LC.to_ocaml_list LC.to_ocaml_value seq)
let empty_env_seq = LC.from_ocaml_list LC.from_ocaml_value (lisp_list_of_list [])
let rec expr_list exprs = match exprs with [] -> LC.EAtom LC.ANIL | hd :: tl -> LC.ECons (hd, expr_list tl)
let seq_of_expr_list exprs = lisp_list_of_list exprs |> LC.from_ocaml_list LC.from_ocaml_expr
let int_of_seq seq = Word.get_value (Memo.to_word seq)

type step_writer = Memo.exec_result -> unit

let current_memo : State.memo option ref = ref None
let current_write_steps : step_writer option ref = ref None

let with_outchannel steps_path f =
  let oc = open_out_gen [ Open_creat; Open_trunc; Open_text; Open_wronly ] 0o644 steps_path in
  Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () -> f oc)

let rec plain_symbol_of_lc = function
  | LC.SLambda -> Plain.SLambda
  | LC.SDefine -> Plain.SDefine
  | LC.SQuote -> Plain.SQuote
  | LC.SEq -> Plain.SEq
  | LC.SCons -> Plain.SCons
  | LC.SCond -> Plain.SCond
  | LC.SAtom -> Plain.SAtom
  | LC.SCar -> Plain.SCar
  | LC.SCdr -> Plain.SCdr
  | LC.SNull -> Plain.SNull
  | LC.SError -> Plain.SError
  | LC.SIf -> Plain.SIf
  | LC.SDefvar -> Plain.SDefvar
  | LC.SPair -> Plain.SPair
  | LC.SSymbol -> Plain.SSymbol
  | LC.STrue -> Plain.STrue
  | LC.SFalse -> Plain.SFalse
  | LC.SVar -> Plain.SVar
  | LC.SNum -> Plain.SNum
  | LC.SAnd -> Plain.SAnd
  | LC.SElse -> Plain.SElse
  | LC.SPlus -> Plain.SPlus

let rec plain_atom_of_lc = function
  | LC.AVar i -> Plain.AVar i
  | LC.ANumber i -> Plain.ANumber i
  | LC.ASymbol sym -> Plain.ASymbol (plain_symbol_of_lc sym)
  | LC.ANIL -> Plain.ANIL

let rec plain_expr_of_lc = function
  | LC.EAtom atom -> Plain.EAtom (plain_atom_of_lc atom)
  | LC.ECons (l, r) -> Plain.ECons (plain_expr_of_lc l, plain_expr_of_lc r)

let string_of_symbol = function
  | LC.SLambda -> "lambda"
  | LC.SDefine -> "define"
  | LC.SQuote -> "quote"
  | LC.SEq -> "eq"
  | LC.SCons -> "cons"
  | LC.SCond -> "cond"
  | LC.SAtom -> "atom"
  | LC.SCar -> "car"
  | LC.SCdr -> "cdr"
  | LC.SNull -> "null"
  | LC.SError -> "error"
  | LC.SIf -> "if"
  | LC.SDefvar -> "defvar"
  | LC.SPair -> "pair"
  | LC.SSymbol -> "symbol"
  | LC.STrue -> "true"
  | LC.SFalse -> "false"
  | LC.SVar -> "var"
  | LC.SNum -> "num"
  | LC.SAnd -> "and"
  | LC.SElse -> "else"
  | LC.SPlus -> "+"

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
  let memo_profile = Ant.Profile.dump_profile Ant.Profile.memo_profile |> json_of_profile in
  let plain_profile = Ant.Profile.dump_profile Ant.Profile.plain_profile |> json_of_profile in
  Printf.fprintf oc
    "{\"name\":\"exec_time\",\"step\":%d,\"without_memo_step\":%d,\"memo_profile\":%s,\"plain_profile\":%s}\n" r.step
    r.without_memo_step memo_profile plain_profile;
  flush oc

let write_memo_stats_json oc (memo : State.memo) : unit =
  let stats = Memo.memo_stats memo in
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
  Buffer.add_string buf "],\"size_vs_sc\":[";
  List.iteri
    (fun i (entry : Memo.size_vs_sc) ->
      if i > 0 then Buffer.add_char buf ',';
      Buffer.add_string buf "{\"size\":";
      Buffer.add_string buf (string_of_int entry.size);
      Buffer.add_string buf ",\"sc\":";
      Buffer.add_string buf (string_of_int entry.sc);
      Buffer.add_char buf '}')
    stats.size_vs_sc;
  Buffer.add_string buf "]}\n";
  Buffer.output_buffer oc buf;
  flush oc

let eval_plain_slot = Ant.Profile.register_slot Ant.Profile.plain_profile "eval_plain"

let eval_plain expr =
  let plain_expr = plain_expr_of_lc expr in
  ignore (Ant.Profile.with_slot eval_plain_slot (fun () -> Plain.eval plain_expr Plain.Nil))

let string_of_atom = function
  | LC.AVar i -> Printf.sprintf "#%d" i
  | LC.ANumber i -> Printf.sprintf "%d" i
  | LC.ASymbol sym -> Printf.sprintf "%s" (string_of_symbol sym)
  | LC.ANIL -> "()"

let rec string_of_expr = function
  | LC.EAtom atom -> string_of_atom atom
  | LC.ECons (LC.EAtom (LC.ASymbol LC.SQuote), LC.ECons (hd, tl)) when tl = LC.EAtom LC.ANIL ->
      let l = string_of_expr hd in
      Printf.sprintf "'%s" l
  | LC.ECons (hd, tl) ->
      let arr = Dynarray.create () in
      let t = ref tl in
      while match !t with LC.ECons _ -> true | _ -> false do
        match !t with
        | LC.ECons (x, y) ->
            Dynarray.add_last arr x;
            t := y
        | _ -> failwith "impossible"
      done;
      (* assert (!t = LC.EAtom LC.ANIL); *)
      "(" ^ Dynarray.fold_left (fun acc x -> acc ^ " " ^ string_of_expr x) (string_of_expr hd) arr ^ ")"

let rec string_of_value = function
  | LC.VNumber x -> Printf.sprintf "%d" x
  | LC.VSymbol sym -> Printf.sprintf "%s" (string_of_symbol sym)
  | LC.VNIL -> "()"
  | LC.VCons (x, y) -> Printf.sprintf "(%s . %s)" (string_of_value x) (string_of_value y)
  | LC.VClosure _ -> "PROCEDURE"

let string_of_expr_list exprs = "[" ^ String.concat "; " (List.map string_of_expr exprs) ^ "]"
let string_of_option show = function LC.None -> "None" | LC.Some value -> "Some " ^ show value

type eval_details = { value : LC.value; runtime_seconds : float; steps_with_memo : int; steps_without_memo : int }

let expect_equal ?(show = fun _ -> "<value>") label expected actual =
  if expected = actual then Printf.printf "[ok] %s\n" label
  else
    let message = Printf.sprintf "%s: expected %s but got %s" label (show expected) (show actual) in
    failwith message

let expect_value msg (x : LC.value) (y : LC.value) = expect_equal ~show:string_of_value msg x y
let expr_of_int_list ints = List.map (fun n -> LC.EAtom (LC.ANumber n)) ints |> expr_list
let rec value_list_of_ints = function [] -> LC.VNIL | x :: xs -> LC.VCons (LC.VNumber x, value_list_of_ints xs)

let eval_expr_with_details expr =
  let seq = LC.from_ocaml_expr expr in
  let start = Unix.gettimeofday () in
  let res =
    match !current_memo with
    | Some memo -> LC.eval memo seq empty_env_seq
    | None -> with_memo (fun memo -> LC.eval memo seq empty_env_seq)
  in
  let stop = Unix.gettimeofday () in
  eval_plain expr;
  Option.iter (fun write_steps -> write_steps res) !current_write_steps;
  {
    value = LC.to_ocaml_value res.words;
    runtime_seconds = stop -. start;
    steps_with_memo = res.step;
    steps_without_memo = res.without_memo_step;
  }

let eval_expr expr =
  let details = eval_expr_with_details expr in
  details.value

let eval_string_with_details ?(print_compiled = true) code =
  let expr = Frontend.compile_string code in
  if print_compiled then (
    Printf.printf "compiled: %s\n" (string_of_expr expr);
    print_endline "")
  else ();
  eval_expr_with_details expr

let eval_string ?(print_compiled = true) code =
  let details = eval_string_with_details ~print_compiled code in
  details.value

let expect_eval label code expected =
  let result = eval_string code in
  expect_value label expected result

let test_atom_rejects_cons () =
  let code = "(atom (cons (quote 1) (quote 2)))" in
  expect_eval "atom reports pairs as false" code LC.VNIL

let test_eq_number_literals () = expect_eval "eq returns true for identical numbers" "(eq 4 4)" (LC.VNumber 0)
let test_eq_number_literals_false () = expect_eval "eq returns false for distinct numbers" "(eq 4 5)" LC.VNIL

let test_cond_short_circuits () =
  let code = "(cond ((atom (quote ())) 42) ((quote (0)) 0))" in
  expect_eval "cond picks the first true branch" code (LC.VNumber 42)

let test_car_after_cons () =
  let code = "(car (cons 5 (quote ())))" in
  expect_eval "car unwraps the head of cons cells" code (LC.VNumber 5)

let test_mapinc_list () =
  let list_literal ints =
    let body = String.concat " " (List.map string_of_int ints) in
    Printf.sprintf "(quote (%s))" body
  in
  let code_for ints =
    Printf.sprintf
      "((define mapinc (xs) (cond ((null xs) (quote ())) (else (cons (+ (car xs) 1) (mapinc (cdr xs)))))) (mapinc %s))"
      (list_literal ints)
  in
  let run_case n =
    let input = List.init n Fun.id in
    let expected = value_list_of_ints (List.map (fun x -> x + 1) input) in
    expect_eval (Printf.sprintf "mapinc increments %d elements" n) (code_for input) expected
  in
  List.iter run_case [ 2; 40; 45 ]

let read_file_content filename = In_channel.with_open_text filename In_channel.input_all
let wrap_tests_output_path = "eval_lisp_wrap.json"

let replace_code_placeholder template replacement =
  let placeholder = "%CODE%" in
  let template_len = String.length template in
  let placeholder_len = String.length placeholder in
  let rec search idx =
    if template_len < placeholder_len || idx > template_len - placeholder_len then None
    else if String.sub template idx placeholder_len = placeholder then Some idx
    else search (idx + 1)
  in
  match search 0 with
  | None -> failwith "placeholder %CODE% not found in Lisp.lisp template"
  | Some idx ->
      let before = String.sub template 0 idx in
      let after = String.sub template (idx + placeholder_len) (template_len - idx - placeholder_len) in
      before ^ replacement ^ after

let wrap_code_for_depth depth =
  if depth < 0 then invalid_arg "wrap depth cannot be negative";
  let base_expr = "((lambda (0) (var 0)) 99)" in
  if depth = 0 then Printf.sprintf "'%s" base_expr
  else
    let rec aux remaining current_expr quoted =
      if remaining = 0 then current_expr
      else
        let expr_to_wrap = if quoted then current_expr else Printf.sprintf "'%s" current_expr in
        let wrapped = Printf.sprintf "(wrap %s)" expr_to_wrap in
        aux (remaining - 1) wrapped true
    in
    aux depth base_expr false

(* let write_json_file filename json =
  Out_channel.with_open_text filename (fun oc ->
      Json.pretty_to_channel oc json;
      output_char oc '\n') *)

let json_of_wrap_result depth code details =
  `Assoc
    [
      ("wrap_depth", `Int depth);
      ("code", `String code);
      ("value", `String (string_of_value details.value));
      ("runtime_seconds", `Float details.runtime_seconds);
      ("step", `Int details.steps_with_memo);
      ("without_memo_step", `Int details.steps_without_memo);
    ]

let run_wrap_code_tests () =
  let template = read_file_content "./generated/Lisp.lisp" in
  let wrap_depths = [ 1 ] in
  let results =
    List.map
      (fun depth ->
        let code_for_placeholder = wrap_code_for_depth depth in
        let program = replace_code_placeholder template code_for_placeholder in
        Printf.printf "Running wrap depth %d test...\n" depth;
        Out_channel.flush Stdio.stdout;
        let details = eval_string_with_details ~print_compiled:false program in
        expect_value (Printf.sprintf "wrap depth %d returns 99" depth) (LC.VNumber 99) details.value;
        Out_channel.flush Stdio.stdout;
        (depth, code_for_placeholder, details))
      wrap_depths
  in
  (* let json =
    `Assoc [ ("tests", `List (List.map (fun (d, code, details) -> json_of_wrap_result d code details) results)) ]
  in *)
  (* write_json_file wrap_tests_output_path json; *)
  let result = List.map (fun (d, code, details) -> Json.to_string (json_of_wrap_result d code details)) results in
  Out_channel.with_open_text wrap_tests_output_path (fun oc ->
      for i = 0 to List.length result - 1 do
        Out_channel.output_string oc (List.nth result i);
        output_char oc '\n'
      done);
  Printf.printf "Saved wrap test results to %s\n" wrap_tests_output_path

let run () =
  LC.with_runtime_ (fun () ->
      with_outchannel steps_file (fun oc ->
          let memo = Memo.init_memo () in
          let write_steps = write_steps_json oc in
          current_memo := Some memo;
          current_write_steps := Some write_steps;
          Fun.protect
            ~finally:(fun () ->
              current_memo := None;
              current_write_steps := None)
            (fun () ->
              test_atom_rejects_cons ();
              test_eq_number_literals ();
              test_eq_number_literals_false ();
              test_cond_short_circuits ();
              test_car_after_cons ();
              test_mapinc_list ();
              (* run_wrap_code_tests (); *)
              write_memo_stats_json oc memo));
      print_endline "LispCEK smoke tests completed.")
