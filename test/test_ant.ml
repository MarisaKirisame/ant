open Ant

let parse content =
  let lexbuf = Lexing.from_string content in
  try Parser.prog Lexer.tokenize lexbuf with
  | Parser.Error _ -> failwith "parse error in test fixture"
  | Lexer.Error _ -> failwith "lex error in test fixture"

let read_all file = In_channel.with_open_text file In_channel.input_all

let example_path name =
  let candidates = [ Filename.concat "examples" name; Filename.concat ".." (Filename.concat "examples" name) ] in
  let rec pick = function [] -> None | path :: rest -> if Sys.file_exists path then Some path else pick rest in
  match pick candidates with Some path -> path | None -> failwith ("missing example fixture: " ^ name)

let expect_failure ~needle f =
  try
    let _ = f () in
    failwith ("expected failure containing: " ^ needle)
  with exn ->
    let message = Printexc.to_string exn in
    assert (Core.String.is_substring message ~substring:needle)

let anf_string source =
  source |> parse |> Typing.top_type_of_prog |> Transform.anf_prog |> Syntax.pp_prog |> Syntax.string_of_document

let anf_example_string name = name |> example_path |> read_all |> anf_string
let frontend_prog source = source |> parse |> Typing.top_type_of_prog |> Transform.anf_prog |> Pat.compile
let frontend_example name = name |> example_path |> read_all |> frontend_prog
let compile_with backend prog = prog |> backend |> Syntax.string_of_document

let () =
  let anf = anf_string "let _ = (if true then 1 else 2) + 3;;" in
  assert (Core.String.is_substring anf ~substring:"letcont");
  assert (Core.String.is_substring anf ~substring:"jump")

let () =
  let anf = anf_string "type t = | A of int | B of int;;\nlet _ = (match A 1 with | A x -> x | B y -> y) + 1;;" in
  assert (Core.String.is_substring anf ~substring:"letcont");
  assert (Core.String.is_substring anf ~substring:"jump")

let () =
  let anf = anf_string "let rec loop = fun x -> loop x;;" in
  assert (Core.String.is_substring anf ~substring:"let rec loop")

let () =
  let anf = anf_example_string "AnfJoinIf.ant" in
  assert (Core.String.is_substring anf ~substring:"letcont");
  assert (Core.String.is_substring anf ~substring:"jump")

let () =
  let anf = anf_example_string "AnfJoinMatch.ant" in
  assert (Core.String.is_substring anf ~substring:"letcont");
  assert (Core.String.is_substring anf ~substring:"jump")

let () =
  let info = SynInfo.empty_info in
  let prog =
    ( [
        Syntax.Term
          (Syntax.BRecC
             [
               ( Syntax.PVar ("j", info),
                 Syntax.Lam
                   ( [ Syntax.PVar ("x", info) ],
                     Syntax.Jump (Syntax.Var ("j", info), [ Syntax.Var ("x", info) ], info),
                     info ),
                 info );
             ]);
      ],
      info )
  in
  let rendered = Syntax.string_of_document (Syntax.pp_prog prog) in
  assert (Core.String.is_substring rendered ~substring:"letcont rec")

let () =
  let info = SynInfo.empty_info in
  let jump_prog =
    ([ Syntax.Term (Syntax.BSeq (Syntax.Jump (Syntax.Var ("k", info), [ Syntax.Int 1 ], info), info)) ], info)
  in
  let cont_prog =
    ( [
        Syntax.Term
          (Syntax.BCont
             (Syntax.PVar ("k", info), Syntax.Lam ([ Syntax.PVar ("x", info) ], Syntax.Var ("x", info), info), info));
      ],
      info )
  in
  let rec_cont_prog =
    ( [
        Syntax.Term
          (Syntax.BRecC
             [ (Syntax.PVar ("k", info), Syntax.Lam ([ Syntax.PVar ("x", info) ], Syntax.Var ("x", info), info), info) ]);
      ],
      info )
  in
  let plain_jump = compile_with CompilePlain.Backend.compile jump_prog in
  assert (Core.String.is_substring plain_jump ~substring:"k");
  let plain_cont = compile_with CompilePlain.Backend.compile cont_prog in
  assert (Core.String.is_substring plain_cont ~substring:"let");
  let plain_rec_cont = compile_with CompilePlain.Backend.compile rec_cont_prog in
  assert (Core.String.is_substring plain_rec_cont ~substring:"let rec");
  let seq_jump = compile_with CompileSeq.Backend.compile jump_prog in
  assert (Core.String.is_substring seq_jump ~substring:"k");
  let seq_cont = compile_with CompileSeq.Backend.compile cont_prog in
  assert (Core.String.is_substring seq_cont ~substring:"let");
  let seq_rec_cont = compile_with CompileSeq.Backend.compile rec_cont_prog in
  assert (Core.String.is_substring seq_rec_cont ~substring:"let rec");
  expect_failure ~needle:"jump" (fun () -> ignore (compile_with CompileMemo.Backend.compile jump_prog));
  expect_failure ~needle:"CompileMemo backend placeholder: Term BCont not implemented" (fun () ->
      ignore (compile_with CompileMemo.Backend.compile cont_prog));
  expect_failure ~needle:"CompileMemo backend placeholder: Term BRecC not implemented" (fun () ->
      ignore (compile_with CompileMemo.Backend.compile rec_cont_prog))

let () =
  let plain_if = compile_with CompilePlain.Backend.compile (frontend_example "AnfJoinIf.ant") in
  assert (Core.String.is_substring plain_if ~substring:"let");
  let plain_match = compile_with CompilePlain.Backend.compile (frontend_example "AnfJoinMatch.ant") in
  assert (Core.String.is_substring plain_match ~substring:"match");
  let seq_if = compile_with CompileSeq.Backend.compile (frontend_example "AnfJoinIf.ant") in
  assert (Core.String.is_substring seq_if ~substring:"if");
  let seq_match = compile_with CompileSeq.Backend.compile (frontend_example "AnfJoinMatch.ant") in
  assert (Core.String.is_substring seq_match ~substring:"match")

let () =
  let regmemo_if = compile_with CompileRegMemo.Backend.compile (frontend_prog "let f = fun x -> (if x then 1 else 2) + 3;;")
  in
  assert (Core.String.is_substring regmemo_if ~substring:"function f#");
  assert (Core.String.is_substring regmemo_if ~substring:"branch");
  assert (Core.String.is_substring regmemo_if ~substring:"jump");
  let regmemo_global =
    compile_with CompileRegMemo.Backend.compile
      (frontend_prog "type t = | A of int | B of int;; let x = (match A 1 with | A n -> n | B m -> m) + 1;;")
  in
  assert (Core.String.is_substring regmemo_global ~substring:"global x#");
  assert (Core.String.is_substring regmemo_global ~substring:"match");
  let regmemo_call =
    compile_with CompileRegMemo.Backend.compile
      (frontend_prog "let g = fun y -> y;; let f = fun x -> g x;;")
  in
  assert (Core.String.is_substring regmemo_call ~substring:"call @g")

let () =
  let info = SynInfo.empty_info in
  let prog =
    ( [
        Syntax.Term
          (Syntax.BOne
             ( Syntax.PVar ("f", info),
               Syntax.Lam
                 ( [ Syntax.PVar ("x", info) ],
                   Syntax.Let
                     ( Syntax.BRecC
                         [
                           ( Syntax.PVar ("loop", info),
                             Syntax.Lam
                               ( [ Syntax.PVar ("y", info) ],
                                 Syntax.Jump (Syntax.Var ("loop", info), [ Syntax.Var ("y", info) ], info),
                                 info ),
                             info );
                         ],
                       Syntax.Jump (Syntax.Var ("loop", info), [ Syntax.Var ("x", info) ], info),
                       info ),
                   info ),
               info ));
      ],
      info )
  in
  let rendered = compile_with CompileRegMemo.Backend.compile prog in
  assert (Core.String.is_substring rendered ~substring:"function f#");
  assert (Core.String.is_substring rendered ~substring:"loop");
  assert (Core.String.is_substring rendered ~substring:"jump")

let () =
  let info = SynInfo.empty_info in
  let bad_prog =
    ( [
        Syntax.Term
          (Syntax.BCont
             (Syntax.PVar ("k", info), Syntax.Lam ([ Syntax.PVar ("x", info) ], Syntax.Var ("x", info), info), info));
      ],
      info )
  in
  expect_failure ~needle:"CompileRegMemo Phase 1 unsupported: top-level BCont" (fun () ->
      ignore (compile_with CompileRegMemo.Backend.compile bad_prog))

module TestMonoidHash (M : Hash.MonoidHash) = struct
  let test_hash () =
    let open M in
    let rec print_list = function [] -> "" | e :: l -> (string_of_int @@ hash e) ^ " " ^ print_list l in
    print_endline "Testing MonoidHash";
    print_endline name;
    let h1 = from_int 114514 in
    let h2 = mul h1 unit in
    let h3 = mul unit h1 in
    let init = hash h1 in
    let hlist = [ h1; h2; h3 ] in
    if List.exists (fun i -> hash i <> init) [ h1; h2; h3 ] then failwith ("hash is not idempotent " ^ print_list hlist);
    let l1 = List.init 10000 (fun _ -> Int64.to_int @@ Random.bits64 ()) in
    let l2 = List.init 10000 (fun _ -> Int64.to_int @@ Random.bits64 ()) in
    let list = l1 @ l2 in
    let foldl x = List.fold_left (fun acc i -> mul acc (from_int i)) unit x in
    let foldr x = List.fold_right (fun i acc -> mul (from_int i) acc) x unit in
    let foldx x = List.fold_left (fun acc i -> mul unit (mul (mul acc (from_int i)) unit)) unit x in
    let h4 = foldl list in
    let h5 = foldr list in
    let h6 = mul (foldl l1) (foldl l2) in
    let h7 = mul (foldr l1) (foldr l2) in
    let h8 = mul (foldl l1) (foldr l2) in
    let h9 = foldx list in
    let init = hash h4 in
    let hlist = [ h4; h5; h6; h7; h8; h9 ] in
    if List.exists (fun i -> hash i <> init) hlist then failwith ("hash is not associative " ^ print_list hlist);
    let random_assoc =
      let rec aux left = function
        | [] -> left
        | hd :: tl ->
            let current = from_int hd in
            if Random.bool () then aux (mul left current) tl else mul left (aux current tl)
      in
      aux unit
    in
    let h10 = random_assoc list in
    if hash h10 <> init then failwith "hash is not associative (random)";
    let gen_hlist_for_single x = List.init 1000 (fun _ -> from_int x) in
    if
      List.exists
        (fun i ->
          let hl = gen_hlist_for_single i in
          let init = hash @@ List.hd hl in
          List.exists (fun h -> hash h <> init) hl)
        list
    then failwith "hash is not deterministic"
end

let _ =
  let x = Intmap.create 32 in
  for i = 0 to 9 do
    Intmap.add x i (i + 1)
  done;
  assert (Intmap.mem x 5);
  assert (Intmap.find x 5 = 6);
  assert (Intmap.find_opt x 5 = Some 6);
  assert (Intmap.length x = 10);
  assert (Intmap.fold (fun _k v acc -> acc + v) x 0 = 55);
  assert (Intmap.fold (fun k _v acc -> acc + k) x 0 = 45);
  Intmap.remove x 5;
  assert (not (Intmap.mem x 5));
  assert (Intmap.find_opt x 5 = None);
  assert (Intmap.length x = 9);
  let module SL2 = TestMonoidHash (Hash.SL2) in
  let module SL2Slow = TestMonoidHash (Hash.SL2Slow) in
  let module MCRC32C = TestMonoidHash (Hash.MCRC32C) in
  let module DebugHash = TestMonoidHash (Hash.DebugHash) in
  (* buggy when length > a threshold *)
  SL2.test_hash ();
  SL2Slow.test_hash ();
  MCRC32C.test_hash ();
  DebugHash.test_hash ()
