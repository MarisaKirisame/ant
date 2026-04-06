open Common
open PPrint
open Syntax
open Code
open State
include ControlFlowGraph
module Hashtbl = AntHashtbl

let failf fmt = Printf.ksprintf failwith fmt

type slot = RegAlloc.slot
type block_layout = RegAlloc.block_layout
type unit_allocation = RegAlloc.unit_allocation

type prog_slot_analysis = {
  ir : prog_ir;
  liveness : CfgLiveness.unit_liveness list;
  allocations : unit_allocation list;
}

let lower_prog = AnfToCfg.lower_prog

let live_on_jump_edge (succ : block) (succ_info : CfgLiveness.block_liveness) (args : operand list) =
  try
    let live_params =
      List.fold_left2
        (fun live param arg ->
          if IntSet.mem param succ_info.live_params then IntMap.add param (RegAlloc.operand_uses arg) live else live)
        IntMap.empty succ.params args
    in
    { CfgLiveness.live_in = succ_info.live_in; live_params }
  with Invalid_argument _ ->
    failf "CompileRegMemo: jump to block b%d expects %d args, got %d" succ.id (List.length succ.params)
      (List.length args)

let successors (block : block) =
  match block.term with
  | Return _ -> []
  | Jump (succ_id, _) -> [ succ_id ]
  | Branch (_, then_id, else_id) -> [ then_id; else_id ]
  | Match (_, arms) -> List.map (fun ({ block; _ } : match_arm) -> block) arms

let allocate_unit_slots = RegAlloc.allocate_unit_slots

let analyze_unit_liveness (unit_ir : unit_ir) : CfgLiveness.unit_liveness =
  CfgLiveness.analyze_unit ~blocks:unit_ir.blocks ~entry_id:unit_ir.entry ~successors
    ~block_id:(fun (block : block) -> block.id)
    ~params:(fun (block : block) -> block.params)
    ~body:(fun (block : block) -> block.body)
    ~transfer_stmt:RegAlloc.transfer_stmt
    ~analyze_term:(fun ~find_block ~find_live (block : block) ->
      let live_on_edge, live_out =
        match block.term with
        | Return _ -> (IntMap.empty, IntSet.empty)
        | Jump (succ_id, args) ->
            let succ = find_block succ_id in
            let succ_info = find_live succ_id in
            let edge_live = live_on_jump_edge succ succ_info args in
            (IntMap.singleton succ_id edge_live, CfgLiveness.edge_live_values edge_live)
        | Branch (_, then_id, else_id) ->
            let then_live = (find_live then_id).live_in in
            let else_live = (find_live else_id).live_in in
            ( IntMap.(
                empty
                |> add then_id { CfgLiveness.live_in = then_live; live_params = IntMap.empty }
                |> add else_id { CfgLiveness.live_in = else_live; live_params = IntMap.empty }),
              IntSet.union then_live else_live )
        | Match (_, arms) ->
            List.fold_left
              (fun (edge_acc, live_acc) ({ block = succ_id; _ } : match_arm) ->
                let succ_live = (find_live succ_id).live_in in
                ( IntMap.add succ_id { CfgLiveness.live_in = succ_live; live_params = IntMap.empty } edge_acc,
                  IntSet.union live_acc succ_live ))
              (IntMap.empty, IntSet.empty) arms
      in
      let live_before_term =
        match block.term with
        | Return operand -> RegAlloc.add_operand_uses IntSet.empty operand
        | Jump _ -> live_out
        | Branch (cond, _, _) -> RegAlloc.add_operand_uses live_out cond
        | Match (cond, _) -> RegAlloc.add_operand_uses live_out cond
      in
      (live_on_edge, live_out, live_before_term))

let analyze_prog (ir : prog_ir) : prog_analysis =
  CfgLiveness.analyze_prog ~units:ir.units ~analyze_unit:analyze_unit_liveness ir

let analyze_prog_slots (ir : prog_ir) : prog_slot_analysis =
  let phase2 = analyze_prog ir in
  let allocations =
    try List.map2 allocate_unit_slots phase2.ir.units phase2.liveness
    with Invalid_argument _ -> failwith "CompileRegMemo: mismatched unit/liveness counts in phase 3"
  in
  { ir = phase2.ir; liveness = phase2.liveness; allocations }

let pp_slot slot = string ("s" ^ string_of_int slot)
let pp_slot_binding values (id, slot) = pp_value_ref values id ^^ space ^^ string "->" ^^ space ^^ pp_slot slot

let pp_slot_assignments (unit_ir : unit_ir) (allocation : unit_allocation) =
  let values = value_table unit_ir in
  match IntMap.bindings allocation.slot_of_value with
  | [] -> string "slots=[]"
  | bindings ->
      string "slots=" ^^ lbracket
      ^^ pp_list (pp_slot_binding values) (comma ^^ space)
           (List.sort
              (fun (id_a, slot_a) (id_b, slot_b) ->
                match Int.compare slot_a slot_b with 0 -> Int.compare id_a id_b | order -> order)
              bindings)
      ^^ rbracket

let pp_block_layout _values block_id (layout : block_layout) =
  string "block" ^^ space
  ^^ string ("b" ^ string_of_int block_id)
  ^^ space ^^ string "params=" ^^ lbracket
  ^^ pp_list pp_slot (comma ^^ space) layout.param_slots
  ^^ rbracket ^^ space ^^ string "frame="
  ^^ string (string_of_int layout.frame_size)

let pp_unit_allocation (unit_ir : unit_ir) (allocation : unit_allocation) =
  let values = value_table unit_ir in
  let scratch_doc =
    match allocation.scratch_slot with Some slot -> string "scratch=" ^^ pp_slot slot | None -> string "scratch=none"
  in
  let block_layouts_doc =
    match IntMap.bindings allocation.block_layouts with
    | [] -> string "layouts=[]"
    | layouts ->
        string "layouts:" ^^ hardline
        ^^ separate_map hardline
             (fun (block_id, layout) -> string "  " ^^ pp_block_layout values block_id layout)
             layouts
  in
  pp_slot_assignments unit_ir allocation ^^ space ^^ string "frame_size="
  ^^ string (string_of_int allocation.frame_size)
  ^^ space ^^ scratch_doc ^^ hardline ^^ block_layouts_doc

let pp_prog_analysis (analysis : prog_analysis) =
  match List.combine analysis.ir.units analysis.liveness with
  | [] -> string "regmemo liveness empty"
  | units ->
      separate_map (hardline ^^ hardline)
        (fun (unit_ir, liveness) ->
          pp_unit_ir unit_ir ^^ hardline ^^ string "liveness:" ^^ hardline ^^ pp_unit_liveness unit_ir liveness)
        units

let pp_prog_ir (prog : prog_ir) =
  match prog.units with [] -> string "regmemo empty" | units -> separate_map (hardline ^^ hardline) pp_unit_ir units

let pp_prog_slot_analysis (analysis : prog_slot_analysis) =
  match List.combine (List.combine analysis.ir.units analysis.liveness) analysis.allocations with
  | [] -> string "regmemo allocation empty"
  | units ->
      separate_map (hardline ^^ hardline)
        (fun ((unit_ir, liveness), allocation) ->
          pp_unit_ir unit_ir ^^ hardline ^^ string "liveness:" ^^ hardline ^^ pp_unit_liveness unit_ir liveness
          ^^ hardline ^^ string "allocation:" ^^ hardline ^^ pp_unit_allocation unit_ir allocation)
        units

type unit_codegen = {
  unit_ir : unit_ir;
  liveness : CfgLiveness.unit_liveness;
  allocation : unit_allocation;
  entry_pc : pc;
  block_pcs : pc IntMap.t;
  block_map : block IntMap.t;
}

type cg_ctx = {
  arity : (string, int) Hashtbl.t;
  ctag : (string, int) Hashtbl.t;
  ctag_name : (string, string) Hashtbl.t;
  constructor_degree : int Dynarray.t;
  conts : (string * (world code -> Value.seq code -> unit code)) Dynarray.t;
  mutable conts_count : int;
  func_entry : (string, unit_codegen) Hashtbl.t;
  codes : (world -> unit) code option Dynarray.t;
  apply_cont : pc;
}

type move_source = MoveSlot of slot | MoveOperand of operand
type move = { src : move_source; dst : slot }

let get_ctor_tag_name (name : string) : string = "tag_" ^ name

let add_code (ctx : cg_ctx) (c : (world -> unit) code option) : pc =
  let pc = Dynarray.length ctx.codes in
  Dynarray.add_last ctx.codes c;
  int_to_pc pc

let set_code (ctx : cg_ctx) (pc : pc) (c : (world -> unit) code) : unit = Dynarray.set ctx.codes (pc_to_int pc) (Some c)

let add_code_k (ctx : cg_ctx) (k : pc -> (world -> unit) code * 'a) : 'a =
  let pc = add_code ctx None in
  let code, ret = k pc in
  set_code ctx pc code;
  ret

let ctor_tag_name (ctx : cg_ctx) (cname : string) : int code = raw (Hashtbl.find_exn ctx.ctag_name cname)

let add_cont (ctx : cg_ctx) (name : string) (arity : int) (app : world code -> Value.seq code -> unit code) : unit =
  Hashtbl.add_exn ctx.arity ~key:name ~data:arity;
  Hashtbl.add_exn ctx.ctag ~key:name ~data:(Hashtbl.length ctx.ctag);
  Hashtbl.add_exn ctx.ctag_name ~key:name ~data:(get_ctor_tag_name name);
  Dynarray.add_last ctx.constructor_degree (1 - arity);
  Dynarray.add_last ctx.conts (name, app);
  ctx.conts_count <- ctx.conts_count + 1

let register_constructor (ctx : cg_ctx) con_name types =
  let arity = List.length types in
  Hashtbl.add_exn ~key:con_name ~data:arity ctx.arity;
  let constructor_index = Hashtbl.length ctx.ctag in
  Hashtbl.add_exn ~key:con_name ~data:constructor_index ctx.ctag;
  let tag_name = get_ctor_tag_name con_name in
  Hashtbl.add_exn ~key:con_name ~data:tag_name ctx.ctag_name;
  Dynarray.add_last ctx.constructor_degree (1 - arity)

let register_constructors (ctx : cg_ctx) ctors =
  List.iter (fun (con_name, types, _) -> register_constructor ctx con_name types) ctors

let new_cg_ctx () =
  let arity = Hashtbl.create () in
  let ctag = Hashtbl.create () in
  let ctag_name = Hashtbl.create () in
  let constructor_degree = Dynarray.create () in
  let conts = Dynarray.create () in
  let func_entry = Hashtbl.create () in
  Hashtbl.add_exn arity ~key:"cont_done" ~data:0;
  Hashtbl.add_exn ctag ~key:"cont_done" ~data:0;
  Hashtbl.add_exn ctag_name ~key:"cont_done" ~data:(get_ctor_tag_name "cont_done");
  Dynarray.add_last constructor_degree 1;
  let codes = Dynarray.create () in
  let dummy =
    { arity; ctag; ctag_name; constructor_degree; conts; conts_count = 0; func_entry; codes; apply_cont = Pc 0 }
  in
  let apply_cont = add_code dummy None in
  { dummy with apply_cont }

let bool_nonzero_code value = code $ parens (uncode value ^^ string " <> 0")
let unsupported_codegen construct = failf "CompileRegMemo codegen unsupported: %s" construct

let slot_of_value (allocation : unit_allocation) value_id =
  match IntMap.find_opt value_id allocation.slot_of_value with
  | Some slot -> slot
  | None -> failf "CompileRegMemo: missing slot for value v%d" value_id

let block_layout_exn (allocation : unit_allocation) block_id =
  match IntMap.find_opt block_id allocation.block_layouts with
  | Some layout -> layout
  | None -> failf "CompileRegMemo: missing layout for block b%d" block_id

let block_liveness_exn (unit_cg : unit_codegen) block_id =
  match IntMap.find_opt block_id unit_cg.liveness.blocks with
  | Some info -> info
  | None -> failf "CompileRegMemo: missing liveness for block b%d" block_id

let compiled_unit_exn (ctx : cg_ctx) name =
  match Hashtbl.find ctx.func_entry name with
  | Some unit_cg -> unit_cg
  | None -> failf "CompileRegMemo: unknown direct call target `%s`" name

let dummy_value_ = memo_from_int_ (int_ 0)

let operand_value_code (ctx : cg_ctx) (unit_cg : unit_codegen) (w : world code) = function
  | OLocal value_id -> get_env_slot_ w (int_ (slot_of_value unit_cg.allocation value_id))
  | OInt i -> memo_from_int_ (int_ i)
  | OBool b -> memo_from_int_ (int_ (if b then 1 else 0))
  | OUnit -> memo_from_int_ (int_ 0)
  | OCtor name -> from_constructor_ (ctor_tag_name ctx name)
  | OGlobal name -> unsupported_codegen ("global value operand @" ^ name)
  | OBuiltin name -> unsupported_codegen ("builtin operand " ^ name)
  | OFloat _ -> unsupported_codegen "float operand"
  | OString _ -> unsupported_codegen "string operand"

let binop_code op =
  match op with
  | "+" -> add_
  | "*" -> mul_
  | "/" -> div_
  | "-" -> sub_
  | "=" -> eq_
  | "<" -> lt_
  | "<=" -> le_
  | ">" -> gt_
  | ">=" -> ge_
  | "&&" -> land_
  | "||" -> lor_
  | _ -> unsupported_codegen ("binop " ^ op)

let nth_seq parts index = list_nth_ parts (int_ index)

let bind_values prefix values k =
  let rec aux index acc = function
    | [] -> k (List.rev acc)
    | value :: rest ->
        let_in_ (Printf.sprintf "%s%d" prefix index) value (fun bound -> aux (index + 1) (bound :: acc) rest)
  in
  aux 0 [] values

let string_of_ir_pattern (unit_cg : unit_codegen) pattern =
  string_of_document (pp_pattern (value_table unit_cg.unit_ir) pattern)

let unsupported_match_pattern (unit_cg : unit_codegen) pattern =
  failf "CompileRegMemo: unsupported match pattern `%s`; only patterns accepted by CompileMemo are supported"
    (string_of_ir_pattern unit_cg pattern)

let block_pc_or_fail (unit_cg : unit_codegen) block_id =
  match IntMap.find_opt block_id unit_cg.block_pcs with
  | Some pc -> pc
  | None -> failf "CompileRegMemo: missing pc for block b%d" block_id

let write_slot w slot value = set_env_slot_ w (int_ slot) value
let seqs_of_writers writers = seqs_ (List.map (fun writer -> fun _ -> writer ()) writers)

let stmt_live_afters (block : block) (info : CfgLiveness.block_liveness) =
  let _, afters =
    List.fold_right
      (fun stmt (live_after, acc) -> (RegAlloc.transfer_stmt stmt live_after, live_after :: acc))
      block.body (info.live_before_term, [])
  in
  afters

let saved_slots_for_call (allocation : unit_allocation) dst live_after =
  IntSet.remove dst live_after |> IntSet.elements |> List.map (slot_of_value allocation) |> List.sort_uniq Int.compare

let move_sources pending =
  List.fold_left
    (fun acc ({ src; _ } : move) -> match src with MoveSlot slot -> IntSet.add slot acc | MoveOperand _ -> acc)
    IntSet.empty pending

let rewrite_source from_slot to_slot ({ src; _ } as move : move) =
  match src with MoveSlot slot when slot = from_slot -> { move with src = MoveSlot to_slot } | _ -> move

let schedule_parallel_moves scratch_slot moves =
  let local_moves, const_moves =
    List.partition (fun ({ src; _ } : move) -> match src with MoveSlot _ -> true | MoveOperand _ -> false) moves
  in
  let rec loop pending acc =
    match pending with
    | [] -> List.rev_append acc const_moves
    | _ -> (
        let sources = move_sources pending in
        let ready, blocked = List.partition (fun ({ dst; _ } : move) -> not (IntSet.mem dst sources)) pending in
        if ready <> [] then loop blocked (List.rev_append ready acc)
        else
          match (scratch_slot, pending) with
          | None, _ -> failwith "CompileRegMemo: jump parallel copy needs scratch slot but none was allocated"
          | Some scratch, ({ src = MoveSlot src_slot; _ } : move) :: _ ->
              let pending = List.map (rewrite_source src_slot scratch) pending |> List.sort_uniq Stdlib.compare in
              loop pending ({ src = MoveSlot src_slot; dst = scratch } :: acc)
          | Some _, { src = MoveOperand _; _ } :: _ ->
              failwith "CompileRegMemo: internal error while scheduling parallel copy"
          | Some _, [] -> List.rev acc)
  in
  loop local_moves []

let compile_match_success unit_cg w block assigns =
  [%seqs
    seqs_
      (List.map
         (fun (value_id, value) ->
           let slot = slot_of_value unit_cg.allocation value_id in
           fun _ -> set_env_slot_ w (int_ slot) value)
         assigns);
    goto_ w (block_pc_or_fail unit_cg block)]

let tuple_pattern_assigns unit_cg whole_pattern patterns parts =
  List.rev
    (List.fold_left2
       (fun acc pattern part ->
         match pattern with
         | RPAny -> acc
         | RPBind value_id -> (value_id, part) :: acc
         | _ -> unsupported_match_pattern unit_cg whole_pattern)
       [] patterns parts)

let compile_tuple_bindings unit_cg whole_pattern parts patterns w block =
  bind_values "part"
    (List.mapi (fun index _ -> nth_seq parts index) patterns)
    (fun bound_parts ->
      compile_match_success unit_cg w block (tuple_pattern_assigns unit_cg whole_pattern patterns bound_parts))

let compile_default_arm unit_cg subject arms w current_pc =
  let rec loop = function
    | [] -> unreachable_ (pc_to_int current_pc)
    | ({ pattern = RPAny; block } : match_arm) :: _ -> compile_match_success unit_cg w block []
    | ({ pattern = RPBind value_id; block } : match_arm) :: _ ->
        compile_match_success unit_cg w block [ (value_id, subject) ]
    | _ :: rest -> loop rest
  in
  loop arms

let compile_ctor_arm ctx unit_cg pair w default ({ pattern; block } : match_arm) =
  match pattern with
  | RPCtor (ctor, payload) ->
      let body =
        match payload with
        | None | Some RPAny -> compile_match_success unit_cg w block []
        | Some (RPBind value_id) -> compile_match_success unit_cg w block [ (value_id, pair_value_ pair) ]
        | Some (RPTuple patterns) ->
            let whole_pattern = RPCtor (ctor, Some (RPTuple patterns)) in
            let_in_ "parts"
              (memo_splits_ (pair_value_ pair))
              (fun parts ->
                let len_ok =
                  code
                  $ parens
                      (string "List.length " ^^ uncode parts ^^ string " = "
                      ^^ string (string_of_int (List.length patterns)))
                in
                if_ len_ok (compile_tuple_bindings unit_cg whole_pattern parts patterns w block) default)
        | Some unsupported -> unsupported_match_pattern unit_cg (RPCtor (ctor, Some unsupported))
      in
      (Hashtbl.find_exn ctx.ctag ctor, Hashtbl.find_exn ctx.ctag_name ctor, body)
  | _ -> failwith "CompileRegMemo: internal error while compiling ctor dispatch"

let compile_ctor_dispatch ctx unit_cg raw_subject resolved arms w current_pc =
  let default = compile_default_arm unit_cg raw_subject arms w current_pc in
  let ctor_arms =
    List.filter_map
      (fun ({ pattern; _ } as arm) ->
        match pattern with
        | RPCtor _ -> Some arm
        | RPAny | RPBind _ -> None
        | unsupported -> unsupported_match_pattern unit_cg unsupported)
      arms
  in
  match ctor_arms with
  | [] -> default
  | _ ->
      let_in_ "tag"
        (word_get_value_ (zro_ resolved))
        (fun tag ->
          match_ctor_tag_literal_default_ tag
            (List.map (compile_ctor_arm ctx unit_cg resolved w default) ctor_arms)
            default)

let compile_non_call_rhs (ctx : cg_ctx) (unit_cg : unit_codegen) (w : world code) dst rhs =
  let dst_slot = slot_of_value unit_cg.allocation dst in
  match rhs with
  | Move operand -> write_slot w dst_slot (operand_value_code ctx unit_cg w operand)
  | Construct (name, args) ->
      write_slot w dst_slot
        (memo_appends_ (from_constructor_ (ctor_tag_name ctx name) :: List.map (operand_value_code ctx unit_cg w) args))
  | Tuple values -> write_slot w dst_slot (memo_appends_ (List.map (operand_value_code ctx unit_cg w) values))
  | Array _ -> unsupported_codegen "array literal"
  | Select (target, FIndex index) ->
      let_in_ "parts"
        (memo_splits_ (operand_value_code ctx unit_cg w target))
        (fun parts -> write_slot w dst_slot (nth_seq parts index))
  | Select (_, FName name) -> unsupported_codegen ("named field selection ." ^ name)
  | BinOp (op, lhs, rhs) ->
      let op_code = binop_code op in
      let resolve_word operand k =
        match operand with
        | OLocal value_id ->
            let slot = slot_of_value unit_cg.allocation value_id in
            let_in_ "resolved" (resolve_ w (src_E_ slot)) (fun pair -> k (zro_ pair))
        | _ -> k (memo_to_word_ (operand_value_code ctx unit_cg w operand))
      in
      resolve_word lhs (fun lhs_word ->
          resolve_word rhs (fun rhs_word ->
              write_slot w dst_slot (memo_from_int_ (op_code (word_get_value_ lhs_word) (word_get_value_ rhs_word)))))
  | Call _ -> failwith "CompileRegMemo: call rhs handled separately"

let emit_move (ctx : cg_ctx) (unit_cg : unit_codegen) (w : world code) ({ src; dst } : move) =
  match src with
  | MoveSlot slot -> write_slot w dst (get_env_slot_ w (int_ slot))
  | MoveOperand operand -> write_slot w dst (operand_value_code ctx unit_cg w operand)

let compile_jump_copy (ctx : cg_ctx) (unit_cg : unit_codegen) (block : block) succ_id args (w : world code) =
  let succ =
    match IntMap.find_opt succ_id unit_cg.block_map with
    | Some succ -> succ
    | None -> failf "CompileRegMemo: unknown jump target b%d" succ_id
  in
  let block_info = block_liveness_exn unit_cg block.id in
  let edge_live =
    match IntMap.find_opt succ_id block_info.live_on_edge with
    | Some info -> info
    | None -> failf "CompileRegMemo: missing edge liveness from b%d to b%d" block.id succ_id
  in
  let moves =
    try
      List.fold_left2
        (fun acc param_id arg ->
          if IntMap.mem param_id edge_live.live_params then
            let dst = slot_of_value unit_cg.allocation param_id in
            match arg with
            | OLocal value_id ->
                let src = slot_of_value unit_cg.allocation value_id in
                if src = dst then acc else { src = MoveSlot src; dst } :: acc
            | _ -> { src = MoveOperand arg; dst } :: acc
          else acc)
        [] succ.params args
    with Invalid_argument _ ->
      failf "CompileRegMemo: jump to block b%d expects %d args, got %d" succ.id (List.length succ.params)
        (List.length args)
  in
  let moves = schedule_parallel_moves unit_cg.allocation.scratch_slot moves in
  seqs_of_writers (List.map (fun move -> fun () -> emit_move ctx unit_cg w move) moves)

let compile_call_setup _ctx callee w arg_values =
  let entry_layout = block_layout_exn callee.allocation callee.unit_ir.entry in
  if List.length entry_layout.param_slots <> List.length arg_values then
    failf "CompileRegMemo: direct call to `%s` expects %d args, got %d" callee.unit_ir.name
      (List.length entry_layout.param_slots) (List.length arg_values);
  [%seqs
    init_frame_ w (int_ callee.allocation.frame_size) dummy_value_;
    seqs_ (List.map2 (fun slot value -> fun _ -> set_env_slot_ w (int_ slot) value) entry_layout.param_slots arg_values)]

let rec compile_stmt_chain ctx unit_cg block current_pc w stmts stmt_afters term =
  match (stmts, stmt_afters) with
  | [], [] -> compile_terminator ctx unit_cg block current_pc w term
  | Bind (dst, Call (Direct callee_name, args)) :: rest, live_after :: rest_afters ->
      let callee = compiled_unit_exn ctx callee_name in
      bind_values "arg"
        (List.map (operand_value_code ctx unit_cg w) args)
        (fun arg_values ->
          if rest = [] then
            match term with
            | Return (OLocal return_id) when return_id = dst ->
                [%seqs
                  assert_env_length_ w (int_ unit_cg.allocation.frame_size);
                  compile_call_setup ctx callee w arg_values;
                  goto_ w callee.entry_pc]
            | _ ->
                let resume_pc =
                  add_code_k ctx (fun resume_pc ->
                      ( lam_ "w" (fun w ->
                            [%seqs
                              assert_env_length_ w (int_ unit_cg.allocation.frame_size);
                              compile_terminator ctx unit_cg block resume_pc w term]),
                        resume_pc ))
                in
                compile_non_tail_call ctx unit_cg block w dst callee arg_values live_after resume_pc
          else
            let resume_pc =
              add_code_k ctx (fun resume_pc ->
                  ( lam_ "w" (fun w ->
                        [%seqs
                          assert_env_length_ w (int_ unit_cg.allocation.frame_size);
                          compile_stmt_chain ctx unit_cg block resume_pc w rest rest_afters term]),
                    resume_pc ))
            in
            compile_non_tail_call ctx unit_cg block w dst callee arg_values live_after resume_pc)
  | Bind (_, Call (Indirect _, _)) :: _, _ -> unsupported_codegen "indirect call"
  | Bind (_, Call (Direct _, _)) :: _, [] -> failwith "CompileRegMemo: missing live-after information for call"
  | Bind (dst, rhs) :: rest, _live_after :: rest_afters ->
      [%seqs
        assert_env_length_ w (int_ unit_cg.allocation.frame_size);
        compile_non_call_rhs ctx unit_cg w dst rhs;
        compile_stmt_chain ctx unit_cg block current_pc w rest rest_afters term]
  | _ -> failwith "CompileRegMemo: mismatched statement and liveness counts"

and compile_non_tail_call ctx unit_cg _block w dst callee arg_values live_after resume_pc =
  let keep_slots = saved_slots_for_call unit_cg.allocation dst live_after in
  let cont_name = "cont_" ^ string_of_int ctx.conts_count in
  let dst_slot = slot_of_value unit_cg.allocation dst in
  add_cont ctx cont_name
    (List.length keep_slots + 1)
    (fun w tl ->
      let_in_ "ret"
        (get_env_slot_ w (int_ 0))
        (fun ret ->
          [%seqs
            assert_env_length_ w (int_ 1);
            set_k_ w (get_next_cont_ tl);
            init_frame_ w (int_ unit_cg.allocation.frame_size) dummy_value_;
            restore_env_slots_ w (list_literal_of_ int_ keep_slots) tl;
            set_env_slot_ w (int_ dst_slot) ret;
            goto_ w resume_pc]));
  let saved = collect_env_slots_ w (list_literal_of_ int_ keep_slots) in
  [%seqs
    assert_env_length_ w (int_ unit_cg.allocation.frame_size);
    set_k_ w (memo_appends_ [ from_constructor_ (ctor_tag_name ctx cont_name); saved; world_kont_ w ]);
    compile_call_setup ctx callee w arg_values;
    goto_ w callee.entry_pc]

and compile_terminator ctx unit_cg block current_pc w term =
  match term with
  | Return operand -> return_value_ w (operand_value_code ctx unit_cg w operand) (pc_to_exp_ (pc_ ctx.apply_cont))
  | Jump (succ_id, args) ->
      [%seqs
        assert_env_length_ w (int_ unit_cg.allocation.frame_size);
        compile_jump_copy ctx unit_cg block succ_id args w;
        goto_ w
          (match IntMap.find_opt succ_id unit_cg.block_pcs with
          | Some pc -> pc
          | None -> failf "CompileRegMemo: missing pc for block b%d" succ_id)]
  | Branch (cond, then_id, else_id) -> (
      let then_pc =
        match IntMap.find_opt then_id unit_cg.block_pcs with
        | Some pc -> pc
        | None -> failf "CompileRegMemo: missing pc for block b%d" then_id
      in
      let else_pc =
        match IntMap.find_opt else_id unit_cg.block_pcs with
        | Some pc -> pc
        | None -> failf "CompileRegMemo: missing pc for block b%d" else_id
      in
      let do_branch cond_word =
        if_ (bool_nonzero_code (word_get_value_ cond_word)) (goto_ w then_pc) (goto_ w else_pc)
      in
      match cond with
      | OLocal value_id ->
          let slot = slot_of_value unit_cg.allocation value_id in
          let_in_ "resolved" (resolve_ w (src_E_ slot)) (fun pair -> do_branch (zro_ pair))
      | _ ->
          let cond_word = memo_to_word_ (operand_value_code ctx unit_cg w cond) in
          do_branch cond_word)
  | Match (scrutinee, arms) ->
      [%seqs
        assert_env_length_ w (int_ unit_cg.allocation.frame_size);
        match scrutinee with
        | OLocal value_id ->
            let slot = slot_of_value unit_cg.allocation value_id in
            let_in_ "resolved"
              (resolve_ w (src_E_ slot))
              (fun resolved ->
                compile_ctor_dispatch ctx unit_cg
                  (operand_value_code ctx unit_cg w scrutinee)
                  resolved arms w current_pc)
        | _ -> failf "CompileRegMemo: match scrutinee must be a local operand"]

let compile_block ctx unit_cg (block : block) =
  let current_pc =
    match IntMap.find_opt block.id unit_cg.block_pcs with
    | Some pc -> pc
    | None -> failf "CompileRegMemo: missing pc for block b%d" block.id
  in
  let info = block_liveness_exn unit_cg block.id in
  let stmt_afters = stmt_live_afters block info in
  set_code ctx current_pc
    (lam_ "w" (fun w ->
         [%seqs
           assert_env_length_ w (int_ unit_cg.allocation.frame_size);
           compile_stmt_chain ctx unit_cg block current_pc w block.body stmt_afters block.term]))

let generate_apply_cont (ctx : cg_ctx) =
  set_code ctx ctx.apply_cont
    (lam_ "w" (fun w ->
         let loop tl =
           List.init (Dynarray.length ctx.conts) (fun i ->
               let name, action = Dynarray.get ctx.conts i in
               (Hashtbl.find_exn ctx.ctag name, Hashtbl.find_exn ctx.ctag_name name, action w tl))
         in
         [%seqs
           assert_env_length_ w (int_ 1);
           let hd, hd_pat = genvar "hd" in
           let tl, tl_pat = genvar "tl" in
           let pat = pair_pat_ hd_pat tl_pat in
           let_pat_in_ pat
             (resolve_ w (raw "K"))
             (paren
             $ match_ctor_tag_literal_default_ (word_get_value_ hd) (loop tl) (unreachable_ (pc_to_int ctx.apply_cont))
             )]))

let ctor_tag_decls ctx =
  let xs = List.sort (fun (_, x) (_, y) -> Int.compare x y) (Hashtbl.to_alist ctx.ctag) in
  separate_map hardline
    (fun (name, tag) ->
      let tag_name = get_ctor_tag_name name in
      string "let " ^^ string tag_name ^^ string " = " ^^ string (string_of_int tag))
    xs

let compile_unit_wrapper (ctx : cg_ctx) (unit_cg : unit_codegen) =
  let arg_count = List.length unit_cg.unit_ir.params in
  let args = List.init arg_count (fun i -> string ("(x" ^ string_of_int i ^ " : Value.seq)")) in
  let entry_layout = block_layout_exn unit_cg.allocation unit_cg.unit_ir.entry in
  if List.length entry_layout.param_slots <> arg_count then
    failf "CompileRegMemo: wrapper for `%s` expects %d args, got %d slots" unit_cg.unit_ir.name arg_count
      (List.length entry_layout.param_slots);
  let arg_inits =
    List.map2
      (fun slot i ->
        string "Dynarray.set initial_env "
        ^^ string (string_of_int slot)
        ^^ space
        ^^ string ("x" ^ string_of_int i)
        ^^ semi)
      entry_layout.param_slots (List.init arg_count Fun.id)
  in
  let name = unit_cg.unit_ir.name in
  string "let " ^^ string name ^^ space ^^ string "memo"
  ^^ (if args = [] then empty else space ^^ separate space args)
  ^^ string " : exec_result =" ^^ hardline
  ^^ string "  let initial_env = Dynarray.init "
  ^^ string (string_of_int unit_cg.allocation.frame_size)
  ^^ string " (fun _ -> " ^^ uncode dummy_value_ ^^ string ") in"
  ^^ (if arg_inits = [] then empty else hardline ^^ separate_map hardline (fun doc -> string "  " ^^ doc) arg_inits)
  ^^ hardline ^^ string "  exec_cek "
  ^^ string ("(pc_to_exp (int_to_pc " ^ string_of_int (pc_to_int unit_cg.entry_pc) ^ "))")
  ^^ string " initial_env ("
  ^^ uncode (from_constructor_ (ctor_tag_name ctx "cont_done"))
  ^^ string ") memo"

let compile_type_stmt (ctx : cg_ctx) = function
  | Type (TBOne (_, Enum { ctors; _ }) as binding) ->
      register_constructors ctx ctors;
      CompileType.compile_ty_binding ctx.ctag binding
  | Type (TBRec defs as binding) ->
      List.iter (fun (_, Enum { ctors; _ }) -> register_constructors ctx ctors) defs;
      CompileType.compile_ty_binding ctx.ctag binding
  | Term _ -> empty

(* NOTE: !!! set this to enable IR dump *)

let dump_cfg = false

let analysis_comment (analysis : prog_slot_analysis) =
  let rendered = Syntax.string_of_document (pp_prog_slot_analysis analysis) in
  if dump_cfg then string "(*" ^^ hardline ^^ string rendered ^^ hardline ^^ string "*)" else empty

let compile_reg_memo (prog : 'a prog) : document =
  let analysis = analyze_prog_slots (lower_prog prog) in
  let ctx = new_cg_ctx () in
  let units =
    try
      List.combine (List.combine analysis.ir.units analysis.liveness) analysis.allocations
      |> List.map (fun ((unit_ir, liveness), allocation) -> (unit_ir, liveness, allocation))
    with Invalid_argument _ -> failwith "CompileRegMemo: mismatched unit/liveness/allocation counts"
  in
  let type_docs =
    let stmts, _ = prog in
    List.filter_map (function Type _ as stmt -> Some (compile_type_stmt ctx stmt) | Term _ -> None) stmts
  in
  List.iter
    (fun (unit_ir, liveness, allocation) ->
      let block_pcs =
        List.fold_left
          (fun acc (block : block) -> IntMap.add block.id (add_code ctx None) acc)
          IntMap.empty unit_ir.blocks
      in
      let block_map =
        List.fold_left (fun acc (block : block) -> IntMap.add block.id block acc) IntMap.empty unit_ir.blocks
      in
      let entry_pc =
        match IntMap.find_opt unit_ir.entry block_pcs with
        | Some pc -> pc
        | None -> failf "CompileRegMemo: missing entry block pc for %s" unit_ir.name
      in
      Hashtbl.add_exn ctx.func_entry ~key:unit_ir.name
        ~data:{ unit_ir; liveness; allocation; entry_pc; block_pcs; block_map })
    units;
  Hashtbl.iter ctx.func_entry ~f:(fun unit_cg -> List.iter (compile_block ctx unit_cg) unit_cg.unit_ir.blocks);
  generate_apply_cont ctx;
  let wrapper_docs =
    Hashtbl.to_alist ctx.func_entry |> List.map snd
    |> List.sort (fun a b -> Int.compare a.unit_ir.id b.unit_ir.id)
    |> List.map (compile_unit_wrapper ctx)
  in
  analysis_comment analysis ^^ hardline ^^ hardline ^^ string "open Ant" ^^ hardline ^^ string "open Word" ^^ hardline
  ^^ string "open Memo" ^^ hardline ^^ string "open Value" ^^ hardline ^^ string "open Common" ^^ hardline
  ^^ (if Dynarray.length ctx.constructor_degree = 0 then empty else ctor_tag_decls ctx)
  ^^ (if type_docs = [] then empty else hardline ^^ separate_map (hardline ^^ hardline) Fun.id type_docs)
  ^^ (if wrapper_docs = [] then empty
      else hardline ^^ hardline ^^ separate_map (hardline ^^ hardline) Fun.id wrapper_docs)
  ^^ hardline ^^ hardline ^^ string "let populate_state () =" ^^ hardline ^^ string "  Memo.reset ();" ^^ hardline
  ^^ string "  Words.reset ();" ^^ hardline
  ^^ separate_map hardline
       (fun i ->
         string "  add_exp "
         ^^ uncode (Option.get (Dynarray.get ctx.codes i))
         ^^ space
         ^^ string (string_of_int i)
         ^^ semi)
       (List.init (Dynarray.length ctx.codes) Fun.id)
  ^^ hardline
  ^^ separate_map hardline
       (fun i ->
         string "  Words.set_constructor_degree "
         ^^ string (string_of_int i)
         ^^ string " ("
         ^^ string (string_of_int (Dynarray.get ctx.constructor_degree i))
         ^^ string ");")
       (List.init (Dynarray.length ctx.constructor_degree) Fun.id)

module Backend = struct
  let compile = compile_reg_memo
end
