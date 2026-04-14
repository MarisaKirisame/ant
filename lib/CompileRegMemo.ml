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
type edge_plan = RegAlloc.edge_plan

type prog_slot_analysis = {
  ir : prog_ir;
  liveness : CfgLiveness.unit_liveness list;
  allocations : unit_allocation list;
}

(* Lower ANF to the block IR, then run the inlining optimization passes
   (trivial block inlining and dispatch inlining) before liveness / register
   allocation. The passes reduce the number of memo-visible PCs and
   cross-block frame reshapes while preserving canonical memo boundaries. *)
let lower_prog (prog : 'a prog) : prog_ir = prog |> AnfToCfg.lower_prog |> BlockInlining.apply

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

let successors (block : block) = terminator_external_succs block.term
let allocate_unit_slots = RegAlloc.allocate_unit_slots

(* Compute the live-in set at the top of an inline region, given a function
   that returns the live_in of a real block. This walks the region's body
   backward from its terminator's live-out. Inlined arms inside the region's
   terminator recurse. *)
let rec inline_region_live_in ~(find_live : block_id -> CfgLiveness.block_liveness) (region : inline_region) : IntSet.t
    =
  let live_out = terminator_live_out ~find_live region.term in
  List.fold_right RegAlloc.transfer_stmt region.body live_out

and terminator_live_out ~(find_live : block_id -> CfgLiveness.block_liveness) (term : terminator) : IntSet.t =
  match term with
  | Return operand -> RegAlloc.add_operand_uses IntSet.empty operand
  | Jump (succ_id, _) -> (find_live succ_id).live_in
  | Branch (cond, t, e) ->
      let t_live =
        match t.inline with None -> (find_live t.block).live_in | Some r -> inline_region_live_in ~find_live r
      in
      let e_live =
        match e.inline with None -> (find_live e.block).live_in | Some r -> inline_region_live_in ~find_live r
      in
      RegAlloc.add_operand_uses (IntSet.union t_live e_live) cond
  | Match (scrutinee, arms) ->
      let live =
        List.fold_left
          (fun acc (arm : match_arm) ->
            let arm_live =
              match arm.inline with
              | None -> (find_live arm.block).live_in
              | Some r -> inline_region_live_in ~find_live r
            in
            IntSet.union acc arm_live)
          IntSet.empty arms
      in
      RegAlloc.add_operand_uses live scrutinee

let analyze_unit_liveness (unit_ir : unit_ir) : CfgLiveness.unit_liveness =
  CfgLiveness.analyze_unit ~blocks:unit_ir.blocks ~entry_id:unit_ir.entry ~successors
    ~block_id:(fun (block : block) -> block.id)
    ~params:(fun (block : block) -> block.params)
    ~body:(fun (block : block) -> block.body)
    ~transfer_stmt:RegAlloc.transfer_stmt
    ~analyze_term:(fun ~find_block ~find_live (block : block) ->
      (* Walk the terminator (including inline regions for dispatch-inlined
         arms) and register live_on_edge entries for each *external* successor
         block reached. Edges originating inside an inline region are
         attributed to the dispatcher block, because the inlined arm does not
         own a PC. *)
      let edges = ref IntMap.empty in
      let add_edge succ_id edge = edges := IntMap.add succ_id edge !edges in
      let rec walk_term term =
        match term with
        | Return operand -> RegAlloc.add_operand_uses IntSet.empty operand
        | Jump (succ_id, args) ->
            let succ = find_block succ_id in
            let succ_info = find_live succ_id in
            let edge_live = live_on_jump_edge succ succ_info args in
            add_edge succ_id edge_live;
            CfgLiveness.edge_live_values edge_live
        | Branch (cond, t, e) ->
            let t_live = walk_branch_target t in
            let e_live = walk_branch_target e in
            RegAlloc.add_operand_uses (IntSet.union t_live e_live) cond
        | Match (scrutinee, arms) ->
            let live =
              List.fold_left (fun acc (arm : match_arm) -> IntSet.union acc (walk_match_arm arm)) IntSet.empty arms
            in
            RegAlloc.add_operand_uses live scrutinee
      and walk_branch_target (tgt : branch_target) =
        match tgt.inline with
        | None ->
            let live = (find_live tgt.block).live_in in
            add_edge tgt.block { CfgLiveness.live_in = live; live_params = IntMap.empty };
            live
        | Some region -> walk_inline region
      and walk_match_arm (arm : match_arm) =
        match arm.inline with
        | None ->
            let live = (find_live arm.block).live_in in
            add_edge arm.block { CfgLiveness.live_in = live; live_params = IntMap.empty };
            live
        | Some region ->
            let region_live = walk_inline region in
            (* Match-arm binders are defined inside the region (by the pattern
               match); remove them from the region's entry-live set so they
               don't leak upward into the parent's live_before_term. *)
            let binders = pattern_binders arm.pattern in
            List.fold_left (fun acc id -> IntSet.remove id acc) region_live binders
      and walk_inline (region : inline_region) =
        let term_live = walk_term region.term in
        List.fold_right RegAlloc.transfer_stmt region.body term_live
      in
      let live_out = walk_term block.term in
      let live_before_term =
        match block.term with
        | Return operand -> RegAlloc.add_operand_uses IntSet.empty operand
        | Jump _ -> live_out
        | Branch (cond, _, _) -> RegAlloc.add_operand_uses live_out cond
        | Match (cond, _) -> RegAlloc.add_operand_uses live_out cond
      in
      (!edges, live_out, live_before_term))

let analyze_prog (ir : prog_ir) : prog_analysis =
  CfgLiveness.analyze_prog ~units:ir.units ~analyze_unit:analyze_unit_liveness ir

let analyze_prog_slots (ir : prog_ir) : prog_slot_analysis =
  let phase2 = analyze_prog ir in
  let allocations =
    try List.map2 allocate_unit_slots phase2.ir.units phase2.liveness
    with Invalid_argument _ -> failwith "CompileRegMemo: mismatched unit/liveness counts in phase 3"
  in
  { ir = phase2.ir; liveness = phase2.liveness; allocations }

(* Pretty printing *)

let pp_slot slot = string ("s" ^ string_of_int slot)
let pp_slot_binding values (id, slot) = pp_value_ref values id ^^ space ^^ string "->" ^^ space ^^ pp_slot slot

let pp_block_layout values block_id (layout : block_layout) =
  let slot_doc =
    match IntMap.bindings layout.slot_of_value with
    | [] -> string "slots=[]"
    | bindings ->
        string "slots=" ^^ lbracket
        ^^ pp_list (pp_slot_binding values) (comma ^^ space)
             (List.sort (fun (_, slot_a) (_, slot_b) -> Int.compare slot_a slot_b) bindings)
        ^^ rbracket
  in
  string "block" ^^ space
  ^^ string ("b" ^ string_of_int block_id)
  ^^ space ^^ string "params=" ^^ lbracket
  ^^ pp_list pp_slot (comma ^^ space) layout.param_slots
  ^^ rbracket ^^ space ^^ string "entry="
  ^^ string (string_of_int layout.entry_size)
  ^^ string " frame="
  ^^ string (string_of_int layout.frame_size)
  ^^ space ^^ slot_doc

let pp_frame_source = function
  | RegAlloc.FromSlot s -> string "s" ^^ string (string_of_int s)
  | RegAlloc.FromOperand _ -> string "op"
  | RegAlloc.FromBinder vid -> string "binder(v" ^^ string (string_of_int vid) ^^ string ")"
  | RegAlloc.Unset -> string "_"

let pp_edge_plan = function
  | RegAlloc.Elide -> string "elide"
  | RegAlloc.Shuffle { mapping } ->
      string "shuffle[" ^^ pp_list pp_frame_source (string ", ") (Array.to_list mapping) ^^ string "]"

let pp_unit_allocation (unit_ir : unit_ir) (allocation : unit_allocation) =
  let values = value_table unit_ir in
  let layouts_doc =
    match IntMap.bindings allocation.block_layouts with
    | [] -> string "layouts=[]"
    | layouts ->
        string "layouts:" ^^ hardline
        ^^ separate_map hardline
             (fun (block_id, layout) -> string "  " ^^ pp_block_layout values block_id layout)
             layouts
  in
  let plans_doc =
    let plan_lines =
      IntMap.fold
        (fun src_id succs acc ->
          IntMap.fold
            (fun dst_id plan acc ->
              (string "  b"
              ^^ string (string_of_int src_id)
              ^^ string " -> b"
              ^^ string (string_of_int dst_id)
              ^^ string ": " ^^ pp_edge_plan plan)
              :: acc)
            succs acc)
        allocation.edge_plans []
    in
    match plan_lines with
    | [] -> empty
    | _ -> hardline ^^ string "edge_plans:" ^^ hardline ^^ separate hardline (List.rev plan_lines)
  in
  layouts_doc ^^ plans_doc

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

(* Codegen types *)

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

(* Block-layout helpers *)

let block_layout_exn (allocation : unit_allocation) block_id =
  match IntMap.find_opt block_id allocation.block_layouts with
  | Some layout -> layout
  | None -> failf "CompileRegMemo: missing layout for block b%d" block_id

let slot_in_layout (layout : block_layout) value_id =
  match IntMap.find_opt value_id layout.slot_of_value with
  | Some slot -> slot
  | None -> failf "CompileRegMemo: missing slot for value v%d in block layout" value_id

let block_liveness_exn (unit_cg : unit_codegen) block_id =
  match IntMap.find_opt block_id unit_cg.liveness.blocks with
  | Some info -> info
  | None -> failf "CompileRegMemo: missing liveness for block b%d" block_id

let compiled_unit_exn (ctx : cg_ctx) name =
  match Hashtbl.find ctx.func_entry name with
  | Some unit_cg -> unit_cg
  | None -> failf "CompileRegMemo: unknown direct call target `%s`" name

let edge_plan_exn (unit_cg : unit_codegen) src_id dst_id =
  match IntMap.find_opt src_id unit_cg.allocation.edge_plans with
  | Some succs -> (
      match IntMap.find_opt dst_id succs with
      | Some plan -> plan
      | None -> failf "CompileRegMemo: missing edge plan b%d -> b%d" src_id dst_id)
  | None -> failf "CompileRegMemo: missing edge plans for block b%d" src_id

let dummy_value_ = memo_from_int_ (int_ 0)

let operand_value_code (ctx : cg_ctx) (layout : block_layout) (w : world code) = function
  | OLocal value_id -> get_env_slot_ w (int_ (slot_in_layout layout value_id))
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

let stmt_live_afters (block : block) (info : CfgLiveness.block_liveness) =
  let _, afters =
    List.fold_right
      (fun stmt (live_after, acc) -> (RegAlloc.transfer_stmt stmt live_after, live_after :: acc))
      block.body (info.live_before_term, [])
  in
  afters

let saved_values_for_call (layout : block_layout) dst live_after =
  IntSet.remove dst live_after |> IntSet.elements
  |> List.map (fun value_id -> (value_id, slot_in_layout layout value_id))
  |> List.sort (fun (_, slot_a) (_, slot_b) -> Int.compare slot_a slot_b)

let saved_slots_for_call (layout : block_layout) dst live_after =
  List.map snd (saved_values_for_call layout dst live_after)

(* Source-side working frame: minimum parent-layout frame needed to access
   saved continuation slots and call argument operands before init_frame. *)
let call_src_working (layout : block_layout) dst live_after args =
  let max_s = List.fold_left max (layout.entry_size - 1) (saved_slots_for_call layout dst live_after) in
  let max_s =
    List.fold_left
      (fun acc -> function
        | OLocal vid -> ( match IntMap.find_opt vid layout.slot_of_value with Some s -> max s acc | None -> acc)
        | _ -> acc)
      max_s args
  in
  max_s + 1

(* Emit frame growth from entry_size to working frame_size when needed. *)
let emit_frame_growth (w : world code) (entry : int) (working : int) =
  if working > entry then resize_frame_ w (int_ working) dummy_value_ else unit_

(* Trim resolved env back to the step's canonical entry size before step exits.
   `step_entry` is the env size at step start (layout.entry_size for block entry,
   rsize for non-tail call resume points). *)
let emit_trim_resolved (w : world code) (step_entry : int) (working : int) =
  if working > step_entry then trim_resolved_ w (int_ step_entry) else unit_

(* ----- Shuffle-based edge code generation ----- *)

let reverse_entry_slots (layout : block_layout) (entry_values : IntSet.t) =
  IntMap.fold
    (fun vid slot acc -> if IntSet.mem vid entry_values then IntMap.add slot vid acc else acc)
    layout.slot_of_value IntMap.empty

let edge_mapping_is_elide (src_layout : block_layout) (mapping : RegAlloc.frame_source array) =
  src_layout.frame_size = Array.length mapping
  &&
  let rec check i = i >= Array.length mapping || (mapping.(i) = RegAlloc.FromSlot i && check (i + 1)) in
  check 0

(* Translate a RegAlloc.frame_source into a code-level frame_source expression. *)
let compile_frame_source (ctx : cg_ctx) (src_layout : block_layout) (w : world code)
    (extra_assigns : (value_id * Value.seq code) list) (src : RegAlloc.frame_source) : 'a code =
  match src with
  | FromSlot s -> old_slot_ s
  | FromOperand op -> new_value_ (operand_value_code ctx src_layout w op)
  | FromBinder vid -> (
      match List.assoc_opt vid extra_assigns with
      | Some value_code -> new_value_ value_code
      | None -> failf "CompileRegMemo: missing binder value for v%d" vid)
  | Unset -> failf "CompileRegMemo: Unset"

let compile_shuffle_mapping (unit_cg : unit_codegen) (src_layout : block_layout) (src_block_id : block_id)
    (target_id : block_id) ?jump_args (extra_assigns : (value_id * Value.seq code) list) =
  let dst_layout = block_layout_exn unit_cg.allocation target_id in
  let dst_info = block_liveness_exn unit_cg target_id in
  let dst_entry_values = IntSet.union dst_info.live_in dst_info.live_params in
  let dst_reverse = reverse_entry_slots dst_layout dst_entry_values in
  let target_block =
    match IntMap.find_opt target_id unit_cg.block_map with
    | Some block -> block
    | None -> failf "CompileRegMemo: missing block b%d" target_id
  in
  let param_ids = List.fold_left (fun acc param_id -> IntSet.add param_id acc) IntSet.empty target_block.params in
  let edge_live =
    match jump_args with
    | None -> CfgLiveness.empty_edge_liveness
    | Some _ -> (
        let block_info = block_liveness_exn unit_cg src_block_id in
        match IntMap.find_opt target_id block_info.live_on_edge with
        | Some edge_live -> edge_live
        | None -> failf "CompileRegMemo: missing edge liveness b%d -> b%d" src_block_id target_id)
  in
  let param_index =
    List.mapi (fun i param_id -> (param_id, i)) target_block.params
    |> List.fold_left (fun acc (param_id, i) -> IntMap.add param_id i acc) IntMap.empty
  in
  Array.init dst_layout.entry_size (fun slot ->
      match IntMap.find_opt slot dst_reverse with
      | None -> RegAlloc.Unset
      | Some vid ->
          if IntSet.mem vid dst_info.live_in && not (IntSet.mem vid param_ids) then
            RegAlloc.FromSlot (slot_in_layout src_layout vid)
          else if IntSet.mem vid param_ids then
            match List.assoc_opt vid extra_assigns with
            | Some _ -> RegAlloc.FromBinder vid
            | None -> (
                match jump_args with
                | Some args when IntMap.mem vid edge_live.live_params -> (
                    let arg_index =
                      match IntMap.find_opt vid param_index with
                      | Some i -> i
                      | None -> failf "CompileRegMemo: missing jump param index for v%d" vid
                    in
                    let arg = List.nth args arg_index in
                    match arg with
                    | OLocal arg_vid -> RegAlloc.FromSlot (slot_in_layout src_layout arg_vid)
                    | operand -> RegAlloc.FromOperand operand)
                | _ -> RegAlloc.Unset)
          else RegAlloc.Unset)

(* Compile an edge from the current layout to the target block's canonical
   entry layout. [step_entry] is the env size at the start of the current
   step. *)
let compile_shuffle_edge (ctx : cg_ctx) (unit_cg : unit_codegen) (src_layout : block_layout) ~step_entry
    (w : world code) (src_block_id : block_id) (target_id : block_id) ?jump_args
    (extra_assigns : (value_id * Value.seq code) list) =
  let target_pc = block_pc_or_fail unit_cg target_id in
  let trim = emit_trim_resolved w step_entry src_layout.frame_size in
  let mapping = compile_shuffle_mapping unit_cg src_layout src_block_id target_id ?jump_args extra_assigns in
  if extra_assigns = [] && edge_mapping_is_elide src_layout mapping then
    [%seqs
      trim;
      goto_ w target_pc]
  else
    let sources = Array.to_list mapping in
    let compiled_sources = List.map (compile_frame_source ctx src_layout w extra_assigns) sources in
    [%seqs
      trim;
      shuffle_frame_ w (frame_mapping_ compiled_sources) dummy_value_;
      goto_ w target_pc]

(* Match support.

   For non-inlined arms, control transfers to the arm's block via a shuffled
   edge (compile_shuffle_edge). For inlined (dispatch-inlined) arms, the arm
   block has been merged into the dispatcher's step: we write pattern binder
   values directly into their parent-layout slots, then emit the inline
   region's body and terminator in place. *)

let tuple_pattern_assigns unit_cg whole_pattern patterns parts =
  List.rev
    (List.fold_left2
       (fun acc pattern part ->
         match pattern with
         | RPAny -> acc
         | RPBind value_id -> (value_id, part) :: acc
         | _ -> unsupported_match_pattern unit_cg whole_pattern)
       [] patterns parts)

(* Non-call RHS *)

let compile_non_call_rhs (ctx : cg_ctx) (layout : block_layout) (w : world code) dst rhs =
  let dst_slot = slot_in_layout layout dst in
  match rhs with
  | Move operand -> write_slot w dst_slot (operand_value_code ctx layout w operand)
  | Construct (name, args) ->
      write_slot w dst_slot
        (memo_appends_ (from_constructor_ (ctor_tag_name ctx name) :: List.map (operand_value_code ctx layout w) args))
  | Tuple values -> write_slot w dst_slot (memo_appends_ (List.map (operand_value_code ctx layout w) values))
  | Array _ -> unsupported_codegen "array literal"
  | Select (target, FIndex index) ->
      let_in_ "parts"
        (memo_splits_ (operand_value_code ctx layout w target))
        (fun parts -> write_slot w dst_slot (nth_seq parts index))
  | Select (_, FName name) -> unsupported_codegen ("named field selection ." ^ name)
  | BinOp (op, lhs, rhs) ->
      let op_code = binop_code op in
      let resolve_word operand k =
        match operand with
        | OLocal value_id ->
            let slot = slot_in_layout layout value_id in
            let_in_ "resolved" (resolve_ w (src_E_ slot)) (fun pair -> k (zro_ pair))
        | _ -> k (memo_to_word_ (operand_value_code ctx layout w operand))
      in
      resolve_word lhs (fun lhs_word ->
          resolve_word rhs (fun rhs_word ->
              write_slot w dst_slot (memo_from_int_ (op_code (word_get_value_ lhs_word) (word_get_value_ rhs_word)))))
  | Call _ -> failwith "CompileRegMemo: call rhs handled separately"

(* Call setup *)

let compile_call_setup _ctx callee w arg_values =
  let entry_layout = block_layout_exn callee.allocation callee.unit_ir.entry in
  if List.length entry_layout.param_slots <> List.length arg_values then
    failf "CompileRegMemo: direct call to `%s` expects %d args, got %d" callee.unit_ir.name
      (List.length entry_layout.param_slots) (List.length arg_values);
  [%seqs
    init_frame_ w (int_ entry_layout.entry_size) dummy_value_;
    seqs_ (List.map2 (fun slot value -> fun _ -> set_env_slot_ w (int_ slot) value) entry_layout.param_slots arg_values)]

(* Statement chain and call handling *)

type call_resume = {
  layout : block_layout;
  saved_src_slots : int list;
  saved_dst_slots : int list;
  dst_slot : int;
  src_working : int;
}

let rhs_resident_values rhs = RegAlloc.add_rhs_uses IntSet.empty rhs
let stmt_resident_values (Bind (dst, rhs)) = IntSet.add dst (rhs_resident_values rhs)

let compact_layout_subset (layout : block_layout) (param_ids : value_id list) ~(entry_ids : IntSet.t)
    ~(resident_ids : IntSet.t) =
  let resident_ids = IntSet.filter (fun value_id -> IntMap.mem value_id layout.slot_of_value) resident_ids in
  let entry_ids = IntSet.filter (fun value_id -> IntMap.mem value_id layout.slot_of_value) entry_ids in
  let add_old_slots ids acc =
    IntSet.fold
      (fun value_id acc ->
        match IntMap.find_opt value_id layout.slot_of_value with Some slot -> IntSet.add slot acc | None -> acc)
      ids acc
  in
  let entry_slots = add_old_slots entry_ids IntSet.empty in
  let resident_slots = add_old_slots resident_ids IntSet.empty in
  let slot_remap = ref IntMap.empty in
  let next_slot = ref 0 in
  let assign old_slot =
    if not (IntMap.mem old_slot !slot_remap) then (
      slot_remap := IntMap.add old_slot !next_slot !slot_remap;
      incr next_slot)
  in
  List.iter assign (IntSet.elements entry_slots);
  List.iter assign (IntSet.elements resident_slots);
  let slot_of_value =
    IntSet.fold
      (fun value_id acc ->
        match IntMap.find_opt value_id layout.slot_of_value with
        | Some old_slot -> (
            match IntMap.find_opt old_slot !slot_remap with
            | Some new_slot -> IntMap.add value_id new_slot acc
            | None -> acc)
        | None -> acc)
      resident_ids IntMap.empty
  in
  let entry_size =
    IntSet.fold
      (fun value_id acc ->
        match IntMap.find_opt value_id slot_of_value with Some slot -> Int.max acc (slot + 1) | None -> acc)
      entry_ids 0
  in
  let frame_size = !next_slot in
  let param_slots =
    List.filter_map
      (fun param_id -> match IntMap.find_opt param_id slot_of_value with Some slot -> Some slot | None -> None)
      param_ids
  in
  let compacted_layout : block_layout = { RegAlloc.slot_of_value; param_slots; entry_size; frame_size } in
  (compacted_layout, !slot_remap)

let build_call_resume_layout (unit_cg : unit_codegen) (block : block) (layout : block_layout) dst live_after args
    (rest : stmt list) (rest_afters : IntSet.t list) term =
  let saved_values = saved_values_for_call layout dst live_after in
  let entry_ids =
    List.fold_left (fun acc (value_id, _) -> IntSet.add value_id acc) (IntSet.singleton dst) saved_values
  in
  let suffix_lives = List.fold_left IntSet.union live_after rest_afters in
  let suffix_stmt_values =
    List.fold_left (fun acc stmt -> IntSet.union acc (stmt_resident_values stmt)) IntSet.empty rest
  in
  let find_live id = block_liveness_exn unit_cg id in
  let term_values = terminator_live_out ~find_live term in
  let inline_values =
    List.fold_left
      (fun acc stmt -> IntSet.union acc (stmt_resident_values stmt))
      IntSet.empty (collect_inline_stmts term)
  in
  let resident_ids =
    IntSet.union entry_ids
      (IntSet.union suffix_lives (IntSet.union suffix_stmt_values (IntSet.union term_values inline_values)))
    |> IntSet.union (inlined_arm_binders term)
  in
  let src_working = call_src_working layout dst live_after args in
  let layout, slot_remap = compact_layout_subset layout block.params ~entry_ids ~resident_ids in
  let saved_src_slots = List.map snd saved_values in
  let saved_dst_slots =
    List.map
      (fun (_, old_slot) ->
        match IntMap.find_opt old_slot slot_remap with
        | Some slot -> slot
        | None -> failf "CompileRegMemo: missing compact resume slot for old slot %d" old_slot)
      saved_values
  in
  let dst_slot =
    match IntMap.find_opt dst layout.slot_of_value with
    | Some slot -> slot
    | None -> failf "CompileRegMemo: missing compact resume slot for v%d" dst
  in
  { layout; saved_src_slots; saved_dst_slots; dst_slot; src_working }

let rec compile_stmt_chain ctx unit_cg block layout ~step_entry ~working current_pc w stmts stmt_afters term =
  match (stmts, stmt_afters) with
  | [], [] -> compile_terminator ctx unit_cg block layout ~step_entry current_pc w term
  | Bind (dst, Call (Direct callee_name, args)) :: rest, live_after :: rest_afters ->
      let callee = compiled_unit_exn ctx callee_name in
      bind_values "arg"
        (List.map (operand_value_code ctx layout w) args)
        (fun arg_values ->
          if rest = [] then
            match term with
            | Return (OLocal return_id) when return_id = dst ->
                [%seqs
                  assert_env_length_ w (int_ working);
                  emit_trim_resolved w step_entry working;
                  compile_call_setup ctx callee w arg_values;
                  goto_ w callee.entry_pc]
            | _ ->
                let resume = build_call_resume_layout unit_cg block layout dst live_after args [] [] term in
                let resume_pc =
                  add_code_k ctx (fun resume_pc ->
                      ( lam_ "w" (fun w ->
                            [%seqs
                              assert_env_length_ w (int_ resume.layout.entry_size);
                              emit_frame_growth w resume.layout.entry_size resume.layout.frame_size;
                              compile_terminator ctx unit_cg block resume.layout ~step_entry:resume.layout.entry_size
                                resume_pc w term]),
                        resume_pc ))
                in
                compile_non_tail_call ctx layout ~step_entry ~working w callee arg_values resume resume_pc
          else
            let resume = build_call_resume_layout unit_cg block layout dst live_after args rest rest_afters term in
            let resume_working =
              match (rest, rest_afters) with
              | Bind (dst2, Call (Direct _, args2)) :: _, live_after2 :: _ ->
                  call_src_working resume.layout dst2 live_after2 args2
              | _ -> resume.layout.frame_size
            in
            let resume_pc =
              add_code_k ctx (fun resume_pc ->
                  ( lam_ "w" (fun w ->
                        [%seqs
                          assert_env_length_ w (int_ resume.layout.entry_size);
                          emit_frame_growth w resume.layout.entry_size resume_working;
                          compile_stmt_chain ctx unit_cg block resume.layout ~step_entry:resume.layout.entry_size
                            ~working:resume_working resume_pc w rest rest_afters term]),
                    resume_pc ))
            in
            compile_non_tail_call ctx layout ~step_entry ~working w callee arg_values resume resume_pc)
  | Bind (_, Call (Indirect _, _)) :: _, _ -> unsupported_codegen "indirect call"
  | Bind (_, Call (Direct _, _)) :: _, [] -> failwith "CompileRegMemo: missing live-after information for call"
  | Bind (dst, rhs) :: rest, _live_after :: rest_afters ->
      [%seqs
        assert_env_length_ w (int_ working);
        compile_non_call_rhs ctx layout w dst rhs;
        compile_stmt_chain ctx unit_cg block layout ~step_entry ~working current_pc w rest rest_afters term]
  | _ -> failwith "CompileRegMemo: mismatched statement and liveness counts"

and compile_non_tail_call (ctx : cg_ctx) (layout : block_layout) ~step_entry ~working w (callee : unit_codegen)
    arg_values (resume : call_resume) resume_pc =
  let cont_name = "cont_" ^ string_of_int ctx.conts_count in
  add_cont ctx cont_name
    (List.length resume.saved_src_slots + 1)
    (fun w tl ->
      let_in_ "ret"
        (get_env_slot_ w (int_ 0))
        (fun ret ->
          [%seqs
            assert_env_length_ w (int_ 1);
            set_k_ w (get_next_cont_ tl);
            init_frame_ w (int_ resume.layout.entry_size) dummy_value_;
            restore_env_slots_ w (list_literal_of_ int_ resume.saved_dst_slots) tl;
            set_env_slot_ w (int_ resume.dst_slot) ret;
            goto_ w resume_pc]));
  let saved = collect_env_slots_ w (list_literal_of_ int_ resume.saved_src_slots) in
  [%seqs
    assert_env_length_ w (int_ working);
    set_k_ w (memo_appends_ [ from_constructor_ (ctor_tag_name ctx cont_name); saved; world_kont_ w ]);
    emit_trim_resolved w step_entry working;
    compile_call_setup ctx callee w arg_values;
    goto_ w callee.entry_pc]

and compile_terminator ctx unit_cg block layout ~step_entry current_pc w term =
  match term with
  | Return operand ->
      [%seqs
        emit_trim_resolved w step_entry layout.frame_size;
        return_value_ w (operand_value_code ctx layout w operand) (pc_to_exp_ (pc_ ctx.apply_cont))]
  | Jump (succ_id, args) ->
      [%seqs
        assert_env_length_ w (int_ layout.frame_size);
        compile_shuffle_edge ctx unit_cg layout ~step_entry w block.id succ_id ~jump_args:args []]
  | Branch (cond, then_tgt, else_tgt) -> (
      let do_branch (tgt : branch_target) =
        match tgt.inline with
        | None -> compile_shuffle_edge ctx unit_cg layout ~step_entry w block.id tgt.block []
        | Some region -> compile_inline_region ctx unit_cg block layout ~step_entry current_pc w region
      in
      let do_resolve_branch cond_word =
        if_ (bool_nonzero_code (word_get_value_ cond_word)) (do_branch then_tgt) (do_branch else_tgt)
      in
      match cond with
      | OLocal value_id ->
          let slot = slot_in_layout layout value_id in
          let_in_ "resolved" (resolve_ w (src_E_ slot)) (fun pair -> do_resolve_branch (zro_ pair))
      | _ ->
          let cond_word = memo_to_word_ (operand_value_code ctx layout w cond) in
          do_resolve_branch cond_word)
  | Match (scrutinee, arms) ->
      [%seqs
        assert_env_length_ w (int_ layout.frame_size);
        match scrutinee with
        | OLocal value_id ->
            let slot = slot_in_layout layout value_id in
            let_in_ "resolved"
              (resolve_ w (src_E_ slot))
              (fun resolved ->
                compile_ctor_dispatch ctx unit_cg block layout ~step_entry current_pc
                  (operand_value_code ctx layout w scrutinee)
                  resolved arms w)
        | _ -> failf "CompileRegMemo: match scrutinee must be a local operand"]

(* Compile an inline region in place: its body runs inside the dispatcher's
   memo step using the dispatcher's layout, and its terminator is compiled
   recursively. *)
and compile_inline_region ctx unit_cg parent_block parent_layout ~step_entry current_pc w (region : inline_region) =
  let find_live id = block_liveness_exn unit_cg id in
  let live_before_term = terminator_live_out ~find_live region.term in
  let _, afters =
    List.fold_right
      (fun stmt (live_after, acc) -> (RegAlloc.transfer_stmt stmt live_after, live_after :: acc))
      region.body (live_before_term, [])
  in
  compile_stmt_chain ctx unit_cg parent_block parent_layout ~step_entry ~working:parent_layout.frame_size current_pc w
    region.body afters region.term

(* Match arm dispatch: handle both non-inlined and inlined arms. *)
and compile_arm_take ctx unit_cg (parent_block : block) parent_layout ~step_entry current_pc w (arm : match_arm)
    (assigns : (value_id * Value.seq code) list) =
  match arm.inline with
  | None -> compile_shuffle_edge ctx unit_cg parent_layout ~step_entry w parent_block.id arm.block assigns
  | Some region ->
      (* Write each binder value into its slot in the dispatcher's layout, then
         run the inline region. The binder's slot was allocated as part of the
         parent block's residents. *)
      let write_binders =
        seqs_
          (List.map
             (fun (binder_id, value_code) ->
               let slot = slot_in_layout parent_layout binder_id in
               fun _ -> set_env_slot_ w (int_ slot) value_code)
             assigns)
      in
      [%seqs
        write_binders;
        compile_inline_region ctx unit_cg parent_block parent_layout ~step_entry current_pc w region]

and compile_tuple_bindings ctx unit_cg parent_block parent_layout ~step_entry current_pc whole_pattern parts patterns w
    arm =
  bind_values "part"
    (List.mapi (fun index _ -> nth_seq parts index) patterns)
    (fun bound_parts ->
      compile_arm_take ctx unit_cg parent_block parent_layout ~step_entry current_pc w arm
        (tuple_pattern_assigns unit_cg whole_pattern patterns bound_parts))

and compile_default_arm ctx unit_cg parent_block parent_layout ~step_entry subject arms w current_pc =
  let rec loop = function
    | [] -> unreachable_ (pc_to_int current_pc)
    | ({ pattern = RPAny; _ } as arm : match_arm) :: _ ->
        compile_arm_take ctx unit_cg parent_block parent_layout ~step_entry current_pc w arm []
    | ({ pattern = RPBind value_id; _ } as arm : match_arm) :: _ ->
        compile_arm_take ctx unit_cg parent_block parent_layout ~step_entry current_pc w arm [ (value_id, subject) ]
    | _ :: rest -> loop rest
  in
  loop arms

and compile_ctor_arm ctx unit_cg parent_block parent_layout ~step_entry current_pc pair w default (arm : match_arm) =
  match arm.pattern with
  | RPCtor (ctor, payload) ->
      let body =
        match payload with
        | None | Some RPAny -> compile_arm_take ctx unit_cg parent_block parent_layout ~step_entry current_pc w arm []
        | Some (RPBind value_id) ->
            compile_arm_take ctx unit_cg parent_block parent_layout ~step_entry current_pc w arm
              [ (value_id, pair_value_ pair) ]
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
                if_ len_ok
                  (compile_tuple_bindings ctx unit_cg parent_block parent_layout ~step_entry current_pc whole_pattern
                     parts patterns w arm)
                  default)
        | Some unsupported -> unsupported_match_pattern unit_cg (RPCtor (ctor, Some unsupported))
      in
      (Hashtbl.find_exn ctx.ctag ctor, Hashtbl.find_exn ctx.ctag_name ctor, body)
  | _ -> failwith "CompileRegMemo: internal error while compiling ctor dispatch"

and compile_ctor_dispatch ctx unit_cg parent_block parent_layout ~step_entry current_pc raw_subject resolved arms w =
  let default = compile_default_arm ctx unit_cg parent_block parent_layout ~step_entry raw_subject arms w current_pc in
  let ctor_arms =
    List.filter_map
      (fun ({ pattern; _ } as arm : match_arm) ->
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
            (List.map
               (compile_ctor_arm ctx unit_cg parent_block parent_layout ~step_entry current_pc resolved w default)
               ctor_arms)
            default)

let compile_block ctx unit_cg (blk : block) =
  let current_pc =
    match IntMap.find_opt blk.id unit_cg.block_pcs with
    | Some pc -> pc
    | None -> failf "CompileRegMemo: missing pc for block b%d" blk.id
  in
  let info = block_liveness_exn unit_cg blk.id in
  let layout = block_layout_exn unit_cg.allocation blk.id in
  let stmt_afters = stmt_live_afters blk info in
  let working =
    match (blk.body, stmt_afters) with
    | Bind (dst, Call (Direct _, args)) :: _, live_after :: _ -> call_src_working layout dst live_after args
    | _ -> layout.frame_size
  in
  set_code ctx current_pc
    (lam_ "w" (fun w ->
         seq_
           (assert_env_length_ w (int_ layout.entry_size))
           (fun () ->
             seq_ (emit_frame_growth w layout.entry_size working) (fun () ->
                 compile_stmt_chain ctx unit_cg blk layout ~step_entry:layout.entry_size ~working current_pc w blk.body
                   stmt_afters blk.term))))

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
  let slot_to_arg = Array.make entry_layout.entry_size (uncode dummy_value_) in
  List.iteri (fun i slot -> slot_to_arg.(slot) <- string ("x" ^ string_of_int i)) entry_layout.param_slots;
  let env_elems = Array.to_list slot_to_arg in
  let name = unit_cg.unit_ir.name in
  string "let " ^^ string name ^^ space ^^ string "memo"
  ^^ (if args = [] then empty else space ^^ separate space args)
  ^^ string " : exec_result =" ^^ hardline ^^ string "  exec_cek "
  ^^ string ("(pc_to_exp (int_to_pc " ^ string_of_int (pc_to_int unit_cg.entry_pc) ^ "))")
  ^^ string " (Dynarray.of_list [ "
  ^^ separate (string "; ") env_elems
  ^^ string " ]) ("
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
