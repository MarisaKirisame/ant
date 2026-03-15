open PPrint
open Syntax
include ControlFlowGraph

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
  CfgLiveness.analyze_unit ~blocks:unit_ir.blocks
    ~entry_id:unit_ir.entry
    ~successors
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

let pp_slot_binding values (id, slot) =
  pp_value_ref values id ^^ space ^^ string "->" ^^ space ^^ pp_slot slot

let pp_slot_assignments (unit_ir : unit_ir) (allocation : unit_allocation) =
  let values = value_table unit_ir in
  match IntMap.bindings allocation.slot_of_value with
  | [] -> string "slots=[]"
  | bindings ->
      string "slots="
      ^^ lbracket
      ^^ pp_list (pp_slot_binding values) (comma ^^ space)
           (List.sort
              (fun (id_a, slot_a) (id_b, slot_b) ->
                match Int.compare slot_a slot_b with 0 -> Int.compare id_a id_b | order -> order)
              bindings)
      ^^ rbracket

let pp_block_layout _values block_id (layout : block_layout) =
  string "block" ^^ space ^^ string ("b" ^ string_of_int block_id) ^^ space ^^ string "params="
  ^^ lbracket
  ^^ pp_list pp_slot (comma ^^ space) layout.param_slots
  ^^ rbracket ^^ space ^^ string "frame=" ^^ string (string_of_int layout.frame_size)

let pp_unit_allocation (unit_ir : unit_ir) (allocation : unit_allocation) =
  let values = value_table unit_ir in
  let scratch_doc =
    match allocation.scratch_slot with
    | Some slot -> string "scratch=" ^^ pp_slot slot
    | None -> string "scratch=none"
  in
  let block_layouts_doc =
    match IntMap.bindings allocation.block_layouts with
    | [] -> string "layouts=[]"
    | layouts ->
        string "layouts:" ^^ hardline
        ^^ separate_map hardline (fun (block_id, layout) -> string "  " ^^ pp_block_layout values block_id layout)
             layouts
  in
  pp_slot_assignments unit_ir allocation ^^ space ^^ string "frame_size=" ^^ string (string_of_int allocation.frame_size)
  ^^ space ^^ scratch_doc ^^ hardline ^^ block_layouts_doc

let pp_prog_analysis (analysis : prog_analysis) =
  match List.combine analysis.ir.units analysis.liveness with
  | [] -> string "regmemo liveness empty"
  | units ->
      separate_map (hardline ^^ hardline)
        (fun (unit_ir, liveness) -> pp_unit_ir unit_ir ^^ hardline ^^ string "liveness:" ^^ hardline ^^ pp_unit_liveness unit_ir liveness)
        units

let pp_prog_ir (prog : prog_ir) =
  match prog.units with [] -> string "regmemo empty" | units -> separate_map (hardline ^^ hardline) pp_unit_ir units

let pp_prog_slot_analysis (analysis : prog_slot_analysis) =
  match List.combine (List.combine analysis.ir.units analysis.liveness) analysis.allocations with
  | [] -> string "regmemo allocation empty"
  | units ->
      separate_map (hardline ^^ hardline)
        (fun ((unit_ir, liveness), allocation) ->
          pp_unit_ir unit_ir
          ^^ hardline ^^ string "liveness:" ^^ hardline
          ^^ pp_unit_liveness unit_ir liveness
          ^^ hardline ^^ string "allocation:" ^^ hardline
          ^^ pp_unit_allocation unit_ir allocation)
        units

let compile_reg_memo (prog : 'a prog) : document = pp_prog_slot_analysis (analyze_prog_slots (lower_prog prog))

module Backend = struct
  let compile = compile_reg_memo
end
