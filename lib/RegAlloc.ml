open ControlFlowGraph

(* Register allocation for the regmemo CFG is a simple liveness-driven graph coloring pass.
   We first build an interference graph from block-entry and statement-level live sets,
   then assign slots with a degree-ordered greedy coloring strategy. Block parameters are
   not handled as a separate calling convention: they participate in the same allocation
   space as locals and temporaries, and each block records the slots its parameters occupy.
   After coloring, jump edges are inspected for slot-to-slot move cycles; if an edge needs
   a cyclic parallel copy, we reserve one extra scratch slot in the frame layout. *)

let failf fmt = Printf.ksprintf failwith fmt

type slot = int
type block_layout = { param_slots : slot list; frame_size : int }

type unit_allocation = {
  slot_of_value : slot IntMap.t;
  block_layouts : block_layout IntMap.t;
  frame_size : int;
  scratch_slot : slot option;
  interference : IntSet.t IntMap.t;
}

let add_operand_uses (live : IntSet.t) = function
  | OLocal id -> IntSet.add id live
  | OGlobal _ | OBuiltin _ | OInt _ | OFloat _ | OBool _ | OString _ | OUnit | OCtor _ -> live

let operand_uses operand = add_operand_uses IntSet.empty operand
let add_operands_uses (live : IntSet.t) operands = List.fold_left add_operand_uses live operands

let add_rhs_uses (live : IntSet.t) = function
  | Move operand -> add_operand_uses live operand
  | Call (target, args) ->
      let live = match target with Direct _ -> live | Indirect operand -> add_operand_uses live operand in
      add_operands_uses live args
  | Construct (_, args) | Tuple args | Array args -> add_operands_uses live args
  | Select (target, _) -> add_operand_uses live target
  | BinOp (_, lhs, rhs) -> add_operand_uses (add_operand_uses live lhs) rhs

let transfer_stmt (stmt : stmt) (live_after : IntSet.t) =
  match stmt with Bind (id, rhs) -> add_rhs_uses (IntSet.remove id live_after) rhs

let allocatable_value (info : value_info) =
  match info.kind with JoinBinder -> false | Param | Local | Temp | JoinParam | MatchBinder -> true

let add_interference_edge a b (graph : IntSet.t IntMap.t) =
  if a = b then graph
  else
    let add_neighbor src dst graph =
      let neighbors = match IntMap.find_opt src graph with Some neighbors -> neighbors | None -> IntSet.empty in
      IntMap.add src (IntSet.add dst neighbors) graph
    in
    graph |> add_neighbor a b |> add_neighbor b a

let add_live_clique (live : IntSet.t) (graph : IntSet.t IntMap.t) =
  let rec aux graph = function
    | [] -> graph
    | id :: rest ->
        let graph = List.fold_left (fun graph other -> add_interference_edge id other graph) graph rest in
        aux graph rest
  in
  aux graph (IntSet.elements live)

let intset_of_list values = List.fold_left (fun acc value -> IntSet.add value acc) IntSet.empty values

let add_block_entry_interference (block : block) (info : CfgLiveness.block_liveness) graph =
  add_live_clique (IntSet.union info.live_in (intset_of_list block.params)) graph

let add_stmt_interference (block : block) (info : CfgLiveness.block_liveness) graph =
  let graph = add_live_clique info.live_before_term graph in
  let _, graph =
    List.fold_right
      (fun stmt (live_after, graph) ->
        let graph = add_live_clique live_after graph in
        (transfer_stmt stmt live_after, graph))
      block.body (info.live_before_term, graph)
  in
  graph

let build_unit_interference (unit_ir : unit_ir) (liveness : CfgLiveness.unit_liveness) =
  let graph =
    List.fold_left
      (fun graph (info : value_info) -> if allocatable_value info then IntMap.add info.id IntSet.empty graph else graph)
      IntMap.empty unit_ir.values
  in
  List.fold_left
    (fun graph (block : block) ->
      let info =
        match IntMap.find_opt block.id liveness.blocks with
        | Some info -> info
        | None -> failf "RegAlloc: missing liveness for block b%d" block.id
      in
      graph |> add_block_entry_interference block info |> add_stmt_interference block info)
    graph unit_ir.blocks

let value_priority (info : value_info) =
  match info.kind with Param | JoinParam | MatchBinder -> 0 | Local | Temp -> 1 | JoinBinder -> 2

let allocate_slots (unit_ir : unit_ir) (interference : IntSet.t IntMap.t) =
  let value_infos =
    List.filter allocatable_value unit_ir.values
    |> List.sort (fun (a : value_info) (b : value_info) ->
        let degree_a = match IntMap.find_opt a.id interference with Some live -> IntSet.cardinal live | None -> 0 in
        let degree_b = match IntMap.find_opt b.id interference with Some live -> IntSet.cardinal live | None -> 0 in
        match Int.compare degree_b degree_a with
        | 0 -> (
            match Int.compare (value_priority a) (value_priority b) with 0 -> Int.compare a.id b.id | order -> order)
        | order -> order)
  in
  List.fold_left
    (fun slot_of_value (info : value_info) ->
      let neighbors = match IntMap.find_opt info.id interference with Some live -> live | None -> IntSet.empty in
      let used_slots =
        IntSet.fold
          (fun neighbor used_slots ->
            match IntMap.find_opt neighbor slot_of_value with
            | Some slot -> IntSet.add slot used_slots
            | None -> used_slots)
          neighbors IntSet.empty
      in
      let rec pick slot = if IntSet.mem slot used_slots then pick (slot + 1) else slot in
      IntMap.add info.id (pick 0) slot_of_value)
    IntMap.empty value_infos

let block_layouts_of_slots (unit_ir : unit_ir) (slot_of_value : slot IntMap.t) ~frame_size =
  List.fold_left
    (fun layouts (block : block) ->
      let param_slots =
        List.map
          (fun param_id ->
            match IntMap.find_opt param_id slot_of_value with
            | Some slot -> slot
            | None -> failf "RegAlloc: missing slot for block parameter v%d in b%d" param_id block.id)
          block.params
      in
      IntMap.add block.id { param_slots; frame_size } layouts)
    IntMap.empty unit_ir.blocks

let add_move_edge src dst graph =
  if src = dst then graph
  else
    let succs = match IntMap.find_opt src graph with Some succs -> succs | None -> IntSet.empty in
    IntMap.add src (IntSet.add dst succs) graph

let directed_graph_has_cycle graph =
  let colors = ref IntMap.empty in
  let rec visit slot =
    match IntMap.find_opt slot !colors with
    | Some 1 -> true
    | Some 2 -> false
    | Some _ -> false
    | None ->
        colors := IntMap.add slot 1 !colors;
        let succs = match IntMap.find_opt slot graph with Some succs -> succs | None -> IntSet.empty in
        let found_cycle = IntSet.exists visit succs in
        colors := IntMap.add slot 2 !colors;
        found_cycle
  in
  IntMap.exists (fun slot _ -> visit slot) graph

let jump_edge_needs_scratch (block_map : block IntMap.t) (liveness : CfgLiveness.unit_liveness)
    (slot_of_value : slot IntMap.t) (block : block) =
  match block.term with
  | Jump (succ_id, args) ->
      let succ =
        match IntMap.find_opt succ_id block_map with
        | Some succ -> succ
        | None -> failf "RegAlloc: unknown jump target b%d" succ_id
      in
      let block_info =
        match IntMap.find_opt block.id liveness.blocks with
        | Some info -> info
        | None -> failf "RegAlloc: missing liveness for block b%d" block.id
      in
      let edge_live =
        match IntMap.find_opt succ_id block_info.live_on_edge with
        | Some info -> info
        | None -> failf "RegAlloc: missing edge liveness from b%d to b%d" block.id succ_id
      in
      let move_graph =
        try
          List.fold_left2
            (fun graph param_id arg ->
              if IntMap.mem param_id edge_live.live_params then
                let dst_slot =
                  match IntMap.find_opt param_id slot_of_value with
                  | Some slot -> slot
                  | None -> failf "RegAlloc: missing slot for live jump parameter v%d" param_id
                in
                match arg with
                | OLocal value_id -> (
                    match IntMap.find_opt value_id slot_of_value with
                    | Some src_slot -> add_move_edge src_slot dst_slot graph
                    | None -> graph)
                | OGlobal _ | OBuiltin _ | OInt _ | OFloat _ | OBool _ | OString _ | OUnit | OCtor _ -> graph
              else graph)
            IntMap.empty succ.params args
        with Invalid_argument _ ->
          failf "RegAlloc: jump to block b%d expects %d args, got %d" succ.id (List.length succ.params)
            (List.length args)
      in
      directed_graph_has_cycle move_graph
  | Return _ | Branch _ | Match _ -> false

let needs_scratch_slot (unit_ir : unit_ir) (liveness : CfgLiveness.unit_liveness) (slot_of_value : slot IntMap.t) =
  let block_map =
    List.fold_left (fun acc (block : block) -> IntMap.add block.id block acc) IntMap.empty unit_ir.blocks
  in
  List.exists (jump_edge_needs_scratch block_map liveness slot_of_value) unit_ir.blocks

let allocate_unit_slots (unit_ir : unit_ir) (liveness : CfgLiveness.unit_liveness) : unit_allocation =
  let interference = build_unit_interference unit_ir liveness in
  let slot_of_value = allocate_slots unit_ir interference in
  let base_frame_size = IntMap.fold (fun _ slot frame_size -> Int.max frame_size (slot + 1)) slot_of_value 0 in
  let scratch_slot = if needs_scratch_slot unit_ir liveness slot_of_value then Some base_frame_size else None in
  let frame_size = match scratch_slot with Some slot -> slot + 1 | None -> base_frame_size in
  let block_layouts = block_layouts_of_slots unit_ir slot_of_value ~frame_size in
  { slot_of_value; block_layouts; frame_size; scratch_slot; interference }
