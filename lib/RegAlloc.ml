open ControlFlowGraph

(* Per-block register allocation for the regmemo CFG.
   Each block gets its own dense slot layout, containing only the values that are
   live at the block's entry plus values defined inside it.  The allocator builds
   a block-local interference graph and colours it independently.

   Because different blocks have different layouts, every CFG edge implies a
   "reshape": the predecessor reads the values needed by the successor from its
   own frame, initialises the successor's frame, and writes them into the new
   slots.  No in-place parallel-copy scheduler is needed — the generated code
   binds everything into OCaml let-bindings first, then writes. *)

let failf fmt = Printf.ksprintf failwith fmt

type slot = int
type block_layout = { slot_of_value : slot IntMap.t; param_slots : slot list; frame_size : int }
type unit_allocation = { block_layouts : block_layout IntMap.t }

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

let value_priority (info : value_info) =
  match info.kind with Param | JoinParam | MatchBinder -> 0 | Local | Temp -> 1 | JoinBinder -> 2

(* Determine which values need slots within a single block.
   This is the union of:
   - values live at block entry (live_in ∪ live_params)
   - all block parameters (even dead ones, for alignment with jump args)
   - values defined in the block body *)
let block_resident_values (block : block) (info : CfgLiveness.block_liveness) =
  let entry_live = IntSet.union info.live_in info.live_params in
  let all_params = intset_of_list block.params in
  let defined =
    List.fold_left (fun acc stmt -> match stmt with Bind (id, _) -> IntSet.add id acc) IntSet.empty block.body
  in
  IntSet.union (IntSet.union entry_live all_params) defined

(* Build an interference graph restricted to a block's resident values.
   Only edges between residents are recorded.
   For Entry blocks, all params interfere at entry (the caller writes all of them).
   For other blocks (Join, MatchArm, etc.), only live params are written by reshaping,
   so dead params need not interfere. *)
let build_block_interference (block : block) (info : CfgLiveness.block_liveness) (residents : IntSet.t) =
  let graph = IntSet.fold (fun id graph -> IntMap.add id IntSet.empty graph) residents IntMap.empty in
  let add_resident_clique live graph = add_live_clique (IntSet.inter live residents) graph in
  let entry_live =
    match block.kind with
    | Entry -> IntSet.union info.live_in (intset_of_list block.params)
    | Join _ | IfThen | IfElse | MatchArm -> IntSet.union info.live_in info.live_params
  in
  let graph = add_resident_clique entry_live graph in
  let graph = add_resident_clique info.live_before_term graph in
  let _, graph =
    List.fold_right
      (fun stmt (live_after, graph) ->
        let graph = add_resident_clique live_after graph in
        (transfer_stmt stmt live_after, graph))
      block.body (info.live_before_term, graph)
  in
  graph

(* Greedy graph-colouring: assign the smallest available slot to each value,
   ordered by descending interference degree, then by priority/id. *)
let allocate_slots_for (value_infos : value_info list) (interference : IntSet.t IntMap.t) =
  let sorted =
    List.sort
      (fun (a : value_info) (b : value_info) ->
        let degree_a = match IntMap.find_opt a.id interference with Some live -> IntSet.cardinal live | None -> 0 in
        let degree_b = match IntMap.find_opt b.id interference with Some live -> IntSet.cardinal live | None -> 0 in
        match Int.compare degree_b degree_a with
        | 0 -> (
            match Int.compare (value_priority a) (value_priority b) with 0 -> Int.compare a.id b.id | order -> order)
        | order -> order)
      value_infos
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
    IntMap.empty sorted

(* Allocate slots for a single block independently. *)
let allocate_block (value_map : value_info IntMap.t) (block : block) (info : CfgLiveness.block_liveness) : block_layout
    =
  let residents = block_resident_values block info in
  let allocatable_residents =
    IntSet.filter
      (fun id -> match IntMap.find_opt id value_map with Some vi -> allocatable_value vi | None -> false)
      residents
  in
  let interference = build_block_interference block info allocatable_residents in
  let value_infos = IntSet.elements allocatable_residents |> List.filter_map (fun id -> IntMap.find_opt id value_map) in
  let slot_of_value = allocate_slots_for value_infos interference in
  let param_slots =
    List.map
      (fun param_id ->
        match IntMap.find_opt param_id slot_of_value with
        | Some slot -> slot
        | None -> failf "RegAlloc: missing slot for block parameter v%d in b%d" param_id block.id)
      block.params
  in
  let frame_size = IntMap.fold (fun _ slot acc -> Int.max acc (slot + 1)) slot_of_value 0 in
  { slot_of_value; param_slots; frame_size }

let allocate_unit_slots (unit_ir : unit_ir) (liveness : CfgLiveness.unit_liveness) : unit_allocation =
  let value_map = List.fold_left (fun acc (vi : value_info) -> IntMap.add vi.id vi acc) IntMap.empty unit_ir.values in
  let block_layouts =
    List.fold_left
      (fun layouts (block : block) ->
        let info =
          match IntMap.find_opt block.id liveness.blocks with
          | Some info -> info
          | None -> failf "RegAlloc: missing liveness for block b%d" block.id
        in
        IntMap.add block.id (allocate_block value_map block info) layouts)
      IntMap.empty unit_ir.blocks
  in
  { block_layouts }
