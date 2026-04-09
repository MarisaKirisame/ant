open ControlFlowGraph

(* Per-block register allocation with cross-block slot coalescing.

   Phase 1 — per-block layout: each block gets a dense slot layout containing
   only its resident values (live-at-entry + defined-in-body).

   Phase 2 — coalescing: edges impose slot *affinities* between values that flow
   across blocks.  When colouring a block we try to honour affinities from
   already-coloured neighbours, but never override interference constraints.

   Phase 3 — edge planning: after all blocks are coloured, each CFG edge is
   classified as Elide (identical layout), InPlace (same frame size, parallel
   copy), or Reshape (different frame size, bind-init-write). *)

let failf fmt = Printf.ksprintf failwith fmt

type slot = int
type block_layout = { slot_of_value : slot IntMap.t; param_slots : slot list; frame_size : int }

(* Edge transition plan *)
type move_src = SlotSrc of slot | OperandSrc of operand
type move_step = { src : move_src; dst : slot }

type edge_plan =
  | Elide
  | InPlace of { moves : move_step list; scratch : slot option }
  | Reshape of int (* target frame_size; caller does bind-init-write *)

type unit_allocation = {
  block_layouts : block_layout IntMap.t;
  edge_plans : edge_plan IntMap.t IntMap.t; (* src_block -> succ_block -> plan *)
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

let value_priority (info : value_info) =
  match info.kind with Param | JoinParam | MatchBinder -> 0 | Local | Temp -> 1 | JoinBinder -> 2

let block_resident_values (block : block) (info : CfgLiveness.block_liveness) =
  let entry_live = IntSet.union info.live_in info.live_params in
  let all_params = intset_of_list block.params in
  let defined =
    List.fold_left (fun acc stmt -> match stmt with Bind (id, _) -> IntSet.add id acc) IntSet.empty block.body
  in
  IntSet.union (IntSet.union entry_live all_params) defined

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

(* ----- Affinity computation ----- *)

(* An affinity is (value_id, preferred_slot).  We collect affinities for a block
   from edges to/from already-coloured neighbours. *)

type affinity = { value_id : int; preferred_slot : slot }

(* Collect affinities for `block` from already-coloured layouts.
   For each value that appears in both the current block's residents and an
   already-coloured neighbour, suggest the neighbour's slot. *)
let collect_affinities (unit_ir : unit_ir) (liveness : CfgLiveness.unit_liveness) (coloured : block_layout IntMap.t)
    (block : block) (_info : CfgLiveness.block_liveness) (residents : IntSet.t) : affinity list =
  let block_map = List.fold_left (fun acc (b : block) -> IntMap.add b.id b acc) IntMap.empty unit_ir.blocks in
  let affinities = ref [] in
  let add_affinity vid slot = affinities := { value_id = vid; preferred_slot = slot } :: !affinities in
  (* From successors: if succ is already coloured, values that flow to it suggest their succ slot *)
  let succs =
    match block.term with
    | Return _ -> []
    | Jump (succ_id, _) -> [ succ_id ]
    | Branch (_, then_id, else_id) -> [ then_id; else_id ]
    | Match (_, arms) -> List.map (fun ({ block = b; _ } : match_arm) -> b) arms
  in
  List.iter
    (fun succ_id ->
      match IntMap.find_opt succ_id coloured with
      | None -> ()
      | Some (succ_layout : block_layout) -> (
          let succ_info = match IntMap.find_opt succ_id liveness.blocks with Some i -> i | None -> assert false in
          (* live-in values of succ that are our residents *)
          IntSet.iter
            (fun vid ->
              if IntSet.mem vid residents then
                match IntMap.find_opt vid succ_layout.slot_of_value with Some s -> add_affinity vid s | None -> ())
            succ_info.live_in;
          (* Jump params: map arg → param slot *)
          match block.term with
          | Jump (sid, args) when sid = succ_id -> (
              let succ_block = match IntMap.find_opt succ_id block_map with Some b -> b | None -> assert false in
              try
                List.iter2
                  (fun param_id arg ->
                    match (arg, IntMap.find_opt param_id succ_layout.slot_of_value) with
                    | OLocal vid, Some s when IntSet.mem vid residents -> add_affinity vid s
                    | _ -> ())
                  succ_block.params args
              with Invalid_argument _ -> ())
          | _ -> ()))
    succs;
  (* From predecessors: if pred is already coloured, shared live-through values suggest their pred slot *)
  List.iter
    (fun (pred : block) ->
      match IntMap.find_opt pred.id coloured with
      | None -> ()
      | Some (pred_layout : block_layout) ->
          (* Values live-in to us that the pred also has *)
          IntMap.iter (fun vid slot -> if IntSet.mem vid residents then add_affinity vid slot) pred_layout.slot_of_value)
    (List.filter
       (fun (b : block) ->
         List.mem block.id
           (match b.term with
           | Return _ -> []
           | Jump (s, _) -> [ s ]
           | Branch (_, t, e) -> [ t; e ]
           | Match (_, arms) -> List.map (fun ({ block = b; _ } : match_arm) -> b) arms))
       unit_ir.blocks);
  !affinities

(* ----- Coalescing-aware allocation ----- *)

(* Like allocate_slots_for, but with soft slot preferences from affinities. *)
let allocate_slots_with_affinities (value_infos : value_info list) (interference : IntSet.t IntMap.t)
    (affinities : affinity list) =
  (* Build a preferred-slot map: value_id → slot (first affinity wins in case of conflict) *)
  let preferred =
    List.fold_left
      (fun acc { value_id; preferred_slot } ->
        if IntMap.mem value_id acc then acc else IntMap.add value_id preferred_slot acc)
      IntMap.empty affinities
  in
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
      (* Try preferred slot first *)
      let slot =
        match IntMap.find_opt info.id preferred with
        | Some pref when not (IntSet.mem pref used_slots) -> pref
        | _ ->
            let rec pick slot = if IntSet.mem slot used_slots then pick (slot + 1) else slot in
            pick 0
      in
      IntMap.add info.id slot slot_of_value)
    IntMap.empty sorted

let allocate_slots_for = allocate_slots_with_affinities

let allocate_block_with_affinities (value_map : value_info IntMap.t) (block : block) (info : CfgLiveness.block_liveness)
    (affinities : affinity list) : block_layout =
  let residents = block_resident_values block info in
  let allocatable_residents =
    IntSet.filter
      (fun id -> match IntMap.find_opt id value_map with Some vi -> allocatable_value vi | None -> false)
      residents
  in
  let interference = build_block_interference block info allocatable_residents in
  let value_infos = IntSet.elements allocatable_residents |> List.filter_map (fun id -> IntMap.find_opt id value_map) in
  let slot_of_value = allocate_slots_for value_infos interference affinities in
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

(* ----- Reverse-postorder traversal ----- *)

let compute_rpo (unit_ir : unit_ir) =
  let block_map = List.fold_left (fun acc (b : block) -> IntMap.add b.id b acc) IntMap.empty unit_ir.blocks in
  let visited = ref IntSet.empty in
  let order = ref [] in
  let rec visit id =
    if not (IntSet.mem id !visited) then (
      visited := IntSet.add id !visited;
      let block = match IntMap.find_opt id block_map with Some b -> b | None -> assert false in
      let succs =
        match block.term with
        | Return _ -> []
        | Jump (s, _) -> [ s ]
        | Branch (_, t, e) -> [ t; e ]
        | Match (_, arms) -> List.map (fun ({ block = b; _ } : match_arm) -> b) arms
      in
      List.iter visit succs;
      order := id :: !order)
  in
  visit unit_ir.entry;
  (* Also visit blocks unreachable from entry (shouldn't happen, but be safe) *)
  List.iter (fun (b : block) -> visit b.id) unit_ir.blocks;
  !order

(* ----- Edge plan computation ----- *)

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

(* Schedule parallel moves, detecting cycles and inserting scratch-slot usage.
   Returns (ordered_moves, scratch_slot_option). *)
let schedule_parallel_moves (moves : move_step list) =
  let local_moves, const_moves =
    List.partition (fun ({ src; _ } : move_step) -> match src with SlotSrc _ -> true | OperandSrc _ -> false) moves
  in
  let move_sources pending =
    List.fold_left
      (fun acc ({ src; _ } : move_step) -> match src with SlotSrc s -> IntSet.add s acc | OperandSrc _ -> acc)
      IntSet.empty pending
  in
  (* Check if we need a scratch slot *)
  let move_graph =
    List.fold_left
      (fun g ({ src; dst } : move_step) -> match src with SlotSrc s -> add_move_edge s dst g | OperandSrc _ -> g)
      IntMap.empty local_moves
  in
  let has_cycle = directed_graph_has_cycle move_graph in
  let all_slots =
    List.fold_left
      (fun acc ({ src; dst } : move_step) ->
        let acc = IntSet.add dst acc in
        match src with SlotSrc s -> IntSet.add s acc | OperandSrc _ -> acc)
      IntSet.empty moves
  in
  let scratch_slot =
    if has_cycle then
      let rec find s = if IntSet.mem s all_slots then find (s + 1) else s in
      Some (find 0)
    else None
  in
  let rewrite_source from_slot to_slot ({ src; _ } as move : move_step) =
    match src with SlotSrc s when s = from_slot -> { move with src = SlotSrc to_slot } | _ -> move
  in
  let rec loop pending acc =
    match pending with
    | [] -> List.rev_append acc const_moves
    | _ -> (
        let sources = move_sources pending in
        let ready, blocked = List.partition (fun ({ dst; _ } : move_step) -> not (IntSet.mem dst sources)) pending in
        if ready <> [] then loop blocked (List.rev_append ready acc)
        else
          match (scratch_slot, pending) with
          | Some scratch, ({ src = SlotSrc src_slot; _ } : move_step) :: _ ->
              let pending = List.map (rewrite_source src_slot scratch) pending |> List.sort_uniq Stdlib.compare in
              loop pending ({ src = SlotSrc src_slot; dst = scratch } :: acc)
          | _ -> List.rev_append acc (List.rev pending))
  in
  let ordered = loop local_moves [] in
  (ordered, scratch_slot)

(* Compute the edge plan between two blocks for a set of value transfers.
   `transfers` is a list of (value_id, src_slot_in_src_layout, dst_slot_in_dst_layout). *)
let compute_edge_plan_for_transfers (src_layout : block_layout) (dst_layout : block_layout)
    (value_transfers : (int * slot * slot) list) (operand_transfers : (operand * slot) list) =
  (* Check for Elide: same frame_size and all value transfers are identity *)
  let all_identity =
    src_layout.frame_size = dst_layout.frame_size
    && List.for_all (fun (_, src_s, dst_s) -> src_s = dst_s) value_transfers
  in
  if all_identity && operand_transfers = [] then Elide
  else if src_layout.frame_size = dst_layout.frame_size then
    (* InPlace: same frame size, build parallel copy *)
    let moves =
      List.filter_map
        (fun (_, src_s, dst_s) -> if src_s = dst_s then None else Some { src = SlotSrc src_s; dst = dst_s })
        value_transfers
      @ List.map (fun (op, dst_s) -> { src = OperandSrc op; dst = dst_s }) operand_transfers
    in
    match moves with
    | [] -> Elide
    | _ ->
        let ordered, scratch = schedule_parallel_moves moves in
        InPlace { moves = ordered; scratch }
  else Reshape dst_layout.frame_size

(* Compute edge plan for a Jump edge *)
let compute_jump_edge_plan (unit_ir : unit_ir) (liveness : CfgLiveness.unit_liveness) (layouts : block_layout IntMap.t)
    (block : block) (succ_id : block_id) (args : operand list) =
  let src_layout =
    match IntMap.find_opt block.id layouts with Some l -> l | None -> failf "missing layout b%d" block.id
  in
  let dst_layout =
    match IntMap.find_opt succ_id layouts with Some l -> l | None -> failf "missing layout b%d" succ_id
  in
  let succ =
    match List.find_opt (fun (b : block) -> b.id = succ_id) unit_ir.blocks with
    | Some b -> b
    | None -> failf "missing block b%d" succ_id
  in
  let block_info =
    match IntMap.find_opt block.id liveness.blocks with Some i -> i | None -> failf "missing liveness b%d" block.id
  in
  let dst_info =
    match IntMap.find_opt succ_id liveness.blocks with Some i -> i | None -> failf "missing liveness b%d" succ_id
  in
  let edge_live =
    match IntMap.find_opt succ_id block_info.live_on_edge with
    | Some i -> i
    | None -> failf "missing edge liveness b%d->b%d" block.id succ_id
  in
  let param_ids = intset_of_list succ.params in
  (* Value transfers: live-in values that are not params *)
  let value_transfers =
    IntSet.diff dst_info.live_in param_ids |> IntSet.elements
    |> List.map (fun vid ->
        let src_s =
          match IntMap.find_opt vid src_layout.slot_of_value with Some s -> s | None -> failf "missing v%d src" vid
        in
        let dst_s =
          match IntMap.find_opt vid dst_layout.slot_of_value with Some s -> s | None -> failf "missing v%d dst" vid
        in
        (vid, src_s, dst_s))
  in
  (* Param transfers *)
  let param_value_transfers = ref [] in
  let param_operand_transfers = ref [] in
  (try
     List.iter2
       (fun param_id arg ->
         if IntMap.mem param_id edge_live.live_params then
           let dst_s =
             match IntMap.find_opt param_id dst_layout.slot_of_value with
             | Some s -> s
             | None -> failf "missing param v%d dst" param_id
           in
           match arg with
           | OLocal vid ->
               let src_s =
                 match IntMap.find_opt vid src_layout.slot_of_value with
                 | Some s -> s
                 | None -> failf "missing v%d src" vid
               in
               param_value_transfers := (vid, src_s, dst_s) :: !param_value_transfers
           | op -> param_operand_transfers := (op, dst_s) :: !param_operand_transfers)
       succ.params args
   with Invalid_argument _ -> failf "jump arg count mismatch b%d" succ_id);
  let all_value_transfers = value_transfers @ List.rev !param_value_transfers in
  let all_operand_transfers = List.rev !param_operand_transfers in
  compute_edge_plan_for_transfers src_layout dst_layout all_value_transfers all_operand_transfers

(* Compute edge plan for a Branch/Match edge (no params, only live-in transfer) *)
let compute_branch_edge_plan (liveness : CfgLiveness.unit_liveness) (layouts : block_layout IntMap.t)
    (src_block_id : block_id) (target_id : block_id) =
  let src_layout =
    match IntMap.find_opt src_block_id layouts with Some l -> l | None -> failf "missing layout b%d" src_block_id
  in
  let dst_layout =
    match IntMap.find_opt target_id layouts with Some l -> l | None -> failf "missing layout b%d" target_id
  in
  let dst_info =
    match IntMap.find_opt target_id liveness.blocks with Some i -> i | None -> failf "missing liveness b%d" target_id
  in
  let value_transfers =
    IntSet.elements dst_info.live_in
    |> List.map (fun vid ->
        let src_s =
          match IntMap.find_opt vid src_layout.slot_of_value with Some s -> s | None -> failf "missing v%d src" vid
        in
        let dst_s =
          match IntMap.find_opt vid dst_layout.slot_of_value with Some s -> s | None -> failf "missing v%d dst" vid
        in
        (vid, src_s, dst_s))
  in
  compute_edge_plan_for_transfers src_layout dst_layout value_transfers []

let compute_all_edge_plans (unit_ir : unit_ir) (liveness : CfgLiveness.unit_liveness) (layouts : block_layout IntMap.t)
    =
  List.fold_left
    (fun plans (block : block) ->
      let block_plans =
        match block.term with
        | Return _ -> IntMap.empty
        | Jump (succ_id, args) ->
            let plan = compute_jump_edge_plan unit_ir liveness layouts block succ_id args in
            IntMap.singleton succ_id plan
        | Branch (_, then_id, else_id) ->
            let then_plan = compute_branch_edge_plan liveness layouts block.id then_id in
            let else_plan = compute_branch_edge_plan liveness layouts block.id else_id in
            IntMap.add then_id then_plan (IntMap.singleton else_id else_plan)
        | Match (_, arms) ->
            List.fold_left
              (fun acc ({ block = succ_id; _ } : match_arm) ->
                (* Match edges have extra assigns handled by codegen, so plan as Reshape *)
                let dst_layout =
                  match IntMap.find_opt succ_id layouts with Some l -> l | None -> failf "missing layout b%d" succ_id
                in
                IntMap.add succ_id (Reshape dst_layout.frame_size) acc)
              IntMap.empty arms
      in
      IntMap.add block.id block_plans plans)
    IntMap.empty unit_ir.blocks

(* ----- Main entry point ----- *)

let allocate_unit_slots (unit_ir : unit_ir) (liveness : CfgLiveness.unit_liveness) : unit_allocation =
  let value_map = List.fold_left (fun acc (vi : value_info) -> IntMap.add vi.id vi acc) IntMap.empty unit_ir.values in
  let rpo = compute_rpo unit_ir in
  (* Allocate blocks in RPO, collecting affinities from already-coloured blocks *)
  let block_layouts =
    List.fold_left
      (fun coloured block_id ->
        let block =
          match List.find_opt (fun (b : block) -> b.id = block_id) unit_ir.blocks with
          | Some b -> b
          | None -> failf "RegAlloc: missing block b%d" block_id
        in
        let info =
          match IntMap.find_opt block.id liveness.blocks with
          | Some i -> i
          | None -> failf "RegAlloc: missing liveness for block b%d" block.id
        in
        let residents = block_resident_values block info in
        let allocatable_residents =
          IntSet.filter
            (fun id -> match IntMap.find_opt id value_map with Some vi -> allocatable_value vi | None -> false)
            residents
        in
        let affinities = collect_affinities unit_ir liveness coloured block info allocatable_residents in
        let layout = allocate_block_with_affinities value_map block info affinities in
        IntMap.add block.id layout coloured)
      IntMap.empty rpo
  in
  let edge_plans = compute_all_edge_plans unit_ir liveness block_layouts in
  { block_layouts; edge_plans }
