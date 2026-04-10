open ControlFlowGraph

(* Per-block register allocation with cross-block slot coalescing.

   Phase 1 — per-block layout with entry/working split:
     Each block has a canonical entry layout (live_in + live_params, densely packed
     in low slots) and a working frame that may grow to hold body-defined values.

   Phase 2 — coalescing: edges impose slot *affinities* between values that flow
   across blocks.  When colouring a block we try to honour affinities from
   already-coloured neighbours, but never override interference constraints.

   Phase 3 — edge planning: after all blocks are coloured, each CFG edge is
   classified as Elide (identical layout) or Shuffle (declarative frame_source
   mapping targeting the successor's canonical entry layout). *)

let failf fmt = Printf.ksprintf failwith fmt

type slot = int

type block_layout = {
  slot_of_value : slot IntMap.t;
  param_slots : slot list;
  entry_size : int; (* canonical entry frame: live_in + live_params only *)
  frame_size : int; (* working frame: all values including body-defined *)
}

(* Frame reshaping descriptor — each target slot describes its data source. *)
type frame_source =
  | FromSlot of slot (* value copied from old frame slot *)
  | FromOperand of operand (* fresh value from jump arg or constant *)
  | FromBinder of value_id (* match binder: value injected by codegen *)
  | Unset (* intentionally undefined / filled with default *)

(* Edge transition plan *)
type edge_plan =
  | Elide (* source frame already matches target canonical entry *)
  | Shuffle of { mapping : frame_source array }
(* target-slot-indexed mapping *)

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
  (* Include values defined in inlined arms of the terminator. Inline regions
     execute within this block's memo step and their locals must live in this
     block's working frame. *)
  let inline_defined =
    List.fold_left
      (fun acc stmt -> match stmt with Bind (id, _) -> IntSet.add id acc)
      IntSet.empty (collect_inline_stmts block.term)
  in
  (* Match-arm binders from inlined arms are also allocated in this block. *)
  let inline_binders = inlined_arm_binders block.term in
  IntSet.union (IntSet.union (IntSet.union (IntSet.union entry_live all_params) defined) inline_defined) inline_binders

(* Compute external successors' live_in for the terminator (what must be live
   when control leaves the block toward an external block).  For inlined arms,
   this is the live_in of the arm's continuation block (walked transitively
   through inline regions). *)
let rec inline_region_entry_live ~(arm_live_in : block_id -> IntSet.t) (region : inline_region) : IntSet.t =
  let live_out = terminator_live_out ~arm_live_in region.term in
  List.fold_right transfer_stmt region.body live_out

and terminator_live_out ~(arm_live_in : block_id -> IntSet.t) (term : terminator) : IntSet.t =
  match term with
  | Return operand -> add_operand_uses IntSet.empty operand
  | Jump (succ_id, _) -> arm_live_in succ_id
  | Branch (cond, t, e) ->
      let then_live =
        match t.inline with None -> arm_live_in t.block | Some r -> inline_region_entry_live ~arm_live_in r
      in
      let else_live =
        match e.inline with None -> arm_live_in e.block | Some r -> inline_region_entry_live ~arm_live_in r
      in
      add_operand_uses (IntSet.union then_live else_live) cond
  | Match (scrutinee, arms) ->
      let live =
        List.fold_left
          (fun acc (arm : match_arm) ->
            let arm_live =
              match arm.inline with None -> arm_live_in arm.block | Some r -> inline_region_entry_live ~arm_live_in r
            in
            IntSet.union acc arm_live)
          IntSet.empty arms
      in
      add_operand_uses live scrutinee

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
  (* Add interference edges for values that live inside inlined arms: their
     match binders and body-defined values. We over-approximate by treating
     them as a single clique that remains live with the parent's
     [live_before_term]; values in different arms may therefore end up
     interfering with each other, but this is safe and keeps the allocator
     simple. *)
  let inline_stmts = collect_inline_stmts block.term in
  let inline_defs = List.fold_left (fun acc (Bind (id, _)) -> IntSet.add id acc) IntSet.empty inline_stmts in
  let binders = inlined_arm_binders block.term in
  let inline_vals = IntSet.union inline_defs binders in
  let graph =
    if IntSet.is_empty inline_vals then graph
    else add_resident_clique (IntSet.union info.live_before_term inline_vals) graph
  in
  let graph =
    List.fold_left
      (fun graph (Bind (id, rhs)) ->
        let uses = add_rhs_uses IntSet.empty rhs in
        let live = IntSet.union info.live_before_term (IntSet.add id uses) in
        add_resident_clique live graph)
      graph inline_stmts
  in
  graph

(* ----- Affinity computation ----- *)

type affinity = { value_id : int; preferred_slot : slot }

let collect_affinities (unit_ir : unit_ir) (liveness : CfgLiveness.unit_liveness) (coloured : block_layout IntMap.t)
    (block : block) (_info : CfgLiveness.block_liveness) (residents : IntSet.t) : affinity list =
  let block_map = List.fold_left (fun acc (b : block) -> IntMap.add b.id b acc) IntMap.empty unit_ir.blocks in
  let affinities = ref [] in
  let add_affinity vid slot = affinities := { value_id = vid; preferred_slot = slot } :: !affinities in
  let succs = terminator_external_succs block.term in
  List.iter
    (fun succ_id ->
      match IntMap.find_opt succ_id coloured with
      | None -> ()
      | Some (succ_layout : block_layout) -> (
          let succ_info = match IntMap.find_opt succ_id liveness.blocks with Some i -> i | None -> assert false in
          IntSet.iter
            (fun vid ->
              if IntSet.mem vid residents then
                match IntMap.find_opt vid succ_layout.slot_of_value with Some s -> add_affinity vid s | None -> ())
            succ_info.live_in;
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
  List.iter
    (fun (pred : block) ->
      match IntMap.find_opt pred.id coloured with
      | None -> ()
      | Some (pred_layout : block_layout) ->
          IntMap.iter (fun vid slot -> if IntSet.mem vid residents then add_affinity vid slot) pred_layout.slot_of_value)
    (List.filter (fun (b : block) -> List.mem block.id (terminator_external_succs b.term)) unit_ir.blocks);
  !affinities

(* ----- Two-phase allocation ----- *)

(* Greedy graph colouring: assign lowest free slot, honouring soft affinity preferences. *)
let colour_values (value_infos : value_info list) (interference : IntSet.t IntMap.t) (affinities : affinity list)
    (existing : slot IntMap.t) =
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
      let slot =
        match IntMap.find_opt info.id preferred with
        | Some pref when not (IntSet.mem pref used_slots) -> pref
        | _ ->
            let rec pick slot = if IntSet.mem slot used_slots then pick (slot + 1) else slot in
            pick 0
      in
      IntMap.add info.id slot slot_of_value)
    existing sorted

let allocate_block_with_affinities (value_map : value_info IntMap.t) (block : block) (info : CfgLiveness.block_liveness)
    (affinities : affinity list) : block_layout =
  let residents = block_resident_values block info in
  let allocatable_residents =
    IntSet.filter
      (fun id -> match IntMap.find_opt id value_map with Some vi -> allocatable_value vi | None -> false)
      residents
  in
  let interference = build_block_interference block info allocatable_residents in
  (* Partition values into entry (live at block entry) and body-only. *)
  let entry_values =
    match block.kind with
    | Entry -> IntSet.union info.live_in (intset_of_list block.params)
    | Join _ | IfThen | IfElse | MatchArm -> IntSet.union info.live_in info.live_params
  in
  let entry_ids = IntSet.inter entry_values allocatable_residents in
  let body_ids = IntSet.diff allocatable_residents entry_ids in
  let entry_infos = IntSet.elements entry_ids |> List.filter_map (fun id -> IntMap.find_opt id value_map) in
  let body_infos = IntSet.elements body_ids |> List.filter_map (fun id -> IntMap.find_opt id value_map) in
  (* Phase 1: colour entry values first — they get dense low slots. *)
  let slot_of_value = colour_values entry_infos interference affinities IntMap.empty in
  let entry_size = IntMap.fold (fun _ slot acc -> Int.max acc (slot + 1)) slot_of_value 0 in
  (* Phase 2: colour body-only values — may reuse dead entry slots or extend. *)
  let slot_of_value = colour_values body_infos interference affinities slot_of_value in
  let frame_size = IntMap.fold (fun _ slot acc -> Int.max acc (slot + 1)) slot_of_value 0 in
  let param_slots =
    List.map
      (fun param_id ->
        match IntMap.find_opt param_id slot_of_value with
        | Some slot -> slot
        | None -> failf "RegAlloc: missing slot for block parameter v%d in b%d" param_id block.id)
      block.params
  in
  { slot_of_value; param_slots; entry_size; frame_size }

(* ----- Reverse-postorder traversal ----- *)

let compute_rpo (unit_ir : unit_ir) =
  let block_map = List.fold_left (fun acc (b : block) -> IntMap.add b.id b acc) IntMap.empty unit_ir.blocks in
  let visited = ref IntSet.empty in
  let order = ref [] in
  let rec visit id =
    if not (IntSet.mem id !visited) then (
      visited := IntSet.add id !visited;
      let block = match IntMap.find_opt id block_map with Some b -> b | None -> assert false in
      List.iter visit (terminator_external_succs block.term);
      order := id :: !order)
  in
  visit unit_ir.entry;
  List.iter (fun (b : block) -> visit b.id) unit_ir.blocks;
  !order

(* ----- Shuffle-based edge plan computation ----- *)

(* Build reverse map: slot → value_id for entry values only. *)
let reverse_entry_slots (layout : block_layout) (entry_values : IntSet.t) =
  IntMap.fold
    (fun vid slot acc -> if IntSet.mem vid entry_values then IntMap.add slot vid acc else acc)
    layout.slot_of_value IntMap.empty

(* Check whether a mapping is an identity (elide). *)
let is_elide_mapping src_frame_size (mapping : frame_source array) =
  src_frame_size = Array.length mapping
  &&
  let rec check i = i >= Array.length mapping || (mapping.(i) = FromSlot i && check (i + 1)) in
  check 0

(* Compute shuffle mapping for a Jump edge. *)
let compute_jump_shuffle (unit_ir : unit_ir) (liveness : CfgLiveness.unit_liveness) (layouts : block_layout IntMap.t)
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
  (* Build reverse map of target entry slots *)
  let dst_entry_values = IntSet.union dst_info.live_in dst_info.live_params in
  let dst_reverse = reverse_entry_slots dst_layout dst_entry_values in
  (* Build param index map: param_id → position *)
  let param_index =
    List.mapi (fun i pid -> (pid, i)) succ.params
    |> List.fold_left (fun acc (pid, i) -> IntMap.add pid i acc) IntMap.empty
  in
  let mapping =
    Array.init dst_layout.entry_size (fun slot ->
        match IntMap.find_opt slot dst_reverse with
        | None -> Unset
        | Some vid ->
            if IntSet.mem vid dst_info.live_in && not (IntSet.mem vid param_ids) then
              (* Live-through value *)
              match IntMap.find_opt vid src_layout.slot_of_value with
              | Some s -> FromSlot s
              | None -> failf "missing v%d in src layout" vid
            else if IntSet.mem vid param_ids && IntMap.mem vid edge_live.live_params then
              (* Live param — find corresponding arg *)
              let idx =
                match IntMap.find_opt vid param_index with Some i -> i | None -> failf "missing param index v%d" vid
              in
              let arg = List.nth args idx in
              match arg with
              | OLocal arg_vid -> (
                  match IntMap.find_opt arg_vid src_layout.slot_of_value with
                  | Some s -> FromSlot s
                  | None -> failf "missing v%d in src layout" arg_vid)
              | op -> FromOperand op
            else Unset)
  in
  if is_elide_mapping src_layout.frame_size mapping then Elide else Shuffle { mapping }

(* Compute shuffle mapping for a Branch/Match-without-binders edge. *)
let compute_branch_shuffle (liveness : CfgLiveness.unit_liveness) (layouts : block_layout IntMap.t)
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
  let dst_entry_values = IntSet.union dst_info.live_in dst_info.live_params in
  let dst_reverse = reverse_entry_slots dst_layout dst_entry_values in
  let mapping =
    Array.init dst_layout.entry_size (fun slot ->
        match IntMap.find_opt slot dst_reverse with
        | None -> Unset
        | Some vid -> (
            match IntMap.find_opt vid src_layout.slot_of_value with
            | Some s -> FromSlot s
            | None -> failf "missing v%d in src layout" vid))
  in
  if is_elide_mapping src_layout.frame_size mapping then Elide else Shuffle { mapping }

(* Compute shuffle mapping for a Match arm edge (may have binder values). *)
let compute_match_arm_shuffle (liveness : CfgLiveness.unit_liveness) (layouts : block_layout IntMap.t)
    (src_block_id : block_id) (succ_id : block_id) (succ_block : block) =
  let src_layout =
    match IntMap.find_opt src_block_id layouts with Some l -> l | None -> failf "missing layout b%d" src_block_id
  in
  let dst_layout =
    match IntMap.find_opt succ_id layouts with Some l -> l | None -> failf "missing layout b%d" succ_id
  in
  let dst_info =
    match IntMap.find_opt succ_id liveness.blocks with Some i -> i | None -> failf "missing liveness b%d" succ_id
  in
  let dst_entry_values = IntSet.union dst_info.live_in dst_info.live_params in
  let dst_reverse = reverse_entry_slots dst_layout dst_entry_values in
  let param_ids = intset_of_list succ_block.params in
  let mapping =
    Array.init dst_layout.entry_size (fun slot ->
        match IntMap.find_opt slot dst_reverse with
        | None -> Unset
        | Some vid ->
            if IntSet.mem vid dst_info.live_in && not (IntSet.mem vid param_ids) then
              (* Live-through value from source *)
              match IntMap.find_opt vid src_layout.slot_of_value with
              | Some s -> FromSlot s
              | None -> failf "missing v%d in src layout" vid
            else if IntSet.mem vid param_ids then
              (* Match binder param: value injected by codegen *)
              FromBinder vid
            else Unset)
  in
  if is_elide_mapping src_layout.frame_size mapping then Elide else Shuffle { mapping }

(* Compute edge plans for a terminator, including edges originating from
   inlined arms' terminators (which use the parent block's layout). Inlined
   arms themselves do not receive incoming edge plans, because they do not
   cross a memo boundary. *)
let rec collect_term_edge_plans ~(unit_ir : unit_ir) ~(liveness : CfgLiveness.unit_liveness)
    ~(layouts : block_layout IntMap.t) ~(block_map : block IntMap.t) ~(src_block : block) (term : terminator) plans =
  match term with
  | Return _ -> plans
  | Jump (succ_id, args) ->
      let plan = compute_jump_shuffle unit_ir liveness layouts src_block succ_id args in
      IntMap.add succ_id plan plans
  | Branch (_, t, e) ->
      let plans = collect_branch_target_plans ~unit_ir ~liveness ~layouts ~block_map ~src_block t plans in
      collect_branch_target_plans ~unit_ir ~liveness ~layouts ~block_map ~src_block e plans
  | Match (_, arms) ->
      List.fold_left
        (fun plans arm -> collect_match_arm_plans ~unit_ir ~liveness ~layouts ~block_map ~src_block arm plans)
        plans arms

and collect_branch_target_plans ~unit_ir ~liveness ~layouts ~block_map ~src_block (tgt : branch_target) plans =
  match tgt.inline with
  | None ->
      let plan = compute_branch_shuffle liveness layouts src_block.id tgt.block in
      IntMap.add tgt.block plan plans
  | Some region -> collect_term_edge_plans ~unit_ir ~liveness ~layouts ~block_map ~src_block region.term plans

and collect_match_arm_plans ~unit_ir ~liveness ~layouts ~block_map ~src_block (arm : match_arm) plans =
  match arm.inline with
  | None ->
      let succ_block =
        match IntMap.find_opt arm.block block_map with Some b -> b | None -> failf "missing block b%d" arm.block
      in
      let plan = compute_match_arm_shuffle liveness layouts src_block.id arm.block succ_block in
      IntMap.add arm.block plan plans
  | Some region -> collect_term_edge_plans ~unit_ir ~liveness ~layouts ~block_map ~src_block region.term plans

let compute_all_edge_plans (unit_ir : unit_ir) (liveness : CfgLiveness.unit_liveness) (layouts : block_layout IntMap.t)
    =
  let block_map = List.fold_left (fun acc (b : block) -> IntMap.add b.id b acc) IntMap.empty unit_ir.blocks in
  List.fold_left
    (fun plans (block : block) ->
      let block_plans =
        collect_term_edge_plans ~unit_ir ~liveness ~layouts ~block_map ~src_block:block block.term IntMap.empty
      in
      IntMap.add block.id block_plans plans)
    IntMap.empty unit_ir.blocks

(* ----- Main entry point ----- *)

let allocate_unit_slots (unit_ir : unit_ir) (liveness : CfgLiveness.unit_liveness) : unit_allocation =
  let value_map = List.fold_left (fun acc (vi : value_info) -> IntMap.add vi.id vi acc) IntMap.empty unit_ir.values in
  let rpo = compute_rpo unit_ir in
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
