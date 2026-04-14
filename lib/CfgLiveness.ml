module IntSet = Set.Make (Int)
module IntMap = Map.Make (Int)

type edge_liveness = { live_in : IntSet.t; live_params : IntSet.t IntMap.t }

type block_liveness = {
  live_in : IntSet.t;
  live_out : IntSet.t;
  live_before_term : IntSet.t;
  live_params : IntSet.t;
  live_on_edge : edge_liveness IntMap.t;
}

type unit_liveness = { blocks : block_liveness IntMap.t }
type 'prog prog_analysis = { ir : 'prog; liveness : unit_liveness list }

let entry_live (info : block_liveness) = IntSet.union info.live_in info.live_params

let edge_live_values (info : edge_liveness) =
  IntMap.fold (fun _ live acc -> IntSet.union live acc) info.live_params info.live_in

let empty_edge_liveness = { live_in = IntSet.empty; live_params = IntMap.empty }

let empty_block_liveness =
  {
    live_in = IntSet.empty;
    live_out = IntSet.empty;
    live_before_term = IntSet.empty;
    live_params = IntSet.empty;
    live_on_edge = IntMap.empty;
  }

let intset_of_list values = List.fold_left (fun acc value -> IntSet.add value acc) IntSet.empty values

let find_exn thing id table =
  match IntMap.find_opt id table with
  | Some value -> value
  | None -> failwith (Printf.sprintf "CFGLiveness: unknown %s %d" thing id)

let partition_entry_live entry_live block_params =
  (IntSet.diff entry_live block_params, IntSet.inter entry_live block_params)

let equal_edge_liveness (old_info : edge_liveness) (new_info : edge_liveness) =
  IntSet.equal old_info.live_in new_info.live_in && IntMap.equal IntSet.equal old_info.live_params new_info.live_params

let equal_block_liveness old_info new_info =
  IntSet.equal old_info.live_in new_info.live_in
  && IntSet.equal old_info.live_out new_info.live_out
  && IntSet.equal old_info.live_before_term new_info.live_before_term
  && IntSet.equal old_info.live_params new_info.live_params
  && IntMap.equal equal_edge_liveness old_info.live_on_edge new_info.live_on_edge

let check_monotone_param_liveness block_id succ_id old_params new_params =
  IntMap.iter
    (fun param_id old_live ->
      let new_live =
        match IntMap.find_opt param_id new_params with
        | Some live -> live
        | None ->
            failwith
              (Printf.sprintf "CfgLiveness: non-monotone update in block %d, edge to block %d lost param %d" block_id
                 succ_id param_id)
      in
      if not (IntSet.subset old_live new_live) then
        failwith
          (Printf.sprintf "CfgLiveness: non-monotone update in block %d, edge to block %d shrank param %d" block_id
             succ_id param_id))
    old_params

let check_monotone_edge_liveness block_id old_edges new_edges =
  IntMap.iter
    (fun succ_id (old_info : edge_liveness) ->
      let new_info : edge_liveness =
        match IntMap.find_opt succ_id new_edges with
        | Some info -> info
        | None ->
            failwith
              (Printf.sprintf "CfgLiveness: non-monotone update in block %d, edge to block %d disappeared" block_id
                 succ_id)
      in
      if not (IntSet.subset old_info.live_in new_info.live_in) then
        failwith
          (Printf.sprintf "CfgLiveness: non-monotone update in block %d, edge to block %d shrank live_in" block_id
             succ_id);
      check_monotone_param_liveness block_id succ_id old_info.live_params new_info.live_params)
    old_edges

let check_monotone_block_update block_id old_info new_info =
  if
    not
      (IntSet.subset old_info.live_in new_info.live_in
      && IntSet.subset old_info.live_out new_info.live_out
      && IntSet.subset old_info.live_before_term new_info.live_before_term
      && IntSet.subset old_info.live_params new_info.live_params)
  then failwith (Printf.sprintf "CfgLiveness: non-monotone update in block %d" block_id);
  check_monotone_edge_liveness block_id old_info.live_on_edge new_info.live_on_edge

let compute_postorder ~find_block ~blocks ~entry_id ~block_id ~successors =
  let visited = ref IntSet.empty in
  let order = ref [] in
  let rec visit id =
    if not (IntSet.mem id !visited) then (
      visited := IntSet.add id !visited;
      List.iter visit (successors (find_block id));
      order := find_block id :: !order)
  in
  visit entry_id;
  List.iter (fun block -> visit (block_id block)) blocks;
  List.rev !order

let analyze_unit ~blocks ~entry_id ~successors ~block_id ~params ~body ~transfer_stmt ~analyze_term =
  let block_map = List.fold_left (fun acc block -> IntMap.add (block_id block) block acc) IntMap.empty blocks in
  let initial =
    List.fold_left (fun acc block -> IntMap.add (block_id block) empty_block_liveness acc) IntMap.empty blocks
  in
  let find_block id = find_exn "block" id block_map in
  let analysis_order = compute_postorder ~find_block ~blocks ~entry_id ~block_id ~successors in
  let rec fixpoint infos =
    let changed = ref false in
    let current_infos = ref infos in
    List.iter
      (fun block ->
        let find_live id = find_exn "liveness block" id !current_infos in
        let live_on_edge, live_out, live_before_term = analyze_term ~find_block ~find_live block in
        let entry_live = List.fold_right transfer_stmt (body block) live_before_term in
        let block_params = intset_of_list (params block) in
        let live_in, live_params = partition_entry_live entry_live block_params in
        let current_block_id = block_id block in
        let info = { live_in; live_out; live_before_term; live_params; live_on_edge } in
        let old_info = find_live current_block_id in
        check_monotone_block_update current_block_id old_info info;
        if not (equal_block_liveness old_info info) then (
          changed := true;
          current_infos := IntMap.add current_block_id info !current_infos))
      analysis_order;
    if !changed then fixpoint !current_infos else { blocks = !current_infos }
  in
  fixpoint initial

let analyze_prog ~units ~analyze_unit ir = { ir; liveness = List.map analyze_unit units }
