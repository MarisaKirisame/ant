module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)

type edge = int * int
type heuristic = Degree_sum | Degree_product

let vertices_of_edges edges =
  List.fold_left (fun vertices (src, dst) -> vertices |> IntSet.add src |> IntSet.add dst) IntSet.empty edges

let edge_is_active removed (src, dst) = (not (IntSet.mem src removed)) && not (IntSet.mem dst removed)
let active_edges removed edges = List.filter (edge_is_active removed) edges

let add_vertex vertex adjacency =
  if IntMap.mem vertex adjacency then adjacency else IntMap.add vertex IntSet.empty adjacency

let add_edge adjacency (src, dst) =
  let adjacency = adjacency |> add_vertex src |> add_vertex dst in
  let out = IntMap.find src adjacency in
  IntMap.add src (IntSet.add dst out) adjacency

let adjacency_of_edges edges = List.fold_left add_edge IntMap.empty edges

type color = White | Gray | Black

let color_of vertex colors = Option.value (IntMap.find_opt vertex colors) ~default:White

let take_cycle target stack =
  let rec aux acc = function
    | [] -> List.rev acc
    | vertex :: rest ->
        let acc = vertex :: acc in
        if vertex = target then List.rev acc else aux acc rest
  in
  aux [] stack

let find_directed_cycle edges =
  let adjacency = adjacency_of_edges edges in
  let vertices = vertices_of_edges edges in
  let colors = ref IntMap.empty in
  let exception Found_cycle of int list in
  let rec dfs stack vertex =
    colors := IntMap.add vertex Gray !colors;
    let successors = Option.value (IntMap.find_opt vertex adjacency) ~default:IntSet.empty in
    IntSet.iter
      (fun successor ->
        match color_of successor !colors with
        | White -> dfs (successor :: stack) successor
        | Gray -> raise (Found_cycle (take_cycle successor stack))
        | Black -> ())
      successors;
    colors := IntMap.add vertex Black !colors
  in
  try
    IntSet.iter (fun vertex -> if color_of vertex !colors = White then dfs [ vertex ] vertex) vertices;
    None
  with Found_cycle cycle -> Some cycle

let degrees edges =
  List.fold_left
    (fun (indegrees, outdegrees) (src, dst) ->
      let bump vertex degrees =
        let current = Option.value (IntMap.find_opt vertex degrees) ~default:0 in
        IntMap.add vertex (current + 1) degrees
      in
      (bump dst indegrees, bump src outdegrees))
    (IntMap.empty, IntMap.empty) edges

let degree_score heuristic edges =
  let indegrees, outdegrees = degrees edges in
  fun vertex ->
    let indegree = Option.value (IntMap.find_opt vertex indegrees) ~default:0 in
    let outdegree = Option.value (IntMap.find_opt vertex outdegrees) ~default:0 in
    match heuristic with Degree_sum -> indegree + outdegree | Degree_product -> indegree * outdegree

let choose_vertex heuristic edges cycle =
  let score = degree_score heuristic edges in
  let better left right =
    let left_score = score left in
    let right_score = score right in
    left_score > right_score || (left_score = right_score && left < right)
  in
  match cycle with
  | [] -> invalid_arg "Mfvs.choose_vertex: empty cycle"
  | vertex :: rest -> List.fold_left (fun best vertex -> if better vertex best then vertex else best) vertex rest

let solve_with heuristic edges =
  let rec aux removed selected =
    let edges = active_edges removed edges in
    match find_directed_cycle edges with
    | None -> List.rev selected
    | Some cycle ->
        let vertex = choose_vertex heuristic edges cycle in
        aux (IntSet.add vertex removed) (vertex :: selected)
  in
  aux IntSet.empty []

let solve edges =
  let by_sum = solve_with Degree_sum edges in
  let by_product = solve_with Degree_product edges in
  if List.length by_sum <= List.length by_product then by_sum else by_product
