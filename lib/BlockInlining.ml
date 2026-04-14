(* BlockInlining — trivial block inlining and dispatch inlining for the
   reg-memo backend.

   This module runs between [AnfToCfg.lower_prog] and the liveness / register
   allocation passes. It reduces the number of memo-visible PCs and the
   number of cross-block frame reshapes by fusing tiny successor blocks into
   their predecessors, while preserving semantically-important memo boundaries
   (function entries, join blocks, loop headers, the eval-style dispatchers).

   Two passes:

   - Trivial block inlining: when a block A ends with an unconditional
     [Jump B(args)] and B is small and safely inlineable, we physically merge
     A and B into a single block. B's body becomes part of A's body (with its
     params bound from A's jump arguments via Move statements), and A's
     terminator is replaced with B's. If B has multiple predecessors we allow
     bounded copying within a strict cost budget.

   - Dispatch inlining: when a block A dispatches via [Match] or [Branch] to a
     small, single-predecessor arm block B, we record B's body and terminator
     as an inline region on the dispatcher's arm. B is removed from the unit's
     block list: its work executes inside A's memo step without crossing a
     boundary. Match-pattern binders become locals in A's working frame. *)

open ControlFlowGraph
module IntSet = CfgLiveness.IntSet
module IntMap = CfgLiveness.IntMap

let failf fmt = Printf.ksprintf failwith fmt

(* Cost budgets. A block's cost is roughly the number of statements it carries
   plus a small constant for the terminator. Copying is allowed into multiple
   predecessors only when the total extra cost stays under these limits. *)
let trivial_inline_stmt_limit = 6
let trivial_inline_copy_total_extra = 6
let dispatch_inline_stmt_limit = 8

(* ----- Shared helpers ----- *)

let block_by_id (unit_ir : unit_ir) : block IntMap.t =
  List.fold_left (fun acc (b : block) -> IntMap.add b.id b acc) IntMap.empty unit_ir.blocks

let value_by_id (unit_ir : unit_ir) : value_info IntMap.t =
  List.fold_left (fun acc (vi : value_info) -> IntMap.add vi.id vi acc) IntMap.empty unit_ir.values

(* Conservative cost estimate for a block: statement count + small term cost. *)
let block_cost (b : block) : int =
  let term_cost =
    match b.term with Return _ | Jump _ -> 1 | Branch _ -> 2 | Match (_, arms) -> 1 + List.length arms
  in
  List.length b.body + term_cost

(* Collect the ids of blocks referenced by a terminator's external edges
   (i.e. non-inlined successors). *)
let term_outgoing_block_ids (term : terminator) : IntSet.t =
  terminator_external_succs term |> List.fold_left (fun s id -> IntSet.add id s) IntSet.empty

(* Set of block ids referenced by incoming edges across the unit. Includes
   edges from inlined arms' terminators, because those eventually dispatch
   back into real blocks. *)
let compute_referenced_ids (unit_ir : unit_ir) : IntSet.t =
  let acc = ref IntSet.empty in
  List.iter
    (fun (b : block) -> IntSet.iter (fun id -> acc := IntSet.add id !acc) (term_outgoing_block_ids b.term))
    unit_ir.blocks;
  acc := IntSet.add unit_ir.entry !acc;
  !acc

(* Count how many external references each block id has, across the whole
   unit (counting each arm slot independently so a dispatcher that lists the
   same arm block twice counts twice). Inlined arms are ignored — they do not
   contribute a PC transition. *)
let predecessor_counts (unit_ir : unit_ir) : int IntMap.t =
  let counts = ref IntMap.empty in
  let bump id = counts := IntMap.update id (function None -> Some 1 | Some n -> Some (n + 1)) !counts in
  List.iter
    (fun (b : block) ->
      let rec walk_term = function
        | Return _ -> ()
        | Jump (s, _) -> bump s
        | Branch (_, t, e) -> (
            (match t.inline with None -> bump t.block | Some r -> walk_term r.term);
            match e.inline with None -> bump e.block | Some r -> walk_term r.term)
        | Match (_, arms) ->
            List.iter
              (fun (arm : match_arm) -> match arm.inline with None -> bump arm.block | Some r -> walk_term r.term)
              arms
      in
      walk_term b.term)
    unit_ir.blocks;
  !counts

(* Approximate loop-header / back-edge detection. A block is considered a
   loop header if it appears on the DFS stack of one of its predecessors via
   a path that doesn't leave the function — i.e. it is reachable from itself.
   We compute this as the set of ids that lie on any cycle. *)
let compute_loop_header_ids (unit_ir : unit_ir) : IntSet.t =
  let block_map = block_by_id unit_ir in
  let succs id = match IntMap.find_opt id block_map with Some b -> terminator_external_succs b.term | None -> [] in
  let on_stack = Hashtbl.create 16 in
  let visited = Hashtbl.create 16 in
  let loops = ref IntSet.empty in
  let rec dfs id =
    if Hashtbl.mem on_stack id then loops := IntSet.add id !loops
    else if not (Hashtbl.mem visited id) then begin
      Hashtbl.add visited id ();
      Hashtbl.add on_stack id ();
      List.iter dfs (succs id);
      Hashtbl.remove on_stack id
    end
  in
  dfs unit_ir.entry;
  List.iter (fun (b : block) -> dfs b.id) unit_ir.blocks;
  !loops

(* ----- Value substitution (for trivial Jump inlining) ----- *)

(* Trivial inlining may copy a block body B into multiple predecessors. Each
   copy needs fresh value ids for the values B defines (locals, temps, match
   binders) so they don't collide with the other copies. We create a fresh
   counter on the ctx and add new value_info entries to the unit. *)

type fresh_ctx = { mutable next_value_id : int; mutable values_rev : value_info list }

let make_fresh_ctx (unit_ir : unit_ir) : fresh_ctx =
  let max_id = List.fold_left (fun acc (vi : value_info) -> max acc vi.id) (-1) unit_ir.values in
  { next_value_id = max_id + 1; values_rev = List.rev unit_ir.values }

let fresh_value (ctx : fresh_ctx) (name : string) (kind : value_kind) : value_id =
  let id = ctx.next_value_id in
  ctx.next_value_id <- id + 1;
  ctx.values_rev <- { id; name; kind } :: ctx.values_rev;
  id

let unit_with_values (unit_ir : unit_ir) (ctx : fresh_ctx) : unit_ir = { unit_ir with values = List.rev ctx.values_rev }

(* Substitute value ids in operands / rhs / stmt / terminator / inline region.
   [subst] maps old id -> new id. Unmentioned ids pass through unchanged. *)

let subst_value_id (subst : int IntMap.t) (id : value_id) : value_id =
  match IntMap.find_opt id subst with Some v -> v | None -> id

let subst_operand (subst : int IntMap.t) (op : operand) : operand =
  match op with OLocal id -> OLocal (subst_value_id subst id) | op -> op

let subst_call_target (subst : int IntMap.t) (t : call_target) : call_target =
  match t with Direct _ -> t | Indirect op -> Indirect (subst_operand subst op)

let subst_rhs (subst : int IntMap.t) (rhs : rhs) : rhs =
  match rhs with
  | Move op -> Move (subst_operand subst op)
  | Call (target, args) -> Call (subst_call_target subst target, List.map (subst_operand subst) args)
  | Construct (name, args) -> Construct (name, List.map (subst_operand subst) args)
  | Tuple args -> Tuple (List.map (subst_operand subst) args)
  | Array args -> Array (List.map (subst_operand subst) args)
  | Select (target, field) -> Select (subst_operand subst target, field)
  | BinOp (op, lhs, rhs) -> BinOp (op, subst_operand subst lhs, subst_operand subst rhs)

let subst_stmt (subst : int IntMap.t) (Bind (id, rhs)) : stmt = Bind (subst_value_id subst id, subst_rhs subst rhs)

let rec subst_pattern (subst : int IntMap.t) (pat : ir_pattern) : ir_pattern =
  match pat with
  | RPAny | RPInt _ | RPBool _ | RPUnit -> pat
  | RPBind id -> RPBind (subst_value_id subst id)
  | RPTuple ps -> RPTuple (List.map (subst_pattern subst) ps)
  | RPCtor (name, None) -> RPCtor (name, None)
  | RPCtor (name, Some p) -> RPCtor (name, Some (subst_pattern subst p))

let rec subst_terminator (subst : int IntMap.t) (term : terminator) : terminator =
  match term with
  | Return op -> Return (subst_operand subst op)
  | Jump (id, args) -> Jump (id, List.map (subst_operand subst) args)
  | Branch (cond, t, e) -> Branch (subst_operand subst cond, subst_branch_target subst t, subst_branch_target subst e)
  | Match (scrut, arms) -> Match (subst_operand subst scrut, List.map (subst_match_arm subst) arms)

and subst_branch_target (subst : int IntMap.t) (tgt : branch_target) : branch_target =
  { block = tgt.block; inline = Option.map (subst_inline_region subst) tgt.inline }

and subst_match_arm (subst : int IntMap.t) (arm : match_arm) : match_arm =
  {
    pattern = subst_pattern subst arm.pattern;
    block = arm.block;
    inline = Option.map (subst_inline_region subst) arm.inline;
  }

and subst_inline_region (subst : int IntMap.t) (region : inline_region) : inline_region =
  { body = List.map (subst_stmt subst) region.body; term = subst_terminator subst region.term }

(* Collect all value ids defined by a block (params + body defs + inline defs
   + inline-arm binders). Used by trivial inlining when copying into a new
   predecessor; each defined id must be freshened. *)
let collect_block_defined_ids (b : block) : IntSet.t =
  let params = List.fold_left (fun acc id -> IntSet.add id acc) IntSet.empty b.params in
  let body_defs = List.fold_left (fun acc (Bind (id, _)) -> IntSet.add id acc) IntSet.empty b.body in
  let inline_defs = block_and_inline_defined_values b in
  let inline_binders = inlined_arm_binders b.term in
  IntSet.union (IntSet.union (IntSet.union params body_defs) inline_defs) inline_binders

(* Create a substitution that maps every id in a block's defined set to a
   freshly allocated value. Values retain their original name (suffixed) and
   kind. *)
let freshen_block_values (fresh : fresh_ctx) (value_map : value_info IntMap.t) (b : block) : int IntMap.t =
  let defined = collect_block_defined_ids b in
  IntSet.fold
    (fun old_id acc ->
      let info =
        match IntMap.find_opt old_id value_map with
        | Some i -> i
        | None -> failf "BlockInlining: missing value_info for v%d while freshening b%d" old_id b.id
      in
      let new_id = fresh_value fresh (info.name ^ "'") info.kind in
      IntMap.add old_id new_id acc)
    defined IntMap.empty

(* ----- Trivial Jump inlining ----- *)

(* A block is eligible to be inlined at a Jump site when:
   - it is not the entry block of the function;
   - it is not a loop header (no back-edges to it);
   - it is not a Join block (no BCont / join-point targets);
   - its body is within the stmt limit;
   - its terminator is a non-Jump/Branch/Match/Return that we can emit inline
     (we accept all four — Jumps can chain, Branch/Match we support because
     their arms already point to existing blocks or inlined regions);
   - for the "copy into multiple preds" case, the total extra cost is bounded. *)

(* A Join block is eligible for trivial inlining only if it has exactly one
   predecessor — at that point it isn't actually joining anything and acts as
   a plain pass-through. True multi-predecessor joins must remain as memo
   boundaries. The single-pred check happens at the call site. *)
let block_is_eligible_trivial_target (unit_ir : unit_ir) (loop_headers : IntSet.t) (pred_counts : int IntMap.t)
    (b : block) : bool =
  b.id <> unit_ir.entry
  && (match b.kind with
    | Join _ -> ( match IntMap.find_opt b.id pred_counts with Some 1 -> true | _ -> false)
    | Entry | IfThen | IfElse | MatchArm -> true)
  && (not (IntSet.mem b.id loop_headers))
  && List.length b.body <= trivial_inline_stmt_limit

(* Materialize the Jump-inlined fragment: Bind(param_i, Move(arg_i)) prefix
   followed by B's body and terminator, with all value ids freshened so the
   fragment can be spliced into a fresh predecessor context. *)
let materialize_jump_fragment (fresh : fresh_ctx) (value_map : value_info IntMap.t) (b : block) (args : operand list) :
    stmt list * terminator =
  let subst = freshen_block_values fresh value_map b in
  let param_binds =
    try List.map2 (fun param_id arg -> Bind (subst_value_id subst param_id, Move arg)) b.params args
    with Invalid_argument _ ->
      failf "BlockInlining: jump to b%d has %d args, expected %d" b.id (List.length args) (List.length b.params)
  in
  let body' = List.map (subst_stmt subst) b.body in
  let term' = subst_terminator subst b.term in
  (param_binds @ body', term')

(* One round of trivial Jump inlining. Returns (new_unit, changed). *)
let trivial_inline_once (unit_ir : unit_ir) : unit_ir * bool =
  let loop_headers = compute_loop_header_ids unit_ir in
  let pred_counts = predecessor_counts unit_ir in
  let block_map = block_by_id unit_ir in
  let value_map = value_by_id unit_ir in
  let fresh = make_fresh_ctx unit_ir in
  let changed = ref false in
  (* Decide which target blocks are candidates. To keep things simple and
     deterministic we inline each eligible Jump site independently, copying
     the target body into its caller. If the same target is used by several
     callers we allow it only when the combined cost is within the copy
     budget. *)
  let candidate_blocks =
    List.filter
      (fun (b : block) ->
        block_is_eligible_trivial_target unit_ir loop_headers pred_counts b
        && match IntMap.find_opt b.id pred_counts with Some n -> n >= 1 | None -> false)
      unit_ir.blocks
  in
  let single_pred_ids =
    List.filter_map
      (fun (b : block) -> match IntMap.find_opt b.id pred_counts with Some 1 -> Some b.id | _ -> None)
      candidate_blocks
    |> List.fold_left (fun s id -> IntSet.add id s) IntSet.empty
  in
  (* Total extra cost budget for multi-pred inlining, shared across all
     targets, so a single pathological block can't blow up the code. *)
  let extra_budget = ref trivial_inline_copy_total_extra in
  let should_inline_target (target : block) (caller_count : int) =
    if IntSet.mem target.id single_pred_ids then true
    else if caller_count = 1 then
      (* Unique caller after previous inlining decisions. *)
      true
    else
      let cost = block_cost target * (caller_count - 1) in
      if cost <= !extra_budget then begin
        extra_budget := !extra_budget - cost;
        true
      end
      else false
  in
  let rewritten_blocks =
    List.map
      (fun (b : block) ->
        match b.term with
        | Jump (target_id, args) -> (
            match IntMap.find_opt target_id block_map with
            | Some target
              when block_is_eligible_trivial_target unit_ir loop_headers pred_counts target
                   && target_id <> b.id (* don't inline a jump-to-self *)
                   &&
                   let caller_count = match IntMap.find_opt target_id pred_counts with Some n -> n | None -> 0 in
                   should_inline_target target caller_count ->
                let binds, term' = materialize_jump_fragment fresh value_map target args in
                changed := true;
                { b with body = b.body @ binds; term = term' }
            | _ -> b)
        | _ -> b)
      unit_ir.blocks
  in
  (* Drop inlined targets that are now unreferenced. A target is unreferenced
     if it is not the entry block and no remaining terminator points at it. *)
  let rewritten_unit = { (unit_with_values unit_ir fresh) with blocks = rewritten_blocks } in
  let referenced = compute_referenced_ids rewritten_unit in
  let filtered_blocks =
    List.filter (fun (b : block) -> b.id = rewritten_unit.entry || IntSet.mem b.id referenced) rewritten_unit.blocks
  in
  let final_unit = { rewritten_unit with blocks = filtered_blocks } in
  (final_unit, !changed)

let trivial_inline_unit (unit_ir : unit_ir) : unit_ir =
  let rec loop u =
    let u', changed = trivial_inline_once u in
    if changed then loop u' else u'
  in
  loop unit_ir

(* ----- Dispatch inlining ----- *)

(* An arm target B is eligible for dispatch inlining when:
   - it is a normal arm target (not already inlined);
   - it is not the entry block;
   - it is not a loop header;
   - it is not a Join block;
   - it has exactly one predecessor (the dispatcher);
   - its body is small;
   - it does not recursively reference itself through inline or non-inline;
   - none of its match-binder params are eventually resolved (used as a Match
     scrutinee, Branch condition, or BinOp operand) within its body. The
     resolve operation can fail in the make_step re-execution if the binder
     value is extracted from a sub-reference of a larger value, so we must
     keep the original memo boundary in that case. *)

(* Operand uses that cause [resolve] to be invoked on their referent. *)
let operand_uses_tainted (tainted : IntSet.t) (op : operand) : bool =
  match op with OLocal id -> IntSet.mem id tainted | _ -> false

let stmt_walk_taint (tainted : IntSet.t ref) (Bind (id, rhs)) : bool =
  let triggers_resolve =
    match rhs with BinOp (_, l, r) -> operand_uses_tainted !tainted l || operand_uses_tainted !tainted r | _ -> false
  in
  (* Propagate taint through Move (a Move just aliases the value, the alias is
     equally vulnerable to a downstream resolve). *)
  (match rhs with Move (OLocal src) when IntSet.mem src !tainted -> tainted := IntSet.add id !tainted | _ -> ());
  triggers_resolve

let rec term_walk_taint (tainted : IntSet.t ref) (term : terminator) : bool =
  match term with
  | Return _ | Jump _ -> false
  | Branch (cond, t, e) -> (
      operand_uses_tainted !tainted cond
      || (match t.inline with None -> false | Some r -> inline_walk_taint tainted r)
      || match e.inline with None -> false | Some r -> inline_walk_taint tainted r)
  | Match (scrutinee, arms) ->
      operand_uses_tainted !tainted scrutinee
      || List.exists
           (fun (arm : match_arm) ->
             match arm.inline with None -> false | Some region -> inline_walk_taint tainted region)
           arms

and inline_walk_taint (tainted : IntSet.t ref) (region : inline_region) : bool =
  let stmt_triggers = List.exists (stmt_walk_taint tainted) region.body in
  stmt_triggers || term_walk_taint tainted region.term

(* Check whether inlining the arm block would resolve any of its pattern
   binders. If so, we keep the arm as a separate memo boundary. *)
let arm_resolves_binder (b : block) : bool =
  let binders = b.params in
  if binders = [] then false
  else
    let tainted = ref (List.fold_left (fun acc id -> IntSet.add id acc) IntSet.empty binders) in
    let body_triggers = List.exists (stmt_walk_taint tainted) b.body in
    body_triggers || term_walk_taint tainted b.term

let arm_block_eligible_dispatch (unit_ir : unit_ir) (loop_headers : IntSet.t) (pred_counts : int IntMap.t) (b : block) :
    bool =
  b.id <> unit_ir.entry
  && (match b.kind with Join _ -> false | Entry | IfThen | IfElse | MatchArm -> true)
  && (not (IntSet.mem b.id loop_headers))
  && (match IntMap.find_opt b.id pred_counts with Some 1 -> true | _ -> false)
  && List.length b.body <= dispatch_inline_stmt_limit
  && (not (IntSet.mem b.id (term_outgoing_block_ids b.term)))
  && not (arm_resolves_binder b)

(* Build an inline_region from a block's body/term. The arm's block params
   are the match binders; they're already present as MatchBinder values with
   the same ids in the arm's body/term. The dispatcher will write the binder
   values into their slots directly before running the inline region. *)
let inline_region_of_block (b : block) : inline_region = { body = b.body; term = b.term }

(* Reverse-post-order traversal of the unit IR so a block's external
   successors are visited before the block itself. This means when we decide
   to inline an arm target B into a predecessor A, B's body/term has already
   been rewritten (any of B's own dispatch-inlinable arms have been folded
   in), so the snapshot we store in A's inline region reflects the final
   content of B. *)
let compute_post_order (unit_ir : unit_ir) : block_id list =
  let block_map = block_by_id unit_ir in
  let visited = Hashtbl.create 16 in
  let order = ref [] in
  let rec visit id =
    if not (Hashtbl.mem visited id) then begin
      Hashtbl.add visited id ();
      (match IntMap.find_opt id block_map with
      | Some b -> List.iter visit (terminator_external_succs b.term)
      | None -> ());
      order := id :: !order
    end
  in
  visit unit_ir.entry;
  List.iter (fun (b : block) -> visit b.id) unit_ir.blocks;
  List.rev !order

let dispatch_inline_once (unit_ir : unit_ir) : unit_ir * bool =
  let loop_headers = compute_loop_header_ids unit_ir in
  let pred_counts = predecessor_counts unit_ir in
  let changed = ref false in
  (* The "current" body/term map: as we rewrite blocks in post-order, we store
     each block's latest version here so downstream predecessors see the
     updated version when deciding to inline. *)
  let current : block IntMap.t ref =
    ref (List.fold_left (fun acc (b : block) -> IntMap.add b.id b acc) IntMap.empty unit_ir.blocks)
  in
  let drop = ref IntSet.empty in
  let try_inline_block (b : block) : inline_region option =
    if IntSet.mem b.id !drop then None
    else if arm_block_eligible_dispatch unit_ir loop_headers pred_counts b then begin
      drop := IntSet.add b.id !drop;
      changed := true;
      Some (inline_region_of_block b)
    end
    else None
  in
  let rewrite_arm_target (tgt : branch_target) : branch_target =
    match tgt.inline with
    | Some _ -> tgt
    | None -> (
        match IntMap.find_opt tgt.block !current with
        | Some arm_block -> (
            match try_inline_block arm_block with Some region -> { tgt with inline = Some region } | None -> tgt)
        | None -> tgt)
  in
  let rewrite_match_arm (arm : match_arm) : match_arm =
    match arm.inline with
    | Some _ -> arm
    | None -> (
        match IntMap.find_opt arm.block !current with
        | Some arm_block -> (
            match try_inline_block arm_block with Some region -> { arm with inline = Some region } | None -> arm)
        | None -> arm)
  in
  let post_order = compute_post_order unit_ir in
  List.iter
    (fun block_id ->
      match IntMap.find_opt block_id !current with
      | None -> ()
      | Some b ->
          let term' =
            match b.term with
            | Return _ | Jump _ -> b.term
            | Branch (cond, t, e) -> Branch (cond, rewrite_arm_target t, rewrite_arm_target e)
            | Match (scrut, arms) -> Match (scrut, List.map rewrite_match_arm arms)
          in
          current := IntMap.add block_id { b with term = term' } !current)
    post_order;
  let rewritten_blocks =
    List.filter_map
      (fun (b : block) -> if IntSet.mem b.id !drop then None else Some (IntMap.find b.id !current))
      unit_ir.blocks
  in
  ({ unit_ir with blocks = rewritten_blocks }, !changed)

let dispatch_inline_unit (unit_ir : unit_ir) : unit_ir =
  let rec loop u =
    let u', changed = dispatch_inline_once u in
    if changed then loop u' else u'
  in
  loop unit_ir

(* ----- Top-level entry points ----- *)

let apply_unit (unit_ir : unit_ir) : unit_ir =
  let unit_ir = trivial_inline_unit unit_ir in
  let unit_ir = dispatch_inline_unit unit_ir in
  (* Repeat trivial inlining once more in case dispatch inlining exposed new
     opportunities (e.g. an arm block whose sole successor is now dispatched
     into a neighbour that became small enough after dispatch-inlining its
     own arms). *)
  let unit_ir = trivial_inline_unit unit_ir in
  unit_ir

let apply (prog : prog_ir) : prog_ir = { units = List.map apply_unit prog.units }
