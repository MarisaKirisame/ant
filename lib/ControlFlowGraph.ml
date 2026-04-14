open PPrint
open Syntax
module IntSet = CfgLiveness.IntSet
module IntMap = CfgLiveness.IntMap
module StrMap = Map.Make (String)

type value_id = int
type block_id = int
type function_id = int
type value_kind = Param | Local | Temp | JoinBinder | JoinParam | MatchBinder
type value_info = { id : value_id; name : string; kind : value_kind }

type operand =
  | OLocal of value_id
  | OGlobal of string
  | OBuiltin of string
  | OInt of int
  | OFloat of float
  | OBool of bool
  | OString of string
  | OUnit
  | OCtor of string

type call_target = Direct of string | Indirect of operand

type rhs =
  | Move of operand
  | Call of call_target * operand list
  | Construct of string * operand list
  | Tuple of operand list
  | Array of operand list
  | Select of operand * field
  | BinOp of string * operand * operand

type stmt = Bind of value_id * rhs

type ir_pattern =
  | RPAny
  | RPInt of int
  | RPBool of bool
  | RPUnit
  | RPBind of value_id
  | RPTuple of ir_pattern list
  | RPCtor of string * ir_pattern option

(* An inline_region represents the body and terminator of a block that has been
   dispatch-inlined into a predecessor's Match/Branch arm. The inlined fragment
   executes in-place without crossing a memo-step boundary, reusing the
   dispatcher's working frame for its local values. *)
type inline_region = { body : stmt list; term : terminator }

and match_arm = {
  pattern : ir_pattern;
  block : block_id;
  (* When [inline] is [Some region], control does NOT transfer to [block] via a
     PC; [region.body] and [region.term] run inline inside the dispatcher's step,
     and [block] is removed from [unit_ir.blocks]. The [block] field is retained
     only as a provenance tag for pretty-printing and debugging. *)
  inline : inline_region option;
}

and branch_target = { block : block_id; inline : inline_region option }

and terminator =
  | Jump of block_id * operand list
  | Branch of operand * branch_target * branch_target
  | Match of operand * match_arm list
  | Return of operand

type block_kind = Entry | Join of value_id | IfThen | IfElse | MatchArm

type block = {
  id : block_id;
  label : string;
  kind : block_kind;
  params : value_id list;
  body : stmt list;
  term : terminator;
}

let mk_plain_branch_target block_id : branch_target = { block = block_id; inline = None }
let mk_plain_match_arm pattern block_id : match_arm = { pattern; block = block_id; inline = None }

type unit_kind = Function | Global

type unit_ir = {
  id : function_id;
  name : string;
  kind : unit_kind;
  entry : block_id;
  params : value_id list;
  values : value_info list;
  blocks : block list;
}

type prog_ir = { units : unit_ir list }
type prog_analysis = prog_ir CfgLiveness.prog_analysis

let kind_name = function
  | Param -> "param"
  | Local -> "local"
  | Temp -> "temp"
  | JoinBinder -> "join"
  | JoinParam -> "join_param"
  | MatchBinder -> "match"

let block_kind_name = function
  | Entry -> "entry"
  | Join value_id -> "join(v" ^ string_of_int value_id ^ ")"
  | IfThen -> "if_then"
  | IfElse -> "if_else"
  | MatchArm -> "match_arm"

let rec pp_list pp sep = function [] -> empty | [ x ] -> pp x | x :: xs -> pp x ^^ sep ^^ pp_list pp sep xs

let pp_field = function
  | FName name -> string "." ^^ string name
  | FIndex index -> string "." ^^ string (string_of_int index)

let pp_value_ref (values : value_info StrMap.t) (id : value_id) =
  let label =
    match StrMap.find_opt (string_of_int id) values with
    | Some info -> Printf.sprintf "v%d[%s]" info.id info.name
    | None -> Printf.sprintf "v%d" id
  in
  string label

let value_table (unit_ir : unit_ir) : value_info StrMap.t =
  List.fold_left
    (fun acc (info : value_info) -> StrMap.add (string_of_int info.id) info acc)
    StrMap.empty unit_ir.values

let pp_operand (values : value_info StrMap.t) = function
  | OLocal id -> pp_value_ref values id
  | OGlobal name -> string ("@" ^ name)
  | OBuiltin name -> string ("builtin:" ^ name)
  | OInt i -> string (string_of_int i)
  | OFloat f -> string (string_of_float f)
  | OBool b -> string (string_of_bool b)
  | OString s -> dquotes (string (String.escaped s))
  | OUnit -> string "()"
  | OCtor name -> string ("#" ^ name)

let pp_call_target values = function
  | Direct name -> string ("@" ^ name)
  | Indirect operand -> pp_operand values operand

let pp_rhs values = function
  | Move operand -> pp_operand values operand
  | Call (target, args) ->
      string "call " ^^ pp_call_target values target ^^ parens (pp_list (pp_operand values) (comma ^^ space) args)
  | Construct (name, args) ->
      string "construct " ^^ string name ^^ parens (pp_list (pp_operand values) (comma ^^ space) args)
  | Tuple values' -> string "tuple" ^^ parens (pp_list (pp_operand values) (comma ^^ space) values')
  | Array values' -> string "array" ^^ brackets (pp_list (pp_operand values) (semi ^^ space) values')
  | Select (target, field) -> pp_operand values target ^^ pp_field field
  | BinOp (op, lhs, rhs) -> pp_operand values lhs ^^ space ^^ string op ^^ space ^^ pp_operand values rhs

let pp_stmt values = function
  | Bind (id, rhs) -> pp_value_ref values id ^^ space ^^ string "<-" ^^ space ^^ pp_rhs values rhs

let rec pp_pattern values = function
  | RPAny -> underscore
  | RPInt n -> string (string_of_int n)
  | RPBool b -> string (string_of_bool b)
  | RPUnit -> string "()"
  | RPBind id -> pp_value_ref values id
  | RPTuple patterns -> parens (pp_list (pp_pattern values) (comma ^^ space) patterns)
  | RPCtor (ctor, None) -> string ctor
  | RPCtor (ctor, Some payload) -> string ctor ^^ space ^^ pp_pattern values payload

let rec pp_terminator values = function
  | Jump (block_id, args) ->
      string "jump" ^^ space
      ^^ string ("b" ^ string_of_int block_id)
      ^^ parens (pp_list (pp_operand values) (comma ^^ space) args)
  | Branch (cond, if_true, if_false) ->
      string "branch" ^^ space ^^ pp_operand values cond ^^ space ^^ string "?" ^^ space
      ^^ pp_branch_target values if_true ^^ space ^^ colon ^^ space ^^ pp_branch_target values if_false
  | Match (scrutinee, arms) ->
      string "match" ^^ space ^^ pp_operand values scrutinee ^^ space ^^ string "with" ^^ space
      ^^ pp_list (pp_match_arm values) (space ^^ string "|" ^^ space) arms
  | Return operand -> string "return" ^^ space ^^ pp_operand values operand

and pp_branch_target values { block; inline } =
  match inline with
  | None -> string ("b" ^ string_of_int block)
  | Some region ->
      string "inline<" ^^ string ("b" ^ string_of_int block) ^^ string ">" ^^ pp_inline_region values region

and pp_match_arm values { pattern; block; inline } =
  let pat = pp_pattern values pattern ^^ space ^^ string "->" ^^ space in
  match inline with
  | None -> pat ^^ string ("b" ^ string_of_int block)
  | Some region ->
      pat ^^ string "inline<" ^^ string ("b" ^ string_of_int block) ^^ string ">" ^^ pp_inline_region values region

and pp_inline_region values { body; term } =
  let body_doc =
    match body with
    | [] -> empty
    | stmts -> hardline ^^ separate_map hardline (fun stmt -> string "    " ^^ pp_stmt values stmt) stmts
  in
  space ^^ lbrace ^^ body_doc ^^ hardline ^^ string "    " ^^ pp_terminator values term ^^ hardline ^^ string "  "
  ^^ rbrace

let pp_block values (block : block) =
  let header =
    string "block" ^^ space
    ^^ string ("b" ^ string_of_int block.id)
    ^^ parens (pp_list (pp_value_ref values) (comma ^^ space) block.params)
    ^^ space ^^ lbrace ^^ space ^^ string block.label ^^ comma ^^ space
    ^^ string (block_kind_name block.kind)
    ^^ space ^^ rbrace
  in
  let body =
    match block.body with
    | [] -> empty
    | stmts -> hardline ^^ separate_map hardline (fun stmt -> string "  " ^^ pp_stmt values stmt) stmts
  in
  header ^^ body ^^ hardline ^^ string "  " ^^ pp_terminator values block.term

let pp_value_info (info : value_info) = string (Printf.sprintf "v%d[%s]:%s" info.id info.name (kind_name info.kind))
let pp_unit_kind = function Function -> "function" | Global -> "global"

let pp_unit_ir (unit_ir : unit_ir) =
  let values = value_table unit_ir in
  let header =
    string (pp_unit_kind unit_ir.kind)
    ^^ space ^^ string unit_ir.name ^^ string "#"
    ^^ string (string_of_int unit_ir.id)
    ^^ parens (pp_list (pp_value_ref values) (comma ^^ space) unit_ir.params)
    ^^ space ^^ string "entry=b"
    ^^ string (string_of_int unit_ir.entry)
  in
  let values_doc =
    match unit_ir.values with
    | [] -> empty
    | values' ->
        hardline ^^ string "values:" ^^ hardline
        ^^ separate_map hardline (fun info -> string "  " ^^ pp_value_info info) values'
  in
  let blocks_doc =
    match unit_ir.blocks with
    | [] -> empty
    | blocks -> hardline ^^ string "blocks:" ^^ hardline ^^ separate_map (hardline ^^ hardline) (pp_block values) blocks
  in
  header ^^ values_doc ^^ blocks_doc

let pp_liveness_set values live =
  match IntSet.elements live with
  | [] -> lbracket ^^ rbracket
  | ids -> lbracket ^^ pp_list (pp_value_ref values) (comma ^^ space) ids ^^ rbracket

let pp_edge_param_liveness values param_live =
  match IntMap.bindings param_live with
  | [] -> lbrace ^^ rbrace
  | bindings ->
      lbrace
      ^^ pp_list
           (fun (param_id, live) ->
             pp_value_ref values param_id ^^ space ^^ string "<-" ^^ space ^^ pp_liveness_set values live)
           (comma ^^ space) bindings
      ^^ rbrace

let pp_edge_liveness values (edge_live : CfgLiveness.edge_liveness IntMap.t) =
  match IntMap.bindings edge_live with
  | [] -> empty
  | edges ->
      hardline ^^ string "edge_live:" ^^ hardline
      ^^ separate_map hardline
           (fun (block_id, info) ->
             string "  -> b"
             ^^ string (string_of_int block_id)
             ^^ space ^^ string "live="
             ^^ pp_liveness_set values (CfgLiveness.edge_live_values info)
             ^^ space ^^ string "params="
             ^^ pp_edge_param_liveness values info.live_params)
           edges

let pp_block_liveness values block_id (info : CfgLiveness.block_liveness) =
  string "block" ^^ space
  ^^ string ("b" ^ string_of_int block_id)
  ^^ space ^^ string "live_in=" ^^ pp_liveness_set values info.live_in ^^ space ^^ string "live_params="
  ^^ pp_liveness_set values info.live_params ^^ space ^^ string "live_out=" ^^ pp_liveness_set values info.live_out
  ^^ space ^^ string "before_term="
  ^^ pp_liveness_set values info.live_before_term
  ^^ pp_edge_liveness values info.live_on_edge

let pp_unit_liveness (unit_ir : unit_ir) (liveness : CfgLiveness.unit_liveness) =
  let values = value_table unit_ir in
  separate_map hardline (fun (block_id, info) -> pp_block_liveness values block_id info)
  @@ IntMap.bindings liveness.blocks

let pp_prog_analysis (analysis : prog_analysis) =
  match List.combine analysis.ir.units analysis.liveness with
  | [] -> string "liveness empty"
  | units ->
      separate_map (hardline ^^ hardline)
        (fun (unit_ir, liveness) ->
          pp_unit_ir unit_ir ^^ hardline ^^ string "liveness:" ^^ hardline ^^ pp_unit_liveness unit_ir liveness)
        units

let pp_prog_ir (prog : prog_ir) =
  match prog.units with [] -> string "prog empty" | units -> separate_map (hardline ^^ hardline) pp_unit_ir units

(* ----- Helpers for traversing terminators with inline regions ----- *)

(* External (cross-block) successors of a terminator. An inlined arm contributes
   the successors of its inline region's terminator (recursively) rather than
   the arm's nominal block id, because control does not transfer to that block
   as a memo boundary. *)
let rec terminator_external_succs (term : terminator) : block_id list =
  match term with
  | Return _ -> []
  | Jump (s, _) -> [ s ]
  | Branch (_, then_tgt, else_tgt) -> branch_target_external_succs then_tgt @ branch_target_external_succs else_tgt
  | Match (_, arms) -> List.concat_map match_arm_external_succs arms

and branch_target_external_succs (tgt : branch_target) : block_id list =
  match tgt.inline with None -> [ tgt.block ] | Some region -> terminator_external_succs region.term

and match_arm_external_succs (arm : match_arm) : block_id list =
  match arm.inline with None -> [ arm.block ] | Some region -> terminator_external_succs region.term

(* Walk all inline regions reachable from a terminator. *)
let rec terminator_iter_inline (f : inline_region -> unit) (term : terminator) : unit =
  match term with
  | Return _ | Jump _ -> ()
  | Branch (_, t, e) ->
      Option.iter
        (fun region ->
          f region;
          terminator_iter_inline f region.term)
        t.inline;
      Option.iter
        (fun region ->
          f region;
          terminator_iter_inline f region.term)
        e.inline
  | Match (_, arms) ->
      List.iter
        (fun (arm : match_arm) ->
          Option.iter
            (fun region ->
              f region;
              terminator_iter_inline f region.term)
            arm.inline)
        arms

(* All statements in a terminator's inline regions (flattened, includes nested). *)
let collect_inline_stmts (term : terminator) : stmt list =
  let acc = ref [] in
  terminator_iter_inline (fun region -> acc := !acc @ region.body) term;
  !acc

(* All value ids defined across a block's body and all its inline regions. *)
let block_and_inline_defined_values (block : block) : IntSet.t =
  let add_stmt_def set (Bind (id, _)) = IntSet.add id set in
  let from_body = List.fold_left add_stmt_def IntSet.empty block.body in
  let from_inline = List.fold_left add_stmt_def IntSet.empty (collect_inline_stmts block.term) in
  IntSet.union from_body from_inline

(* Extract value ids introduced by a match-arm pattern (binders). *)
let rec pattern_binders (pat : ir_pattern) : int list =
  match pat with
  | RPAny | RPInt _ | RPBool _ | RPUnit -> []
  | RPBind id -> [ id ]
  | RPTuple ps -> List.concat_map pattern_binders ps
  | RPCtor (_, None) -> []
  | RPCtor (_, Some p) -> pattern_binders p

(* Collect match-arm binder value ids that belong to inlined arms anywhere in
   a terminator (including nested inline regions). These binders must live in
   the enclosing dispatcher block's working frame because the inlined arm runs
   in that block's memo step. *)
let inlined_arm_binders (term : terminator) : IntSet.t =
  let acc = ref IntSet.empty in
  let add_binders ids = List.iter (fun id -> acc := IntSet.add id !acc) ids in
  let rec walk = function
    | Return _ | Jump _ -> ()
    | Branch (_, t, e) ->
        Option.iter (fun (r : inline_region) -> walk r.term) t.inline;
        Option.iter (fun (r : inline_region) -> walk r.term) e.inline
    | Match (_, arms) ->
        List.iter
          (fun (arm : match_arm) ->
            match arm.inline with
            | None -> ()
            | Some region ->
                add_binders (pattern_binders arm.pattern);
                walk region.term)
          arms
  in
  walk term;
  !acc
