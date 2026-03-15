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

type match_arm = { pattern : ir_pattern; block : block_id }

type terminator =
  | Jump of block_id * operand list
  | Branch of operand * block_id * block_id
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

let pp_terminator values = function
  | Jump (block_id, args) ->
      string "jump" ^^ space
      ^^ string ("b" ^ string_of_int block_id)
      ^^ parens (pp_list (pp_operand values) (comma ^^ space) args)
  | Branch (cond, if_true, if_false) ->
      string "branch" ^^ space ^^ pp_operand values cond ^^ space ^^ string "?" ^^ space
      ^^ string ("b" ^ string_of_int if_true)
      ^^ space ^^ colon ^^ space
      ^^ string ("b" ^ string_of_int if_false)
  | Match (scrutinee, arms) ->
      let pp_arm { pattern; block } =
        pp_pattern values pattern ^^ space ^^ string "->" ^^ space ^^ string ("b" ^ string_of_int block)
      in
      string "match" ^^ space ^^ pp_operand values scrutinee ^^ space ^^ string "with" ^^ space
      ^^ pp_list pp_arm (space ^^ string "|" ^^ space) arms
  | Return operand -> string "return" ^^ space ^^ pp_operand values operand

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
