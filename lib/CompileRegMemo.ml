open PPrint
open Syntax
include ControlFlowGraph

let failf fmt = Printf.ksprintf failwith fmt

let lower_prog = AnfToCfg.lower_prog

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

let live_on_jump_edge (succ : block) (succ_info : CfgLiveness.block_liveness) (args : operand list) =
  try
    let live_params =
      List.fold_left2
        (fun live param arg ->
          if IntSet.mem param succ_info.live_params then IntMap.add param (operand_uses arg) live else live)
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

let analyze_unit_liveness (unit_ir : unit_ir) : CfgLiveness.unit_liveness =
  CfgLiveness.analyze_unit ~blocks:unit_ir.blocks
    ~entry_id:unit_ir.entry
    ~successors
    ~block_id:(fun (block : block) -> block.id)
    ~params:(fun (block : block) -> block.params)
    ~body:(fun (block : block) -> block.body)
    ~transfer_stmt
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
        | Return operand -> add_operand_uses IntSet.empty operand
        | Jump _ -> live_out
        | Branch (cond, _, _) -> add_operand_uses live_out cond
        | Match (cond, _) -> add_operand_uses live_out cond
      in
      (live_on_edge, live_out, live_before_term))

let analyze_prog (ir : prog_ir) : prog_analysis =
  CfgLiveness.analyze_prog ~units:ir.units ~analyze_unit:analyze_unit_liveness ir

let pp_prog_analysis (analysis : prog_analysis) =
  match List.combine analysis.ir.units analysis.liveness with
  | [] -> string "regmemo.phase2 empty"
  | units ->
      separate_map (hardline ^^ hardline)
        (fun (unit_ir, liveness) -> pp_unit_ir unit_ir ^^ hardline ^^ string "liveness:" ^^ hardline ^^ pp_unit_liveness unit_ir liveness)
        units

let pp_prog_ir (prog : prog_ir) =
  match prog.units with
  | [] -> string "regmemo.phase1 empty"
  | units -> separate_map (hardline ^^ hardline) pp_unit_ir units

let compile_reg_memo (prog : 'a prog) : document = pp_prog_ir (lower_prog prog)

module Backend = struct
  let compile = compile_reg_memo
end
