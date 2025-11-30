open Ppxlib

let lazy_ ~loc (e : expression) : expression = [%expr fun _ -> [%e e]]

let fold_seqs ~loc xs =
  let mapped = List.map (lazy_ ~loc) xs in
  let seqs = Ast_builder.Default.elist ~loc mapped in
  [%expr seqs_ [%e seqs]]

let rec flatten_sequence ~loc expr =
  let rec aux acc e =
    match e.pexp_desc with
    | Pexp_sequence (e1, e2) ->
        let acc = aux acc e2 in
        aux acc e1
    | _ -> flatten_let_in ~loc e :: acc
  in
  aux [] expr

and flatten_let_in ~loc expr =
  match expr.pexp_desc with
  | Pexp_letop { let_ = { pbop_op = { txt = "let$"; _ }; pbop_pat = pat; pbop_exp = value; _ }; body; ands = [] } -> (
      match pat.ppat_desc with
      | Ppat_var { txt; _ } ->
          let str = Ast_builder.Default.estring ~loc txt in
          let sub = flatten_sequence ~loc body in
          if List.length sub == 1 then [%expr let_in_ [%e str] [%e value] (fun [%p pat] -> [%e List.nth sub 0])]
          else
            let seqs = fold_seqs ~loc sub in
            [%expr let_in_ [%e str] [%e value] (fun [%p pat] -> [%e seqs])]
      | _ -> failwith "only identifier is valid here")
  | _ -> expr

let expand_seqs ~ctxt payload_expr =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let module B = Ast_builder.Default in
  let items = flatten_sequence payload_expr ~loc in
  fold_seqs ~loc items

let seqs_extension =
  Extension.V3.declare "seqs" Extension.Context.expression Ast_pattern.(single_expr_payload __) expand_seqs

let () = Driver.register_transformation ~rules:[ Context_free.Rule.extension seqs_extension ] "seqs"
