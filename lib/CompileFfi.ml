open PPrint
open Syntax
module Hashtbl = Core.Hashtbl

type 'a ops = {
  type_name_of : string -> string;
  compile_ty : 'a ty -> document;
  compile_conv : is_to:bool -> 'a ty -> document -> document;
  appends : document list -> document;
  from_constructor : document -> document;
  list_match : document -> document;
  word_get_value : document -> document;
  splits : document -> document;
  splits_n : int -> (document -> document) option;
  tag_expr : cname:string -> tag_id:int -> document;
  match_tag : cname:string -> tag_id:int -> document;
}

let compile_type_defs (ops : 'a ops) (binding : 'a ty_binding) : document =
  let compile_ctor (name, tys, _) =
    string name
    ^^
    match tys with
    | [] -> empty
    | _ -> space ^^ string "of" ^^ space ^^ separate_map (space ^^ string "*" ^^ space) ops.compile_ty tys
  in
  let compile_enum is_and name params ctors =
    let params_doc =
      match params with
      | [] -> empty
      | [ x ] -> string "'" ^^ string x ^^ space
      | _ -> parens (separate_map (string ",") (fun param -> string "'" ^^ string param) params) ^^ space
    in
    (if is_and then string "and" else string "type")
    ^^ space ^^ params_doc
    ^^ string (ops.type_name_of name)
    ^^ space ^^ string "="
    ^^
    match ctors with
    | [] -> empty
    | _ -> break 1 ^^ string "| " ^^ separate_map (break 1 ^^ string "| ") compile_ctor ctors
  in
  match binding with
  | TBOne (name, Enum { params; ctors }) -> compile_enum false name params ctors
  | TBRec decls ->
      separate_map (break 1)
        (fun (name, Enum { params; ctors }, i) -> compile_enum (i <> 0) name params ctors)
        (List.mapi (fun i (name, kind) -> (name, kind, i)) decls)

let compile_conversions (ops : 'a ops) (ctx : (string, int) Hashtbl.t) (binding : 'a ty_binding) : document =
  let compile_from_ctor (cname, args, _) =
    let pat_args =
      if args = [] then empty
      else
        space ^^ parens
        @@ separate_map (comma ^^ space) (fun i -> string ("x" ^ string_of_int i)) (List.mapi (fun i _ -> i) args)
    in
    let pattern = string cname ^^ pat_args in
    let tag_id = Hashtbl.find_exn ctx cname in
    let body_list =
      ops.from_constructor (ops.tag_expr ~cname ~tag_id)
      :: List.mapi (fun i ty -> ops.compile_conv ~is_to:false ty (string ("x" ^ string_of_int i))) args
    in
    let body = ops.appends body_list in
    break 1 ^^ string "|" ^^ space ^^ pattern ^^ space ^^ string "->" ^^ nest 2 (break 1 ^^ body)
  in
  let compile_to_ctor (cname, args, _) =
    let tag_id = Hashtbl.find_exn ctx cname in
    let match_case = ops.match_tag ~cname ~tag_id in
    let extraction =
      if args = [] then empty
      else
        let vars = List.mapi (fun i _ -> string ("x" ^ string_of_int i)) args in
        let arity = List.length args in
        match ops.splits_n arity with
        | Some split_fn ->
            let rhs = split_fn (string "t") in
            let lhs = if arity > 1 then parens (separate (comma ^^ space) vars) else List.hd vars in
            break 1 ^^ string "let" ^^ space ^^ lhs ^^ space ^^ equals ^^ space ^^ rhs ^^ space ^^ string "in"
        | None ->
            break 1 ^^ string "let args_list = "
            ^^ ops.splits (string "t")
            ^^ string " in"
            ^^ concat_map
                 (fun (i, v) ->
                   break 1 ^^ string "let " ^^ v ^^ string " = List.nth args_list "
                   ^^ string (string_of_int i)
                   ^^ string " in")
                 (List.mapi (fun i v -> (i, v)) vars)
    in
    let reconstruction =
      let ctor_app =
        if args = [] then string cname
        else
          let conv_args =
            List.mapi (fun i ty -> ops.compile_conv ~is_to:true ty (string ("x" ^ string_of_int i))) args
          in
          string cname ^^ space ^^ parens (separate (comma ^^ space) conv_args)
      in
      break 1 ^^ ctor_app
    in
    match_case ^^ nest 2 (extraction ^^ reconstruction)
  in
  let compile_funcs (name, Enum { params; ctors }) is_first =
    let params_pat =
      match params with [] -> empty | _ -> separate_map space (fun p -> string ("from_generic_" ^ p)) params ^^ space
    in
    let header_from =
      (if is_first then string "let rec" else string "and")
      ^^ space
      ^^ string ("from_ocaml_" ^ name)
      ^^ space ^^ params_pat ^^ string "x ="
      ^^ nest 2 (break 1 ^^ string "match x with" ^^ concat_map compile_from_ctor ctors)
    in
    let params_pat_to =
      match params with [] -> empty | _ -> separate_map space (fun p -> string ("to_generic_" ^ p)) params ^^ space
    in
    let header_to =
      (if is_first then string "let rec" else string "and")
      ^^ space
      ^^ string ("to_ocaml_" ^ name)
      ^^ space ^^ params_pat_to ^^ string "x ="
      ^^ nest 2
           (break 1 ^^ string "let h, t = Option.get ("
           ^^ ops.list_match (string "x")
           ^^ string ") in" ^^ break 1 ^^ string "match "
           ^^ ops.word_get_value (string "h")
           ^^ string " with" ^^ concat_map compile_to_ctor ctors ^^ break 1
           ^^ string "| _ -> failwith \"unreachable\"")
    in
    (header_from, header_to)
  in
  let decls = match binding with TBOne (n, k) -> [ (n, k) ] | TBRec ds -> ds in
  let from_docs, to_docs = List.split (List.mapi (fun i d -> compile_funcs d (i = 0)) decls) in
  separate (break 1) from_docs ^^ hardline ^^ separate (break 1) to_docs
