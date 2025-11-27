open PPrint
open Syntax

let compile_ty_binding (binding : 'a ty_binding) : document =
  let ctor_tag_map = Hashtbl.create 16 in
  let tag_counter = ref 1 in
  let get_tag c =
    match Hashtbl.find_opt ctor_tag_map c with
    | Some t -> t
    | None ->
        let t = !tag_counter in
        incr tag_counter;
        Hashtbl.add ctor_tag_map c t;
        t
  in

  let collect_ctors = function
    | TBOne (_, Enum { ctors; _ }) -> List.iter (fun (c, _, _) -> ignore (get_tag c)) ctors
    | TBRec decls ->
        List.iter (fun (_, Enum { ctors; _ }) -> List.iter (fun (c, _, _) -> ignore (get_tag c)) ctors) decls
  in
  collect_ctors binding;

  let compile_tags =
    let sorted_tags = List.sort (fun (_, t1) (_, t2) -> compare t1 t2) (List.of_seq (Hashtbl.to_seq ctor_tag_map)) in
    concat_map (fun (c, t) -> string ("let tag_" ^ c ^ " = " ^ string_of_int t) ^^ hardline) sorted_tags
  in

  let rec compile_conv is_to ty v =
    match ty with
    | TUnit ->
        if is_to then string "(ignore (Word.get_value (Memo.to_word " ^^ v ^^ string ")); ())"
        else string "(Memo.from_int 0)"
    | TInt ->
        if is_to then string "(Word.get_value (Memo.to_word " ^^ v ^^ string "))"
        else string "(Memo.from_int " ^^ v ^^ string ")"
    | TBool ->
        if is_to then string "(Word.get_value (Memo.to_word " ^^ v ^^ string ") <> 0)"
        else string "(Memo.from_int (if " ^^ v ^^ string " then 1 else 0))"
    | TNamed n ->
        let fname = if is_to then "to_ocaml_" ^ n else "from_ocaml_" ^ n in
        string fname ^^ space ^^ v
    | TNamedVar n ->
        let fname = if is_to then "to_generic_" ^ n else "from_generic_" ^ n in
        string fname ^^ space ^^ v
    | TApply (TNamed n, args) ->
        let fname = if is_to then "to_ocaml_" ^ n else "from_ocaml_" ^ n in
        let args_conv =
          separate_map space (fun arg -> parens (string "fun x -> " ^^ compile_conv is_to arg (string "x"))) args
        in
        string fname ^^ space ^^ args_conv ^^ space ^^ v
    | TTuple _ -> string "failwith \"tuples not supported directly\""
    | _ -> string "failwith \"complex type not supported\""
  in

  let compile_from_ctor (cname, args, _) =
    let pat_args =
      if args = [] then empty
      else
        space ^^ parens
        @@ separate_map (comma ^^ space) (fun i -> string ("x" ^ string_of_int i)) (List.mapi (fun i _ -> i) args)
    in
    let pattern = string cname ^^ pat_args in
    let body_list =
      string ("Memo.from_constructor tag_" ^ cname)
      :: List.mapi (fun i ty -> compile_conv false ty (string ("x" ^ string_of_int i))) args
    in
    let body = string "Memo.appends" ^^ space ^^ brackets (separate (semi ^^ space) body_list) in
    break 1 ^^ string "|" ^^ space ^^ pattern ^^ space ^^ string "->" ^^ nest 2 (break 1 ^^ body)
  in

  let compile_to_ctor (cname, args, _) =
    let match_case =
      string "|" ^^ space ^^ string "c" ^^ space ^^ string "when c = tag_" ^^ string cname ^^ space ^^ string "->"
    in
    let extraction =
      if args = [] then empty
      else
        let vars = List.mapi (fun i _ -> string ("x" ^ string_of_int i)) args in
        if List.length args > 4 then
          break 1
          ^^ string "let args_list = Memo.splits t in"
          ^^ concat_map
               (fun (i, v) ->
                 break 1 ^^ string "let " ^^ v ^^ string " = List.nth args_list "
                 ^^ string (string_of_int i)
                 ^^ string " in")
               (List.mapi (fun i v -> (i, v)) vars)
        else
          let split_func =
            match List.length args with
            | 1 -> string "Memo.splits_1"
            | 2 -> string "Memo.splits_2"
            | 3 -> string "Memo.splits_3"
            | 4 -> string "Memo.splits_4"
            | _ -> failwith "unreachable"
          in
          let rhs = split_func ^^ space ^^ string "t" in
          let lhs = if List.length args > 1 then parens (separate (comma ^^ space) vars) else List.hd vars in
          break 1 ^^ string "let" ^^ space ^^ lhs ^^ space ^^ equals ^^ space ^^ rhs ^^ string " in"
    in
    let reconstruction =
      let ctor_app =
        if args = [] then string cname
        else
          let conv_args = List.mapi (fun i ty -> compile_conv true ty (string ("x" ^ string_of_int i))) args in
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
           (break 1
           ^^ string "let h, t = Option.get (Memo.list_match x) in"
           ^^ break 1 ^^ string "match Word.get_value h with" ^^ concat_map compile_to_ctor ctors ^^ break 1
           ^^ string "| _ -> failwith \"unreachable\"")
    in
    (header_from, header_to)
  in

  let decls = match binding with TBOne (n, k) -> [ (n, k) ] | TBRec ds -> ds in

  let from_docs, to_docs = List.split (List.mapi (fun i d -> compile_funcs d (i = 0)) decls) in

  compile_tags ^^ pp_stmt (Type binding) ^^ hardline
  ^^ separate (break 1) from_docs
  ^^ hardline
  ^^ separate (break 1) to_docs
