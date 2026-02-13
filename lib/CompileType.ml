open PPrint
open Syntax
module Hashtbl = AntHashtbl

let compile_ty_binding (ctx : (string, int) Hashtbl.t) (binding : 'a ty_binding) : document =
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
  let ops : 'a CompileFfi.ops =
    {
      type_name_of = (fun name -> name);
      compile_ty = Syntax.pp_ty;
      compile_conv = (fun ~is_to ty v -> compile_conv is_to ty v);
      appends = (fun xs -> string "Memo.appends" ^^ space ^^ brackets (separate (semi ^^ space) xs));
      from_constructor = (fun tag -> string "Memo.from_constructor" ^^ space ^^ tag);
      list_match = (fun v -> string "Memo.list_match" ^^ space ^^ v);
      word_get_value = (fun v -> string "Word.get_value" ^^ space ^^ v);
      splits = (fun v -> string "Memo.splits" ^^ space ^^ v);
      splits_n =
        (fun n ->
          match n with
          | 1 -> Some (fun v -> string "Memo.splits_1" ^^ space ^^ v)
          | 2 -> Some (fun v -> string "Memo.splits_2" ^^ space ^^ v)
          | 3 -> Some (fun v -> string "Memo.splits_3" ^^ space ^^ v)
          | 4 -> Some (fun v -> string "Memo.splits_4" ^^ space ^^ v)
          | _ -> None);
      tag_expr = (fun ~cname ~tag_id:_ -> string ("tag_" ^ cname));
      match_tag =
        (fun ~cname ~tag_id ->
          string "|" ^^ space
          ^^ string (string_of_int tag_id)
          ^^ space ^^ string "(*" ^^ space
          ^^ string ("tag_" ^ cname)
          ^^ space ^^ string "*) ->");
    }
  in
  CompileFfi.compile_type_defs ops binding ^^ hardline ^^ CompileFfi.compile_conversions ops ctx binding
