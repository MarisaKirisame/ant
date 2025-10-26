open PPrint

type ir = Raw of document | Unit | Seqs of ir list | Function of string | Paren of ir | App of ir * ir list

let rec ir_to_doc ir =
  match ir with
  | Raw doc -> doc
  | Seqs [] -> string "()"
  | Seqs [ x ] -> ir_to_doc x
  | Seqs (x :: xs) -> parens (List.fold_left (fun acc x -> acc ^^ string ";" ^^ ir_to_doc x) (ir_to_doc x) xs)
  | Unit -> string "()"
  | Function str -> string str
  | Paren inner -> string "(" ^^ ir_to_doc inner ^^ string ")"
  | App (fn, args) -> List.fold_left (fun acc x -> acc ^^ string " " ^^ ir_to_doc x) (ir_to_doc fn) args

let show_ir ir =
  let doc = ir_to_doc ir in
  let buf = Buffer.create 128 in
  PPrint.ToBuffer.pretty 0.8 80 buf doc;
  Buffer.contents buf

let rec optimize_ir ir =
  match ir with
  | Raw d -> Raw d
  | Seqs xs ->
      let xs = List.map optimize_ir xs in
      Seqs (List.flatten (List.map (fun ir -> match ir with Unit -> [] | Seqs xs -> xs | x -> [ x ]) xs))
  | Function f -> Function f
  | Unit -> Unit
  | App (Paren (App (fn, args1)), args2) ->
      optimize_ir (App (optimize_ir fn, List.map optimize_ir args1 @ List.map optimize_ir args2))
  | App (fn, args) -> App (optimize_ir fn, List.map optimize_ir args)
  | Paren ir -> Paren (optimize_ir ir)
