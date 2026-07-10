open NamedExpr

let parenthesize s = "(" ^ s ^ ")"

let rec source_of_nexpr = function
  | NEInt i -> string_of_int i
  | NEPlus (l, r) -> parenthesize (source_of_nexpr l ^ " + " ^ source_of_nexpr r)
  | NEMinus (l, r) -> parenthesize (source_of_nexpr l ^ " - " ^ source_of_nexpr r)
  | NELt (l, r) -> parenthesize (source_of_nexpr l ^ " < " ^ source_of_nexpr r)
  | NELe (l, r) -> parenthesize (source_of_nexpr l ^ " <= " ^ source_of_nexpr r)
  | NEGt (l, r) -> parenthesize (source_of_nexpr l ^ " > " ^ source_of_nexpr r)
  | NEGe (l, r) -> parenthesize (source_of_nexpr l ^ " >= " ^ source_of_nexpr r)
  | NEAnd (l, r) -> parenthesize (source_of_nexpr l ^ " && " ^ source_of_nexpr r)
  | NEVar v -> v
  | NEAbs (arg, body) -> parenthesize ("fun " ^ arg ^ " -> " ^ source_of_nexpr body)
  | NEApp (f, a) -> source_of_nexpr f ^ "(" ^ source_of_nexpr a ^ ")"
  | NELet (name, bound, body) ->
      parenthesize ("let " ^ name ^ " = " ^ source_of_nexpr bound ^ " in " ^ source_of_nexpr body)
  | NETrue -> "true"
  | NEFalse -> "false"
  | NEIf (c, t, e) ->
      parenthesize ("if " ^ source_of_nexpr c ^ " then " ^ source_of_nexpr t ^ " else " ^ source_of_nexpr e)
  | NENil -> "[]"
  | NECons (h, t) -> parenthesize (source_of_nexpr h ^ " :: " ^ source_of_nexpr t)
  | NEMatchList (target, nil_case, hd, tl, cons_case) ->
      parenthesize
        ("case " ^ source_of_nexpr target ^ " | [] => " ^ source_of_nexpr nil_case ^ " | " ^ hd ^ " :: " ^ tl ^ " => "
       ^ source_of_nexpr cons_case ^ " end")
  | NEFix (fname, arg, body) ->
      (* Hazel recursion is expressed with fix over a recursive function name.
            We curry additional arguments with an inner lambda. *)
      parenthesize ("fix " ^ fname ^ " -> " ^ parenthesize ("fun " ^ arg ^ " -> " ^ source_of_nexpr body))
  | NEPair (x, y) -> parenthesize (source_of_nexpr x ^ ", " ^ source_of_nexpr y)
  | NEZro p ->
      (* Model tuple projection through pattern matching to avoid relying
         on internal tuple-label AST nodes. *)
      parenthesize ("case " ^ source_of_nexpr p ^ " | (x, _) => x end")
  | NEFst p -> parenthesize ("case " ^ source_of_nexpr p ^ " | (_, y) => y end")
  | NEUnit -> "()"
  | NEHole -> "?"
  | NESeq (x, y) -> parenthesize ("let _ = " ^ source_of_nexpr x ^ " in " ^ source_of_nexpr y)
