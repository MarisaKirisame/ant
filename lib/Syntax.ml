open PPrint

type builtin = Builtin of string

type 'a pattern =
  | PAny
  | PInt of int
  | PBool of bool
  | PUnit
  | PVar of string * 'a
  | PTup of 'a pattern list * 'a
  | PCtorApp of string * 'a pattern option * 'a

type field = FName of string | FIndex of int

type 'a binding =
  | BSeq of 'a expr * 'a
  | BOne of 'a pattern * 'a expr * 'a
  | BRec of ('a pattern * 'a expr * 'a) list
  (* the following definitions are not used yet *)
  | BCont of 'a pattern * 'a expr * 'a
  | BRecC of ('a pattern * 'a expr * 'a) list

and 'a expr =
  | Unit
  | Int of int
  | Float of float
  | Bool of bool
  | Str of string
  | Builtin of builtin * 'a
  | Var of string * 'a
  | GVar of string * 'a
  | Ctor of string * 'a
  | App of 'a expr * 'a expr list * 'a
  | Op of string * 'a expr * 'a expr * 'a
  | Tup of 'a expr list * 'a
  | Arr of 'a expr list * 'a
  | Lam of 'a pattern list * 'a expr * 'a
  | Let of 'a binding * 'a expr * 'a
  | Sel of 'a expr * field * 'a
  | If of 'a expr * 'a expr * 'a expr * 'a
  | Match of 'a expr * 'a cases * 'a

and 'a cases = MatchPattern of ('a pattern * 'a expr) list

type 'a ty =
  | TUnit
  | TInt
  | TFloat
  | TBool
  | TApply of 'a ty * 'a ty list
  | TArrow of 'a ty * 'a ty
  | TTuple of 'a ty list
  | TNamed of string
  | TNamedVar of string

type 'a ty_kind = Enum of { params : string list; ctors : (string * 'a ty list * 'a) list }
type 'a ty_binding = TBOne of string * 'a ty_kind | TBRec of (string * 'a ty_kind) list
type 'a stmt = Type of 'a ty_binding | Term of 'a binding
type 'a prog = 'a stmt list * 'a

(* Pretty-printing for source syntax *)
let rec pp_pattern_internal c pat =
  let pp inner = if c then parens inner else inner in
  match pat with
  | PAny -> underscore
  | PInt n -> string (string_of_int n)
  | PBool b -> string (string_of_bool b)
  | PUnit -> string "()"
  | PCtorApp (fn, a, _) -> (
      match a with
      | None -> string fn
      | Some a ->
          let inner = string fn ^^ space ^^ pp_pattern_internal true a in
          pp inner)
  | PTup (xs, _) ->
      let inner = separate_map (comma ^^ space) (pp_pattern_internal true) xs in
      pp inner
  | PVar (x, _) -> string x

(* pp_pattern' adds explicit parenthesis around the tuple *)
let pp_pattern pat = pp_pattern_internal false pat
let pp_pattern' pat = pp_pattern_internal true pat
let pp_field = function FName name -> string name | FIndex index -> string (string_of_int index)

let pp_expr =
  let fc pf ef (p, e) =
    break 1 ^^ group @@ align @@ string "|" ^^ space ^^ pf p ^^ space ^^ string "->" ^^ nest 2 @@ break 1 ^^ ef true e
  in
  let fm ef e inf = group @@ align @@ string "match" ^^ space ^^ ef false e ^^ space ^^ string "with" ^^ inf in
  let fsb pro lhs rhs = group @@ align @@ pro ^^ space ^^ lhs ^^ space ^^ string "=" ^^ nest 2 @@ break 1 ^^ rhs in
  let fl lhs rhs tail =
    align @@ group @@ group (fsb (string "let") lhs rhs ^^ break 1 ^^ string "in") ^^ break 1 ^^ tail
  in
  let flr lhs rhs others tail =
    align @@ group
    @@ group
         ((fsb (string "let rec") lhs rhs ^^ concat_map (fun (lhs, rhs) -> break 1 ^^ fsb (string "and") lhs rhs) others)
         ^^ break 1 ^^ string "in")
    ^^ break 1 ^^ tail
  in
  let rec f c (expr : 'a expr) =
    let pp inner = if c then parens inner else inner in
    match expr with
    | Unit -> string "()"
    | Int n -> string (string_of_int n)
    | Float f -> string (string_of_float f)
    | Bool b -> string (string_of_bool b)
    | Str s -> string (String.escaped s) |> dquotes
    | Builtin (Builtin b, _) -> string b
    | Var (x, _) -> string x
    | Ctor (c, _) -> string c
    | App (Ctor (ct, aux1), xs, aux2) when List.length xs > 1 ->
        (* special case for ctor application. for OCaml compatibility *)
        f c (App (Var (ct, aux1), [ Tup (xs, aux2) ], aux2))
    | App (fn, [], _) -> f c fn
    | App (fn, xs, _) -> f true fn ^^ space ^^ separate_map space (f true) xs |> pp
    | Op (op, lhs, rhs, _) -> f true lhs ^^ space ^^ string op ^^ space ^^ f true rhs |> pp
    | Tup (xs, _) -> separate_map (comma ^^ space) (f true) xs |> parens
    | Lam (xs, e, _) ->
        group @@ align @@ string "fun" ^^ space ^^ separate_map space pp_pattern' xs ^^ space ^^ string "->" ^^ nest 2
        @@ break 1 ^^ f true e
        |> pp
    | Arr (xs, _) -> separate_map (semi ^^ space) (f true) xs |> brackets
    | Let ((BOne (x, e1, _) | BCont (x, e1, _)), e2, _) -> fl (pp_pattern x) (f false e1) (f false e2)
    | Let (BSeq (e1, _), e2, _) -> align @@ f false e1 ^^ semi ^^ break 1 ^^ f false e2
    | Let ((BRec [] | BRecC []), _, _) -> failwith "Empty recursive group"
    | Let ((BRec xs | BRecC xs), e2, _) ->
        let lhs, rhs, _ = List.hd xs in
        let tail_lhs_rhs = List.tl xs in
        let lhs = pp_pattern lhs in
        let rhs = f false rhs in
        let others =
          List.map
            (fun (lhs, rhs, _) ->
              let lhs = pp_pattern lhs in
              let rhs = f false rhs in
              (lhs, rhs))
            tail_lhs_rhs
        in
        flr lhs rhs others (f false e2)
    | GVar (x, _) -> string ("global:" ^ x)
    | If (e, e1, e2, _) ->
        group @@ align
        @@ (group @@ string "if" ^^ nest 2 @@ break 1 ^^ f true e)
        ^^ break 1 ^^ string "then"
        ^^ (nest 2 @@ break 1 ^^ f true e1)
        ^^ break 1 ^^ string "else" ^^ nest 2 @@ break 1 ^^ f true e2
    | Sel (e, field, _) -> f true e ^^ dot ^^ pp_field field
    | Match (e, MatchPattern cases, _) -> fm f e @@ concat_map (fc pp_pattern f) cases
  in
  fun expr -> f false expr

let pp_ty =
  let rec f c (ty : 'a ty) =
    let pp inner = if c then parens inner else inner in
    match ty with
    | TUnit -> string "unit"
    | TInt -> string "int"
    | TFloat -> string "float"
    | TBool -> string "bool"
    | TApply (ty, []) -> f c ty
    | TApply (ty, tys) ->
        if List.length tys > 1 then (parens @@ separate_map (string ",") (f true) tys) ^^ space ^^ f c ty |> pp
        else separate_map (string ",") (f true) tys ^^ space ^^ f c ty |> pp
    | TArrow (ty1, ty2) -> f true ty1 ^^ space ^^ string "->" ^^ space ^^ f true ty2 |> pp
    | TTuple tys -> separate_map (string "*") (f true) tys |> parens
    | TNamed name -> string name
    | TNamedVar name -> string ("'" ^ name)
  in
  fun ty -> f false ty

let pp_stmt =
  let inner (s : 'a stmt) =
    let pp_ctor (ctor, tys, _) =
      string ctor
      ^^
      match tys with
      | [] -> empty
      | _ -> space ^^ string "of" ^^ space ^^ separate_map (space ^^ string "*" ^^ space) pp_ty tys
    in
    let pp_enum is_and name params ctors =
      group @@ align
      @@ (if is_and then string "and" else string "type")
      ^^ space
      ^^ (match params with
         | [] -> empty
         | [ x ] -> string "'" ^^ string x ^^ space
         | _ -> (parens @@ separate_map (string ",") (fun param -> string "'" ^^ string param) params) ^^ space)
      ^^ string name ^^ space ^^ string "=" ^^ nest 2 @@ break 1 ^^ string "|" ^^ space
      ^^ separate_map (break 1 ^^ string "|" ^^ space) pp_ctor ctors
    in
    match s with
    | Type (TBOne (name, Enum { params; ctors })) -> pp_enum false name params ctors ^^ string ";;"
    | Type (TBRec tbs) ->
        separate_map (break 1)
          (fun (name, Enum { params; ctors }, i) -> pp_enum (i <> 0) name params ctors)
          (List.mapi (fun i (name, ty) -> (name, ty, i)) tbs)
        ^^ string ";;"
    | Term (BSeq (tm, _)) ->
        string "let" ^^ space ^^ underscore ^^ space ^^ string "=" ^^ nest 2 @@ break 1 ^^ group @@ pp_expr tm
        ^^ string ";;"
    | Term (BOne (x, tm, _) | BCont (x, tm, _)) ->
        string "let" ^^ space ^^ pp_pattern x ^^ space ^^ string "=" ^^ nest 2 @@ break 1 ^^ group @@ pp_expr tm
        ^^ string ";;"
    | Term (BRec xs | BRecC xs) ->
        string "let rec" ^^ space
        ^^ separate_map
             (space ^^ string "and" ^^ space)
             (fun (x, tm, _) -> pp_pattern x ^^ space ^^ string "=" ^^ nest 2 @@ break 1 ^^ group @@ pp_expr tm)
             xs
        ^^ string ";;"
  in
  inner

let pp_prog (stmts, _) = separate_map (break 1) pp_stmt stmts

let string_of_document doc =
  let buf = Buffer.create 512 in
  PPrint.ToBuffer.pretty 0.8 80 buf doc;
  Buffer.contents buf
