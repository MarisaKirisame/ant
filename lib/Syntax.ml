open PPrint

type builtin = Builtin of string [@@deriving show]

type pattern =
  | PAny
  | PInt of int
  | PBool of bool
  | PVar of string
  | PUnit
  | PTup of pattern list
  | PApp of string * pattern option
[@@deriving show]

type binding =
  | BSeq of expr
  | BOne of pattern * expr
  | BRec of (pattern * expr) list
  | BCont of pattern * expr
  | BRecC of (pattern * expr) list
[@@deriving show]

and expr =
  | Unit
  | Int of int
  | Float of float
  | Bool of bool
  | Str of string
  | Builtin of builtin
  | Var of string
  | GVar of string
  | Ctor of string
  | App of expr * expr list
  | Op of string * expr * expr
  | Tup of expr list
  | Arr of expr list
  | Lam of pattern list * expr
  | Let of binding * expr
  | Sel of expr * string
  | If of expr * expr * expr
  | Match of expr * cases
[@@deriving show]

and cases = MatchPattern of (pattern * expr) list [@@deriving show]

type ty =
  | TUnit
  | TInt
  | TFloat
  | TBool
  | TApply of string * ty list
  | TArrow of ty * ty
  | TTuple of ty list
  | TVar of ty ref
  | TNamedVar of string
[@@deriving show]

type ty_kind = Enum of { params : string list; ctors : (string * ty list) list } [@@deriving show]
type ty_binding = TBOne of string * ty_kind | TBRec of (string * ty_kind) list [@@deriving show]
type stmt = Type of ty_binding | Term of pattern option * expr | Fun of string * pattern list * expr [@@deriving show]
type prog = stmt list

(* Pretty-printing for source syntax *)

(* pp_pattern' adds explicit parenthesis around the tuple *)
let pp_pattern, pp_pattern' =
  let rec f c (pat : pattern) =
    let pp inner = if c then parens inner else inner in
    match pat with
    | PAny -> underscore
    | PInt n -> string (string_of_int n)
    | PBool b -> string (string_of_bool b)
    | PUnit -> string "()"
    | PApp (fn, a) -> (
        match a with
        | None -> string fn
        | Some a ->
            let inner = string fn ^^ space ^^ f true a in
            pp inner)
    | PTup xs ->
        let inner = separate_map (comma ^^ space) (f true) xs in
        pp inner
    | PVar x -> string x
  in
  (f false, f true)

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
  let rec f c (expr : expr) =
    let pp inner = if c then parens inner else inner in
    match expr with
    | Unit -> string "()"
    | Int n -> string (string_of_int n)
    | Float f -> string (string_of_float f)
    | Bool b -> string (string_of_bool b)
    | Str s -> string (String.escaped s) |> dquotes
    | Builtin (Builtin b) -> string b
    | Var x -> string x
    | Ctor c -> string c
    | App (Ctor ct, xs) when List.length xs > 1 ->
        (* special case for ctor application. for OCaml compatibility *)
        f c (App (Var ct, [ Tup xs ]))
    | App (fn, []) -> f c fn
    | App (fn, xs) -> f true fn ^^ space ^^ separate_map space (f true) xs |> pp
    | Op (op, lhs, rhs) -> f true lhs ^^ space ^^ string op ^^ space ^^ f true rhs |> pp
    | Tup xs -> separate_map (comma ^^ space) (f true) xs |> parens
    | Lam (xs, e) ->
        group @@ align @@ string "fun" ^^ space ^^ separate_map space pp_pattern' xs ^^ space ^^ string "->" ^^ nest 2
        @@ break 1 ^^ f true e
        |> pp
    | Arr xs -> separate_map (semi ^^ space) (f true) xs |> brackets
    | Let ((BOne (x, e1) | BCont (x, e1)), e2) -> fl (pp_pattern x) (f false e1) (f false e2)
    | Let (BSeq e1, e2) -> align @@ f false e1 ^^ semi ^^ break 1 ^^ f false e2
    | Let ((BRec [] | BRecC []), _) -> failwith "Empty recursive group"
    | Let ((BRec xs | BRecC xs), e2) ->
        let lhs, rhs = List.hd xs in
        let tail_lhs_rhs = List.tl xs in
        let lhs = pp_pattern lhs in
        let rhs = f false rhs in
        let others =
          List.map
            (fun (lhs, rhs) ->
              let lhs = pp_pattern lhs in
              let rhs = f false rhs in
              (lhs, rhs))
            tail_lhs_rhs
        in
        flr lhs rhs others (f false e2)
    | GVar x -> string ("global:" ^ x)
    | If (e, e1, e2) ->
        group @@ align
        @@ (group @@ string "if" ^^ nest 2 @@ break 1 ^^ f true e)
        ^^ break 1 ^^ string "then"
        ^^ (nest 2 @@ break 1 ^^ f true e1)
        ^^ break 1 ^^ string "else" ^^ nest 2 @@ break 1 ^^ f true e2
    | Sel (e, field) -> f true e ^^ dot ^^ string field
    | Match (e, MatchPattern cases) -> fm f e @@ concat_map (fc pp_pattern f) cases
  in
  f false

let pp_ty =
  let rec f c (ty : ty) =
    let pp inner = if c then parens inner else inner in
    match ty with
    | TUnit -> string "unit"
    | TInt -> string "int"
    | TFloat -> string "float"
    | TBool -> string "bool"
    | TApply (ty, []) -> string ty
    | TApply (ty, [ ty2 ]) -> f c ty2 ^^ space ^^ string ty
    | TApply (ty, tys) -> (parens @@ separate_map (string ",") (f true) tys) ^^ space ^^ string ty |> pp
    | TArrow (ty1, ty2) -> f true ty1 ^^ space ^^ string "->" ^^ space ^^ f true ty2 |> pp
    | TTuple tys -> separate_map (string "*") (f true) tys |> parens
    | TVar { contents = ty } -> f c ty (* TODO: cycle *)
    | TNamedVar name -> string ("'" ^ name)
  in
  f false

let pp_stmt =
  let rec inner (s : stmt) =
    let pp_ctor (ctor, tys) =
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
    | Term (x, tm) ->
        let name = match x with Some x -> pp_pattern x | None -> underscore in
        string "let" ^^ space ^^ name ^^ space ^^ string "=" ^^ nest 2 @@ break 1 ^^ group @@ pp_expr tm ^^ string ";;"
    | Fun (name, args, body) ->
        string "let rec" ^^ space ^^ string name ^^ space ^^ separate_map space pp_pattern args ^^ space ^^ string "="
        ^^ nest 2 @@ break 1 ^^ group @@ pp_expr body ^^ string ";;"
  in
  inner

let pp_prog = separate_map (break 1) pp_stmt
