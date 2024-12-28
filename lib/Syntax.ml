type ctor = Ctor of string * int option [@@deriving show]
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
[@@deriving show]

and expr =
  | Unit
  | Int of int
  | Float of float
  | Bool of bool
  | Str of string
  | Builtin of builtin
  | Var of string
  | App of expr * expr list
  | CApp of ctor * expr list
  | Op of string * expr * expr
  | Tup of expr list
  | Arr of expr list
  | Lam of pattern list * expr
  | Let of binding * expr
  | Sel of expr * string
  | If of expr * expr * expr
  | Match of expr * cases
[@@deriving show]

and cases =
  | SwitchCtor of (ctor * expr) list * expr option
  | SwitchBool of (bool * expr) list * expr option
  | MatchPattern of (pattern * expr) list
[@@deriving show]

type ty =
  | TUnit
  | TInt
  | TFloat
  | TBool
  | TApply of ty * ty list
  | TArrow of ty * ty
  | TTuple of ty list
  | TVar of ty ref
  | TNamed of string
  | TNamedVar of string (* this is the surface syntax used during parsing *)
[@@deriving show]

type ty_decl =
  | Enum of string * (string * ty list) list
  | Record of string * (string * ty) list
[@@deriving show]

type stmt = Type of ty_decl | Term of pattern option * expr [@@deriving show]
type prog = stmt list [@@deriving show]

open PPrint

let pp_pattern =
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
  f false

let pp_expr =
  let fc pf ef (p, e) =
    break 1 ^^ group @@ align @@ string "|" ^^ space ^^ pf p ^^ space
    ^^ string "->" ^^ nest 2 @@ break 1 ^^ ef true e
  in
  let fm ef e inf =
    group @@ align @@ string "match" ^^ space ^^ ef false e ^^ space
    ^^ string "with" ^^ inf
  in
  let fd dflt ef =
    match dflt with
    | Some x -> fc (fun _ -> underscore) ef ((), x)
    | None -> empty
  in
  let fl pro lhs rhs tail =
    align @@ group
    @@ (group @@ pro
       ^^ (nest 2 @@ break 1 ^^ align @@ lhs ^^ space ^^ string "=" ^^ space
         ^^ rhs)
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
    | App (fn, []) -> f c fn
    | App (fn, xs) -> f true fn ^^ space ^^ separate_map space (f true) xs |> pp
    | CApp (Ctor (ct, _arity), []) -> string ct
    | CApp (Ctor (ct, _arity), xs) ->
        string ct ^^ space ^^ separate_map space (f true) xs |> pp
    | Op (op, lhs, rhs) ->
        f true lhs ^^ space ^^ string op ^^ space ^^ f true rhs |> pp
    | Tup xs -> separate_map (comma ^^ space) (f true) xs |> parens
    | Lam (xs, e) ->
        group @@ align @@ string "fun" ^^ space
        ^^ separate_map space pp_pattern xs
        ^^ space ^^ string "->" ^^ nest 2 @@ break 1 ^^ f true e
        |> pp
    | Arr xs -> separate_map (semi ^^ space) (f true) xs |> brackets
    | Let (BOne (x, e1), e2) ->
        fl (string "let") (pp_pattern x) (f false e1) (f false e2)
    | Let (BSeq e1, e2) -> align @@ f false e1 ^^ semi ^^ break 1 ^^ f false e2
    | Let (BRec [], _) -> failwith "Empty recursive group"
    | Let (BRec [ (x, e1) ], e2) ->
        fl (string "let rec") (pp_pattern x) (f false e1) (f false e2)
    | Let (BRec _xs, _e2) -> failwith "Not implemented"
    | If (e, e1, e2) ->
        group @@ align
        @@ (group @@ string "if" ^^ nest 2 @@ break 1 ^^ f true e)
        ^^ break 1 ^^ string "then"
        ^^ (nest 2 @@ break 1 ^^ f true e1)
        ^^ break 1 ^^ string "else" ^^ nest 2 @@ break 1 ^^ f true e2
    | Sel (e, field) -> f true e ^^ dot ^^ string field
    | Match (e, MatchPattern cases) ->
        fm f e @@ concat_map (fc pp_pattern f) cases
    | Match (e, SwitchBool (cases, default)) ->
        let g x = string @@ string_of_bool x in
        fm f e @@ concat_map (fc g f) cases ^^ fd default f
    | Match (e, SwitchCtor (cases, default)) ->
        let g x = pp_pattern (PApp (x, Some PAny)) in
        fm f e
        @@ concat_map (fc g f) (List.map (fun (Ctor (c, _), e) -> (c, e)) cases)
        ^^ fd default f
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
    | TApply (ty, []) -> f c ty
    | TApply (ty, tys) ->
        separate_map space (f true) tys ^^ space ^^ f c ty ^^ space |> pp
    | TArrow (ty1, ty2) ->
        f true ty1 ^^ space ^^ string "->" ^^ space ^^ f true ty2 |> pp
    | TTuple tys -> separate_map (comma ^^ space) (f true) tys |> parens
    | TVar { contents = ty } -> f c ty (* TODO: cycle *)
    | TNamedVar name -> string ("'" ^ name)
    | TNamed name -> string name
  in
  f false

let pp_stmt =
  let f (s : stmt) =
    match s with
    | Type (Enum (name, ctors)) ->
        let pp_ctor (ctor, tys) =
          string ctor
          ^^
          match tys with
          | [] -> empty
          | _ -> space ^^ string "of" ^^ space ^^ separate_map space pp_ty tys
        in
        group @@ align @@ string "type" ^^ space ^^ string name ^^ space
        ^^ string "=" ^^ nest 2 @@ break 1 ^^ string "|" ^^ space
        ^^ separate_map (break 1 ^^ string "|" ^^ space) pp_ctor ctors
        ^^ string ";;"
    | Type (Record (_name, _fields)) -> failwith "Not implemented"
    | Term (x, tm) ->
        let name = match x with Some x -> pp_pattern x | None -> underscore in
        string "let" ^^ space ^^ name ^^ space ^^ string "=" ^^ space ^^ group
        @@ pp_expr tm ^^ string ";;"
  in
  f

let pp_prog = separate_map (break 1) pp_stmt
