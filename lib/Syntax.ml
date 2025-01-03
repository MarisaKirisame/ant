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
  | TApply of ty * ty list
  | TArrow of ty * ty
  | TTuple of ty list
  | TVar of ty ref
  | TNamed of string
  | TNamedVar of string (* this is the surface syntax used during parsing *)
[@@deriving show]

type ty_kind =
  | Enum of { params : string list; ctors : (string * ty list) list }
[@@deriving show]

type ty_binding = TBOne of string * ty_kind | TBRec of (string * ty_kind) list
[@@deriving show]

type stmt = Type of ty_binding | Term of pattern option * expr
[@@deriving show]

type prog = stmt list

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
  let fsb pro lhs rhs =
    group @@ pro ^^ nest 2 @@ break 1 ^^ align @@ lhs ^^ space ^^ string "="
    ^^ space ^^ rhs
  in
  let fl lhs rhs tail =
    align @@ group
    @@ fsb (string "let") lhs rhs
    ^^ break 1 ^^ string "in" ^^ break 1 ^^ tail
  in
  let flr lhs rhs others tail =
    align @@ group
    @@ (fsb (string "let rec") lhs rhs
       ^^ concat_map
            (fun (lhs, rhs) -> break 1 ^^ fsb (string "and") lhs rhs)
            others)
    ^^ break 1 ^^ string "in" ^^ break 1 ^^ tail
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
    | App (fn, []) -> f c fn
    | App (fn, xs) -> f true fn ^^ space ^^ separate_map space (f true) xs |> pp
    | Op (op, lhs, rhs) ->
        f true lhs ^^ space ^^ string op ^^ space ^^ f true rhs |> pp
    | Tup xs -> separate_map (comma ^^ space) (f true) xs |> parens
    | Lam (xs, e) ->
        group @@ align @@ string "fun" ^^ space
        ^^ separate_map space pp_pattern xs
        ^^ space ^^ string "->" ^^ nest 2 @@ break 1 ^^ f true e
        |> pp
    | Arr xs -> separate_map (semi ^^ space) (f true) xs |> brackets
    | Let (BOne (x, e1), e2) -> fl (pp_pattern x) (f false e1) (f false e2)
    | Let (BSeq e1, e2) -> align @@ f false e1 ^^ semi ^^ break 1 ^^ f false e2
    | Let (BRec [], _) -> failwith "Empty recursive group"
    | Let (BRec xs, e2) ->
        let lhs, rhs = List.hd xs in
        flr (pp_pattern lhs) (f false rhs)
          (List.map (fun (lhs, rhs) -> (pp_pattern lhs, f false rhs))
          @@ List.tl xs)
          (f false e2)
    | If (e, e1, e2) ->
        group @@ align
        @@ (group @@ string "if" ^^ nest 2 @@ break 1 ^^ f true e)
        ^^ break 1 ^^ string "then"
        ^^ (nest 2 @@ break 1 ^^ f true e1)
        ^^ break 1 ^^ string "else" ^^ nest 2 @@ break 1 ^^ f true e2
    | Sel (e, field) -> f true e ^^ dot ^^ string field
    | Match (e, MatchPattern cases) ->
        fm f e @@ concat_map (fc pp_pattern f) cases
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
    | TApply (ty, [ ty2 ]) -> f true ty ^^ space ^^ f c ty2
    | TApply (ty, tys) ->
        (parens @@ separate_map (string ",") (f true) tys) ^^ space ^^ f c ty
        |> pp
    | TArrow (ty1, ty2) ->
        f true ty1 ^^ space ^^ string "->" ^^ space ^^ f true ty2 |> pp
    | TTuple tys -> separate_map (string "*") (f true) tys |> parens
    | TVar { contents = ty } -> f c ty (* TODO: cycle *)
    | TNamedVar name -> string ("'" ^ name)
    | TNamed name -> string name
  in
  f false

let pp_stmt =
  let f (s : stmt) =
    let pp_ctor (ctor, tys) =
      string ctor
      ^^
      match tys with
      | [] -> empty
      | _ ->
          space ^^ string "of" ^^ space
          ^^ separate_map (space ^^ string "*" ^^ space) pp_ty tys
    in
    let pp_enum is_and name params ctors =
      group @@ align
      @@ (if is_and then string "and" else string "type")
      ^^ space
      ^^ (match params with
         | [] -> empty
         | [ x ] -> string "'" ^^ string x ^^ space
         | _ ->
             (parens
             @@ separate_map (string ",")
                  (fun param -> string "'" ^^ string param)
                  params)
             ^^ space)
      ^^ string name ^^ space ^^ string "=" ^^ nest 2 @@ break 1 ^^ string "|"
      ^^ space
      ^^ separate_map (break 1 ^^ string "|" ^^ space) pp_ctor ctors
    in
    match s with
    | Type (TBOne (name, Enum { params; ctors })) ->
        pp_enum false name params ctors ^^ string ";;"
    | Type (TBRec tbs) ->
        separate_map (break 1)
          (fun (name, Enum { params; ctors }, i) ->
            pp_enum (i <> 0) name params ctors)
          (List.mapi (fun i (name, ty) -> (name, ty, i)) tbs)
        ^^ string ";;"
    | Term (x, tm) ->
        let name = match x with Some x -> pp_pattern x | None -> underscore in
        string "let" ^^ space ^^ name ^^ space ^^ string "=" ^^ space ^^ group
        @@ pp_expr tm ^^ string ";;"
  in
  f

let pp_prog = separate_map (break 1) pp_stmt

module Hashtbl = Core.Hashtbl

type env = { arity : (string, int) Hashtbl.t; ctag : (string, int) Hashtbl.t }

let new_env () : env =
  {
    arity = Hashtbl.create (module Core.String);
    ctag = Hashtbl.create (module Core.String);
  }

let ant_pp_ocaml_adt adt_name ctors =
  string
    ("type ocaml_" ^ adt_name ^ " = "
    ^ String.concat " | "
        (List.map
           (fun (con_name, types) ->
             if List.length types == 0 then con_name
             else
               con_name ^ " of "
               ^ String.concat " * "
                   (List.map
                      (fun ty ->
                        match ty with
                        | TNamed "int" -> "int"
                        | TNamed _ -> "Seq.seq"
                        | _ -> failwith (show_ty ty))
                      types))
           ctors))

let ant_pp_adt_constructors (e : env) adt_name ctors =
  separate_map (break 1)
    (fun (con_name, types) ->
      Hashtbl.add_exn ~key:con_name ~data:(List.length types) e.arity;
      let set_constructor_degree =
        string
          ("let () = Seq.set_constructor_degree" ^ " "
          ^ string_of_int (Hashtbl.length e.ctag)
          ^ "("
          ^ string_of_int (1 - List.length types)
          ^ ")")
      in
      Hashtbl.add_exn ~key:con_name ~data:(Hashtbl.length e.ctag) e.ctag;
      let register_constructor =
        string
          ("let " ^ adt_name ^ "_" ^ con_name ^ " "
          ^ String.concat " "
              (List.mapi (fun i _ -> "x" ^ string_of_int i) types)
          ^ ": Seq.seq = Seq.appends ["
          ^ String.concat ";"
              (("Seq.from_constructor "
               ^ string_of_int (Hashtbl.find_exn e.ctag con_name))
              :: List.mapi
                   (fun i ty ->
                     let argname = "x" ^ string_of_int i in
                     match ty with
                     | TNamed "int" -> "Seq.from_int " ^ argname
                     | TNamed _ -> argname
                     | _ -> failwith (show_ty ty))
                   types)
          ^ "]")
      in
      set_constructor_degree ^^ break 1 ^^ register_constructor)
    ctors

let ant_pp_adt_ffi e adt_name ctors =
  ignore e;
  string
    ("let from_ocaml_" ^ adt_name ^ " x = match x with | "
    ^ String.concat " | "
        (List.map
           (fun (con_name, types) ->
             let args = List.mapi (fun i _ -> "x" ^ string_of_int i) types in
             (if List.length types == 0 then con_name
              else con_name ^ "(" ^ String.concat ", " args ^ ")")
             ^ " -> " ^ adt_name ^ "_" ^ con_name ^ " " ^ String.concat " " args)
           ctors))
  ^^ break 1
  ^^ string
       ("let to_ocaml_" ^ adt_name
      ^ " x = let (h, t) = Option.get (Seq.list_match x) in match \
         (Word.get_value h) with | "
       ^ String.concat " | "
           (List.map
              (fun (con_name, types) ->
                string_of_int (Hashtbl.find_exn e.ctag con_name)
                ^ " -> "
                ^
                if List.length types == 0 then con_name
                else
                  "let ["
                  ^ String.concat ";"
                      (List.mapi (fun i _ -> "x" ^ string_of_int i) types)
                  ^ "] = Seq.splits t in " ^ con_name ^ "("
                  ^ String.concat ","
                      (List.mapi
                         (fun i ty ->
                           match ty with
                           | TNamed "int" ->
                               "Seq.to_int(" ^ "x" ^ string_of_int i ^ ")"
                           | TNamed _ -> "x" ^ string_of_int i
                           | _ -> failwith (show_ty ty))
                         types)
                  ^ ")")
              ctors))

let ant_pp_adt (e : env) adt_name ctors =
  (*force evaluation order via let*)
  let generate_ocaml_adt = ant_pp_ocaml_adt adt_name ctors in
  let generate_adt_constructors = ant_pp_adt_constructors e adt_name ctors in
  generate_ocaml_adt ^^ break 1 ^^ generate_adt_constructors ^^ break 1
  ^^ ant_pp_adt_ffi e adt_name ctors

let rec ant_pp_expr (e : expr) : document =
  match e with
  | Lam (xs, e) ->
      string "fun "
      ^^ separate_map (string " ") pp_pattern xs
      ^^ string " -> " ^^ ant_pp_expr e
  | Match (value, MatchPattern cases) ->
      string "match (" ^^ string "to_ocaml_int_list" ^^ string " "
      ^^ ant_pp_expr value ^^ string ") with | "
      ^^ separate_map
           (break 1 ^^ string "|")
           (fun (pat, expr) ->
             pp_pattern pat ^^ string " -> " ^^ ant_pp_expr expr)
           cases
  | Var x -> string x
  | App (Ctor cname, []) -> string "int_list_" ^^ string cname
  | App (Ctor cname, es) ->
      string "int_list_" ^^ string cname ^^ string "("
      ^^ separate_map (string ",") ant_pp_expr es
      ^^ string ")"
  | App (f, xs) ->
      string "("
      ^^ separate_map (string " ") ant_pp_expr (f :: xs)
      ^^ string ")"
  | Op (op, l, r) ->
      string "(" ^^ ant_pp_expr l ^^ string op ^^ ant_pp_expr r ^^ string ")"
  | Int i -> string "(" ^^ string (string_of_int i) ^^ string ")"
  | _ -> failwith (show_expr e)

let ant_pp_stmt (e : env) (s : stmt) : document =
  match s with
  | Type (TBOne (name, Enum { params = _; ctors })) ->
      (* TODO *) ant_pp_adt e name ctors
  | Type (TBRec _) -> failwith "Not implemented (TODO)"
  | Term (x, tm) ->
      let name = match x with Some x -> pp_pattern x | None -> underscore in
      string "let rec" ^^ space ^^ name ^^ space ^^ string "=" ^^ space ^^ group
      @@ ant_pp_expr tm ^^ string ";;"

let pp_ant x =
  string "open Ant" ^^ break 1
  ^^ string "module Word = Seq.Word"
  ^^ break 1
  ^^ separate_map (break 1) (ant_pp_stmt (new_env ())) x
