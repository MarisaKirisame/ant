type ctor = Ctor of string * int option
type builtin = Builtin of string

type pattern =
  | PAny
  | PInt of int
  | PBool of bool
  | PVar of string
  | PUnit
  | PTup of pattern list
  | PApp of string * pattern option

type binding = BSeq of expr | BOne of pattern * expr | BRec of (pattern * expr) list

and expr =
  | Unit
  | Int of int
  | Float of float
  | Bool of bool
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
  | Match of expr * cases
  | If of expr * expr * expr

and cases =
  | SwitchCtor of (ctor * expr) list * expr option
  | SwitchInt of (int * expr) list * expr option
  | SwitchBool of (bool * expr) list * expr option
  | MatchPattern of (pattern * expr) list

type ty = TUnit | TInt | TFloat | TBool | TArrow of ty list * ty | TTuple of ty list | TVar of ty ref
type stmt = Type of ty | Term of pattern option * expr

type prog = stmt list
