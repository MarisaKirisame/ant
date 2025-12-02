type nexpr =
  | NEInt of int
  | NEPlus of nexpr * nexpr
  | NELt of nexpr * nexpr
  | NELe of nexpr * nexpr
  | NEGt of nexpr * nexpr
  | NEGe of nexpr * nexpr
  | NEVar of string
  | NEAbs of string * nexpr
  | NEApp of nexpr * nexpr
  | NELet of string * nexpr * nexpr
  | NETrue
  | NEFalse
  | NEIf of nexpr * nexpr * nexpr
  | NENil
  | NECons of nexpr * nexpr
  | NEMatchList of nexpr * nexpr * string * string * nexpr
  | NEFix of string * string * nexpr
  | NEHole
