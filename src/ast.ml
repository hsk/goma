type t =
  | Ty of string
  | TFun of t * t list
  | TPtr of t

type e =
  | EInt of int
  | EBin of e * string * e
  | EPre of string * e
  | ECall of e * e list
  | ECallM of string * e * e list
  | EArr of e * e list
  | EVar of string
  | EString of string
  | EEmpty
  | ECast of t * e

type s = 
  | SBlock of s list
  | SIf of e * s * s
  | SEmpty
  | SExp of e
  | SRet of e
  | SFun of t * string * (t * string) list * s
  | SInclude of string
  | SLet of t * e * e
  | SStruct of string * string * (t * s) list
  | SCon of (t * string) list * e list * s
  | STrait of  string * (t * s) list
  | SImpl of string * string * s list
  | SList of s list
