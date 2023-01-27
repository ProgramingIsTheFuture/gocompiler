type typ =
  | TInt

type expr =
  (* Can be integers *)
  | Const of int
  (* Arithmetic operators: + - * / *)
  | Op of expr * string * expr
  (* Variables *)
  | Var of string
  (* Apply to the function this expr list *)
  | Apply of string * expr list
and stmt =
  | Return of expr
  (* Define variables with name, optional typ and a value *)
  | Let of string * typ option * expr
  (* Name | Params with name type | Return type | Statements for the body function *)
  | Func of string * (string * typ) list * typ option * stmt list

