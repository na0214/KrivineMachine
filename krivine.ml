type expr =
  | Var of string
  | App of expr * expr
  | Let of string * expr * expr
  | Obs of expr
  | Coeff of expr
  | Succ of expr
  | Value of value
[@@deriving show]

and value = Lambda of string * expr | Exp of expr | Nat of int
[@@deriving show]

type configure = closure * stack [@@deriving show]

and closure = expr * env [@@deriving show]

and env = Env of (string * closure) list [@@deriving show]

and stack =
  | Empty
  | Closure of closure * stack
  | Coeff of stack
  | Succ of stack
  | Case of expr * string * expr * env * stack
[@@deriving show]