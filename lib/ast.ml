type op = 
  | Add
  | Multiply
  | Divide
  | Subtract
  | Negate
  | Not
  | Greater
  | Equal
  | GreaterEqual
  | Less
  | LessEqual
  | NotEqual

type control = 
  | Or

type value =
  | String of string
  | Number of float
  | Boolean of bool
  | Nil

type expr =
  | Value of value
  | Identifier of string
  | Binop of expr * op * expr
  | Unop of op * expr
  | Grouping of expr
  | Control of expr * control * expr