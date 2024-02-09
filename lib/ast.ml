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

type literal =
  | String of string
  | Number of float
  | Boolean of bool
  | Nil

type expr =
  | Literal of literal
  | Identifier of string
  | Binop of expr * op * expr
  | Unop of op * expr
  | Grouping of expr
  | Control of expr * control * expr

type statement = 
  | ExprStmt of expr
  | PrintStmt of expr

type program = statement list