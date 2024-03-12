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
  | Or
  | And

type literal =
  | String of string
  | Number of float
  | Boolean of bool
  | Nil

type expr =
  | Assign of string * expr
  | Logical of expr * op * expr
  | Literal of literal
  | Identifier of string
  | Binop of expr * op * expr
  | Unop of op * expr
  | Grouping of expr

type statement = 
  | ExprStmt of expr
  | IfStmt of expr * statement * statement
  | PrintStmt of expr
  | BlockStmt of declaration list

and declaration = 
  | VarDecl of string * expr
  | Stmt of statement

type program = declaration list