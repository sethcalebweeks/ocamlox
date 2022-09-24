type binop = 
  | Add
  | Mult

type control = 
  | Or

type value =
  | String of string
  | Number of float
  | Boolean of bool

type expr =
  | Number of float
  | Identifier of string
  | Binop of expr * binop * expr
  | Control of expr * control * expr