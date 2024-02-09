open Ast
  
let print_op = function
  | Equal -> print_string " = "
  | NotEqual -> print_string " != "
  | Add -> print_string " + "
  | Multiply -> print_string " * "
  | Subtract -> print_string " - "
  | Divide -> print_string " / "
  | Greater -> print_string " > "
  | GreaterEqual -> print_string " >= "
  | Less -> print_string " < "
  | LessEqual -> print_string " <= "
  | Negate -> print_string " -"
  | Not -> print_string " !"

let print_control = function
  | Or -> print_string " or "

let print_literal = function
  | String s -> print_string ("\"" ^ s ^ "\"")
  | Number n -> print_float n
  | Boolean b -> if b then print_string "true" else print_string "false"
  | Nil -> print_string "nil"

let rec print = function
  | Literal l -> print_literal l
  | Identifier id -> print_string id
  | Binop (l, op, r) -> print l; print_op op; print r
  | Unop (op, e) -> print_op op; print e
  | Grouping e -> print_string "("; print e; print_string ")"
  | Control (l, control, r) -> print l; print_control control; print r
