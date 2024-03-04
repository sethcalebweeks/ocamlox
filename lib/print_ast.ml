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

let rec print_expr = function
  | Assign (id, e) -> print_string "Assign {"; print_string id; print_string ", "; print_expr e; print_string "}"
  | Literal l -> print_literal l
  | Identifier id -> print_string "Id {"; print_string id; print_string "}"
  | Binop (l, op, r) -> print_expr l; print_op op; print_expr r
  | Unop (op, e) -> print_op op; print_expr e
  | Grouping e -> print_string "("; print_expr e; print_string ")"
  | Control (l, control, r) -> print_expr l; print_control control; print_expr r

let print_stmt = function
  | ExprStmt e -> print_string "ExprStmt {"; print_expr e; print_string "}\n"
  | PrintStmt e -> print_string "PrintStmt {"; print_expr e; print_string "}\n"
  | BlockStmt _ -> print_string "BlockStmt {}"

let print_decl = function
  | VarDecl (id, e) -> print_string "VarDecl {"; print_string id; print_string ", "; print_expr e; print_string "}\n"
  | Stmt s -> print_string "Stmt {"; print_stmt s; print_string "}\n"

let print_ast = List.iter print_decl
