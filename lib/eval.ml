open Ast
open Print

let globals = Hashtbl.create 10

let eval_binop = function
  | Literal (Number l), Add, Literal (Number r) -> Literal (Number (l +. r))
  | Literal (Number l), Subtract, Literal (Number r) -> Literal (Number (l -. r))
  | Literal (Number l), Multiply, Literal (Number r) -> Literal (Number (l *. r))
  | Literal (Number l), Divide, Literal (Number r) -> Literal (Number (l /. r))
  | Literal (Number l), Greater, Literal (Number r) -> Literal (Boolean (l > r))
  | Literal (Number l), GreaterEqual, Literal (Number r) -> Literal (Boolean (l >= r))
  | Literal (Number l), Less, Literal (Number r) -> Literal (Boolean (l < r))
  | Literal (Number l), LessEqual, Literal (Number r) -> Literal (Boolean (l <= r))
  | Literal (String l), Add, Literal (String r) -> Literal (String (l ^ r))
  | Literal l, Equal, Literal r -> Literal (Boolean (l = r))
  | Literal l, NotEqual, Literal r -> Literal (Boolean (l <> r))
  | l, _, r -> print l; print r; raise (Invalid_argument "Invalid binary operation")
  (* | _ -> raise (Invalid_argument "Invalid binary operation") *)

let eval_unop = function
  | Negate, Literal (Number n) -> Literal (Number (-.n))
  | Negate, Literal _ -> raise (Invalid_argument "Operand must be a number.")
  | Not, Literal (Boolean b) -> Literal (Boolean (not b))
  | Not, Literal Nil -> Literal (Boolean true)
  | Not, Literal _ -> Literal (Boolean false)
  | _ -> raise (Invalid_argument "Invalid unary operation")

let rec eval_expr = function
  | Assign (id, e) -> let v = eval_expr e in Hashtbl.replace globals id v; v
  | Identifier id -> Hashtbl.find globals id
  | Literal l -> Literal l
  | Binop (l, op, r) -> eval_binop (eval_expr l, op, eval_expr r)
  | Unop (op, e) -> eval_unop (op, eval_expr e)
  | Grouping e -> eval_expr e
  | Control (l, control, r) -> Control (l, control, r)

let eval_stmt = function
  | ExprStmt e -> eval_expr e |> ignore
  | PrintStmt e -> e |> eval_expr |> print

let eval_decl = function
  | VarDecl (id, e) -> Hashtbl.add globals id (eval_expr e)
  | Stmt s -> eval_stmt s

let eval = List.iter eval_decl