open Ast

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
  | _ -> raise (Invalid_argument "Invalid binary operation")

let eval_unop = function
  | Negate, Literal (Number n) -> Literal (Number (-.n))
  | Negate, Literal _ -> raise (Invalid_argument "Operand must be a number.")
  | Not, Literal (Boolean b) -> Literal (Boolean (not b))
  | Not, Literal Nil -> Literal (Boolean true)
  | Not, Literal _ -> Literal (Boolean false)
  | _ -> raise (Invalid_argument "Invalid unary operation")

let rec eval = function
  | Literal l -> Literal l
  | Identifier id -> Identifier id
  | Binop (l, op, r) -> eval_binop (l, op, r)
  | Unop (op, e) -> eval_unop (op, e)
  | Grouping e -> eval e
  | Control (l, control, r) -> Control (l, control, r)