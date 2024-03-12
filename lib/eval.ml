open Ast
open Print

type 'a scope = 
  | Global of 'a
  | Block of 'a scope * 'a

let global = Global (Hashtbl.create 10)

let declare id v = function
  | Global g -> Hashtbl.add g id v
  | Block (_, b) -> Hashtbl.add b id v

let assign id v = function
  | Global g -> Hashtbl.replace g id v; v
  | Block (_, b) -> Hashtbl.replace b id v; v

let rec eval_id id = function
  | Global g -> Hashtbl.find g id
  | Block (parent, b) -> try Hashtbl.find b id with 
    Not_found -> eval_id id parent

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

let rec eval_expr scope = function
  | Assign (id, e) -> assign id (eval_expr scope e) scope
  | Logical (l, op, r) -> eval_logical scope (eval_expr scope l, op, r)
  | Identifier id -> eval_id id scope
  | Literal l -> Literal l
  | Binop (l, op, r) -> eval_binop (eval_expr scope l, op, eval_expr scope r)
  | Unop (op, e) -> eval_unop (op, eval_expr scope e)
  | Grouping e -> eval_expr scope e

and eval_logical scope = function
  | l, Or, r -> if l = Literal (Boolean true) then l else eval_expr scope r
  | l, And, r -> if l = Literal (Boolean false) then l else eval_expr scope r
  | _ -> raise (Invalid_argument "Invalid logical operation")

let rec eval_stmt scope = function
  | ExprStmt e -> eval_expr scope e |> ignore
  | IfStmt (c, t, f) -> 
    if eval_expr scope c = Literal (Boolean true) then eval_stmt scope t
    else eval_stmt scope f
  | PrintStmt e -> e |> eval_expr scope |> print
  | BlockStmt b -> List.iter (eval_decl (Block (scope, Hashtbl.create 10))) b

and eval_decl scope = function
  | VarDecl (id, e) -> declare id (eval_expr scope e) scope
  | Stmt s -> eval_stmt scope s

let eval = eval_decl global |> List.iter 