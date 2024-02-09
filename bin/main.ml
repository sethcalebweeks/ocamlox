open Ocamlox
open Ocamlox.Ast

let parse (s: string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read_token lexbuf in
  ast

let print_op (op: op) : unit = 
  match op with
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

let print_control (control: control) : unit =
  match control with
  | Or -> print_string " or "

let print_literal (literal: literal) : unit = 
  match literal with
  | String s -> print_string ("\"" ^ s ^ "\"")
  | Number n -> print_float n
  | Boolean b -> if b then print_string "true" else print_string "false"
  | Nil -> print_string "nil"

let rec print_expr (e: expr) : unit =
  match e with
  | Literal l -> print_literal l
  | Identifier id -> print_string id
  | Binop (l, op, r) -> print_expr l; print_op op; print_expr r
  | Unop (op, e) -> print_op op; print_expr e
  | Grouping e -> print_string "("; print_expr e; print_string ")"
  | Control (l, control, r) -> print_expr l; print_control control; print_expr r

let eval_binop (l: expr) (op: op) (r: expr) : expr =
  match l, op, r with
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

let eval_unop (op: op) (e: expr) : expr =
  match op, e with
  | Negate, Literal (Number n) -> Literal (Number (-.n))
  | Negate, Literal _ -> raise (Invalid_argument "Operand must be a number.")
  | Not, Literal (Boolean b) -> Literal (Boolean (not b))
  | Not, Literal Nil -> Literal (Boolean true)
  | Not, Literal _ -> Literal (Boolean false)
  | _ -> raise (Invalid_argument "Invalid unary operation")

let rec eval_expr (e: expr) : expr =
  match e with
  | Literal l -> Literal l
  | Identifier id -> Identifier id
  | Binop (l, op, r) -> eval_binop l op r
  | Unop (op, e) -> eval_unop op e
  | Grouping e -> eval_expr e
  | Control (l, control, r) -> Control (l, control, r)

let () = "1 - 3" |> parse |> eval_expr |> print_expr