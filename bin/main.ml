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

let print_value (value: value) : unit = 
  match value with
  | String s -> print_string ("\"" ^ s ^ "\"")
  | Number n -> print_float n
  | Boolean b -> if b then print_string "true" else print_string "false"
  | Nil -> print_string "nil"

let rec print_expr (e: expr) : unit =
  match e with
  | Value v -> print_value v
  | Identifier id -> print_string id
  | Binop (l, op, r) -> print_expr l; print_op op; print_expr r
  | Unop (op, e) -> print_op op; print_expr e
  | Grouping e -> print_string "("; print_expr e; print_string ")"
  | Control (l, control, r) -> print_expr l; print_control control; print_expr r

let () = "-2 * (1 - 3) > true < nil >= false <= \"test\"" |> parse |> print_expr