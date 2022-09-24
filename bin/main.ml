open Ocamlox
open Ocamlox.Ast

let parse (s: string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read_token lexbuf in
  ast

let print_binop (binop: binop) : unit = 
  match binop with
  | Add -> print_string " + "
  | Mult -> print_string " * "

let print_control (control: control) : unit =
  match control with
  | Or -> print_string " or "

let rec print_expr (e: expr) : unit =
  match e with
  | Number n -> print_float n
  | Identifier id -> print_string id
  | Binop (e1, binop, e2) -> print_expr e1; print_binop binop; print_expr e2
  | Control (e1, control, e2) -> print_expr e1; print_control control; print_expr e2

let () = "orchid" |> parse |> print_expr