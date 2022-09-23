(* open Ocamlox
open Ocamlox.Ast

let parse (s: string) : expr = 
  let lexbuf = Lexing.from_string s in
  Parser.prog Lexer.read_token lexbuf

let string_of_val (e: expr) : string = 
  match e with
  | Or -> "Or"

let () = print_endline (string_of_val (parse "orchid")) *)