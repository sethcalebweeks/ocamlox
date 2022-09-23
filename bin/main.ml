open Ocamlox

let parse (s: string) : string = 
  let lexbuf = Lexing.from_string s in
  let _ = Lexer.read_token lexbuf in
  "success"

let () = print_endline (parse "orchid")