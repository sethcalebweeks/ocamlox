open Ocamlox
open Ocamlox.Print
open Ocamlox.Eval

let parse s = s |> Lexing.from_string |> Parser.prog Lexer.read_token

let () = "1 - 3" |> parse |> eval |> print