open Ocamlox
open Ocamlox.Eval
(* open Ocamlox.Print_ast *)

let read_file file = In_channel.with_open_bin file In_channel.input_all

let parse s = s |> Lexing.from_string |> Parser.prog Lexer.read_token

let rec repl () = (
  print_string "\nlox> ";
  read_line () |> parse |> eval;
  repl ();
)

let () = 
  if Array.length Sys.argv > 1 then
    Sys.argv.(1) |> read_file |> parse |> eval
  else
    repl ()