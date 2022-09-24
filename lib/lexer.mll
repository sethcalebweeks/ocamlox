{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- {
    pos with pos_bol = lexbuf.lex_curr_pos;
    pos_lnum = pos.pos_lnum + 1;
  }
}

(* Define helper regexes *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let number = '-'? (digit '.')? digit+ 
let id = (alpha) (alpha|digit|'_')*
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read_token = parse
  | "(" { LEFT_PAREN }
  | ")" { RIGHT_PAREN }
  | "{" { LEFT_BRACE }
  | "}" { RIGHT_BRACE }
  | "," { COMMA }
  | "." { DOT }
  | "-" { MINUS }
  | "+" { PLUS }
  | ";" { SEMICOLON }
  | "/" { SLASH }
  | "*" { STAR }
  | "!=" { BANG_EQUAL }
  | "!" { BANG }
  | "==" { EQUAL_EQUAL }
  | "=" { EQUAL }
  | ">=" { GREATER_EQUAL }
  | ">" { GREATER }
  | "<=" { LESS_EQUAL }
  | "<" { LESS }

  (* Keywords *)
  | "and" { AND }
  | "class" { CLASS }
  | "else" { ELSE }
  | "false" { FALSE }
  | "fun" { FUN }
  | "for" { FOR }
  | "if" { IF }
  | "nil" { NIL }
  | "or" { OR }
  | "print" { PRINT }
  | "return" { RETURN }
  | "super" { SUPER }
  | "this" { THIS }
  | "true" { TRUE }
  | "var" { VAR }
  | "while" { WHILE }
  | "//" { read_single_line_comment lexbuf }
  | "/*" { read_multi_line_comment lexbuf }
  | '"' { read_string (Buffer.create 17) lexbuf }
  | number { NUMBER (float_of_string (Lexing.lexeme lexbuf))}
  | id { IDENTIFIER (Lexing.lexeme lexbuf) }
  | whitespace { read_token lexbuf }
  | newline { next_line lexbuf; read_token lexbuf }
  | eof { EOF }
  | _ {raise (SyntaxError ("Lexer - Illegal character: " ^ Lexing.lexeme lexbuf)) }

and read_single_line_comment = parse
  | newline { next_line lexbuf; read_token lexbuf }
  | eof { EOF }
  | _ { read_single_line_comment lexbuf }

and read_multi_line_comment = parse
  | "*/" { read_token lexbuf }
  | newline { next_line lexbuf; read_multi_line_comment lexbuf }
  | eof { raise (SyntaxError ("Lexer - Unexpected EOF - please terminate your comment.")) }
  | _ { read_multi_line_comment lexbuf }

and read_string buf = parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  
  (* Other regexes to handle escaping special characters *)
  
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }