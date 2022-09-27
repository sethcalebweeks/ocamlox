%{
  open Ast
%}

%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACE
%token RIGHT_BRACE
%token COMMA
%token DOT
%token MINUS
%token PLUS
%token SEMICOLON
%token SLASH
%token STAR
%token BANG
%token BANG_EQUAL
%token EQUAL
%token EQUAL_EQUAL
%token GREATER
%token GREATER_EQUAL
%token LESS
%token LESS_EQUAL
%token AND
%token CLASS
%token ELSE
%token FALSE
%token FUN
%token FOR
%token IF
%token NIL
%token OR
%token PRINT
%token RETURN
%token SUPER
%token THIS
%token TRUE
%token VAR
%token WHILE
%token EOF


%token <string> IDENTIFIER
%token <string> STRING
%token <float> NUMBER

%start <Ast.expr> prog

%%

prog:
  | e = expr; EOF { e }
  ;

// expr:
//   | n = NUMBER { Number n }
//   | id = IDENTIFIER { Identifier id }
//   | e1 = expr; PLUS; e2 = expr { Binop (e1, Add, e2) }
//   | e1 = expr; GREATER; e2 = expr { Binop (e1, GT, e2) }
//   | e1 = expr; EQUAL; e2 = expr { Binop (e1, EQ, e2) }
//   | e1 = expr; GREATER_EQUAL; e2 = expr { Binop (e1, GTE, e2) }
//   | e1 = expr; OR; e2 = expr { Control (e1, Or, e2) }
//   ;

expr:
  | v = value { Value v }
  | l = expr; binop = binop; r = expr; { Binop (l, binop, r) }
  | unop = unop; e = expr; { Unop (unop, e) }
  | LEFT_PAREN; e = expr; RIGHT_PAREN; { Grouping e }
  ;

binop:
  | EQUAL_EQUAL { Equal }
  | BANG_EQUAL { NotEqual }
  | GREATER { Greater }
  | GREATER_EQUAL { GreaterEqual }
  | LESS { Less }
  | LESS_EQUAL { LessEqual }
  | PLUS { Add }
  | MINUS { Subtract }
  | STAR { Multiply }
  | SLASH { Divide }
  ;

unop:
  | BANG { Not }
  | MINUS { Negate }

value:
  | n = NUMBER { Number n }
  | s = STRING { String s }
  | TRUE { Boolean true }
  | FALSE { Boolean false }
  | NIL { Nil }
  ;