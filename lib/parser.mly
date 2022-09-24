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

expr:
  | n = NUMBER { Number n }
  | id = IDENTIFIER { Identifier id }
  | e1 = expr; PLUS; e2 = expr { Binop (e1, Add, e2) }
  | e1 = expr; OR; e2 = expr { Control (e1, Or, e2) }
  ;
