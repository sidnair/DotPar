/* Part 1 yacc (boolean logic) */

%{
#include <ctype.h>
#include <stdio.h>
%}

%token TRUE
%token FALSE
%token NIL
%token STRUCT
%token NUMBER
%token BOOLEAN
%token CHAR
%token ARRAY
%token IF
%token ELSE
%token ELIF
%token FOR
%token IN
%token APPEND
%token AND
%token OR
%token NOT
%token GT
%token LT
%token GEQ
%token LEQ
%token EQ
%token NEQ
%token ASSIGN
%token PLUS
%token MINUS
%token MULT
%token DIV
%token MOD

/*
%left OR
%left AND
%right NOT
*/

%%

lines : lines expr '\n'
      | lines '\n'
      | /* empty */
      ;

expr : TRUE
     ;

/*primary_expr: identify*/

/*expr : expr OR expr     { $$ = !!($1 || $3); }*/
     /*| expr AND expr    { $$ = !!($1 && $3); }*/
     /*| NOT expr         { $$ = !($2); }*/
     /*| '(' expr ')'     { $$ = $2; }*/
     /*| TRUE             { $$ = 1; }*/
     /*| FALSE            { $$ = 0; }*/
     /*;*/

%%

int yywrap() {
  return 1;
}
