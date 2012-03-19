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

statement_list : statement_list statement
               | statement
               /*| statement_list declaration*/
               /*| declaration*/
               ;

statement: expression_statement
         | compound_statement
         /*| selection_statement*/
         /*| iteration_statement*/
         ;

expression_statement: expression ';'
                    | ';'
                    ;

expression: TRUE
          ;

compound_statement: '{' statement_list '}'
                  | '{' '}'
                  ;


%%

int yywrap() {
  return 1;
}
