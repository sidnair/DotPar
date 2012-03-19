/* Part 1 yacc (boolean logic) */

%{
#include <ctype.h>
#include <stdio.h>
%}

%token TRUE
%token FALSE
%token OR
%token AND
%token NOT

%left OR
%left AND
%right NOT

%%

lines : lines expr '\n'   {
                            if ($2)
                              printf("true\n");
                            else
                              printf("false\n");
                          }
      | lines '\n'
      | /* empty */
      ;

expr : expr OR expr     { $$ = !!($1 || $3); }
     | expr AND expr    { $$ = !!($1 && $3); }
     | NOT expr         { $$ = !($2); }
     | '(' expr ')'     { $$ = $2; }
     | TRUE             { $$ = 1; }
     | FALSE            { $$ = 0; }
     ;

%%

int yywrap() {
  return 1;
}
