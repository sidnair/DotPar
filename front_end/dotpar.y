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
%token STRING_LITERAL
%token NUM_LITERAL
%token CHAR_LITERAL

/*
%left OR
%left AND
%right NOT
*/

%%

lines: lines constant
     |
     ;

/* CONSTANTS */
constant: char_literal
        | num_literal
        | string_literal
        ;

num_literal: NUM_LITERAL
            ;

char_literal: CHAR_LITERAL
            ;

string_literal: STRING_LITERAL
            ;










/*[>Can compound statement become statement? <]*/
/*statement_list : statement_list statement*/
               /*| statement*/
               /*[>| statement_list declaration<]*/
               /*[>| declaration<]*/
               /*;*/

/*statement: expression_statement*/
         /*| compound_statement*/
         /*| selection_statement*/
         /*[>| iteration_statement<]*/
         /*;*/

/*expression_statement: expression ';'*/
                    /*| ';'*/
                    /*;*/



/*primary_expression: identifier*/
                  /*| constant*/
                  /*| string*/
                  /*| '(' expression ')'*/
                  /*;*/

/*postfix_expression: primary_expression*/
                  /*| postfix_expression '[' expression ']'*/
                  /*[> Function call <]*/
                  /*| postfix_exprssion '(' argument_expression_list_opt ')'*/

/*argument_expression_list: assignment_expression*/
                        /*| argument_expression_list ',' assignment_expression*/
                        /*;*/





/*opt_expression: expression*/
              /*| [> empty statement <]*/
              /*;*/

/*expression: assignment_expression*/
          /*| expression ',' assignment_expression*/
          /*;*/

/*int x, y, z = 4;*/

/*assignment_expression: conditional_expression*/
                     /*| unary_expression assignment_operator assignment_expression*/
                     /*;*/


/*selection_statement: IF '(' expression ')' compound_statement*/
                   /*| IF '(' expression ')' compound_statement ELSE compound_statement*/
                   /*;*/

/*iteration_statement: FOR '(' opt_expression ';' opt_expression ';' opt_expression ')' compound_statement*/
                   /*;*/

/*compound_statement: '{' statement_list '}'*/
                  /*| '{' '}'*/
                  /*;*/


%%

int yywrap() {
  return 1;
}
