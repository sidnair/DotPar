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
%token ADD
%token SUB
%token MULT
%token DIV
%token REM
%token STRING_LITERAL
%token NUM_LITERAL
%token CHAR_LITERAL
%token IDENTIFIER

%left OR
%left AND

%left EQ NEQ
%left GT GEQ LT LEQ

%left ADD SUB
%left MULT DIV REM

%right UMINUS

%%

/* CONSTANTS */
lines: lines primary_expression
     |
     ;

num_literal: NUM_LITERAL
            ;

char_literal: CHAR_LITERAL
            ;

constant: char_literal
        | num_literal
        ;

string_literal: STRING_LITERAL
            ;

identifier: IDENTIFIER
          ;


postfix_expression: TRUE
                  | FALSE
                  ;

unary_expression: postfix_expression
                | NOT unary_expression {printf("not\n");}
                | SUB unary_expression %prec UMINUS {printf("negate\n");}
                ;

arithmetic_expression: unary_expression
                      | arithmetic_expression REM arithmetic_expression { printf("rem\n"); }
                      | arithmetic_expression DIV arithmetic_expression {printf("div\n"); }
                      | arithmetic_expression MULT arithmetic_expression {printf("mult\n");}
                      | arithmetic_expression ADD arithmetic_expression {printf("add\n");}
                      | arithmetic_expression SUB arithmetic_expression {printf("sub\n");}
                      ;

relational_expression: arithmetic_expression
                     | relational_expression GEQ relational_expression {printf(">=\n");}
                     | relational_expression GT relational_expression {printf(">\n");}
                     | relational_expression LT relational_expression {printf("<\n");}
                     | relational_expression LEQ relational_expression {printf("<=\n");}
                     | relational_expression EQ relational_expression {printf("==\n");}
                     | relational_expression NEQ relational_expression {printf("!=\n");}
                     ;

conditional_expression: relational_expression
                      | conditional_expression OR conditional_expression {printf("or\n");}
                      | conditional_expression AND conditional_expression {printf("and\n");}
                      ;

assignment_operator: EQ
                   ;

assignment_expression: conditional_expression
                     /*| unary_expression assignment_operator assignment_expression*/
                     ;

expression: assignment_expression
          ;

primary_expression: identifier
                  | constant
                  | string_literal
                  | '(' expression ')'
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
