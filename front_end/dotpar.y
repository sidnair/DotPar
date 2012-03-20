/* Part 1 yacc (boolean logic) */

%{
#include <ctype.h>
#include <stdio.h>
%}

%token TRUE
%token FALSE
%token NIL
%token BOOLEAN
%token CHAR
%token FUNC
%token NUMBER
%token VOID
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

s: file
 ;

constant: CHAR_LITERAL
        | NUM_LITERAL
        | TRUE
        | FALSE
        ;

argument_expression_list: assignment_expression
                        | argument_expression_list ',' assignment_expression
                        ;

argument_expression_list_opt: argument_expression_list
                            |  /* empty */
                            ;

postfix_expression: primary_expression
                  | postfix_expression '[' expression ']'
                  | postfix_expression '(' argument_expression_list_opt ')'
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

assignment_expression: conditional_expression
                     | postfix_expression ASSIGN assignment_expression
                     ;

expression: assignment_expression
          ;

primary_expression: IDENTIFIER
                  | constant
                  | STRING_LITERAL
                  | '(' expression ')'
                  ;

/* TODO - fix array and func */

type_specifier: type_specifier '[' ']'
              | reg_type
              | func_specifier
              ;

func_specifier: FUNC ':' type_specifier '(' type_list ')' 
              | FUNC ':' type_specifier '(' parameter_list_opt ')'
              ;
reg_type: NUMBER
    | CHAR
    | BOOLEAN
    | VOID
    ;

/* TODO - handle funcs, incl assignment */
declaration: type_specifier declarator ';'
           | type_specifier declarator ASSIGN initializer ';'
           ;

declarator: IDENTIFIER
          | '(' declarator ')'
          ;

type_list: type_specifier
         | type_list ',' type_specifier
         ;

parameter_list_opt: parameter_list
                  | /*empty */
                  ;

parameter_list: parameter_declaration
              | parameter_list ',' parameter_declaration
              ;

parameter_declaration: type_specifier declarator
                     ;


initializer: '[' opt_initializer_list ']'
           | conditional_expression
           ;

opt_initializer_list: initializer_list
                    | /* empty */
                    ;

initializer_list: initializer
                | initializer_list ',' initializer
                ;

expression_statement: expression_opt ';'
                    ;

expression_opt: expression
              | /* empty statement */
              ;

compound_statement: '{' statement_list_opt '}'
                  ;
 Force only compound statements? */
statement_list_opt: statement_list
                  | /* empty */
                  ;

/* Allow statements and declarations to be interwoven. */
statement_list : statement_list statement
               | statement
               | statement_list declaration
               | declaration
               ;

selection_statement: IF '(' expression ')' compound_statement
                   | IF '(' expression ')' compound_statement ELSE compound_statement
                   ;

iteration_statement: FOR '(' expression_opt ';' expression_opt ';' expression_opt ')' compound_statement
                   ;

statement: expression_statement
         | compound_statement
         | selection_statement
         | iteration_statement
         ;

function_definition: FUNC IDENTIFIER ':' type_specifier '(' parameter_list_opt ')' compound_statement
                   ;

external_declaration: function_definition
                    | declaration
                    ;

file: external_declaration
    | file external_declaration
    ;


/* TODOS*/
/*array literals as params - foo(number[] [1, 2, 3]);*/
/*object literals*/
/*while*/
/* *= */



%%

int yywrap() {
  return 1;
}
