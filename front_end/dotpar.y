/* Part 1 yacc (boolean logic) */

%{
#include <ctype.h>
#include <stdio.h>
%}

%token IMPORT
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
%token RETURN
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

lines: imports_opt external_declaration
    | lines external_declaration
    ;

imports_opt: imports
           | /* empty */
           ;

imports: imports import_declaration
       | import_declaration
       ;

import_declaration: IMPORT IDENTIFIER ';'
                  ;

constant: CHAR_LITERAL
        | NUM_LITERAL
        | STRING_LITERAL
        | TRUE
        | FALSE
        | NIL
        ;

argument_expression_list_opt: argument_expression_list
                            |  /* empty */
                            ;

argument_expression_list: assignment_expression
                        | argument_expression_list ',' assignment_expression
                        ;

postfix_expression: primary_expression
                  | postfix_expression '[' expression ']'
                  | postfix_expression '(' argument_expression_list_opt ')'
                  ;

unary_expression: postfix_expression
                | NOT unary_expression
                | SUB unary_expression %prec UMINUS
                ;

arithmetic_expression: unary_expression
                      | arithmetic_expression REM arithmetic_expression
                      | arithmetic_expression DIV arithmetic_expression
                      | arithmetic_expression MULT arithmetic_expression
                      | arithmetic_expression ADD arithmetic_expression
                      | arithmetic_expression SUB arithmetic_expression
                      ;

relational_expression: arithmetic_expression
                     | relational_expression GEQ relational_expression
                     | relational_expression GT relational_expression
                     | relational_expression LT relational_expression
                     | relational_expression LEQ relational_expression
                     | relational_expression EQ relational_expression
                     | relational_expression NEQ relational_expression
                     ;

conditional_expression: relational_expression
                      | conditional_expression OR conditional_expression
                      | conditional_expression AND conditional_expression
                      ;

opt_paren_multi_array_expression_list: '(' multi_array_expression_list ')'
                                     | multi_array_expression_list
                                     ;

/* Has at least two array_expressions */
multi_array_expression_list: array_expression ',' array_expression
                           | array_expression ',' array_expression ',' array_expression_list 
                           ;

array_expression_list: array_expression
                     | array_expression_list ',' array_expression
                     ;
array_expression: conditional_expression
                | '[' list_comprehension ']'
                | '[' opt_initializer_list ']'
                ;

if_comp_opt: if_comp
           | /* empty */
           ;

/* optionally will have parentheses. */
if_comp: IF expression
       ;

list_comprehension: array_expression FOR opt_paren_parameter_list IN array_expression if_comp_opt
                  | array_expression FOR opt_paren_parameter_list IN opt_paren_multi_array_expression_list if_comp_opt
                  ;

assignment_expression: array_expression
                     | anonymous_function_definition
                     | postfix_expression ASSIGN array_expression
                     | postfix_expression ASSIGN function_definition
                     | postfix_expression ASSIGN anonymous_function_definition
                     ;

expression: assignment_expression
          ;

primary_expression: IDENTIFIER
                  | constant
                  | '(' expression ')'
                  ;

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

opt_paren_parameter_list: '(' parameter_list ')'
                        | parameter_list
                        ;


parameter_list: parameter_declaration
              | parameter_list ',' parameter_declaration
              ;

parameter_declaration: type_specifier declarator
                     ;


initializer: array_expression
           | anonymous_function_definition
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

statement_list_opt: statement_list
                  | /* empty */
                  ;

/* Allows statements and declarations to be interwoven. */
statement_list : statement_list_opt statement
               | statement_list_opt declaration
               | statement_list_opt function_definition
               ;

selection_statement: if elifs_opt else_opt
                   ;
if: IF '(' expression ')' compound_statement
  ;

else_opt: else
        | /* empty */
        ;

else: ELSE compound_statement;

elifs_opt: elifs
         | /* empty */
         ;

elifs: ELIF '(' expression ')' compound_statement 
     | elifs ELIF '(' expression ')' compound_statement
     ;

iteration_statement: FOR '(' expression_opt ';' expression_opt ';' expression_opt ')' compound_statement
                   ;

jump_statement: RETURN expression_opt ';'
              ;

statement: expression_statement
         | compound_statement
         | selection_statement
         | iteration_statement
         | jump_statement
         ;

anonymous_function_definition: FUNC ':' type_specifier '(' parameter_list_opt ')' compound_statement
                   ;

function_definition: FUNC IDENTIFIER ':' type_specifier '(' parameter_list_opt ')' compound_statement
                   ;

/* Top level */
external_declaration: function_definition
                    | declaration
                    ;

/* TODOS*/
/* array literals as params - foo(number[] [1, 2, 3]);*/
/* objects */
/* object literals*/
/* while*/
/* self-invoking functions */
/* time */
/*type inference*/
/* *= */
/* array append */
/* for..in */
/* for (number i = 0; i < 10; i = i + 1) { */
/* break, continue */
/* java interop */

%%

int yywrap() {
  return 1;
}
