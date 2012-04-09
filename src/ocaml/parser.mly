%{ open Ast
open Printf %}

%token IMPORT

%token BOOLEAN CHAR FUNC NUMBER VOID

%token IF ELSE ELIF FOR IN RETURN

%token SEMI COMMA COLON LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK

%token AND OR NOT GT LT GEQ LEQ EQ NEQ ASSIGN

%token ADD SUB MULT DIV REM

%token <string> STRING_LITERAL
%token <char> CHAR_LITERAL
%token <float> NUM_LITERAL
%token <string> IDENTIFIER
%token <bool> TRUE
%token <bool> FALSE
%token NIL

%token EOF

%left OR
%left AND

%left EQ NEQ
%left GT GEQ LT LEQ

%left ADD SUB
%left MULT DIV REM

%right UMINUS

%start program
%type <Ast.program> program

%%

program:
  | lines { $1 }

lines:
  | imports_opt external_declaration { [] }
  | lines external_declaration { $2 :: $1 }

/* !!! */
imports_opt:
  | imports { }
  | /* empty */ { }

imports:
  | imports import_declaration { }
  | import_declaration { }

import_declaration:
  | IMPORT IDENTIFIER SEMI { }

constant:
  | CHAR_LITERAL { Char_literal $1 }
  | NUM_LITERAL { Number_literal $1 }
  | STRING_LITERAL { String_literal $1 }
  | TRUE { Boolean_literal $1 }
  | FALSE { Boolean_literal $1 }
  | NIL { Nil_literal }

argument_expression_list:
  | assignment_expression { $1 :: [] }
  | argument_expression_list COMMA assignment_expression { $3 :: $1 }
  | /* empty */ { [] }

postfix_expression:
  | primary_expression { $1 }
  | postfix_expression LBRACK expression RBRACK { Array_access ($1, $3) }
  | postfix_expression LPAREN argument_expression_list RPAREN
      { Function_call ($1, $3) }

unary_expression:
  | postfix_expression { $1 }
  | NOT unary_expression { Unop(Not, $2) }
  | SUB unary_expression %prec UMINUS { Unop(Neg, $2) }

arithmetic_expression:
  | unary_expression { $1 }
  | arithmetic_expression REM arithmetic_expression { Binop ($1,Mod,$3) }
  | arithmetic_expression DIV arithmetic_expression { Binop ($1,Div,$3) }
  | arithmetic_expression MULT arithmetic_expression { Binop ($1,Mult,$3) }
  | arithmetic_expression ADD arithmetic_expression { Binop ($1,Add,$3) }
  | arithmetic_expression SUB arithmetic_expression { Binop ($1,Sub,$3) }

relational_expression:
  | arithmetic_expression { $1 }
  | relational_expression GEQ relational_expression { Binop ($1,Geq,$3) }
  | relational_expression GT relational_expression { Binop ($1,Gt,$3) }
  | relational_expression LT relational_expression { Binop ($1,Lt,$3) }
  | relational_expression LEQ relational_expression { Binop ($1,Leq,$3) }
  | relational_expression EQ relational_expression { Binop ($1,Eq,$3) }
  | relational_expression NEQ relational_expression { Binop ($1,Neq,$3) }

conditional_expression:
  | relational_expression { $1 }
  | conditional_expression OR conditional_expression { Binop ($1,Or,$3) }
  | conditional_expression AND conditional_expression { Binop ($1,And,$3) }

/* !!! */
opt_paren_multi_array_expression_list:
  | LPAREN multi_array_expression_list RPAREN { }
  | multi_array_expression_list { }

/* Has at least two array_expressions */
multi_array_expression_list:
  | array_expression COMMA array_expression { }
  | array_expression COMMA array_expression COMMA array_expression_list { }

array_expression_list:
  | array_expression { }
  | array_expression_list COMMA array_expression { }

array_expression:
  | conditional_expression { $1 }
  /* | LBRACK list_comprehension RBRACK { }
  | LBRACK initer_list RBRACK { } */

/* optionally will have parentheses. part of list comprehension */
if_comp:
  | IF expression { }
  | { }

/* !!! */
list_comprehension:
  | array_expression FOR paren_parameter_list_opt IN array_expression
  if_comp { }
  | array_expression FOR paren_parameter_list_opt IN
  opt_paren_multi_array_expression_list if_comp { }

/* look into allowing named function defs as rvalues */
assignment_expression:
  | array_expression { $1 }
  /* | anonymous_function_definition { } */
  | postfix_expression ASSIGN array_expression { Assignment_expression ($1,$3) }
  /* | postfix_expression ASSIGN function_definition { }
  | postfix_expression ASSIGN anonymous_function_definition { } */

expression:
  | assignment_expression { $1 }
  | { Empty_expression }

primary_expression:
  | IDENTIFIER { Variable $1 }
  | constant { $1 }
  | LPAREN expression RPAREN { $2 }

type_specifier: 
  | type_specifier LBRACK arithmetic_expression RBRACK
      { Fixed_array_type ($1, $3) }
  | type_specifier LBRACK RBRACK { Array_type $1 }
  | basic_type { Basic_type $1 }
  | VOID { Basic_type Void_type }
  | func_specifier { $1 }

func_specifier:
  | FUNC COLON type_specifier LPAREN type_list RPAREN
      { Func_type ($3, $5) }
  | FUNC COLON type_specifier LPAREN parameter_list RPAREN
      { Func_param_type ($3, $5) }

basic_type:
  | NUMBER { Number_type }
  | CHAR { Char_type }
  | BOOLEAN { Boolean_type }

declaration:
  | type_specifier declarator SEMI { Declaration ($1, $2) }
  | type_specifier declarator ASSIGN initer SEMI
      { Declaration_expression ($1, $2, $4) }

declarator:
  | IDENTIFIER { Variable $1 }
  | LPAREN declarator RPAREN { $2 }

type_list:
  | type_specifier { [$1] }
  | type_list COMMA type_specifier { $3 :: $1 }

parameter_list:
  | parameter_declaration { [$1] }
  | parameter_list COMMA parameter_declaration { $3 :: $1 }
  | { [] }

/* this is a list comprehension thing */
paren_parameter_list_opt:
  | LPAREN parameter_list RPAREN { }
  | parameter_list { }

parameter_declaration:
  | type_specifier declarator { Param ($1, $2) }

/* !!! */
initer:
  | array_expression { $1 }
  /* | anonymous_function_definition { $1 } */

initer_list:
  | initer { [$1] }
  | initer_list COMMA initer { $3 :: $1 }
  | { [] }

expression_statement:
  | expression SEMI { Expression $1 }

compound_statement:
  | LBRACE statement_list RBRACE { $2 }

/* !!! */
/* Allows statements and declarations to be interwoven. */
statement_list :
  | statement_list statement { $2 :: $1 }
  /* | statement_list declaration { }
  | statement_list function_definition { } */
  | { [] }

selection_statement:
  | if_statement elifs_opt else_statement
        { Selection {if_cond=$1.if_cond;
                     if_body=$1.if_body;
                     else_body=$3.else_body;
                     elif_conds=$2.if_cond :: $2.elif_conds;
                     elif_bodies=$2.if_body :: $2.elif_bodies;
                   } }

if_statement:
  | IF LPAREN expression RPAREN compound_statement
      { {if_cond=$3;
         if_body=$5;
         else_body=[];
         elif_conds=[];
         elif_bodies=[];
         } }

else_statement:
  | ELSE compound_statement
      { {if_cond=Nil_literal;
         if_body=[];
         else_body=$2;
         elif_conds=[];
         elif_bodies=[];
         } }
  | { {if_cond=Nil_literal;
       if_body=[];
       else_body=[];
       elif_conds=[];
       elif_bodies=[];
     } }

elifs:
  | ELIF LPAREN expression RPAREN compound_statement
      { {if_cond=$3;
         if_body=$5;
         else_body=[];
         elif_conds=[];
         elif_bodies=[];
         } }
  | elifs ELIF LPAREN expression RPAREN compound_statement
      { {if_cond=$4;
         if_body=$6;
         else_body=[];
         elif_conds=$1.if_cond :: $1.elif_conds;
         elif_bodies=$1.if_body :: $1.elif_bodies;
         } }

elifs_opt:
  | elifs { $1 }
  | { {if_cond=Nil_literal;
       if_body=[];
       else_body=[];
       elif_conds=[];
       elif_bodies=[];
     } }

iteration_statement:
  | FOR LPAREN expression SEMI expression SEMI expression RPAREN
      compound_statement
      { Iteration ($3, $5, $7, $9) }
  /* declaration already has a SEMI */
  | FOR LPAREN declaration expression SEMI expression RPAREN
      compound_statement
      { Iteration ($3, $4, $6, $8) }

jump_statement:
  | RETURN expression SEMI { Jump { return=$2; } }

statement:
  | expression_statement { $1 }
  | compound_statement { Statements $1 }
  | selection_statement { $1 }
  | iteration_statement { $1 }
  | jump_statement { $1 }

/* !!! */
anonymous_function_definition:
  | FUNC COLON type_specifier LPAREN parameter_list RPAREN compound_statement
      { }

function_definition:
  | FUNC IDENTIFIER COLON type_specifier LPAREN parameter_list RPAREN
      compound_statement
      { {name=$2; ret_type=$4; body=$8} }

/* Top level */
external_declaration:
  | function_definition { (Function_definition $1) }
  | declaration { Expression $1 }
/* Printf.printf "%s" (string_of_statements ((Function_definition $1) :: Nil)); */
