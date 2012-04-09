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
  /* | NOT unary_expression { }
  | SUB unary_expression %prec UMINUS { } */

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

/* optionally will have parentheses. */
if_comp:
  | IF expression { }
  | { }

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
  /* | { } */

primary_expression:
  | IDENTIFIER { Variable $1 }
  | constant { $1 }
  | LPAREN expression RPAREN { $2 }

type_specifier: 
  /* | type_specifier LBRACK arithmetic_expression RBRACK { }
  | type_specifier LBRACK RBRACK { } */
  | basic_type { Basic_type $1 }
  | VOID { Basic_type Void_type }
  /* | func_specifier { } */

func_specifier:
  | FUNC COLON type_specifier LPAREN type_list RPAREN { }
  | FUNC COLON type_specifier LPAREN parameter_list RPAREN { }

basic_type:
  | NUMBER { Number_type }
  | CHAR { Char_type }
  | BOOLEAN { Boolean_type }

declaration:
  | type_specifier declarator SEMI { }
  | type_specifier declarator ASSIGN initer SEMI { }

declarator:
  | IDENTIFIER { }
  | LPAREN declarator RPAREN { }

type_list:
  | type_specifier { }
  | type_list COMMA type_specifier { }

parameter_list:
  | parameter_declaration { }
  | parameter_list COMMA parameter_declaration { }
  | { }

paren_parameter_list_opt:
  | LPAREN parameter_list RPAREN { }
  | parameter_list { }

parameter_declaration:
  | type_specifier declarator { }

initer:
  | array_expression { }
  | anonymous_function_definition { }

initer_list:
  | initer { }
  | initer_list COMMA initer { }
  | { }

expression_statement:
  | expression SEMI { Expression $1 }

compound_statement:
  | LBRACE statement_list RBRACE { $2 }

/* Allows statements and declarations to be interwoven. */
statement_list :
  | statement_list statement { $2 :: $1 }
  /* | statement_list declaration { }
  | statement_list function_definition { } */
  | { [] }

selection_statement:
  | if_statement elifs_opt else_statement { }

if_statement:
  | IF LPAREN expression RPAREN compound_statement { }

else_statement:
  | ELSE compound_statement { }
  | { }

elifs:
  | ELIF LPAREN expression RPAREN compound_statement { }
  | elifs ELIF LPAREN expression RPAREN compound_statement { }

elifs_opt:
  | elifs { }
  | { }

/* add declarations later */
iteration_statement:
  | FOR LPAREN expression SEMI expression SEMI expression RPAREN compound_statement { }
  /* declaration already has a SEMI */
  | FOR LPAREN declaration expression SEMI expression RPAREN compound_statement { }

jump_statement:
  | RETURN expression SEMI { }

statement:
  | expression_statement { $1 }
  /* | compound_statement { }
  | selection_statement { }
  | iteration_statement { }
  | jump_statement { } */

anonymous_function_definition:
  | FUNC COLON type_specifier LPAREN parameter_list RPAREN compound_statement { }

function_definition:
  | FUNC IDENTIFIER COLON type_specifier LPAREN parameter_list RPAREN
      compound_statement
      { {name=$2; ret_type=$4; body=$8} }

/* Top level */
external_declaration:
  | function_definition { (Function_definition $1) }
  /* | declaration { } */
/* Printf.printf "%s" (string_of_statements ((Function_definition $1) :: Nil)); */
