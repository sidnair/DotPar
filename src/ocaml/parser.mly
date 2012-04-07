%{ open Ast %}

%token IMPORT

%token TRUE FALSE NIL

%token BOOLEAN CHAR FUNC NUMBER VOID

%token IF ELSE ELIF FOR IN RETURN

%token SEMI COMMA COLON LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK

%token AND OR NOT GT LT GEQ LEQ EQ NEQ ASSIGN

%token ADD SUB MULT DIV REM

%token  STRING_LITERAL NUM_LITERAL CHAR_LITERAL IDENTIFIER

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
  | lines { Nil }

lines:
  | imports_opt external_declaration { }
  | lines external_declaration { }

imports_opt:
  | imports { }
  | /* empty */ { }

imports:
  | imports import_declaration { }
  | import_declaration { }

import_declaration:
  | IMPORT IDENTIFIER SEMI { }

constant:
  | CHAR_LITERAL { }
  | NUM_LITERAL { }
  | STRING_LITERAL { }
  | TRUE { }
  | FALSE { }
  | NIL { }

argument_expression_list:
  | assignment_expression { }
  | argument_expression_list COMMA assignment_expression { }
  | /* empty */ { }

postfix_expression:
  | primary_expression { }
  | postfix_expression LBRACK expression RBRACK { }
  | postfix_expression LPAREN argument_expression_list RPAREN { }

unary_expression:
  | postfix_expression { }
  | NOT unary_expression { }
  | SUB unary_expression %prec UMINUS { }

arithmetic_expression:
  | unary_expression { }
  | arithmetic_expression REM arithmetic_expression { }
  | arithmetic_expression DIV arithmetic_expression { }
  | arithmetic_expression MULT arithmetic_expression { }
  | arithmetic_expression ADD arithmetic_expression { }
  | arithmetic_expression SUB arithmetic_expression { }

relational_expression:
  | arithmetic_expression { }
  | relational_expression GEQ relational_expression { }
  | relational_expression GT relational_expression { }
  | relational_expression LT relational_expression { }
  | relational_expression LEQ relational_expression { }
  | relational_expression EQ relational_expression { }
  | relational_expression NEQ relational_expression { }

conditional_expression:
  | relational_expression { }
  | conditional_expression OR conditional_expression { }
  | conditional_expression AND conditional_expression { }

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
  | conditional_expression { }
  | LBRACK list_comprehension RBRACK { }
  | LBRACK initer_list RBRACK { }

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
  | array_expression { }
  | anonymous_function_definition { }
  | postfix_expression ASSIGN array_expression { }
  | postfix_expression ASSIGN function_definition { }
  | postfix_expression ASSIGN anonymous_function_definition { }

expression:
  | assignment_expression { }
  | { }

primary_expression:
  | IDENTIFIER { }
  | constant { }
  | LPAREN expression RPAREN { }

type_specifier:
  | type_specifier LBRACK arithmetic_expression RBRACK { }
  | type_specifier LBRACK RBRACK { }
  | basic_type { }
  | VOID { }
  | func_specifier { }

func_specifier:
  | FUNC COLON type_specifier LPAREN type_list RPAREN { }
  | FUNC COLON type_specifier LPAREN parameter_list RPAREN { }

basic_type:
  | NUMBER { }
  | CHAR { }
  | BOOLEAN { }

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
  | expression SEMI { }

compound_statement:
  | LBRACE statement_list RBRACE { }

/* Allows statements and declarations to be interwoven. */
statement_list :
  | statement_list statement { }
  | statement_list declaration { }
  | statement_list function_definition { }
  | { }

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
  | expression_statement { }
  | compound_statement { }
  | selection_statement { }
  | iteration_statement { }
  | jump_statement { }

anonymous_function_definition:
  | FUNC COLON type_specifier LPAREN parameter_list RPAREN compound_statement { }

function_definition:
  | FUNC IDENTIFIER COLON type_specifier LPAREN parameter_list RPAREN compound_statement { }

/* Top level */
external_declaration:
  | function_definition { }
  | declaration { }
