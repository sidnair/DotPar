
%token IMPORT

%token TRUE FALSE NIL

%token BOOLEAN CHAR FUNC NUMBER VOID

%token IF ELSE ELIF FOR IN RETURN

%token SEMI COMMA COLON LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK

%token AND OR NOT GT LT GEQ LEQ EQ NEQ ASSIGN

%token ADD SUB MULT DIV REM

%token  STRING_LITERAL NUM_LITERAL CHAR_LITERAL IDENTIFIER

%left OR
%left AND

%left EQ NEQ
%left GT GEQ LT LEQ

%left ADD SUB
%left MULT DIV REM

%right UMINUS

%start program
%type <int> program

%%

program:
  | lines { 5 }

lines:
  | imports_opt external_declaration { 5 }
  | lines external_declaration { 5 }

imports_opt:
  | imports { 5 }
  | /* empty */ { 5 }

imports:
  | imports import_declaration { 5 }
  | import_declaration { 5 }

import_declaration:
  | IMPORT IDENTIFIER SEMI { 5 }

constant:
  | CHAR_LITERAL { 5 }
  | NUM_LITERAL { 5 }
  | STRING_LITERAL { 5 }
  | TRUE { 5 }
  | FALSE { 5 }
  | NIL { 5 }

argument_expression_list_opt:
  | argument_expression_list { 5 }
  |  /* empty */ { 5 }

argument_expression_list:
  | assignment_expression { 5 }
  | argument_expression_list COMMA assignment_expression { 5 }

postfix_expression:
  | primary_expression { 5 }
  | postfix_expression LBRACK expression RBRACK { 5 }
  | postfix_expression LPAREN argument_expression_list_opt RPAREN { 5 }

unary_expression:
  | postfix_expression { 5 }
  | NOT unary_expression { 5 }
  | SUB unary_expression %prec UMINUS { 5 }

arithmetic_expression:
  | unary_expression { 5 }
  | arithmetic_expression REM arithmetic_expression { 5 }
  | arithmetic_expression DIV arithmetic_expression { 5 }
  | arithmetic_expression MULT arithmetic_expression { 5 }
  | arithmetic_expression ADD arithmetic_expression { 5 }
  | arithmetic_expression SUB arithmetic_expression { 5 }

relational_expression:
  | arithmetic_expression { 5 }
  | relational_expression GEQ relational_expression { 5 }
  | relational_expression GT relational_expression { 5 }
  | relational_expression LT relational_expression { 5 }
  | relational_expression LEQ relational_expression { 5 }
  | relational_expression EQ relational_expression { 5 }
  | relational_expression NEQ relational_expression { 5 }

conditional_expression:
  | relational_expression { 5 }
  | conditional_expression OR conditional_expression { 5 }
  | conditional_expression AND conditional_expression { 5 }

opt_paren_multi_array_expression_list:
  | LPAREN multi_array_expression_list RPAREN { 5 }
  | multi_array_expression_list { 5 }

/* Has at least two array_expressions */
multi_array_expression_list:
  | array_expression COMMA array_expression { 5 }
  | array_expression COMMA array_expression COMMA array_expression_list { 5 }

array_expression_list:
  | array_expression { 5 }
  | array_expression_list COMMA array_expression { 5 }
                     array_expression:
  | conditional_expression { 5 }
  | LBRACK list_comprehension RBRACK { 5 }
  | LBRACK initer_list_opt RBRACK { 5 }

if_comp_opt:
  | if_comp { 5 }
  | /* empty */ { 5 }

/* optionally will have parentheses. */
if_comp:
  | IF expression { 5 }

list_comprehension:
  | array_expression FOR paren_parameter_list_opt IN array_expression
  if_comp_opt { 5 }
  | array_expression FOR paren_parameter_list_opt IN
  opt_paren_multi_array_expression_list if_comp_opt { 5 }

/* look into allowing named function defs as rvalues */
assignment_expression:
  | array_expression { 5 }
  | anonymous_function_definition { 5 }
  | postfix_expression ASSIGN array_expression { 5 }
  | postfix_expression ASSIGN function_definition { 5 }
  | postfix_expression ASSIGN anonymous_function_definition { 5 }

expression:
  | assignment_expression { 5 }

primary_expression:
  | IDENTIFIER { 5 }
  | constant { 5 }
  | LPAREN expression RPAREN { 5 }

type_specifier:
  | type_specifier LBRACK arithmetic_expression RBRACK { 5 }
  | type_specifier LBRACK RBRACK { 5 }
  | basic_type { 5 }
  | VOID { 5 }
  | func_specifier { 5 }

func_specifier:
  | FUNC COLON type_specifier LPAREN type_list RPAREN { 5 }
  | FUNC COLON type_specifier LPAREN parameter_list_opt RPAREN { 5 }

basic_type:
  | NUMBER { 5 }
  | CHAR { 5 }
  | BOOLEAN { 5 }

declaration:
  | type_specifier declarator SEMI { 5 }
  | type_specifier declarator ASSIGN initer SEMI { 5 }

declarator:
  | IDENTIFIER { 5 }
  | LPAREN declarator RPAREN { 5 }

type_list:
  | type_specifier { 5 }
  | type_list COMMA type_specifier { 5 }

parameter_list_opt:
  | parameter_list { 5 }
  | /*empty */ { 5 }

paren_parameter_list_opt:
  | LPAREN parameter_list RPAREN { 5 }
  | parameter_list { 5 }


parameter_list:
  | parameter_declaration { 5 }
  | parameter_list COMMA parameter_declaration { 5 }

parameter_declaration:
  | type_specifier declarator { 5 }


initer:
  | array_expression { 5 }
  | anonymous_function_definition { 5 }

initer_list_opt:
  | initer_list { 5 }
  | /* empty */ { 5 }

initer_list:
  | initer { 5 }
  | initer_list COMMA initer { 5 }

expression_statement:
  | expression_opt SEMI { 5 }

expression_opt:
  | expression { 5 }
  | /* empty statement */ { 5 }

compound_statement:
  | LBRACE statement_list_opt RBRACE { 5 }

statement_list_opt:
  | statement_list { 5 }
  | /* empty */ { 5 }

/* Allows statements and declarations to be interwoven. */
statement_list :
  | statement_list_opt statement { 5 }
  | statement_list_opt declaration { 5 }
  | statement_list_opt function_definition { 5 }

selection_statement:
  | if_statement elifs_opt else_opt { 5 }

if_statement:
  | IF LPAREN expression RPAREN compound_statement { 5 }

else_opt:
  | else_statement { 5 }
  | /* empty */ { 5 }

else_statement:
  | ELSE compound_statement { 5 }

elifs_opt:
  | elifs { 5 }
  | /* empty */ { 5 }

elifs:
  | ELIF LPAREN expression RPAREN compound_statement { 5 }
  | elifs ELIF LPAREN expression RPAREN compound_statement { 5 }

/* add declarations later */
iteration_statement:
  | FOR LPAREN expression_opt SEMI expression_opt SEMI expression_opt RPAREN compound_statement { 5 }

jump_statement:
  | RETURN expression_opt SEMI { 5 }

statement:
  | expression_statement { 5 }
  | compound_statement { 5 }
  | selection_statement { 5 }
  | iteration_statement { 5 }
  | jump_statement { 5 }

anonymous_function_definition:
  | FUNC COLON type_specifier LPAREN parameter_list_opt RPAREN compound_statement { 5 }

function_definition:
  | FUNC IDENTIFIER COLON type_specifier LPAREN parameter_list_opt RPAREN compound_statement { 5 }

/* Top level */
external_declaration:
  | function_definition { 5 }
  | declaration { 5 }

