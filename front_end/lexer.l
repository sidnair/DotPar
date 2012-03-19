%{
#include "y.tab.h"
%}

%%

"/*"                      { multicomment(); }
"//"                      { comment(); }
"true"                    { return TRUE; }
"false"                   { return FALSE; }
"nil"                     { return NIL; }
"struct"                  { return STRUCT; }
"number"                  { return NUMBER; }
"boolean"                 { return BOOLEAN; }
"char"                    { return CHAR; }
"array"                   { return ARRAY; }
"if"                      { return IF; }
"else"                    { return ELSE; }
"elif"                    { return ELIF; }
"for"                     { return FOR; }
"in"                      { return IN; }
"++"                      { return APPEND; }
"&&"                      { return AND; }
"||"                      { return OR; }
"!"                       { return NOT; }
">"                       { return GT; }
"<"                       { return LT; }
">="                      { return GEQ; }
"<="                      { return LEQ; }
"=="                      { return EQ; }
"!="                      { return NEQ; }
"="                       { return ASSIGN; }
"+"                       { return PLUS; }
"-"                       { return MINUS; }
"*"                       { return MULT; }
"/"                       { return DIV; }
"%"                       { return MOD; }
\n
[ \t]+
.                         { return yytext[0]; }

%%

int multicomment() {
}

int comment() {
}
