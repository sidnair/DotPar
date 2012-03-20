%{
#include "y.tab.h"
%}

D                         [0-9]

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

{D}*\.{D}+|{D}+           { return NUM_CONSTANT; }
'(\\.|[^'\n])'            { return CHAR_CONSTANT; }
\"(\\.|[^\\"\n])*\"       { return STRING_CONSTANT; }


\n
[ \t\v\n\f]+
.                         { return yytext[0]; }

%%

int multicomment() {
}

int comment() {
}
