%{
#include "y.tab.h"
%}

D                         [0-9]
L                         [A-Za-z]
A                         {D}|{L}
I                         {A}|_

%%

"import"                  {show(); return IMPORT; }
"/*"                      {show(); multicomment(); }
"//"                      {show(); comment(); }
"true"                    {show(); return TRUE; }
"false"                   {show(); return FALSE; }
"nil"                     {show(); return NIL; }
"boolean"                 {show(); return BOOLEAN; }
"char"                    {show(); return CHAR; }
"func"                    {show(); return FUNC; }
"number"                  {show(); return NUMBER; }
"void"                    {show(); return VOID; }
"if"                      {show(); return IF; }
"else"                    {show(); return ELSE; }
"elif"                    {show(); return ELIF; }
"for"                     {show(); return FOR; }
"in"                      {show(); return IN; }
"return"                  {show(); return RETURN; }
"++"                      {show(); return APPEND; }
"&&"                      {show(); return AND; }
"||"                      {show(); return OR; }
"!"                       {show(); return NOT; }
">"                       {show(); return GT; }
"<"                       {show(); return LT; }
">="                      {show(); return GEQ; }
"<="                      {show(); return LEQ; }
"=="                      {show(); return EQ; }
"!="                      {show(); return NEQ; }
"="                       {show(); return ASSIGN; }
"+"                       {show(); return ADD; }
"-"                       {show(); return SUB; }
"*"                       {show(); return MULT; }
"/"                       {show(); return DIV; }
"%"                       {show(); return REM; }

{D}*\.{D}+|{D}+           {show(); return NUM_LITERAL; }
'(\\.|[^'\n])'            {show(); return CHAR_LITERAL; }
\"(\\.|[^\\"\n])*\"       {show(); return STRING_LITERAL; }
{L}{I}*                   {show(); return IDENTIFIER; }

\n
[ \t\v\n\f]+
.                         {show(); return yytext[0]; }

%%

int show() {
//  printf("%s", yytext);
}

int multicomment() {
}

int comment() {
}
