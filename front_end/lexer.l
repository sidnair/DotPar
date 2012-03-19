/* Part 1 lex (boolean logic) */

%{
#include "y.tab.h"
%}

%%

true          return TRUE;
false         return FALSE;
or            return OR;
and           return AND;
not           return NOT;
\n            return '\n';
[ \t]
.             return yytext[0];

%%
