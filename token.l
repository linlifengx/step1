%{
#include "ast.hpp"
#include "parser.hpp"

extern "C" int yywrap(){}
int charno = 1;

void saveLocation(){
	yylloc.first_line = yylineno;
	yylloc.first_column = charno;
	yylloc.last_line = yylineno;
	charno += yyleng;
	yylloc.last_column = charno-1;
}

%}

%option yylineno
%x CMNT

%%
[\t ]*	{saveLocation();}
\n	{charno = 1;}
\/\/.*\n	{charno = 1;}
"/*"	{saveLocation(); BEGIN CMNT;}
<CMNT>.	{saveLocation();}
<CMNT>\n	{charno = 1;}
<CMNT>"*/"	{saveLocation(); BEGIN INITIAL;}
return	{saveLocation(); return RETURN;}
[a-zA-Z_][a-zA-Z0-9_]*	{saveLocation();return IDENT;}
[0-9]+\.[0-9]+	{saveLocation();return DOUBLE;}
[0-9]+	{saveLocation();return LONG;}
[=\(\)\+\-\*\/;\{\},\[\]]	{saveLocation();return yytext[0];}
.	{saveLocation(); return LEX_ERROR;}
