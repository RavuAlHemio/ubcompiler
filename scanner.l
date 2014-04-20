/* Ondřej Hošek, e0925631 */

%option noyywrap

%{
#include "common.h"
#include "astmake.h"
#include "parser.h"

#ifdef __GNUC__
static void yyunput(int c, register char * yy_bp) __attribute__((unused));
static int input(void) __attribute__((unused));
#endif
%}

LCASE		[a-z]
UCASE		[A-Z]
DIGIT		[0-9]
HEXDIGIT	[0-9A-Fa-f]
ALPHANUM	[a-zA-Z0-9]
IDFIRST		[a-zA-Z]
IDREST		[a-zA-Z0-9]

%%

";"			return ';';
"("			return '(';
")"			return ')';
","			return ',';
"="			return '=';
"~"			return '~';
"->"			return TOK_SYM_ARROW;
"-"			return '-';
"+"			return '+';
"*"			return '*';
"."			return '.';
">"			return '>';
"_"			return '_';

"cond"			return TOK_KEY_COND;
"end"			return TOK_KEY_END;
"not"			return TOK_KEY_NOT;
"or"			return TOK_KEY_OR;

{DIGIT}+		yylval.numval = lex_dectoint(yytext); return TOK_NUMBER;

{DIGIT}{HEXDIGIT}*"H"	yylval.numval = lex_hextoint(yytext); return TOK_NUMBER;

{IDFIRST}{IDREST}*	yylval.sval = strdup(yytext); return TOK_IDENT;

"//".*			;

[ \t\n\r]+		;

.			lexerrorv("illegal tokens: '%s'", yytext);
