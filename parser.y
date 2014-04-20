/* Ondřej Hošek, e0925631 */
/* vim: set ft=yacc: */

%start ProgramOrEmpty

%token ';'
%token '('
%token ')'
%token ','
%token '='
%token '~'
%token TOK_SYM_ARROW
%token '-'
%token '+'
%token '*'
%token '.'
%token '>'
%token '_'

%token TOK_KEY_COND
%token TOK_KEY_END
%token TOK_KEY_NOT
%token TOK_KEY_OR

%token TOK_NUMBER
%token TOK_IDENT

%{
#include "common.h"
#include "astmake.h"

extern ast_generic_node *ast_root;
%}

%union {
	char *sval;
	numeric_type numval;
	ast_generic_node *n;
	struct
	{
		size_t argcount;
		char **argnames;
	} argnames;
	struct
	{
		size_t argcount;
		ast_generic_node **argexprs;
	} args;
	struct
	{
		size_t casecount;
		ast_generic_node **cases;
	} cases;
}

%%

ProgramOrEmpty
	: Program
	|
	;

Program[pout]
	: Program[pin] Funcdef[func] ';'
		{
			ast_root = $<n>pout = ast_make_program_expand(
				$<n>pin,
				$<n>func
			);
		}
	| Funcdef[func] ';'
		{
			ast_root = $<n>pout = ast_make_program($<n>func);
		}
	;

Funcdef[func]
	: TOK_IDENT[funcname]         /* Funktionsname */
	  '(' ParamNames[params] ')'  /* Parameter */
	  '=' Expr[defn]              /* Definition */
		{
			$<n>func = ast_make_funcdef(
				$<sval>funcname,
				$<argnames>params.argcount,
				$<argnames>params.argnames,
				$<n>defn
			);
			free($<sval>funcname);
		}
	| TOK_IDENT[funcname]
	  '(' ')'                     /* parameterlos */
	  '=' Expr[defn]
		{
			$<n>func = ast_make_funcdef(
				$<sval>funcname,
				0, NULL,
				$<n>defn
			);
			free($<sval>funcname);
		}
	;

ParamNames[paramsout]
	: ParamNames[paramsin] ',' TOK_IDENT[paramname]
		{
			$<argnames>paramsout.argcount = $<argnames>paramsin.argcount + 1;
			$<argnames>paramsout.argnames = ast_make_argnames_expand(
				$<argnames>paramsin.argcount,
				$<argnames>paramsin.argnames,
				$<sval>paramname
			);
			free($<sval>paramname);
		}
	| TOK_IDENT[paramname]
		{
			$<argnames>paramsout.argcount = 1;
			$<argnames>paramsout.argnames = ast_make_argnames($<sval>paramname);
			free($<sval>paramname);
		}
	;

Expr[exprout]
	: TOK_KEY_COND CondExprSeries[condseries] TOK_SYM_ARROW Expr[defexpr] TOK_KEY_END
		{
			$<n>exprout = ast_make_cond(
				$<cases>condseries.casecount,
				$<cases>condseries.cases,
				$<n>defexpr
			);
		}
	| TOK_KEY_COND TOK_SYM_ARROW Expr[defexpr] TOK_KEY_END
		{
			/*
			Don't bother making a conditional construction here.
			Just pass the expression upwards.
			*/
			$<n>exprout = $<n>defexpr;
		}
	| Term[term]
		{
			$<n>exprout = $<n>term;
		}
	| MinusNotSeries[mns]
		{
			$<n>exprout = $<n>mns;
		}
	| PlusSeries[ps]
		{
			$<n>exprout = $<n>ps;
		}
	| AsterSeries[as]
		{
			$<n>exprout = $<n>as;
		}
	| OrSeries[os]
		{
			$<n>exprout = $<n>os;
		}
	| DotSeries[ds]
		{
			$<n>exprout = $<n>ds;
		}
	| Term[tleft] '>' Term[tright]
		{
			$<n>exprout = ast_make_binop(AST_OP_GT, $<n>tleft, $<n>tright);
		}
	| Term[tleft] '=' Term[tright]
		{
			$<n>exprout = ast_make_binop(AST_OP_EQUAL, $<n>tleft, $<n>tright);
		}
	;

Term[tout]
	: '(' Expr[exin] ')'
		{
			$<n>tout = $<n>exin;
		}
	| TOK_NUMBER[num]
		{
			$<n>tout = ast_make_number($<numval>num);
		}
	| TOK_IDENT[funcnm] '(' ')'
		{
			$<n>tout = ast_make_funccall(
				$<sval>funcnm,
				0, NULL
			);
			free($<sval>funcnm);
		}
	| TOK_IDENT[funcnm] '(' Params[params] ')'
		{
			$<n>tout = ast_make_funccall(
				$<sval>funcnm,
				$<args>params.argcount, $<args>params.argexprs
			);
			free($<sval>funcnm);
		}
	| TOK_IDENT[varnm]
		{
			$<n>tout = ast_make_varacc($<sval>varnm);
			free($<sval>varnm);
		}
	;

Params[pout]
	: Params[pin] ',' Expr[ex]
		{
			$<args>pout.argcount = $<args>pin.argcount + 1;
			$<args>pout.argexprs = ast_make_argexprs_expand(
				$<args>pin.argcount,
				$<args>pin.argexprs,
				$<n>ex
			);
		}
	| Expr[ex]
		{
			$<args>pout.argcount = 1;
			$<args>pout.argexprs = ast_make_argexprs($<n>ex);
		}
	;

MinusNotSeries[mnout]
	: '-' MinusNotSeries[mnin]
		{
			$<n>mnout = ast_make_unop(
				AST_OP_MINUS, $<n>mnin
			);
		}
	| TOK_KEY_NOT MinusNotSeries[mnin]
		{
			$<n>mnout = ast_make_unop(
				AST_OP_NOT, $<n>mnin
			);
		}
	| '-' Term[tin]
		{
			$<n>mnout = ast_make_unop(
				AST_OP_MINUS, $<n>tin
			);
		}
	| TOK_KEY_NOT Term[tin]
		{
			$<n>mnout = ast_make_unop(
				AST_OP_NOT, $<n>tin
			);
		}
	;

PlusSeries[pout]
	: PlusSeries[pin] '+' Term[tin]
		{
			$<n>pout = ast_make_binop(
				AST_OP_ADD,
				$<n>pin,
				$<n>tin
			);
		}
	| Term[tleft] '+' Term[tright]
		{
			$<n>pout = ast_make_binop(
				AST_OP_ADD,
				$<n>tleft,
				$<n>tright
			);
		}
	;

AsterSeries[aout]
	: AsterSeries[ain] '*' Term[tin]
		{
			$<n>aout = ast_make_binop(
				AST_OP_MUL,
				$<n>ain,
				$<n>tin
			);
		}
	| Term[tleft] '*' Term[tright]
		{
			$<n>aout = ast_make_binop(
				AST_OP_MUL,
				$<n>tleft,
				$<n>tright
			);
		}
	;

OrSeries[oout]
	: OrSeries[oin] TOK_KEY_OR Term[tin]
		{
			$<n>oout = ast_make_binop(
				AST_OP_OR,
				$<n>oin,
				$<n>tin
			);
		}
	| Term[tleft] TOK_KEY_OR Term[tright]
		{
			$<n>oout = ast_make_binop(
				AST_OP_OR,
				$<n>tleft,
				$<n>tright
			);
		}
	;

DotSeries[dout]
/* left-recursion: left-assoc
	: DotSeries TOK_SYM_DOT Term
		@{
			@i @DotSeries.0.n@ = ast_make_binop(
				AST_OP_CONS,
				@DotSeries.1.n@,
				@Term.n@
			);
		@}*/
/* left-recursion: right-assoc */
	: Term[tin] '.' DotSeries[din]
		{
			$<n>dout = ast_make_binop(
				AST_OP_CONS,
				$<n>tin,
				$<n>din
			);
		}
	| Term[tleft] '.' Term[tright]
		{
			$<n>dout = ast_make_binop(
				AST_OP_CONS,
				$<n>tleft,
				$<n>tright
			);
		}
	;

CondExprSeries[cout]
	: CondExprSeries[cin] CondContentExpr[exin]
		{
			$<cases>cout.casecount = $<cases>cin.casecount + 1;
			$<cases>cout.cases = ast_make_condcasearr_expand(
				$<cases>cin.casecount,
				$<cases>cin.cases,
				$<n>exin
			);
		}
	| CondContentExpr[exin]
		{
			$<cases>cout.casecount = 1;
			$<cases>cout.cases = ast_make_condcasearr(
				$<n>exin
			);
		}
	;

CondContentExpr[exout]
	: Expr[exin] '~' Lexpr[modelin] TOK_SYM_ARROW Expr[exdefn] ';'
		{
			$<n>exout = ast_make_condstruc(
				$<n>exin,
				$<n>modelin,
				$<n>exdefn
			);
		}
	| Expr[excond] TOK_SYM_ARROW Expr[exdefn] ';'
		{
			$<n>exout = ast_make_condcase(
				$<n>excond,
				$<n>exdefn
			);
		}
	;

Lexpr[lout]
/* left-recursion = left-associativity
	: Lexpr TOK_SYM_DOT Lterm
		@{
			@i @Lexpr.0.n@ = ast_make_binop(
				AST_OP_CONS,
				@Lexpr.1.n@,
				@Lterm.n@
			);
		@}*/
/* right-recursion = right-associativity */
	: Lterm[termin] '.' Lexpr[lin]
		{
			$<n>lout = ast_make_binop(
				AST_OP_CONS,
				$<n>termin,
				$<n>lin
			);
		}
	| Lterm[termin]
		{
			$<n>lout = $<n>termin;
		}
	;

Lterm[termout]
	: '(' Lexpr[lin] ')'
		{
			$<n>termout = $<n>lin;
		}
	| TOK_IDENT[varnm]          /* Variablendefinition */
		{
			$<n>termout = ast_make_strucvardef(
				$<sval>varnm
			);
			free($<sval>varnm);
		}
	| '_'
		{
			$<n>termout = ast_make_consumer();
		}
	;
