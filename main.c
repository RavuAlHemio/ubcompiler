/**
 * @file main.c
 *
 * Main compiler module. This is where it all comes together.
 *
 * @author Ondřej Hošek, e0925631
 */

#include "common.h"
#include "ast.h"
#include "ast-codeprinter.h"
#include "codegen-llvm.h"

#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

/** The root node of the AST. */
ast_generic_node *ast_root;

/**
 * Print a lexical error and exit with code 1.
 *
 * @param err Error string to print.
 */
void lexerrorv(const char *err, ...)
{
	va_list va;
	va_start(va, err);

	fputs("Scanner error: ", stderr);
	vfprintf(stderr, err, va);
	fputs("\n", stderr);

	exit(1);
}

/**
 * Print a parser error and exit with code 2.
 *
 * @param err Error string to print.
 */
void yyerror(const char *err)
{
	fprintf(stderr, "Parser error: %s\n", err);
	exit(2);
}

numeric_type lex_dectoint(const char *decnum)
{
	char *end;
	long l;

	l = strtol(decnum, &end, 10);
	if (*end != '\0')
	{
		fprintf(stderr, "Scanner error: given decimal number string ends with %c.\n", *end);
		exit(1);
	}

	return (numeric_type)l;
}

numeric_type lex_hextoint(const char *hexnum)
{
	char *end;
	long l;

	l = strtol(hexnum, &end, 16);
	if (*end != 'H')
	{
		fprintf(stderr, "Scanner error: given hex number string ends with %c.\n", *end);
		exit(1);
	}

	return (numeric_type)l;
}

/**
 * The application's main entry point.
 *
 * @param argc Number of arguments.
 * @param argv Argument array.
 * @returns Exit code; zero on success, nonzero on failure.
 */
int main(int argc, char **argv)
{
	output_type_e spitWhat = OT_X86_64_ASM;
	char spitAst = 0, reflux = 0;
	char optimizeAst = 1, optimizeLlvm = 1, paranoid = 0;

	int opt;

	while ((opt = getopt(argc, argv, "hlaonPc")) != -1)
	{
		switch (opt)
		{
			case 'l':
				if (reflux || spitAst)
				{
					fprintf(stderr, "Options -a, -c and -l are mutually exclusive.\n");
					return 1;
				}
				spitWhat = OT_LLVM_BC;
				break;
			case 'a':
				if (reflux || spitWhat == OT_LLVM_BC)
				{
					fprintf(stderr, "Options -a, -c and -l are mutually exclusive.\n");
					return 1;
				}
				spitAst = 1;
				break;
			case 'c':
				if (spitAst || spitWhat == OT_LLVM_BC)
				{
					fprintf(stderr, "Options -a, -c and -l are mutually exclusive.\n");
					return 1;
				}
				reflux = 1;
				break;
			case 'o':
				optimizeAst = 0;
				break;
			case 'n':
				optimizeLlvm = 0;
				break;
			case 'P':
				paranoid = 1;
				break;
			case 'h':
			case '?':
				fprintf(stderr,
					"Usage: %s OPTIONS\n"
					"   -a  don't compile, just print AST\n"
					"   -c  translate AST back to code\n"
					"   -h  display this help text and exit\n"
					"   -l  output (unoptimized) LLVM bitcode in lieu of x86-64 assembly\n"
					"   -n  disable LLVM-based optimizations\n"
					"   -o  disable AST-based optimizations\n"
					"   -P  paranoid mode: ignore type assurances\n",
					argv[0]
				);
				return 1;
		}
	}

	/* call parser */
	yyparse();

	if (ast_root == NULL)
	{
		fprintf(stderr, "Warning: no AST was created. Empty input perhaps?\n");
		exit(0);
	}

	if (!validate_ast(ast_root))
	{
		free_ast(ast_root);
		return 3;
	}

	if (optimizeAst)
	{
		/* do some nice optimization */
		optimize_ast(ast_root);

		/* validate again */
		if (!validate_ast(ast_root))
		{
			fprintf(stderr, "INTERNAL ERROR: Validation of optimized AST failed.\n");
			return 69;
		}
	}

	if (spitAst)
	{
		print_ast(stdout, ast_root);
	}
	else if (reflux)
	{
		print_ast_code(stdout, ast_root);
	}
	else
	{
		codegen_llvm(ast_root, spitWhat, stdout, optimizeLlvm, paranoid);
	}

	free_ast(ast_root);
	return 0;
}
