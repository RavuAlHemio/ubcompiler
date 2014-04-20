/**
 * @file ast-codeprinter.h
 *
 * Abstract Syntax Tree code printer definitions.
 *
 * @author Ondřej Hošek, e0925631
 */

#ifndef __AST_CODEPRINTER_H__
#define __AST_CODEPRINTER_H__

#include "ast.h"

/**
 * Print this AST in code form.
 *
 * @param f stdio FILE object to print AST into.
 * @param node Root node of AST to print.
 */
void print_ast_code(FILE *f, ast_generic_node *node);

#endif
