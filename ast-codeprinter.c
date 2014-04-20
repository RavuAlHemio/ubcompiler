/**
 * @file ast-codeprinter.c
 *
 * Abstract Syntax Tree code printer definitions.
 *
 * @author Ondřej Hošek, e0925631
 */

#include "ast-codeprinter.h"

/**
 * Print a part of the AST with a given indentation level to a file.
 *
 * @param f stdio FILE object to print to.
 * @param nodeptr Pointer to an ast_generic_node to print (including descendants).
 * @param level Indentation level.
 */
static void print_subast_code(FILE *f, const void *nodeptr, unsigned int level)
{
	size_t i;
	const ast_generic_node *node = (const ast_generic_node *)nodeptr;
	char *tabs = malloc(level + 1);

	if (tabs == NULL)
	{
		fprintf(stderr, "Can't print AST: malloc failed.\n");
		return;
	}
	for (i = 0; i < level; ++i)
	{
		tabs[i] = '\t';
	}
	tabs[i] = '\0';

	switch (node->node.node_type)
	{
	case AST_NODE_PROGRAM:
		for (i = 0; i < node->program.func_count; ++i)
		{
			print_subast_code(f, node->program.funcs[i], level);
		}
		break;
	case AST_NODE_FUNCDEF:
		fprintf(f, "%s%s(", tabs, node->funcdef.name);
		for (i = 0; i < node->funcdef.argcount; ++i)
		{
			fprintf(f, "%s", node->funcdef.argnames[i]);
			if (i < (node->funcdef.argcount-1))
				fprintf(f, ", ");
		}
		fprintf(f, ") = ");
		print_subast_code(f, node->funcdef.expr, level);
		fprintf(f, ";\n\n");
		break;
	case AST_NODE_OP:
		fprintf(f, "(");
		if (node->op.left != NULL)
		{
			fprintf(f, "(");
			print_subast_code(f, node->op.left, level);
			fprintf(f, ") ");
		}

		switch (node->op.op)
		{
		case AST_OP_ADD:
			fprintf(f, "+"); break;
		case AST_OP_MUL:
			fprintf(f, "*"); break;
		case AST_OP_EQUAL:
			fprintf(f, "="); break;
		case AST_OP_OR:
			fprintf(f, "or"); break;
		case AST_OP_CONS:
			fprintf(f, "."); break;
		case AST_OP_GT:
			fprintf(f, ">"); break;
		case AST_OP_NOT:
			fprintf(f, "not"); break;
		case AST_OP_MINUS:
			fprintf(f, "-"); break;
		default:
			fprintf(f, "?"); break;
		}

		if (node->op.right != NULL)
		{
			fprintf(f, " (");
			print_subast_code(f, node->op.right, level);
			fprintf(f, ")");
		}
		fprintf(f, ")");
		break;
	case AST_NODE_VARACC:
		fprintf(f, "%s", node->varacc.varname);
		break;
	case AST_NODE_FUNCCALL:
		fprintf(f, "%s(", node->funccall.funcname);
		for (i = 0; i < node->funccall.argcount; ++i)
		{
			fprintf(f, "(");
			print_subast_code(f, node->funccall.argexprs[i], level);
			fprintf(f, ")");
			if (i < (node->funccall.argcount-1))
			{
				fprintf(f, ", ");
			}
		}
		fprintf(f, ")\n");
		break;
	case AST_NODE_COND:
		fprintf(f, "(cond\n");
		for (i = 0; i < node->cond.casecount; ++i)
		{
			print_subast_code(f, node->cond.cases[i], level+1);
		}
		fprintf(f, "%s\t->\n%s\t\t", tabs, tabs);
		print_subast_code(f, node->cond.defaultcase, level+2);
		fprintf(f, "\nend)");
		break;
	case AST_NODE_CONDCASE:
		fprintf(f, "%s(", tabs);
		print_subast_code(f, node->condcase.condition, level+1);
		fprintf(f, ") ->\n%s\t(", tabs);
		print_subast_code(f, node->condcase.value, level+1);
		fprintf(f, ");\n");
		break;
	case AST_NODE_NUMBER:
		fprintf(f, "%ld", node->number.val);
		break;
	case AST_NODE_CONDSTRUC:
		fprintf(f, "%s(", tabs);
		print_subast_code(f, node->condstruc.checkme, level+1);
		fprintf(f, ") ~ (");
		print_subast_code(f, node->condstruc.structure, level+1);
		fprintf(f, ") ->\n%s\t(", tabs);
		print_subast_code(f, node->condstruc.value, level+1);
		fprintf(f, ");\n");
		break;
	case AST_NODE_STRUCVARDEF:
		fprintf(f, "%s", node->strucvardef.varname);
		break;
	case AST_NODE_CONSUMER:
		fprintf(f, "_");
		break;
	default:
		fprintf(stderr, "Error while printing AST: unknown node type %04x.\n", node->node.node_type);
		break;
	}

	free(tabs);
}

void print_ast_code(FILE *f, ast_generic_node *node)
{
	print_subast_code(f, node, 0);
}
