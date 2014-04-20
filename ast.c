/**
 * @file ast.c
 *
 * Abstract Syntax Tree functions.
 *
 * @author Ondřej Hošek, e0925631
 */

#include "ast.h"
#include "astmake.h"

#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#include <assert.h>

/**************
 * VALIDATION *
 **************/

/**
 * Validate a sub-AST.
 *
 * @param node Pointer to an ast_generic_node.
 * @return Sub-AST is valid.
 */
static inline bool validate_subast(void *node)
{
	return validate_ast((ast_generic_node *)node);
}

/**
 * Checks whether a given node is an expression (i.e. returns a value).
 *
 * @param voidnode Pointer to an ast_node.
 * @return true if the node is an expression, false otherwise.
 */
static inline bool is_expression(void *voidnode)
{
	ast_node *node = (ast_node *)voidnode;

	return ((
		node->node_type == AST_NODE_OP ||
		node->node_type == AST_NODE_VARACC ||
		node->node_type == AST_NODE_FUNCCALL ||
		node->node_type == AST_NODE_COND ||
		node->node_type == AST_NODE_NUMBER
	) ? true : false);
}

/**
 * Counts the occurrences of a variable definition in a structural case.
 *
 * @param nd Node within a structural case structure sub-AST.
 * @param varname Name of the variable whose occurrences are to be counted.
 * @return Number of occurrences of definitions of varname in node and
 *	descendants.
 */
static unsigned int vardef_occurrences_struccase(ast_generic_node *nd, const char *varname)
{
	unsigned int left = 0;
	unsigned int right = 0;
	switch (nd->node.node_type)
	{
	case AST_NODE_OP:
		if (nd->op.op != AST_OP_CONS)
		{
			fprintf(stderr, "Invalid AST: Unexpected operation %04x in structure case.\n", nd->op.op);
			return 0;
		}
		left = vardef_occurrences_struccase(nd->op.left, varname);
		right = vardef_occurrences_struccase(nd->op.right, varname);
		return left + right;
	case AST_NODE_STRUCVARDEF:
		if (strcmp(nd->strucvardef.varname, varname) == 0)
		{
			/* found! */
			return 1;
		}
		return 0;
	case AST_NODE_CONSUMER:
		return 0;
	default:
		fprintf(stderr, "Invalid AST: Unexpected node type %04x below structure case.\n", nd->node.node_type);
		return 0;
	}

	return 0;
}

/**
 * Counts the occurrences of a variable definition upward from a node (i.e.
 * visible from this node).
 *
 * @param startnode Node to walk upward from.
 * @param varname Name of the variable whose visible definitions are to be
 *	counted.
 * @return Number of occurrences of definitions of varname in node and
 *	descendants.
 */
static int vardef_occurrences(ast_generic_node *startnode, const char *varname)
{
	unsigned int counter = 0;
	size_t i;
	ast_generic_node *walker = startnode->node.parent;
	ast_generic_node *walkerschild = startnode;
	while (walker != NULL)
	{
		if (walker->node.node_type == AST_NODE_FUNCDEF)
		{
			/* for each argument name in our function */
			for (i = 0; i < walker->funcdef.argcount; ++i)
			{
				if (strcmp(walker->funcdef.argnames[i], varname) == 0)
				{
					/* variable found */
					counter += 1;
				}
			}
			return counter;
		}
		else if (walker->node.node_type == AST_NODE_CONDSTRUC && walkerschild != walker->condstruc.checkme)
		{
			counter += vardef_occurrences_struccase(walker->condstruc.structure, varname);
		}

		walkerschild = walker;
		walker = walker->node.parent;
	}
	fprintf(stderr, "Invalid AST: parent links don't lead us to a function definition.\n");
	return 0;
}

bool validate_ast(ast_generic_node *node)
{
	size_t i, j;

	if (node == NULL)
	{
		fprintf(stderr, "Invalid AST: validate_ast called on a NULL node.\n");
		return false;
	}

	switch (node->node.node_type)
	{
	case AST_NODE_PROGRAM:
		/* for each function definition */
		for (i = 0; i < node->program.func_count; ++i)
		{
			/* check function names */
			for (j = i + 1; j < node->program.func_count; ++j)
			{
				if (strcmp(
					node->program.funcs[i]->name,
					node->program.funcs[j]->name
				) == 0)
				{
					fprintf(stderr, "Warning: More than one function named '%s'.\n", node->program.funcs[i]->name);
					break;
				}
			}

			/* check parent uplink */
			if (node->program.funcs[i]->parent != node)
			{
				fprintf(stderr, "Internal error: Invalid parent uplink.\n");
				return false;
			}

			/* check subtree */
			if (!validate_subast(node->program.funcs[i]))
			{
				return false;
			}
		}
		break;
	case AST_NODE_FUNCDEF:
		/* for each argument */
		for (i = 0; i < node->funcdef.argcount; ++i)
		{
			/* check argument names */
			for (j = i + 1; j < node->funcdef.argcount; ++j)
			{
				if (strcmp(
					node->funcdef.argnames[i],
					node->funcdef.argnames[j]
				) == 0)
				{
					fprintf(stderr, "Invalid AST: Function '%s' has more than one argument named '%s'.\n",
						node->funcdef.name,
						node->funcdef.argnames[i]
					);
					return false;
				}
			}
		}
		/* check parent uplink */
		if (node->funcdef.expr->node.parent != node)
		{
			fprintf(stderr, "Internal error: Invalid parent uplink.\n");
			return false;
		}
		/* check expression subtree */
		if (!validate_subast(node->funcdef.expr))
		{
			return false;
		}
		break;
	case AST_NODE_OP:
		switch (node->op.op)
		{
		case AST_OP_ADD:
		case AST_OP_MUL:
		case AST_OP_EQUAL:
		case AST_OP_OR:
		case AST_OP_CONS:
		case AST_OP_GT:
			/* check operands */
			if (node->op.left == NULL || node->op.right == NULL)
			{
				fprintf(stderr, "Invalid AST: one of binary operation %04x's arguments is NULL\n", node->op.op);
				return false;
			}
			/* check parent uplink */
			if (node->op.left->node.parent != node || node->op.right->node.parent != node)
			{
				fprintf(stderr, "Internal error: Invalid parent uplink.\n");
				return false;
			}
			/* check subtrees */
			if (!validate_subast(node->op.left) || !validate_subast(node->op.right))
			{
				return false;
			}
			break;
		case AST_OP_NOT:
		case AST_OP_MINUS:
			/* check operand */
			if (node->op.operand == NULL || node->op.nevermind != NULL)
			{
				fprintf(stderr, "Invalid AST: operands of a unary operation %04x are wrong.\n", node->op.op);
				return false;
			}
			/* check parent uplink */
			if (node->op.operand->node.parent != node)
			{
				fprintf(stderr, "Internal error: Invalid parent uplink.\n");
				return false;
			}
			/* check subtree */
			if (!validate_subast(node->op.operand))
			{
				return false;
			}
			break;
		}
		break;
	case AST_NODE_VARACC:
	{
		unsigned int occs = vardef_occurrences(node, node->varacc.varname);
		if (occs == 0)
		{
			fprintf(stderr, "Error: Can't find variable '%s'.\n", node->varacc.varname);
			return false;
		}
		else if (occs > 1)
		{
			fprintf(stderr, "Error: Variable '%s' declared in more than one place.\n", node->varacc.varname);
			return false;
		}
		break;
	}
	case AST_NODE_FUNCCALL:
		/* TODO: check if this function is known? */
		return true;
	case AST_NODE_COND:
		/* for each case */
		for (i = 0; i < node->cond.casecount; ++i)
		{
			/* TODO: check whether conditions aren't equivalent? */
			/* check parent uplink */
			if (node->cond.cases[i]->node.parent != node)
			{
				fprintf(stderr, "Internal error: Invalid parent uplink.\n");
				return false;
			}
			/* check subtree */
			if (!validate_subast(node->cond.cases[i]))
			{
				return false;
			}
		}
		if (!validate_subast(node->cond.defaultcase))
		{
			return false;
		}
		break;
	case AST_NODE_CONDCASE:
		/* check parent uplink */
		if (
			node->condcase.condition->node.parent != node ||
			node->condcase.value->node.parent != node
		)
		{
			fprintf(stderr, "Internal error: Invalid parent uplink.\n");
			return false;
		}
		if (
			!validate_subast(node->condcase.condition) ||
			!validate_subast(node->condcase.value)
		)
		{
			return false;
		}
		break;
	case AST_NODE_NUMBER:
		return true;
	case AST_NODE_CONDSTRUC:
		if (
			node->condstruc.checkme->node.parent != node ||
			node->condstruc.structure->node.parent != node ||
			node->condstruc.value->node.parent != node
		)
		{
			fprintf(stderr, "Internal error: Invalid parent uplink.\n");
			return false;
		}
		if (
			!validate_subast(node->condstruc.checkme) ||
			!validate_subast(node->condstruc.structure) ||
			!validate_subast(node->condstruc.value)
		)
		{
			return false;
		}
		break;
	case AST_NODE_STRUCVARDEF:
		if (vardef_occurrences(node, node->strucvardef.varname) > 1)
		{
			fprintf(stderr, "Invalid AST: Structure variable '%s' definition clashes with another variable.\n", node->strucvardef.varname);
			return false;
		}
		break;
	case AST_NODE_CONSUMER:
		return true;
	default:
		fprintf(stderr, "Invalid AST: Unknown node type %04x.\n", node->node.node_type);
		return false;
	}

	return true;
}

/************
 * QUERYING *
 ************/
bool ast_contains_op(ast_generic_node *node, ast_op op)
{
	size_t i;

	if (node == NULL)
		return false;

	switch (node->node.node_type)
	{
	case AST_NODE_PROGRAM:
		for (i = 0; i < node->program.func_count; ++i)
		{
			if (ast_contains_op((ast_generic_node *)node->program.funcs[i], op))
				return true;
		}
		break;
	case AST_NODE_FUNCDEF:
		if (ast_contains_op(node->funcdef.expr, op))
			return true;
		break;
	case AST_NODE_OP:
		if (node->op.op == op)	/* this one is important! */
			return true;
		if (ast_contains_op(node->op.left, op))
			return true;
		if (ast_contains_op(node->op.right, op))
			return true;
		break;
	case AST_NODE_VARACC:
		break;
	case AST_NODE_FUNCCALL:
		for (i = 0; i < node->funccall.argcount; ++i)
		{
			if (ast_contains_op(node->funccall.argexprs[i], op))
				return true;
		}
		break;
	case AST_NODE_COND:
		if (ast_contains_op(node->cond.defaultcase, op))
			return true;
		for (i = 0; i < node->cond.casecount; ++i)
		{
			if (ast_contains_op(node->cond.cases[i], op))
				return true;
		}
		break;
	case AST_NODE_CONDCASE:
		if (ast_contains_op(node->condcase.condition, op))
			return true;
		if (ast_contains_op(node->condcase.value, op))
			return true;
		break;
	case AST_NODE_NUMBER:
		break;
	case AST_NODE_CONDSTRUC:
		/* warning: doesn't check "structure" member! */
		if (ast_contains_op(node->condstruc.checkme, op))
			return true;
		if (ast_contains_op(node->condstruc.value, op))
			return true;
		break;
	case AST_NODE_STRUCVARDEF:
		break;
	case AST_NODE_CONSUMER:
		break;
	default:
		fprintf(stderr, "Error while searching AST for operation: unknown node type %04x.\n", node->node.node_type);
		return false;
	}

	return false;
}

/***********
 * FREEING *
 ***********/

/**
 * Free a sub-AST.
 *
 * @param nodeptr Pointer to an ast_generic_node.
 */
static inline void free_subast(void *nodeptr)
{
	free_ast((ast_generic_node *)nodeptr);
}

void free_ast(ast_generic_node *node)
{
	size_t i;

	if (node == NULL)
		return;

	switch (node->node.node_type)
	{
	case AST_NODE_PROGRAM:
		for (i = 0; i < node->program.func_count; ++i)
		{
			free_subast(node->program.funcs[i]);
		}
		free(node->program.funcs);
		break;
	case AST_NODE_FUNCDEF:
		free_subast(node->funcdef.expr);
		free(node->funcdef.name);
		for (i = 0; i < node->funcdef.argcount; ++i)
		{
			free(node->funcdef.argnames[i]);
		}
		free(node->funcdef.argnames);
		break;
	case AST_NODE_OP:
		free_subast(node->op.left);	/* in union with nevermind */
		free_subast(node->op.right);	/* in union with operand */
		break;
	case AST_NODE_VARACC:
		free(node->varacc.varname);
		break;
	case AST_NODE_FUNCCALL:
		free(node->funccall.funcname);
		for (i = 0; i < node->funccall.argcount; ++i)
		{
			free_subast(node->funccall.argexprs[i]);
		}
		free(node->funccall.argexprs);
		break;
	case AST_NODE_COND:
		free_subast(node->cond.defaultcase);
		for (i = 0; i < node->cond.casecount; ++i)
		{
			free_subast(node->cond.cases[i]);
		}
		free(node->cond.cases);
		break;
	case AST_NODE_CONDCASE:
		free_subast(node->condcase.condition);
		free_subast(node->condcase.value);
		break;
	case AST_NODE_NUMBER:
		break;
	case AST_NODE_CONDSTRUC:
		free_subast(node->condstruc.checkme);
		free_subast(node->condstruc.structure);
		free_subast(node->condstruc.value);
		break;
	case AST_NODE_STRUCVARDEF:
		free(node->strucvardef.varname);
		break;
	case AST_NODE_CONSUMER:
		break;
	default:
		fprintf(stderr, "Error while freeing AST: unknown node type %04x.\n", node->node.node_type);
		return;
	}

	free(node);
}

/************
 * PRINTING *
 ************/

/**
 * Print a part of the AST with a given indentation level to a file.
 *
 * @param f stdio FILE object to print to.
 * @param nodeptr Pointer to an ast_generic_node to print (including descendants).
 * @param level Indentation level.
 */
static void print_subast(FILE *f, const void *nodeptr, unsigned int level)
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
		fprintf(f, "%s(Program [%zu funcs]\n", tabs, node->program.func_count);
		for (i = 0; i < node->program.func_count; ++i)
		{
			print_subast(f, node->program.funcs[i], level+1);
		}
		fprintf(f, "%s)\n", tabs);
		break;
	case AST_NODE_FUNCDEF:
		fprintf(f, "%s(FuncDef [name \"%s\"]\n", tabs, node->funcdef.name);
		for (i = 0; i < node->funcdef.argcount; ++i)
		{
			fprintf(f, "%s\t(Arg [name \"%s\"])\n", tabs, node->funcdef.argnames[i]);
		}
		print_subast(f, node->funcdef.expr, level+1);
		fprintf(f, "%s)\n", tabs);
		break;
	case AST_NODE_OP:
		fprintf(f, "%s(Op [code %04x]\n", tabs, node->op.op);
		if (node->op.left == NULL)
		{
			fprintf(f, "%s\t(NULL)\n", tabs);
		}
		else
		{
			print_subast(f, node->op.left, level+1);
		}
		if (node->op.right == NULL)
		{
			fprintf(f, "%s\t(NULL)\n", tabs);
		}
		else
		{
			print_subast(f, node->op.right, level+1);
		}
		fprintf(f, "%s)\n", tabs);
		break;
	case AST_NODE_VARACC:
		fprintf(f, "%s(VarAccess [name \"%s\"])\n", tabs, node->varacc.varname);
		break;
	case AST_NODE_FUNCCALL:
		fprintf(f, "%s(FuncCall [name \"%s\", argcount %zu]\n", tabs, node->funccall.funcname, node->funccall.argcount);
		for (i = 0; i < node->funccall.argcount; ++i)
		{
			print_subast(f, node->funccall.argexprs[i], level+1);
		}
		fprintf(f, "%s)\n", tabs);
		break;
	case AST_NODE_COND:
		fprintf(f, "%s(Cond [casecount %zu]\n", tabs, node->cond.casecount);
		for (i = 0; i < node->cond.casecount; ++i)
		{
			print_subast(f, node->cond.cases[i], level+1);
		}
		print_subast(f, node->cond.defaultcase, level+1);
		fprintf(f, "%s)\n", tabs);
		break;
	case AST_NODE_CONDCASE:
		fprintf(f, "%s(CondCase\n", tabs);
		print_subast(f, node->condcase.condition, level+1);
		print_subast(f, node->condcase.value, level+1);
		fprintf(f, "%s)\n", tabs);
		break;
	case AST_NODE_NUMBER:
		fprintf(f, "%s(Number %ld)\n", tabs, node->number.val);
		break;
	case AST_NODE_CONDSTRUC:
		fprintf(f, "%s(CondStruc\n", tabs);
		print_subast(f, node->condstruc.checkme, level+1);
		print_subast(f, node->condstruc.structure, level+1);
		print_subast(f, node->condstruc.value, level+1);
		fprintf(f, "%s)\n", tabs);
		break;
	case AST_NODE_STRUCVARDEF:
		fprintf(f, "%s(StrucVarDef [name \"%s\"])\n", tabs, node->strucvardef.varname);
		break;
	case AST_NODE_CONSUMER:
		fprintf(f, "%s(Consumer [_])\n", tabs);
		break;
	default:
		fprintf(stderr, "Error while printing AST: unknown node type %04x.\n", node->node.node_type);
		break;
	}

	free(tabs);
}

void print_ast(FILE *f, ast_generic_node *node)
{
	print_subast(f, node, 0);
}

/****************
 * OPTIMIZATION *
 ****************/

/**
 * Replace a child of the given node with a different one.
 *
 * @param parent The node whose children should be swizzled.
 * @param oldchild The current child of parent.
 * @param newchild The child that should replace oldchild as a child of parent.
 */
static void replace_child(ast_generic_node *parent, ast_generic_node *oldchild, ast_generic_node *newchild)
{
	size_t i;
	char replaced = 0;

	switch (parent->node.node_type)
	{
		case AST_NODE_PROGRAM:
		{
			for (i = 0; i < parent->program.func_count; ++i)
			{
				if ((ast_generic_node *)parent->program.funcs[i] == oldchild)
				{
					newchild->node.parent = parent;
					parent->program.funcs[i] = (ast_node_funcdef *)newchild;
					replaced = 1;
				}
			}
			break;
		}
		case AST_NODE_FUNCDEF:
		{
			if (parent->funcdef.expr == oldchild)
			{
				newchild->node.parent = parent;
				parent->funcdef.expr = newchild;
				replaced = 1;
			}
			break;
		}
		case AST_NODE_OP:
		{
			if (parent->op.left == oldchild)
			{
				newchild->node.parent = parent;
				parent->op.left = newchild;
				replaced = 1;
			}
			if (parent->op.right == oldchild)
			{
				newchild->node.parent = parent;
				parent->op.right = newchild;
				replaced = 1;
			}
			break;
		}
		case AST_NODE_FUNCCALL:
		{
			for (i = 0; i < parent->funccall.argcount; ++i)
			{
				if (parent->funccall.argexprs[i] == oldchild)
				{
					newchild->node.parent = parent;
					parent->funccall.argexprs[i] = newchild;
					replaced = 1;
				}
			}
			break;
		}
		case AST_NODE_COND:
		{
			for (i = 0; i < parent->cond.casecount; ++i)
			{
				if (parent->cond.cases[i] == oldchild)
				{
					newchild->node.parent = parent;
					parent->cond.cases[i] = newchild;
					replaced = 1;
				}
			}

			if (parent->cond.defaultcase == oldchild)
			{
				newchild->node.parent = parent;
				parent->cond.defaultcase = newchild;
				replaced = 1;
			}
			break;
		}
		case AST_NODE_CONDCASE:
		{
			if (parent->condcase.condition == oldchild)
			{
				newchild->node.parent = parent;
				parent->condcase.condition = newchild;
				replaced = 1;
			}
			if (parent->condcase.value == oldchild)
			{
				newchild->node.parent = parent;
				parent->condcase.value = newchild;
				replaced = 1;
			}
			break;
		}
		case AST_NODE_CONDSTRUC:
		{
			if (parent->condstruc.checkme == oldchild)
			{
				newchild->node.parent = parent;
				parent->condstruc.checkme = newchild;
				replaced = 1;
			}
			if (parent->condstruc.structure == oldchild)
			{
				newchild->node.parent = parent;
				parent->condstruc.structure = newchild;
				replaced = 1;
			}
			if (parent->condstruc.value == oldchild)
			{
				newchild->node.parent = parent;
				parent->condstruc.value = newchild;
				replaced = 1;
			}
			break;
		}
		case AST_NODE_STRUCVARDEF:
		case AST_NODE_VARACC:
		case AST_NODE_NUMBER:
		default:
			fprintf(stderr, "Warning: asked to replace a child of a node that can't be a parent.\n");
			return;
	}

	if (!replaced)
	{
		fprintf(stderr, "Warning: node to replace not found in alleged parent.\n");
	}
}

/**
 * Optimization: constant folding (evaluate operations on constants)
 *
 * @param nd Operation node to attempt constant folding on.
 * @return 1 if constants were folded, 0 otherwise.
 */
static char optimize_op_constfold(ast_generic_node *nd)
{
	assert(nd->node.node_type == AST_NODE_OP && "Node isn't an operation node.");

	/* CONSTANT FOLDING */
	if (nd->op.op & 0x1000)
	{
		/* unary operation */
		ast_generic_node *operand = nd->op.operand;
		if (operand != NULL && operand->node.node_type == AST_NODE_NUMBER)
		{
			/* constant folding go! */
			char go = 0;
			numeric_type newv = 0;

			switch (nd->op.op)
			{
				case AST_OP_NOT:
				{
					go = 1;
					newv = ~operand->number.val;
					break;
				}
				case AST_OP_MINUS:
				{
					go = 1;
					newv = -operand->number.val;
					break;
				}
				default:
					break;
			}

			if (go)
			{
				ast_generic_node *num = ast_make_number(newv);
				replace_child(nd->node.parent, nd, num);
				free_ast(nd);

				return 1;
			}
		}
	}
	else
	{
		/* binary operation */
		ast_generic_node *left = nd->op.left;
		ast_generic_node *right = nd->op.right;
		if (
			left != NULL && left->node.node_type == AST_NODE_NUMBER &&
			right != NULL && right->node.node_type == AST_NODE_NUMBER
		)
		{
			/* constant folding go! */
			char go = 0;
			numeric_type newv = 0;

			switch (nd->op.op)
			{
				case AST_OP_ADD:
				{
					go = 1;
					newv = left->number.val + right->number.val;
					break;
				}
				case AST_OP_MUL:
				{
					go = 1;
					newv = left->number.val * right->number.val;
					break;
				}
				case AST_OP_EQUAL:
				{
					go = 1;
					newv = (left->number.val == right->number.val) ? 0xFFFFFFFFFFFFFFFF : 0x0;
					break;
				}
				case AST_OP_OR:
				{
					go = 1;
					newv = (left->number.val | right->number.val);
					break;
				}
				case AST_OP_GT:
				{
					go = 1;
					newv = (left->number.val > right->number.val) ? 0xFFFFFFFFFFFFFFFF : 0x0;
					break;
				}
				default:
					break;
			}

			if (go)
			{
				ast_generic_node *num = ast_make_number(newv);
				replace_child(nd->node.parent, nd, num);
				free_ast(nd);

				return 1;
			}
		}
	}

	return 0;
}

/**
 * Optimization: elimination of complementary unaries (operator chain reduction)
 *
 * @details Reduces not(not(x)) and -(-(y)) to x and y, respectively.
 *
 * @param nd Operation node to attempt elimination on.
 * @param operation Unary operation to attempt elimination on.
 */
static void optimize_op_complunaries(ast_generic_node *nd, ast_op operation)
{
	ast_generic_node *operand;

	if (nd->node.node_type != AST_NODE_OP || nd->op.op != operation)
	{
		return;
	}

	operand = nd->op.operand;

	if (
		operand == NULL ||
		operand->node.node_type != AST_NODE_OP ||
		operand->op.op != operation
	)
	{
		return;
	}

	/* okay, it's twice the same unary; reduce this */
	replace_child(nd->node.parent, nd, operand->op.operand);
	free(nd);
	free(operand);
}

/**
 * Optimize an operation node.
 *
 * @param nd Operation node to optimize.
 */
static void optimize_op(ast_generic_node *nd)
{
	if (nd->node.node_type != AST_NODE_OP)
	{
		fprintf(stderr, "Warning: optimize_op received non-op node.\n");
		return;
	}

	if (optimize_op_constfold(nd))
	{
		/* the node has been folded and is now invalid. */
		return;
	}
	optimize_op_complunaries(nd, AST_OP_MINUS);
	optimize_op_complunaries(nd, AST_OP_NOT);
}

/**
 * Optimize a conditional block by removing redundant cases.
 *
 * @details If a case's condition is a constant zero (false), this operation
 *	removes it from the conditional block.
 *
 * @param node Conditional block node whose cases to prune.
 */
static void optimize_redundant_cases(ast_generic_node *node)
{
	size_t i, newcasecount = 0;
	ast_generic_node **newcases;

	if (node->node.node_type != AST_NODE_COND)
	{
		fprintf(stderr, "Warning: optimize_redundant_cases received non-cond node.\n");
		return;
	}

	newcases = malloc(node->cond.casecount * sizeof(ast_generic_node *));
	if (newcases == NULL)
	{
		fprintf(stderr, "Error: couldn't allocate memory for new cases array.\n");
		abort();
	}

	for (i = 0; i < node->cond.casecount; ++i)
	{
		ast_generic_node *casend = node->cond.cases[i];

		if (casend->node.node_type == AST_NODE_CONDCASE)
		{
			ast_generic_node *casecond = casend->condcase.condition;

			/* if, after constant folding, the value is a numeric zero, this case will never execute and can be optimized away */
			if (casecond->node.node_type != AST_NODE_NUMBER || casecond->number.val != 0)
			{
				newcases[newcasecount] = casend;
				++newcasecount;
			}
		}
		else if (casend->node.node_type == AST_NODE_CONDSTRUC)
		{
			/* it's kinda hard to optimize these; leave them alone */
			newcases[newcasecount] = casend;
			++newcasecount;
		}
	}

	/* reassign cases array */
	free(node->cond.cases);
	node->cond.cases = newcases;
	node->cond.casecount = newcasecount;
}

void optimize_ast(ast_generic_node *node)
{
	size_t i;

	switch (node->node.node_type)
	{
	case AST_NODE_VARACC:
	case AST_NODE_NUMBER:
	case AST_NODE_STRUCVARDEF:
	case AST_NODE_CONSUMER:
		break;
	case AST_NODE_PROGRAM:
		for (i = 0; i < node->program.func_count; ++i)
		{
			optimize_ast((ast_generic_node *)node->program.funcs[i]);
		}
		break;
	case AST_NODE_FUNCDEF:
		optimize_ast(node->funcdef.expr);
		break;
	case AST_NODE_OP:
		if (node->op.left != NULL)
		{
			optimize_ast(node->op.left);
		}
		if (node->op.right != NULL)
		{
			optimize_ast(node->op.right);
		}
		optimize_op(node);
		break;
	case AST_NODE_FUNCCALL:
		for (i = 0; i < node->funccall.argcount; ++i)
		{
			optimize_ast(node->funccall.argexprs[i]);
		}
		break;
	case AST_NODE_COND:
		for (i = 0; i < node->cond.casecount; ++i)
		{
			optimize_ast(node->cond.cases[i]);
		}
		optimize_redundant_cases(node);
		optimize_ast(node->cond.defaultcase);

		if (node->cond.casecount == 0)
		{
			/* conditional expression is pointless */

			/* free cases array */
			free(node->cond.cases);

			/* default case is now only case */
			replace_child(node->node.parent, node, node->cond.defaultcase);

			/* free me */
			free(node);
		}
		break;
	case AST_NODE_CONDCASE:
		optimize_ast(node->condcase.condition);
		optimize_ast(node->condcase.value);
		break;
	case AST_NODE_CONDSTRUC:
		optimize_ast(node->condstruc.checkme);
		optimize_ast(node->condstruc.structure);
		optimize_ast(node->condstruc.value);
		break;
	default:
		fprintf(stderr, "Error while printing AST: unknown node type %04x.\n", node->node.node_type);
		break;
	}
}
