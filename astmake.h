/**
 * @file astmake.h
 *
 * Utility functions for Abstract Syntax Tree construction.
 *
 * @author Ondřej Hošek, e0925631
 */

#ifndef __ASTMAKE_H__
#define __ASTMAKE_H__

#include "ast.h"

/*****************
 * NODE CREATION *
 *****************/

/**
 * Create a program node with one function definition.
 *
 * @param funcdef0 First function definition in the program.
 * @return Program node containing the single function definition specified in funcdef0.
 */
static inline ast_generic_node *ast_make_program(ast_generic_node *funcdef0)
{
	ast_generic_node *ret = malloc_or_explode(sizeof(ast_node_program));
	ret->program.node_type = AST_NODE_PROGRAM;
	ret->program.parent = NULL;
	ret->program.type_assurance = AST_TA_WRONG;
	ret->program.func_count = 1;
	ret->program.funcs = malloc_or_explode(1*sizeof(ast_node_funcdef *));
	ret->program.funcs[0] = &funcdef0->funcdef;
	ret->program.funcs[0]->parent = ret;
	return ret;
}

/**
 * Create a program node by appending a function definition.
 *
 * @warning Frees the program node passed in the first argument.
 *
 * @param program Existing program node.
 * @param funcdefn Next function definition in the program.
 * @return New program node, to replace the original program node.
 */
static inline ast_generic_node *ast_make_program_expand(ast_generic_node *program, ast_generic_node *funcdefn)
{
	size_t i;
	ast_generic_node *ret = malloc_or_explode(sizeof(ast_node_program));
	ret->program.node_type = AST_NODE_PROGRAM;
	ret->program.parent = NULL;
	ret->program.type_assurance = AST_TA_WRONG;
	ret->program.func_count = program->program.func_count + 1;
	ret->program.funcs = malloc_or_explode(ret->program.func_count*sizeof(ast_node_funcdef *));
	memcpy(ret->program.funcs, program->program.funcs, program->program.func_count*sizeof(ast_node_funcdef *));
	ret->program.funcs[ret->program.func_count-1] = &funcdefn->funcdef;

	for (i = 0; i < ret->program.func_count; ++i)
	{
		ret->program.funcs[i]->parent = ret;
	}

	free(program->program.funcs);
	free(program);
	return ret;
}

/**
 * Create a function definition node.
 *
 * @param funcname Name of the function.
 * @param argcount Number of arguments in the function.
 * @param argnames Names of the argument in the functions. Contains argcount elements.
 * @param expr Expression evaluated within this function.
 * @return New function definition node.
 */
static inline ast_generic_node *ast_make_funcdef(const char *funcname, size_t argcount, char **argnames, ast_generic_node *expr)
{
	ast_generic_node *ret = malloc_or_explode(sizeof(ast_node_funcdef));
	ret->funcdef.node_type = AST_NODE_FUNCDEF;
	ret->funcdef.type_assurance = AST_TA_WRONG;
	ret->funcdef.name = strdup_or_explode(funcname);
	ret->funcdef.argcount = argcount;
	ret->funcdef.argnames = argnames;
	ret->funcdef.expr = expr;
	ret->funcdef.expr->node.parent = ret;
	return ret;
}

/**
 * Create an argument name array with one element.
 *
 * @param arg0name First argument name.
 * @return New argument name array.
 */
static inline char **ast_make_argnames(const char *arg0name)
{
	char **ret = malloc_or_explode(1*sizeof(char*));
	ret[0] = strdup_or_explode(arg0name);
	return ret;
}

/**
 * Create an argument name array by appending an element to an existing one.
 *
 * @warning Frees oldargnames.
 *
 * @param oldargsize Old argument array size.
 * @param oldargnames Old argument array.
 * @param argnname Argument name to append.
 * @return New argument name array.
 */
static inline char **ast_make_argnames_expand(size_t oldargsize, char **oldargnames, const char *argnname)
{
	char **ret = malloc_or_explode((oldargsize+1)*sizeof(char*));
	memcpy(ret, oldargnames, oldargsize*sizeof(char*));
	ret[oldargsize] = strdup_or_explode(argnname);
	free(oldargnames);
	return ret;
}

/**
 * Creates a binary operation node.
 *
 * @warning Doesn't check whether optype actually represents a binary operation.
 *
 * @param optype Operation type.
 * @param lop Left operand expression.
 * @param rop Right operand expression.
 * @return New binary operation node.
 */
static inline ast_generic_node *ast_make_binop(ast_op optype, ast_generic_node *lop, ast_generic_node *rop)
{
	ast_generic_node *ret = malloc_or_explode(sizeof(ast_node_op));
	ret->op.node_type = AST_NODE_OP;
	ret->op.op = optype;
	switch (optype)
	{
		case AST_OP_ADD:
		case AST_OP_MUL:
		case AST_OP_EQUAL:
		case AST_OP_OR:
		case AST_OP_GT:
			ret->op.type_assurance = AST_TA_INTEGER;
			break;
		case AST_OP_CONS:
			ret->op.type_assurance = AST_TA_LIST;
			break;
		default:
			break;
	}
	ret->op.left = lop;
	ret->op.right = rop;
	ret->op.left->node.parent = ret;
	ret->op.right->node.parent = ret;
	return ret;
}

/**
 * Creates a unary operation node.
 *
 * @warning Doesn't check whether optype actually represents a unary operation.
 *
 * @param optype Operation type.
 * @param operand Operand expression.
 * @return New unary operation node.
 */
static inline ast_generic_node *ast_make_unop(ast_op optype, ast_generic_node *operand)
{
	ast_generic_node *ret = malloc_or_explode(sizeof(ast_node_op));
	ret->op.node_type = AST_NODE_OP;
	ret->op.op = optype;
	switch (optype)
	{
		case AST_OP_NOT:
		case AST_OP_MINUS:
			ret->op.type_assurance = AST_TA_INTEGER;
			break;
		default:
			break;
	}
	ret->op.nevermind = NULL;
	ret->op.operand = operand;
	ret->op.operand->node.parent = ret;
	return ret;
}

/**
 * Creates a constant number node.
 *
 * @remark val is not tagged; this is handled by the code generator.
 *
 * @param val Value of the number.
 * @return New constant number node.
 */
static inline ast_generic_node *ast_make_number(unsigned long val)
{
	ast_generic_node *ret = malloc_or_explode(sizeof(ast_node_number));
	ret->number.node_type = AST_NODE_NUMBER;
	ret->number.type_assurance = AST_TA_INTEGER;
	ret->number.val = val;
	return ret;
}

/**
 * Create a function call node.
 *
 * @param funcname Name of the function to call.
 * @param argcount Number of arguments passed to this function.
 * @param argexprs Array of expressions whose values are to be passed as
 *	arguments to the called function. Is argcount elements long.
 * @return New function call node.
 */
static inline ast_generic_node *ast_make_funccall(const char *funcname, size_t argcount, ast_generic_node **argexprs)
{
	size_t i;
	ast_generic_node *ret = malloc_or_explode(sizeof(ast_node_funccall));
	ret->funccall.node_type = AST_NODE_FUNCCALL;
	ret->funccall.type_assurance = AST_TA_NONE;
	ret->funccall.funcname = strdup_or_explode(funcname);
	ret->funccall.argcount = argcount;
	ret->funccall.argexprs = argexprs;
	for (i = 0; i < ret->funccall.argcount; ++i)
	{
		ret->funccall.argexprs[i]->node.parent = ret;
	}
	return ret;
}

/**
 * Create an argument expression array containing one expression.
 *
 * @param expr0 First expression to add to the array.
 * @return New argument expression array.
 */
static inline ast_generic_node **ast_make_argexprs(ast_generic_node *expr0)
{
	ast_generic_node **ret = malloc_or_explode(1*sizeof(ast_generic_node *));
	ret[0] = expr0;
	return ret;
}

/**
 * Create an argument expression array by appending an expression to an existing one.
 *
 * @warning Frees oldargexprs.
 *
 * @param oldargsize Old argument array size.
 * @param oldargexprs Old argument array.
 * @param exprn Argument expression to append.
 * @return New argument expression array.
 */
static inline ast_generic_node **ast_make_argexprs_expand(size_t oldargsize, ast_generic_node **oldargexprs, ast_generic_node *exprn)
{
	ast_generic_node **ret = malloc_or_explode((oldargsize+1)*sizeof(ast_generic_node *));
	memcpy(ret, oldargexprs, oldargsize*sizeof(ast_generic_node *));
	ret[oldargsize] = exprn;
	free(oldargexprs);
	return ret;
}

/**
 * Create a variable access node.
 *
 * @param varname Name of the variable to be accessed.
 * @return New variable access node.
 */
static inline ast_generic_node *ast_make_varacc(const char *varname)
{
	ast_generic_node *ret = malloc_or_explode(sizeof(ast_node_varacc));
	ret->varacc.node_type = AST_NODE_VARACC;
	ret->varacc.type_assurance = AST_TA_NONE;
	ret->varacc.varname = strdup_or_explode(varname);
	return ret;
}

/**
 * Create a conditional expression node.
 *
 * @param casecount Number of cases (except the default case).
 * @param cases Array of cases. Is casecount elements long.
 * @param defaultcase Expression to return if all cases are false.
 * @return New conditional expression node.
 */
static inline ast_generic_node *ast_make_cond(size_t casecount, ast_generic_node **cases, ast_generic_node *defaultcase)
{
	size_t i;
	ast_generic_node *ret = malloc_or_explode(sizeof(ast_node_cond));
	ret->cond.node_type = AST_NODE_COND;
	ret->cond.type_assurance = AST_TA_NONE;
	ret->cond.casecount = casecount;
	ret->cond.cases = cases;
	ret->cond.defaultcase = defaultcase;
	for (i = 0; i < ret->cond.casecount; ++i)
	{
		ret->cond.cases[i]->node.parent = ret;
	}
	ret->cond.defaultcase->node.parent = ret;
	return ret;
}

/**
 * Create a conditional case array containing one element.
 *
 * @param case0 Element to insert into array.
 * @return New conditional case array.
 */
static inline ast_generic_node **ast_make_condcasearr(ast_generic_node *case0)
{
	ast_generic_node **ret = malloc_or_explode(1*sizeof(ast_generic_node *));
	ret[0] = case0;
	return ret;
}

/**
 * Create a conditional case array by appending an element to an existing one.
 *
 * @warning Frees oldcases.
 *
 * @param oldcasessize Old case array size.
 * @param oldcases Old case array.
 * @param casen Case to append.
 * @return New conditional case array.
 */
static inline ast_generic_node **ast_make_condcasearr_expand(size_t oldcasessize, ast_generic_node **oldcases, ast_generic_node *casen)
{
	ast_generic_node **ret = malloc_or_explode((oldcasessize+1)*sizeof(ast_generic_node *));
	memcpy(ret, oldcases, oldcasessize*sizeof(ast_generic_node *));
	ret[oldcasessize] = casen;
	free(oldcases);
	return ret;
}

/**
 * Create a conditional case node.
 *
 * @param cond Condition expression to test.
 * @param val Value to return if cond is true.
 * @return New conditional case node.
 */
static inline ast_generic_node *ast_make_condcase(ast_generic_node *cond, ast_generic_node *val)
{
	ast_generic_node *ret = malloc_or_explode(sizeof(ast_node_condcase));
	ret->condcase.node_type = AST_NODE_CONDCASE;
	ret->condcase.type_assurance = AST_TA_WRONG;
	ret->condcase.condition = cond;
	ret->condcase.value = val;
	ret->condcase.condition->node.parent = ret;
	ret->condcase.value->node.parent = ret;
	return ret;
}

/**
 * Create a structural case node.
 *
 * @param checkme Expression whose structure is to be tested.
 * @param structure Stucture against which expression will be tested.
 * @param value Value to return if checkme corresponds to structure.
 * @return New structural case node.
 */
static inline ast_generic_node *ast_make_condstruc(ast_generic_node *checkme, ast_generic_node *structure, ast_generic_node *value)
{
	ast_generic_node *ret = malloc_or_explode(sizeof(ast_node_condstruc));
	ret->condstruc.node_type = AST_NODE_CONDSTRUC;
	ret->condstruc.type_assurance = AST_TA_WRONG;
	ret->condstruc.checkme = checkme;
	ret->condstruc.structure = structure;
	ret->condstruc.value = value;
	ret->condstruc.checkme->node.parent = ret;
	ret->condstruc.structure->node.parent = ret;
	ret->condstruc.value->node.parent = ret;
	return ret;
}

/**
 * Create a structural variable definition node.
 *
 * @param varname Name of the variable to define.
 * @return New structural variable definition node.
 */
static inline ast_generic_node *ast_make_strucvardef(const char *varname)
{
	ast_generic_node *ret = malloc_or_explode(sizeof(ast_node_strucvardef));
	ret->strucvardef.node_type = AST_NODE_STRUCVARDEF;
	ret->strucvardef.type_assurance = AST_TA_WRONG;
	ret->strucvardef.varname = strdup(varname);
	return ret;
}

/**
 * Creates a structural variable consumer node, which helps assert the structure
 * of an expression within a stuctural case but ignores its value.
 *
 * @return New structural variable consumer node.
 */
static inline ast_generic_node *ast_make_consumer(void)
{
	ast_generic_node *ret = malloc_or_explode(sizeof(ast_node_consumer));
	ret->consumer.node_type = AST_NODE_CONSUMER;
	ret->consumer.type_assurance = AST_TA_WRONG;
	return ret;
}

#endif
