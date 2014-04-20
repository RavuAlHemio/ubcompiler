/**
 * @file ast.h
 *
 * Abstract Syntax Tree type and constant definitions.
 *
 * @author Ondřej Hošek, e0925631
 */

#ifndef __AST_H__
#define __AST_H__

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"

/**************
 * STRUCTURES *
 **************/

/* please don't serialize ASTs. their API is subject to change. */

/* forward-declaration */
union ast_generic_node_u;

/** Enumeration defining types of AST nodes. */
typedef enum
{
	AST_NODE_PROGRAM	= 0x01,		/**< program node */
	AST_NODE_FUNCDEF	= 0x02,		/**< function definition node */
/*	AST_NODE_FUNCSIG	= 0x03,
	AST_NODE_FUNCDEFARG	= 0x04,*/
	AST_NODE_OP		= 0x05,		/**< operator node */
	AST_NODE_VARACC		= 0x06,		/**< variable access node */
	AST_NODE_FUNCCALL	= 0x07,		/**< function call node */
	AST_NODE_COND		= 0x08,		/**< condition block node */
	AST_NODE_CONDCASE	= 0x09,		/**< case node within cond */
	AST_NODE_NUMBER		= 0x0a,		/**< constant number node */
	AST_NODE_CONDSTRUC	= 0x0b,		/**< structure case node within cond */
	AST_NODE_STRUCVARDEF	= 0x0c,		/**< structural variable definition within condstruc */
	AST_NODE_CONSUMER	= 0x0d		/**< variable consumer (_) within condstruc */
} ast_node_type;

/** Enumeration defining types of operations. */
typedef enum
{
	/* binary operations (considering relations to be operations too) */
	AST_OP_ADD	= 0x0001,	/**< add operation. (int, int) &#8594; int */
	AST_OP_MUL	= 0x0002,	/**< multiply operation. (int, int) &#8594; int */
	AST_OP_EQUAL	= 0x0003,	/**< equality test operation. (nm, nm) &#8594; int */
	AST_OP_OR	= 0x0004,	/**< bitwise or operation. (int, int) &#8594; int */
	AST_OP_CONS	= 0x0005,	/**< list construction operation. (nm, nm) &#8594; lst */
	AST_OP_GT	= 0x0006,	/**< greater-than test operation. (int, int) &#8594; int */
	/* unary operations */
	AST_OP_NOT	= 0x1001,	/**< not operation. int &#8594; int */
	AST_OP_MINUS	= 0x1002	/**< two's complement negation operation. int &#8594; int */
} ast_op;

/** Enumeration defining type assurances of nodes. */
typedef enum
{
	/** I don't generate a typed expression. */
	AST_TA_WRONG	= 0x00,

	/** I make no assurances. */
	AST_TA_NONE	= 0x01,

	/** I always return an integer. */
	AST_TA_INTEGER	= 0x02,

	/** I always return a list. */
	AST_TA_LIST	= 0x03
} ast_type_assurance;

/** Declarations common to all nodes. */
#define AST_COMMON_DECLS \
	ast_node_type node_type; \
	union ast_generic_node_u *parent; \
	ast_type_assurance type_assurance

/** A generic AST node. */
typedef struct
{
	AST_COMMON_DECLS;	/**< Put common declarations here. */
} ast_node;

/** An operation AST node. */
typedef struct
{
	AST_COMMON_DECLS;	/**< Put common declarations here. */

	/** specific operation represented by this node */
	ast_op op;

	union
	{
		struct	/* binary */
		{
			union ast_generic_node_u *left;		/**< left operand of binary operation */
			union ast_generic_node_u *right;	/**< right operand of binary operation (in union with operand) */
		};
		struct	/* unary */
		{
			union ast_generic_node_u *nevermind;	/**< irrelevant operand of unary operation */
			union ast_generic_node_u *operand;	/**< operand of unary operation (in union with right) */
		};
	};
} ast_node_op;

/** A single "cond" case AST node. */
typedef struct
{
	AST_COMMON_DECLS;			/**< Put common declarations here. */

	union ast_generic_node_u *condition;	/**< condition to check */
	union ast_generic_node_u *value;	/**< value to return if condition is true */
} ast_node_condcase;

/** A "cond" block AST node. */
typedef struct
{
	AST_COMMON_DECLS;			/**< Put common declarations here. */

	size_t casecount;			/**< number of cases (except default case) */
	union ast_generic_node_u **cases;	/**< array of cases (condcase or condstruc) */

	union ast_generic_node_u *defaultcase;	/**< value to return if all cases fail */
} ast_node_cond;

/** A variable access AST node. */
typedef struct
{
	AST_COMMON_DECLS;	/**< Put common declarations here. */

	char *varname;		/**< name of the variable to access */
} ast_node_varacc;

/** A function call AST node */
typedef struct
{
	AST_COMMON_DECLS;			/**< Put common declarations here. */

	char *funcname;				/**< name of function to call */
	size_t argcount;			/**< number of arguments to pass */
	union ast_generic_node_u **argexprs;	/**< array of argument expressions whose results are passed to function */
} ast_node_funccall;

/** An AST node representing a function definition. */
typedef struct
{
	AST_COMMON_DECLS;			/**< Put common declarations here. */

	char *name;				/**< name of the function defined */
	size_t argcount;			/**< number of arguments */
	char **argnames;			/**< array of argument names */
	union ast_generic_node_u *expr;		/**< expression of this function */
} ast_node_funcdef;

/** The top AST node representing a whole program. */
typedef struct
{
	AST_COMMON_DECLS;		/**< Put common declarations here. */

	size_t func_count;		/**< number of functions in the program */
	ast_node_funcdef **funcs;	/**< array of function definitions */
} ast_node_program;

/** An AST node representing a number. */
typedef struct
{
	AST_COMMON_DECLS;	/**< Put common declarations here. */

	numeric_type val;	/**< value of the constant numeric value (untagged!) */
} ast_node_number;

/** An AST node representing a structural condition. */
typedef struct
{
	AST_COMMON_DECLS;			/**< Put common declarations here. */

	union ast_generic_node_u *checkme;	/**< expression whose structure is to be checked */
	union ast_generic_node_u *structure;	/**< structure to check against */
	union ast_generic_node_u *value;	/**< expression to evaluate and return if checkme matches structure */
} ast_node_condstruc;

/** An AST node representing a structural variable definition. */
typedef struct
{
	AST_COMMON_DECLS;	/**< Put common declarations here. */

	char *varname;		/**< name of the variable to define according to structure */
} ast_node_strucvardef;

/** An AST node representing a stand-in variable consuming one structural item. */
typedef struct
{
	AST_COMMON_DECLS;	/**< Put common declarations here. */
} ast_node_consumer;

/** A union to access a node by any other name. Struct-punning is awesome. */
typedef union ast_generic_node_u
{
	ast_node		node;		/**< generic node */
	ast_node_op		op;		/**< operator node */
	ast_node_condcase	condcase;	/**< condition case node */
	ast_node_cond		cond;		/**< condition block node */
	ast_node_varacc		varacc;		/**< variable access node */
	ast_node_funccall	funccall;	/**< function call node */
	ast_node_funcdef	funcdef;	/**< function definition node */
	ast_node_program	program;	/**< (top-level) program node */
	ast_node_number		number;		/**< number node */
	ast_node_condstruc	condstruc;	/**< sutrcture case node */
	ast_node_strucvardef	strucvardef;	/**< structure variable definition */
	ast_node_consumer	consumer;	/**< structure variable consumer */
} ast_generic_node;

/*************
 * FUNCTIONS *
 *************/

/**
 * Validate the AST and print errors to stderr.
 *
 * @param node The root node of the AST to check. Should be a program or at least a function node.
 * @return true if the AST is valid, false if it is not.
 */
bool validate_ast(ast_generic_node *node);

/**
 * Check if the AST contains a given operation.
 *
 * @param node Node at which to start search.
 * @param op Operation to search for.
 * @returns true if the AST contains the operation, false if it doesn't.
 *
 * @remark Structural cases are @b not checked for occurrences of the cons operation.
 */
bool ast_contains_op(ast_generic_node *node, ast_op op);

/**
 * Free the memory taken up by this AST.
 *
 * @param node AST to free recursively.
 */
void free_ast(ast_generic_node *node);

/**
 * Print this AST in textual form.
 *
 * @param f stdio FILE object to print AST into.
 * @param node Root node of AST to print.
 */
void print_ast(FILE *f, ast_generic_node *node);

/**
 * Run multiple optimizations on the AST.
 *
 * @param node Root node of AST to optimize.
 */
void optimize_ast(ast_generic_node *node);

#endif
