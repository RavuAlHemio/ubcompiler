/**
 * @file common.h
 *
 * Common type and function declarations.
 *
 * @author Ondřej Hošek, e0925631
 */

#ifndef __COMMON_H__
#define __COMMON_H__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/** The numeric type used within this compiler. Warning: codegen doesn't use this yet. */
typedef signed long numeric_type;

/** Launch the lexer. */
int yylex(void);

/** Launch the parser. */
int yyparse(void);

void lexerrorv(const char *fmt, ...);
void yyerror(const char *err);

/**
 * Convert a string containing a decimal number to an integer.
 *
 * @param decstr Decimal string containing number to convert to integer.
 * @return Number converted from decstr.
 */
numeric_type lex_dectoint(const char *decstr);

/**
 * Convert a string containing a hexadecimal number to an integer.
 *
 * @param hexstr Hexadecimal string containing number to convert to integer.
 * @return Number converted from hexstr.
 */
numeric_type lex_hextoint(const char *hexstr);

/**
 * Allocates size bytes of memory on the heap. Exits with code -1 if malloc fails.
 *
 * @param size Number of bytes to allocate on the heap.
 * @return Pointer to allocated memory. free() it when you're done using it.
 */
static inline void *malloc_or_explode(size_t size)
{
	void *ret = malloc(size);
	if (ret == NULL)
	{
		perror("malloc");
		exit(-1);
	}
	return ret;
}

/**
 * Duplicates the string on the heap. Exits with code -1 if strdup fails.
 *
 * @param str String to duplicate.
 * @return Pointer to the str duplicate. free() it when you're done using it.
 */
static inline char *strdup_or_explode(const char *str)
{
	char *ret = strdup(str);
	if (ret == NULL)
	{
		perror("strdup");
		exit(-1);
	}
	return ret;
}

#endif
