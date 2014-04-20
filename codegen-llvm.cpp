/**
 * @file codegen-llvm.cpp
 *
 * Code generator based on the Low Level Virtual Machine.
 *
 * @author Ondřej Hošek, e0925631
 */

/*
	iburg, man's best friend:
	No real documentation, save for an indigestible scientific paper.
	"I made a few functions for you, try figuring out what they do" approach.
	AST nodes apparently can't have more than two children.
*/

/** Declare limit macros despite C++. Required by LLVM. */
#define __STDC_LIMIT_MACROS 1

/** Declare constant macros despite C++. Required by LLVM. */
#define __STDC_CONSTANT_MACROS 1

/** Number of list cell pairs to allocate. */
#define ALLOC_LIST_COUNT 128

#include "codegen-llvm.h"

#include <cstdio>
#include <cassert>

#include <iostream>
#include <vector>
#include <map>
#include <set>

#include <llvm/ADT/Triple.h>
#include <llvm/ADT/ValueMap.h>
#include <llvm/Bitcode/ReaderWriter.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/Support/FormattedStream.h>
#include <llvm/Support/ManagedStatic.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Target/TargetLibraryInfo.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>

/** Map of strings to LLVM values. */
typedef std::map<std::string, llvm::Value *> mapStrToValuePtr;

/** Set of LLVM values. */
typedef std::set<llvm::Value *> valSet;

/** The current state of the code generator. A pointer to it is passed between functions. */
typedef struct llvmgen_state
{
	llvm::IRBuilder<> *irb;			/**< IRBuilder for constructing functions. */
	llvm::LLVMContext *ctx;			/**< Thread-safe LLVM context. */
	llvm::Module *mdl;			/**< The code module being generated. */
	llvm::Function *f;			/**< The function currently being generated. */

	llvm::Function *raisesig;		/**< Call me to raise an error if a runtime type check fails. */
	llvm::Function *checkinteger;		/**< Assert that the argument contains an integer. */
	llvm::Function *checklist;		/**< Assert that the argument contains a list pointer. */
	llvm::Function *heapinit;		/**< Initialize heap if needed. */
	llvm::Function *malloc;			/**< Allocate memory. (Points to standard C function.) */
	llvm::GlobalVariable *heapptr;		/**< Heap pointer to first free list cell. */

	mapStrToValuePtr *variableToValue;	/**< Maps argument and structure-case variable names to their values. */
	valSet *intAssuredVals;			/**< Set of values previously assured to be integers. */

	char paranoid;				/**< Paranoid dynamic type-checking. */
} llvmgen_state;

/**
 * Generate an assertion that a given value is an integer.
 *
 * @param what Value to generate assertion for.
 * @param ls Code generator state.
 */
static inline void genassert_integer(llvm::Value *what, llvmgen_state *ls)
{
	if (ls->intAssuredVals->count(what) == 0)
	{
		ls->irb->CreateCall(
			llvm::cast<llvm::Value>(ls->checkinteger),
			what
		);
		ls->intAssuredVals->insert(what);
	}
}

/**
 * Generate an assertion that a given value is a list.
 *
 * @param what Value to generate assertion for.
 * @param ls Code generator state.
 */
static inline void genassert_list(llvm::Value *what, llvmgen_state *ls)
{
	ls->irb->CreateCall(
		llvm::cast<llvm::Value>(ls->checklist),
		what
	);
}

/**
 * Generate the heap-pointer-initializing preamble.
 *
 * @param ls Code generator state.
 */
static inline void genpreamble_heapinit(llvmgen_state *ls)
{
	ls->irb->CreateCall(
		llvm::cast<llvm::Value>(ls->heapinit)
	);
}

/**
 * Declare the "raisesig" function provided by the Powers That Be.
 *
 * @param ls Code generator state.
 */
static void declfunc_raisesig(llvmgen_state *ls)
{
	ls->raisesig = llvm::cast<llvm::Function>(ls->mdl->getOrInsertFunction(
		"raisesig",				/* func name */
		llvm::Type::getVoidTy(*ls->ctx),	/* void return */
		NULL					/* no args */
	));
	ls->raisesig->addFnAttr(llvm::Attribute::NoReturn);	/* doesn't return */
	ls->raisesig->addFnAttr(llvm::Attribute::ReadNone);	/* doesn't access memory */
	ls->raisesig->addFnAttr(llvm::Attribute::NoUnwind);	/* doesn't unwind stack */
	ls->raisesig->setLinkage(llvm::GlobalValue::ExternalLinkage);
}

/**
 * Declare the "malloc" function provided by the C library.
 *
 * @param ls Code generator state.
 */
static void declfunc_malloc(llvmgen_state *ls)
{
	ls->malloc = llvm::cast<llvm::Function>(ls->mdl->getOrInsertFunction(
		"malloc",				/* func name */
		llvm::Type::getInt64PtrTy(*ls->ctx),	/* return type */
		llvm::Type::getInt64Ty(*ls->ctx),	/* first arg: size */
		NULL					/* no more args */
	));
	ls->malloc->addFnAttr(llvm::Attribute::NoUnwind);
	ls->malloc->setLinkage(llvm::GlobalValue::ExternalLinkage);
}

/**
 * Generate a simple type-checking function.
 *
 * @param fname Name of the function.
 * @param type Type to check against.
 * @param ls Code generator state.
 */
static llvm::Function *genfunc_typecheck(const char *fname, unsigned char type, llvmgen_state *ls)
{
	llvm::IRBuilder<> *irb, *irbpass, *irbfail;

	/* these assertion functions call raisesig if the type is wrong */
	llvm::Function *ret = llvm::cast<llvm::Function>(ls->mdl->getOrInsertFunction(
		fname,					/* func name */
		llvm::Type::getVoidTy(*ls->ctx),	/* void return */
		llvm::Type::getInt64Ty(*ls->ctx),	/* int64 arg */
		NULL					/* no more args */
	));
	irb = new llvm::IRBuilder<>(llvm::BasicBlock::Create(
		*ls->ctx, "hereswherethefunstarts", ret
	));
	llvm::Argument *valArg = &ret->getArgumentList().front();
	valArg->setName("val");

	llvm::Value *and1 = irb->CreateAnd(valArg, 0x1);
	llvm::Value *eq = irb->CreateICmpEQ(and1, irb->getInt64(type));

	/* create cases */
	irbfail = new llvm::IRBuilder<>(llvm::BasicBlock::Create(
		*ls->ctx, "uhoh", ret
	));
	irbfail->CreateCall(ls->raisesig)->setTailCall();
	irbfail->CreateRetVoid();

	irbpass = new llvm::IRBuilder<>(llvm::BasicBlock::Create(
		*ls->ctx, "itsfine", ret
	));
	irbpass->CreateRetVoid();

	/* add branch */
	irb->CreateCondBr(eq, irbpass->GetInsertBlock(), irbfail->GetInsertBlock());

	delete irb;
	delete irbpass;
	delete irbfail;

	ret->addFnAttr(llvm::Attribute::NoUnwind);	/* doesn't unwind stack */
	ret->addFnAttr(llvm::Attribute::ReadNone);	/* accesses only argument values */
	ret->setLinkage(llvm::GlobalValue::InternalLinkage);

	return ret;
}

/**
 * Generate a heap-pointer allocator function.
 *
 * @param ls Code generator state.
 */
static llvm::Function *genfunc_heapinit(llvmgen_state *ls)
{
	llvm::IRBuilder<> *irb, *irbunset, *irbset;

	llvm::Function *ret = llvm::cast<llvm::Function>(ls->mdl->getOrInsertFunction(
		"internal_heapinit",			/* name */
		llvm::Type::getVoidTy(*ls->ctx),	/* void return */
		NULL					/* no args */
	));
	irb = new llvm::IRBuilder<>(llvm::BasicBlock::Create(
		*ls->ctx, "herewego", ret
	));

	llvm::Value *curheapptr = irb->CreateLoad(ls->heapptr, "curheapptr");
	llvm::Value *isnull = irb->CreateIsNull(curheapptr, "isnull");

	/* create cases */
	irbunset = new llvm::IRBuilder<>(llvm::BasicBlock::Create(
		*ls->ctx, "isunset", ret
	));
	llvm::Value *newheapptr = irbunset->CreateCall(ls->malloc, irbunset->getInt64(ALLOC_LIST_COUNT*2*64), "newheapptr");	/* malloc */
	irbunset->CreateStore(newheapptr, ls->heapptr);	/* store heap pointer */
	irbunset->CreateRetVoid();

	irbset = new llvm::IRBuilder<>(llvm::BasicBlock::Create(
		*ls->ctx, "isset", ret
	));
	irbset->CreateRetVoid();

	/* add branch */
	irb->CreateCondBr(isnull, irbunset->GetInsertBlock(), irbset->GetInsertBlock());

	delete irb;
	delete irbunset;
	delete irbset;

	ret->addFnAttr(llvm::Attribute::NoUnwind);	/* doesn't unwind stack */
	ret->addFnAttr(llvm::Attribute::InlineHint);	/* inline if good idea */
	ret->setLinkage(llvm::GlobalValue::InternalLinkage);

	return ret;
}

/**
 * Generate code for an expression.
 *
 * @param expr Expression for which to generate code.
 * @param ls Code generator state.
 */
static llvm::Value *codegen_expr(ast_generic_node *expr, llvmgen_state *ls);

/**
 * Generate code for a list structure expression (in a structure case).
 *
 * @param checkme Check this value for a given structure. Must be an integral value, @b not a pointer.
 * @param structure Check checkme against this structure.
 * @param falseBlock Branch to this block if checkme doesn't correspond to structure.
 * @param ls Code generator state.
 */
static void codegen_struclexpr(llvm::Value *checkme, ast_generic_node *structure, llvm::BasicBlock *falseBlock, llvmgen_state *ls)
{
	switch (structure->node.node_type)
	{
		case AST_NODE_CONSUMER:
			/* I don't care about this value. */
			break;
		case AST_NODE_STRUCVARDEF:
			/* I do care about this one. In fact, I will give it a name! */
			ls->variableToValue->insert(std::make_pair(
				structure->strucvardef.varname, checkme
			));
			break;
		case AST_NODE_OP:
		{
			if (structure->op.op != AST_OP_CONS)
			{
				fprintf(stderr, "Internal error: case structure contains non-cons operation\n");
				return;
			}

			/* is it a list? */
			llvm::Value *listBase = ls->irb->CreateAnd(checkme, 0xFFFFFFFFFFFFFFFE);
			llvm::Value *isList = ls->irb->CreateICmpNE(
				listBase,
				checkme
			);

			/* prepare the block if it is */
			llvm::IRBuilder<> *islistirb = new llvm::IRBuilder<>(llvm::BasicBlock::Create(
				*ls->ctx, "islist", ls->f, falseBlock
			));

			/* emit conditional branch */
			ls->irb->CreateCondBr(
				isList,
				islistirb->GetInsertBlock(),
				falseBlock
			);

			/* swizzle IRBs */
			delete ls->irb;
			ls->irb = islistirb;

			/* cast base value to pointer */
			llvm::Value *leftElementPtr = ls->irb->CreateIntToPtr(
				listBase,
				llvm::Type::getInt64PtrTy(*ls->ctx)
			);

			/* fetch pointer to right element too */
			llvm::Value *rightElementPtr = ls->irb->CreateConstGEP1_64(
				leftElementPtr, 1
			);

			/* fetch their values */
			llvm::Value *leftElement = ls->irb->CreateLoad(leftElementPtr);
			llvm::Value *rightElement = ls->irb->CreateLoad(rightElementPtr);

			/* continue there */
			codegen_struclexpr(leftElement, structure->op.left, falseBlock, ls);
			codegen_struclexpr(rightElement, structure->op.right, falseBlock, ls);

			break;
		}
		default:
			fprintf(stderr, "Internal error: case structure contains child node of an invalid type\n");
			break;
	}
}

/**
 * Generate code for a conditional expression.
 *
 * @param expr Conditional expression for which to generate code.
 * @param ls Code generator state.
 */
static llvm::Value *codegen_cond(ast_generic_node *expr, llvmgen_state *ls)
{
	size_t i;
	llvm::ValueMap<llvm::BasicBlock *, llvm::Value *> labelToValue;

	/* create catching block */
	llvm::IRBuilder<> *catchirb = new llvm::IRBuilder<>(llvm::BasicBlock::Create(
		*ls->ctx, "catcher", ls->f
	));

	for (i = 0; i < expr->cond.casecount; ++i)
	{
		ast_generic_node *casen = expr->cond.cases[i];

		/* false block, before catcher */
		llvm::IRBuilder<> *falseirb = new llvm::IRBuilder<>(llvm::BasicBlock::Create(
			*ls->ctx, "condfalse", ls->f, catchirb->GetInsertBlock()
		));

		/* truth value and expr value if the condition is true */
		llvm::Value *truthval;
		llvm::Value *iftrue;

		if (casen->node.node_type == AST_NODE_CONDCASE)
		{
			/* simple case */

			/* remember this */
			llvm::IRBuilder<> *oldirb = ls->irb;

			/* value if true will be calculated here */
			llvm::IRBuilder<> *trueirb = new llvm::IRBuilder<>(llvm::BasicBlock::Create(
				*ls->ctx, "condtrue", ls->f, catchirb->GetInsertBlock()
			));

			/* calculate condition */
			llvm::Value *condition = codegen_expr(casen->condcase.condition, ls);

			/* check for non-zero-ness */
			truthval = ls->irb->CreateICmpNE(
				condition,
				ls->irb->getInt64(0)
			);

			/* generate the branch for this case */
			oldirb->CreateCondBr(
				truthval,
				trueirb->GetInsertBlock(),
				falseirb->GetInsertBlock()
			);
			delete oldirb;

			/* swap IRBs */
			ls->irb = trueirb;

			/* codegen expression if true */
			iftrue = codegen_expr(casen->condcase.value, ls);
		}
		else if (casen->node.node_type == AST_NODE_CONDSTRUC)
		{
			/* check the structure of this one */
			llvm::Value *checkme = codegen_expr(casen->condstruc.checkme, ls);

			/* handle "new" variable definitions here too */
			mapStrToValuePtr *oldV2V = ls->variableToValue;
			mapStrToValuePtr *subV2V = new mapStrToValuePtr(
				oldV2V->begin(), oldV2V->end()
			);
			ls->variableToValue = subV2V;

			/* generate the structure checking and value extraction (also does IRB swizzling) */
			codegen_struclexpr(
				checkme,
				casen->condstruc.structure,
				falseirb->GetInsertBlock(),
				ls
			);

			/* codegen expression if true */
			iftrue = codegen_expr(casen->condstruc.value, ls);

			/* cleanup */
			delete subV2V;
			ls->variableToValue = oldV2V;
		}

		/* store the value-if-true in the map */
		labelToValue.insert(std::make_pair(
			ls->irb->GetInsertBlock(),
			iftrue
		));

		/* when that's done, branch from true to catcher */
		ls->irb->CreateBr(
			catchirb->GetInsertBlock()
		);

		/* continue where false */
		delete ls->irb;
		ls->irb = falseirb;
	}

	/* generate default case */
	llvm::Value *defval = codegen_expr(expr->cond.defaultcase, ls);

	/* generate unconditional branch to catching block from default-value block */
	ls->irb->CreateBr(catchirb->GetInsertBlock());

	/* generate phi instruction in catching block (this is the fun part) */
	llvm::PHINode *phi = catchirb->CreatePHI(
		llvm::Type::getInt64Ty(*ls->ctx),	/* type, as always */
		labelToValue.size() + 1			/* approx number of items */
	);
	for (
		llvm::ValueMap<llvm::BasicBlock *, llvm::Value *>::iterator iter = labelToValue.begin();
		iter != labelToValue.end();
		++iter
	)
	{
		phi->addIncoming(iter->second, iter->first);
	}
	phi->addIncoming(defval, ls->irb->GetInsertBlock());

	/* the catching block is where it all continues. */
	delete ls->irb;
	ls->irb = catchirb;

	/* the phi node contains the value we're looking for */
	return phi;
}

/**
 * Generate code for an expression.
 *
 * @param expr Expression for which to generate code.
 * @param ls Code generator state.
 */
static llvm::Value *codegen_expr(ast_generic_node *expr, llvmgen_state *ls)
{
	llvm::Value *left;
	llvm::Value *right;
	ast_type_assurance lta, rta;

	switch (expr->node.node_type)
	{
		case AST_NODE_OP:
			switch (expr->op.op)
			{
				case AST_OP_ADD:
				case AST_OP_MUL:
				case AST_OP_EQUAL:
				case AST_OP_OR:
				case AST_OP_CONS:
				case AST_OP_GT:
					left = codegen_expr(expr->op.left, ls);
					right = codegen_expr(expr->op.right, ls);
					if (!ls->paranoid)
					{
						lta = expr->op.left->node.type_assurance;
						rta = expr->op.right->node.type_assurance;
					}
					break;
				case AST_OP_NOT:
				case AST_OP_MINUS:
					right = codegen_expr(expr->op.operand, ls);
					if (!ls->paranoid)
					{
						rta = expr->op.right->node.type_assurance;
					}
					break;
			}

			switch (expr->op.op)
			{
				case AST_OP_ADD:
					if (lta != AST_TA_INTEGER)
						genassert_integer(left, ls);
					if (rta != AST_TA_INTEGER)
						genassert_integer(right, ls);

					/* the LSB = 0 won't be a problem */
					return ls->irb->CreateAdd(left, right);
				case AST_OP_MUL:
				{
					if (lta != AST_TA_INTEGER)
						genassert_integer(left, ls);
					if (rta != AST_TA_INTEGER)
						genassert_integer(right, ls);

					/* fetch integer values of left and right */
					llvm::Value *lint = ls->irb->CreateAShr(left, 1);
					llvm::Value *rint = ls->irb->CreateAShr(right, 1);
					/* multiply */
					llvm::Value *mulret = ls->irb->CreateMul(lint, rint);
					/* shift left to fix LSB */
					return ls->irb->CreateShl(mulret, 1);
				}
				case AST_OP_EQUAL:
				{
					/* LSB not a problem */
					llvm::Value *cmpres = ls->irb->CreateICmpEQ(left, right);
					return ls->irb->CreateSelect(
						cmpres,
						ls->irb->getInt64(0xFFFFFFFFFFFFFFFE),
						ls->irb->getInt64(0x0)
					);
				}
				case AST_OP_OR:
					if (lta != AST_TA_INTEGER)
						genassert_integer(left, ls);
					if (rta != AST_TA_INTEGER)
						genassert_integer(right, ls);
					/* LSB not a problem (0 or 0 == 0) */
					return ls->irb->CreateOr(left, right);
				case AST_OP_CONS:
				{
					/* fetch heap pointer */
					llvm::Value *lptr = ls->irb->CreateLoad(ls->heapptr);
					/* store left */
					ls->irb->CreateStore(left, lptr);
					/* fetch pointer for right */
					llvm::Value *rptr = ls->irb->CreateConstGEP1_64(lptr, 1);
					/* store right */
					ls->irb->CreateStore(right, rptr);
					/* fetch new heap pointer */
					llvm::Value *newhptr = ls->irb->CreateConstGEP1_64(lptr, 2);
					/* store it */
					ls->irb->CreateStore(newhptr, ls->heapptr);
					/* convert original heap pointer to integer */
					llvm::Value *lptrint = ls->irb->CreatePtrToInt(lptr, ls->irb->getInt64Ty());
					/* tag with 1 because it's a list cell */
					llvm::Value *ret = ls->irb->CreateOr(lptrint, 1);
					/* return this pointer-y thing */
					return ret;
				}
				case AST_OP_GT:
				{
					if (lta != AST_TA_INTEGER)
						genassert_integer(left, ls);
					if (rta != AST_TA_INTEGER)
						genassert_integer(right, ls);
					/* LSB not a problem (a > b <=> 2*a > 2*b) */
					llvm::Value *cmpres = ls->irb->CreateICmpSGT(left, right);
					return ls->irb->CreateSelect(
						cmpres,
						ls->irb->getInt64(0xFFFFFFFFFFFFFFFE),
						ls->irb->getInt64(0x0)
					);
				}
				case AST_OP_NOT:
				{
					if (rta != AST_TA_INTEGER)
						genassert_integer(right, ls);
					/* negate without touching LSB */
					return ls->irb->CreateXor(right, 0xFFFFFFFFFFFFFFFE);
				}
				case AST_OP_MINUS:
					if (rta != AST_TA_INTEGER)
						genassert_integer(right, ls);
					/* LSB not a problem ((NOT x...0) + 1 = y...0) */
					return ls->irb->CreateNeg(right);
			}
			break;
		case AST_NODE_VARACC:
			return (*ls->variableToValue)[expr->varacc.varname];
		case AST_NODE_FUNCCALL:
		{
			std::vector<llvm::Value *> args;
			size_t i;

			for (i = 0; i < expr->funccall.argcount; ++i)
			{
				llvm::Value *aex = codegen_expr(expr->funccall.argexprs[i], ls);
				/*aex->dump();*/
				args.push_back(
					aex
				);
			}

			llvm::Constant *target = ls->mdl->getOrInsertFunction(
				expr->funccall.funcname,	/* name */
				llvm::FunctionType::get(
					llvm::Type::getInt64Ty(*ls->ctx),	/* return type */
					true					/* variadic (to allow any argument count) */
				)
			);

			return ls->irb->CreateCall(
				target,		/* call that function */
				args		/* with the specified arguments */
			);
		}
		case AST_NODE_COND:
		{
			return codegen_cond(expr, ls);
		}
		case AST_NODE_NUMBER:
			/* LSB: 0, rest: 63-bit signed integer */
			return ls->irb->getInt64(expr->number.val << 1);
		case AST_NODE_CONDCASE:
		case AST_NODE_CONDSTRUC:
		case AST_NODE_STRUCVARDEF:
		case AST_NODE_CONSUMER:
		case AST_NODE_PROGRAM:
		case AST_NODE_FUNCDEF:
			assert(0 && "Invalid expression in AST.");
			break;
	}
	return 0;
}

/**
 * Generate code for a function.
 *
 * @param func AST function for which to generate code.
 * @param ls Code generator state.
 */
static void codegen_function(ast_node_funcdef *func, llvmgen_state *ls)
{
	size_t i;
	llvm::Value *retval;
	std::vector<llvm::Value *> argvals;

	/* fetch the function */
	ls->f = ls->mdl->getFunction(func->name);

	/* setup name-to-value map and integer-assured value set */
	ls->variableToValue = new std::map<std::string, llvm::Value *>();
	ls->intAssuredVals = new std::set<llvm::Value *>();

	/* add args to the var-to-val map */
	i = 0;
	for (
		llvm::Function::arg_iterator ai = ls->f->arg_begin();
		i < func->argcount;
		++ai, ++i
	)
	{
		ls->variableToValue->insert(std::make_pair(
			func->argnames[i],
			ai
		));
	}

	/* fetch a block maker */
	ls->irb = new llvm::IRBuilder<>(llvm::BasicBlock::Create(
		*ls->ctx, func->name, ls->f
	));

	/* if needed, generate the heap pointer initialization preamble */
	if (ast_contains_op((ast_generic_node *)func, AST_OP_CONS))
	{
		genpreamble_heapinit(ls);
	}

	/* generate the function */
	retval = codegen_expr(func->expr, ls);

	/* create a returner */
	ls->irb->CreateRet(retval);

	/* clean up */
	delete ls->irb;
	delete ls->variableToValue;
	delete ls->intAssuredVals;
}

/**
 * Pre-declare a function in this module.
 *
 * @param func AST function which to pre-declare.
 * @param ls Code generator state.
 */
static void codegen_function_head(ast_node_funcdef *func, llvmgen_state *ls)
{
	size_t i;
	std::vector<llvm::Type *> argtypes;
	llvm::Function *realfunc;

	/* derive argument types */
	for (i = 0; i < func->argcount; ++i)
	{
		argtypes.push_back(llvm::Type::getInt64Ty(*ls->ctx));
	}

	/* declare function header */
	realfunc = llvm::cast<llvm::Function>(ls->mdl->getOrInsertFunction(
		func->name,			/* func name */
		llvm::FunctionType::get(	/* function type */
			llvm::Type::getInt64Ty(*ls->ctx),	/* return */
			argtypes,				/* args */
			false					/* variadic? */
		)
	));

	/* set argument names */
	i = 0;
	for (
		llvm::Function::arg_iterator ai = realfunc->arg_begin();
		i < func->argcount;
		++ai, ++i
	)
	{
		ai->setName(func->argnames[i]);
	}
	realfunc->setLinkage(llvm::GlobalValue::ExternalLinkage);
}

/**
 * Output the Module as x86_64 assembly code.
 *
 * @param mdl Module to output.
 * @param outfobj Wrapped file object to output into.
 * @param optimize Activate or deactivate LLVM-based optimizations.
 */
static void output_x64asm(llvm::Module *mdl, llvm::raw_ostream *outfobj, char optimize)
{
	std::string err;
	llvm::CodeGenOpt::Level optLevel =
		optimize ? llvm::CodeGenOpt::Default : llvm::CodeGenOpt::None
	;

	/* initialize target and asm printer */
	LLVMInitializeX86Target();
	LLVMInitializeX86TargetMC();
	LLVMInitializeX86TargetInfo();
	LLVMInitializeX86AsmPrinter();

	/* set target */
	llvm::Triple myTriple("x86_64-linux-gnu");
	mdl->setTargetTriple(myTriple.getTriple());

	/* set data layout (improve optimization) */
	mdl->setDataLayout(
		"e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-"
		"i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-"
		"a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
	);

	/* find its info */
	const llvm::Target *tgt = llvm::TargetRegistry::lookupTarget(myTriple.getTriple(), err);
	assert(tgt && "Target not found!");
	llvm::TargetMachine *tgtM = tgt->createTargetMachine(
	    myTriple.getTriple(), "", "", llvm::TargetOptions(),
	    llvm::Reloc::Default, llvm::CodeModel::Default, optLevel
	);

	/* prepare toolchain */
	llvm::legacy::PassManager pm;
	//pm.add(new llvm::TargetData(mdl));
	pm.add(new llvm::TargetLibraryInfo(myTriple));
	if (const llvm::DataLayout *dt = tgtM->getDataLayout())
	{
		pm.add(new llvm::DataLayout(*dt));
	}
	else
	{
		pm.add(new llvm::DataLayout(mdl));
	}

	/* choose a few things */
	tgtM->setAsmVerbosityDefault(true);

	/* open file */
	llvm::formatted_raw_ostream fos(*outfobj);

	/* request output */
	tgtM->addPassesToEmitFile(
		pm,					/* pass manager */
		fos,					/* output stream */
		llvm::TargetMachine::CGFT_AssemblyFile,	/* output file type */
		false,					/* false == verify */
		0, 0
	);

	/* go! */
	pm.run(*mdl);

	delete tgtM;
}

extern "C"
void codegen_llvm(ast_generic_node *root, output_type_e outtype, FILE *outfile, char optimize, char paranoid)
{
	size_t i;
	llvmgen_state ls;

	assert(root != NULL && "Tree root is NULL.");
	assert(outfile != NULL && "Output file is NULL.");
	assert(outtype >= 0 && outtype < OT_LAST && "Invalid output type.");

	assert(root->node.node_type == AST_NODE_PROGRAM && "Can't codegen from root which isn't a program node.");

	ls.ctx = new llvm::LLVMContext();

	ls.mdl = new llvm::Module("UBVL", *ls.ctx);

	ls.paranoid = paranoid;

	/* declare global variable for heap pointer */
	llvm::Type *addrPtrType = llvm::Type::getInt64PtrTy(*ls.ctx, 0);
	ls.heapptr = new llvm::GlobalVariable(
		*ls.mdl,					/* module */
		addrPtrType,					/* type */
		false,						/* not constant */
		llvm::GlobalValue::InternalLinkage,		/* linkage type (static) */
		llvm::Constant::getNullValue(addrPtrType),	/* initial value */
		"internal_heapptr"				/* name */
	);

	/* generate prototypes for external functions */
	declfunc_raisesig(&ls);
	declfunc_malloc(&ls);

	/* generate type assert functions */
	ls.checkinteger = genfunc_typecheck("internal_checkinteger", 0x0, &ls);
	ls.checklist = genfunc_typecheck("internal_checklist", 0x1, &ls);

	/* generate heap-pointer initializing function */
	ls.heapinit = genfunc_heapinit(&ls);

	/* pre-declare all functions */
	for (i = 0; i < root->program.func_count; ++i)
	{
		codegen_function_head(root->program.funcs[i], &ls);
	}

	/* codegen all functions */
	for (i = 0; i < root->program.func_count; ++i)
	{
		codegen_function(root->program.funcs[i], &ls);
	}

	llvm::raw_fd_ostream outfobj(fileno(outfile), false);
	switch (outtype)
	{
		case OT_X86_64_ASM:
			output_x64asm(ls.mdl, &outfobj, optimize);
			break;
		case OT_LLVM_BC:
			llvm::WriteBitcodeToFile(ls.mdl, outfobj);
			break;
		default:
			fprintf(stderr, "WARNING: specified output format not supported yet.");
			break;
	}

	/* cleanup */
	delete ls.mdl;
	delete ls.ctx;
	llvm::llvm_shutdown();
}
