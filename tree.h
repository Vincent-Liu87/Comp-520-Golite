#ifndef TREE_H
#define TREE_H

#include <stdbool.h>

typedef enum{
st_INT,
st_FLOAT,
st_STRING,
st_BOOL,
st_UNDEFINED
}SymbolType;

typedef struct PROG PROG;
typedef struct EXP EXP;
typedef struct STMT STMT;
typedef struct DECL DECL;
typedef struct TYPE TYPE;

typedef enum{
	k_NodeKindPackageDec,
	k_NodeKindTopDecs,
	//var dec
	k_NodeKindVarDecLine,
	k_NodeKindVarDecPar,
	k_NodeKindVarDefs,
	k_NodeKindVarDef,
	//type dec
	k_NodeKindTypeDecPar,
	k_NodeKindTypeDecLine,
	k_NodeKindTypeDefs,
	k_NodeKindTypeDef,
	k_NodeKindTypeStruct,
	k_NodeKindStructBody,
	//func dec
	k_NodeKindFuncDec,
	k_NodeKindFuncParams,
	k_NodeKindFuncType,
}DeclKind;

typedef enum{
	//statements
	k_NodeKindStatements,
	k_NodeKindStatement,
	k_NodeKindBlockDec,
	k_NodeKindBlockBody,
	k_NodeKindSimpleStatementDec,
	k_NodeKindSimpleStatementExp,
	k_NodeKindSimpleStatementInc,
	k_NodeKindSimpleStatementDecrease,
	k_NodeKindSimpleStatementEqual,
	k_NodeKindSimpleStatementPlusEqual,
	k_NodeKindSimpleStatementMinEqual,
	k_NodeKindSimpleStatementMulEqual,
	k_NodeKindSimpleStatementDivEqual,
	k_NodeKindSimpleStatementModEqual,
	k_NodeKindSimpleStatementBitAndEqual,
	k_NodeKindSimpleStatementBitOrEqual,
	k_NodeKindSimpleStatementBitXorEqual,
	k_NodeKindSimpleStatementLeftShiftEqual,
	k_NodeKindSimpleStatementRightShiftEqual,
	k_NodeKindSimpleStatementBitClearEqual,
	k_NodeKindSimpleStatementDeclEqual,
	
	k_NodeKindPrintDec,
	k_NodeKindPrintlnDec,
	
	k_NodeKindReturn,
	
	k_NodeKindIfStmt,
	k_NodeKindElseStmt,
	k_NodeKindForDec,
	k_NodeKindForCondition,
	k_NodeKindSwitchDec,
	k_NodeKindSwitchDef,
	k_NodeKindSwitchCases,
	k_NodeKindDefault,
	
	k_NodeKindBreak,
	k_NodeKindContinue,
}StmtKind;

typedef enum{
	//id type
	k_NodeKindArrayType,
	k_NodeKindSliceType,
	k_NodeKindStructType,
	k_NodeKindIdType,
}TypeKind;

typedef enum{
	//identifiers
	k_NodeKindIdentifiers,
	k_NodeKindIdentifier,
	
	k_NodeKindExpressions,
	k_NodeKindExpressionOpt,
	k_NodeKindExpressionsOpt,
	k_NodeKindExpression,
	
	k_NodeKindExpressionBinaryPlus,
	k_NodeKindExpressionBinaryMinus,
	k_NodeKindExpressionBinaryMultiply,
	k_NodeKindExpressionBinaryDivide,
	k_NodeKindExpressionBinaryModulo,
	k_NodeKindExpressionBinaryBitAnd,
	k_NodeKindExpressionBinaryBitOr,
	k_NodeKindExpressionBinaryBitXor,
	k_NodeKindExpressionBinarybitClear,
	k_NodeKindExpressionBinaryLeftShift,
	k_NodeKindExpressionBinaryRightShift,
	k_NodeKindExpressionBinaryIsEqual,
	k_NodeKindExpressionBinaryIsNotEqual,
	k_NodeKindExpressionBinaryLessThan,
	k_NodeKindExpressionBinaryGreaterThan,
	k_NodeKindExpressionBinaryLessThanEqual,
	k_NodeKindExpressionBinaryGreaterThanEqual,
	k_NodeKindExpressionBinaryAnd,
	k_NodeKindExpressionBinaryOr,
	
	k_NodeKindUMinus,
	k_NodeKindUPlus,
	k_NodeKindUNot,
	k_NodeKindUXor,
	
	k_NodeKindIntLiteral,
	k_NodeKindRuneLiteral,
	k_NodeKindFloatLiteral,
	k_NodeKindStringLiteral,
	
	k_NodeKindExpressionPrimary,
	k_NodeKindSelector,
	k_NodeKindIndex,
	
	k_NodeKindAppend,
	k_NodeKindLen,
	k_NodeKindCap,
	k_NodeKindFuncCall,
}ExpKind;

struct Prog{
	int lineno;
	SYMBOL *symbol;
	SymbolType type;
    typeData evalValue;
	struct { Decl *package_dec; Decl *top_decs; } program;
}

struct Decl{
	int lineno;
	SYMBOL *symbol;
	SymbolType type;
    typeData evalValue;
    DeclKind kind;
    union{
    	struct { Exp *identifier;} package_dec;
		struct { Decl *top_decs; Decl *dec;} top_decs;
		struct { Decl *def;} var_dec;
		struct { Decl *var_defs; Decl *var_def;} var_defs;
		struct { Exp *identifiers; Type *identifier_type; Exp *expressions;} var_def;
		struct { Decl *def;}type_dec;
		struct { Decl *type_defs; Decl *type_def;}type_defs;
		struct { Type *identifier; Type *identifier_type;}type_def;
		struct { Type *identifier; Decl *struct_body;}type_struct;
		struct { Decl *struct_body; Exp *identifiers; Type *identifier_type;}struct_body;
		struct { Exp *identifier; Decl *func_params; Decl *func_type; Stmt *block_body;}func_dec;
		struct { Decl *func_params; Exp *identifiers; Type *identifier_type;}func_params;
		struct { Type *identifier_type;}func_type;
    }val
}

struct Type{
	int lineno;
	SYMBOL *symbol;
	SymbolType type;
    typeData evalValue;
    TypeKind kind;
    union{
    	char *identifier;
    	struct { int size; Type *identifier_type}identifier_type;
    }val
}

struct Stmt{
	int lineno;
	SYMBOL *symbol;
	SymbolType type;
    typeData evalValue;
    StmtKind kind;
    union{
		struct { Stmt *statements; Stmt *statement;}statements;
		struct { Stmt *stmt;} statement;
		struct { Stmt *block_body;}block_dec;
		struct { Stmt *statements;}block_body;
		struct { Stmt *statement;}simple_statement_dec;
		struct { Exp *lhs; Exp *rhs;}simple_statement;
		struct { Exp *expression_opt;}print_dec;
		struct { Exp *expressions_opt;}return_dec;
		struct { Stmt *simple_statement_dec; Exp *expression; Stmt *block_body; Stmt *else_stmt;}if_stmt;
		struct { Stmt *if_stmt; Stmt *block_body;}else_stmt;
		struct { Stmt *for_condition; Stmt *block_body;}for_dec;
		struct { Stmt *left; Exp *expression; Stmt *right;}for_condition;
		struct { Stmt *switch_def; Stmt *switch_cases;}switch_dec;
		struct { Stmt *simple_statement; Exp *expression_opt;}switch_def;
		struct { Stmt *cases; Exp *expressions; Stmt *statements;}switch_cases;
    }val
}

struct Exp{
	int lineno;
	SYMBOL *symbol;
	SymbolType type;
    typeData evalValue;
    ExpKind kind;
    union{
    	char *identifier;
		char* stringLiteral;
		int intLiteral;
		double floatLiteral;
		char* runeLiteral;
		struct { Exp *expressions; Exp *expression;}expressions;
		struct { Exp *expression;}expression;
		struct { Exp *lhs; Exp *rhs;}binary;
		struct { Exp *operand;}unary;
		struct { Exp *primary_expression; Exp *selector; Exp *index; Exp *identifier;}primary_expression;
		struct { Exp *identifier;}selector;
		struct { Exp *expression;}index;
		struct { Exp *expression1; Exp *expression2;}builtins;
		struct { Exp *identifier; Exp *expressions_opt;}func_call;
    }val
}

// Function declerations
//Node* newNode(NodeKind k, int lineno);
Exp *newIdentifiers(Exp* ids, Exp* id, int lineno);
Exp *newIdentifier(char* id, int lineno);
Prog *newProgram(Decl* package_dec, Decl* top_decs, int lineno);
Decl *newPackage_dec(Exp* identifier, int lineno);
Decl *newTop_decs(Decl* top_decs, Decl* dec, int lineno);
Decl *newVarDec(Decl* def, DeclKind kind, int lineno);
Decl *newVarDefs(Decl* defs, Decl* def, int lineno);
Decl *newVarDef(Decl* identifiers, Type* identifier_type, Exp* expressions, int lineno);
Decl *newTypeDec(Decl* def, DeclKind kind, int lineno);
Decl *newTypeDefs(Decl* type_defs, Decl* type_def, int lineno);
Decl *newTypeDef(Type* identifier, Type* identifier_type, int lineno);
Decl *newTypeStruct(Type* identifier, Decl* struct_body, int lineno);
Decl *newStructBody(Decl *struct_body, Exp *ids, Type *identifier_type, int lineno);
Decl *newFuncDec(Exp* id, Decl* params, Type* id_type, Stmt* block, int lineno);
Decl *newFuncParams(Decl* func_params, Exp* ids, Type* id_type, int lineno);
Decl *newFuncType(Type* id_type, int lineno);

Type *newIdentifierType(int size, Type *identifier_type, TypeKind kind, int lineno);

Stmt *newStatements(Stmt *statements, Stmt *statement, int lineno);
Stmt *newStatement(Stmt *stmt, int lineno);
Stmt *newBlockBody(Stmt *statements, int lineno);
Stmt *newSimpleStatementDec(Stmt *statement, int lineno);
Stmt *newSimpleStatement(Exp *lhs, Exp *rhs, StmtKind kind, int lineno);
Stmt *newPrintStatement(Stmt *expression_opt, StmtKind kind, int lineno);
Stmt *newReturnStatement(Stmt *expression_opt, int lineno);
Stmt *newIfStmt(Stmt* simple_statement_dec, Exp *expression, Stmt* block_body, Stmt* else_stmt, int lineno);
Stmt *newElseStmt(Stmt *if_stmt, Stmt* block_body, int lineno);
Stmt *newForDec(Stmt *for_condition, Stmt* block_body, int lineno);
Stmt *newForCondition(Stmt *left, Exp *expression, Stmt *right, int lineno);
Stmt *newSwitchDec(Stmt *switch_def, Stmt *switch_cases, int lineno);
Stmt *newSwitchDef(Stmt *simple_statement, Exp *expression_opt, int lineno);
Stmt *newSwitchCases(Stmt *cases, Exp *expressions, Stmt *statements, StmtKind kind, int lineno);
Stmt *newBreak(int lineno);
Stmt *newContinue(int lineno);


Exp *newExpressions(Exp *expressions, Exp *expression, int lineno);
Exp *newExpressionOpt(Exp *expression, int lineno);
Exp *newExpressionsOpt(Exp *expressions, int lineno);
Exp *newExpression(Exp *expression, int lineno);
Exp *newExpressionBinary(Exp *lhs, Exp *rhs, ExpKind kind, int lineno);
Exp *newExpressionUnary(Exp *operand, ExpKind kind, int lineno);
Exp *newIntLiteral(int literal, int lineno);
Exp *newRuneLiteral(char* literal, int lineno);
Exp *newFloatLiteral(double literal, int lineno);
Exp *newStringLiteral(char* literal, int lineno);
Exp *newExpressionPrimary(Exp *primary_expression, Exp *selector, Exp *index, Exp *identifier, int lineno);
Exp *newSelector(Exp *identifier, int lineno);
Exp *newIndex(Exp *expression, int lineno);
Exp *newBuiltin(Exp *expression1, Exp *expression2, ExpKind kind, int lineno);
Exp *newFuncCall(Exp *identifier, Exp *expressions_opt, int lineno);

#endif /* !TREE_H */
