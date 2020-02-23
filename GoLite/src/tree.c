#include <stdlib.h>
#include <stdbool.h>
#include "tree.h"

extern int yylineno;

Node *newNode(NodeKind k, int lineno){		// Create new node with specified kind.
    Node *n = malloc(sizeof(Node));
    n->lineno = lineno;
    n->kind = k;
    return n;
}

//identifiers
Node *newIdentifiers(Node* ids, Node* id, int lineno){ //create new identifiers
	Node *n = newNode(k_NodeKindIdentifiers, lineno);
	n->val.identifiers.identifiers = ids;
	n->val.identifiers.identifier = id;
	return n;
}

Node *newIdentifier(char* id, int lineno){ //create a new identifier
	Node *n = newNode(k_NodeKindIdentifier, lineno);
	n->val.identifier = id;
	return n;
}

//toplevels
Node *newProgram(Node* package_dec, Node* top_decs, int lineno){	// Create new program node
    Node *n = newNode(k_NodeKindProgram, lineno);
    n->val.program.package_dec = package_dec;
    n->val.program.top_decs = top_decs;
    return n;
}

Node *newPackage_dec(Node* identifier, int lineno){	// Create new package node
    Node *n = newNode(k_NodeKindPackageDec, lineno);
    n->val.package_dec.identifier = identifier;
    return n;
}

Node *newTop_decs(Node* top_decs, Node* dec, int lineno){	// Create new top_decs node
    Node *n = newNode(k_NodeKindTopDecs, lineno);
    n->val.top_decs.top_decs = top_decs;
	n->val.top_decs.dec = dec;
    return n;
}

//variables
Node *newVarDec(Node* def, NodeKind kind, int lineno){ //create new variable declaration
	Node *n = newNode(kind, lineno);
	n->val.var_dec.def = def;
	return n;
}

Node *newVarDefs(Node* defs, Node* def, int lineno){ //create new variable definitions
	Node *n = newNode(k_NodeKindVarDefs, lineno);
	n->val.var_defs.var_defs = defs;
	n->val.var_defs.var_def = def;
	return n;
}

Node *newVarDef(Node* identifiers, Node* identifier_type, Node* expressions, int lineno){ //create a new variable deninition
	Node *n = newNode(k_NodeKindVarDef, lineno);
	n->val.var_def.identifiers = identifiers;
	n->val.var_def.identifier_type = identifier_type;
	n->val.var_def.expressions = expressions;
	return n;
}

//types
Node *newTypeDec(Node* def, NodeKind kind, int lineno){ //create new type declaration
	Node *n = newNode(kind, lineno);
	n->val.type_dec.def = def;
	return n;
}

Node *newTypeDefs(Node* type_defs, Node* type_def, int lineno){ //create new type definitions
	Node *n = newNode(k_NodeKindTypeDefs, lineno);
	n->val.type_defs.type_defs = type_defs;
	n->val.type_defs.type_def = type_def;
	return n;
}

Node *newTypeDef(Node* identifier, Node* identifier_type, int lineno){ //create a new type definition
	Node *n = newNode(k_NodeKindTypeDef, lineno);
	n->val.type_def.identifier = identifier;
	n->val.type_def.identifier_type = identifier_type;
	return n;
}

//functions
Node *newFuncDec(Node* id, Node* params, Node* id_type, Node* block, int lineno){ //create new function declaration
	Node *n = newNode(k_NodeKindFuncDec, lineno);
	n->val.func_dec.identifier = id;
	n->val.func_dec.func_params = params;
	n->val.func_dec.func_type = id_type;
	n->val.func_dec.block_body = block;
	return n;
}

Node *newFuncParams(Node* func_params, Node* ids, Node* id_type, int lineno){//create new function parameters
	Node *n = newNode(k_NodeKindFuncParams, lineno);
	n->val.func_params.func_params = func_params;
	n->val.func_params.identifiers = ids;
	n->val.func_params.identifier_type = id_type;
	return n;
}

Node *newFuncType(Node* id_type, int lineno){
	Node *n = newNode(k_NodeKindFuncType, lineno);
	n->val.func_type.identifier_type = id_type;
	return n;
}

Node *newIdentifierType(int size, Node *identifier_type, Node* struct_body, Node* identifier, NodeKind kind, int lineno){
	Node *n = newNode(kind, lineno);
	n->val.identifier_type.size = size;
	n->val.identifier_type.identifier_type = identifier_type;
	n->val.identifier_type.struct_body = struct_body;
	n->val.identifier_type.identifier = identifier;
	return n;
}

Node *newStructBody(Node *struct_body, Node *ids, Node *identifier_type, int lineno){
	Node *n = newNode(k_NodeKindStructBody, lineno);
	n->val.struct_body.struct_body = struct_body;
	n->val.struct_body.identifiers = ids;
	n->val.struct_body.identifier_type = identifier_type;
	return n;
}

Node *newStatements(Node *statements, Node *statement, int lineno){
	Node *n = newNode(k_NodeKindStatements, lineno);
	n->val.statements.statements = statements;
	n->val.statements.statement = statement;
	return n;
}

Node *newStatement(Node *stmt, int lineno){
	Node *n = newNode(k_NodeKindStatement, lineno);
	n->val.statement.stmt = stmt;
	return n;
}

Node *newBlockDec(Node *block_body, int lineno){
	Node *n = newNode(k_NodeKindBlockDec, lineno);
	n->val.block_dec.block_body = block_body;
	return n;
}

Node *newBlockBody(Node *statements, int lineno){
	Node *n = newNode(k_NodeKindBlockBody, lineno);
	n->val.block_body.statements = statements;
	return n;
}

Node *newSimpleStatementDec(Node *statement, int lineno){
	Node *n = newNode(k_NodeKindSimpleStatementDec, lineno);
	n->val.simple_statement_dec.statement = statement;
	return n;
}

Node *newSimpleStatement(Node *lhs, Node *rhs, NodeKind kind, int lineno){
	Node *n = newNode(kind, lineno);
	n->val.simple_statement.lhs = lhs;
	n->val.simple_statement.rhs = rhs;
	return n;
}

Node *newPrintStatement(Node *expression_opt, NodeKind kind, int lineno){
	Node *n = newNode(kind, lineno);
	n->val.print_dec.expression_opt = expression_opt;
	return n;
}

Node *newReturnStatement(Node *expression_opt, int lineno){
	Node *n = newNode(k_NodeKindReturn, lineno);
	n->val.return_dec.expressions_opt = expression_opt;
	return n;
}

Node *newIfStmt(Node* simple_statement_dec, Node *expression, Node* block_body, Node* else_stmt, int lineno){
	Node *n = newNode(k_NodeKindIfStmt, lineno);
	n->val.if_stmt.simple_statement_dec = simple_statement_dec;
	n->val.if_stmt.expression = expression;
	n->val.if_stmt.block_body = block_body;
	n->val.if_stmt.else_stmt = else_stmt;
	return n;
}

Node *newElseStmt(Node *if_stmt, Node* block_body, int lineno){
	Node *n = newNode(k_NodeKindElseStmt, lineno);
	n->val.else_stmt.if_stmt = if_stmt;
	n->val.else_stmt.block_body = block_body;
	return n;
}

Node *newForDec(Node *for_condition, Node* block_body, int lineno){
	Node *n = newNode(k_NodeKindForDec, lineno);
	n->val.for_dec.for_condition = for_condition;
	n->val.for_dec.block_body = block_body;
	return n;
}

Node *newForCondition(Node *left, Node *expression, Node *right, int lineno){
	Node *n = newNode(k_NodeKindForCondition, lineno);
	n->val.for_condition.left = left;
	n->val.for_condition.expression = expression;
	n->val.for_condition.right = right;
	return n;
}

Node *newSwitchDec(Node *switch_def, Node *switch_cases, int lineno){
	Node *n = newNode(k_NodeKindSwitchDec, lineno);
	n->val.switch_dec.switch_def = switch_def;
	n->val.switch_dec.switch_cases = switch_cases;
	return n;
}

Node *newSwitchDef(Node *simple_statement, Node *expression_opt, int lineno){
	Node *n = newNode(k_NodeKindSwitchDef, lineno);
	n->val.switch_def.simple_statement = simple_statement;
	n->val.switch_def.expression_opt=expression_opt;
	return n;
}

Node *newSwitchCases(Node *cases, Node *expressions, Node *statements, NodeKind kind, int lineno){
	Node *n = newNode(kind, lineno);
	n->val.switch_cases.cases = cases;
	n->val.switch_cases.expressions = expressions;
	n->val.switch_cases.statements = statements;
	return n;
}

Node *newBreak(int lineno){
	Node *n = newNode(k_NodeKindBreak, lineno);
	return n;
}

Node *newContinue(int lineno){
	Node *n = newNode(k_NodeKindContinue, lineno);
	return n;
}

Node *newExpressions(Node *expressions, Node *expression, int lineno){
	Node *n = newNode(k_NodeKindExpressions, lineno);
	n->val.expressions.expressions = expressions;
	n->val.expressions.expression = expression;
	return n;
}

Node *newExpressionOpt(Node *expression, int lineno){
	Node *n = newNode(k_NodeKindExpressionOpt, lineno);
	n->val.expression.expression = expression;
	return n;
}

Node *newExpressionsOpt(Node *expressions, int lineno){
	Node *n = newNode(k_NodeKindExpressionsOpt, lineno);
	n->val.expressions.expressions = expressions;
	return n;
}

Node *newExpression(Node *expression, int lineno){
	Node *n = newNode(k_NodeKindExpression, lineno);
	n->val.expression.expression = expression;
	return n;
}

Node *newExpressionBinary(Node *lhs, Node *rhs, NodeKind kind, int lineno){
	Node *n = newNode(kind, lineno); 
	n->val.binary.lhs = lhs;
	n->val.binary.rhs = rhs;
	return n;
}

Node *newExpressionUnary(Node *operand, NodeKind kind, int lineno){
	Node *n = newNode(kind, lineno); 
	n->val.unary.operand = operand;
	return n;
}

Node *newIntLiteral(int literal, int lineno){
	Node *n = newNode(k_NodeKindIntLiteral, lineno);
	n->val.intLiteral = literal;
	return n;
}

Node *newRuneLiteral(char* literal, int lineno){
	Node *n = newNode(k_NodeKindRuneLiteral, lineno);
	n->val.runeLiteral = literal;
	return n;
}

Node *newFloatLiteral(double literal, int lineno){
	Node *n = newNode(k_NodeKindFloatLiteral, lineno);
	n->val.floatLiteral = literal;
	return n;
}

Node *newStringLiteral(char* literal, int lineno){
	Node *n = newNode(k_NodeKindStringLiteral, lineno);
	n->val.stringLiteral = literal;
	return n;
}

Node *newExpressionPrimary(Node *primary_expression, Node *selector, Node *index, Node *identifier, int lineno){
	Node *n = newNode(k_NodeKindExpressionPrimary, lineno); 
	n->val.primary_expression.primary_expression = primary_expression;
	n->val.primary_expression.selector = selector;
	n->val.primary_expression.index = index;
	n->val.primary_expression.identifier = identifier;
	return n;
}

Node *newSelector(Node *identifier, int lineno){
	Node *n = newNode(k_NodeKindSelector, lineno); 
	n->val.selector.identifier = identifier;
	return n;
}

Node *newIndex(Node *expression, int lineno){
	Node *n = newNode(k_NodeKindIndex, lineno); 
	n->val.index.expression = expression;
	return n;
}

Node *newBuiltin(Node *expression1, Node *expression2, NodeKind kind, int lineno){
	Node *n = newNode(kind, lineno);
	n->val.builtins.expression1 = expression1;
	n->val.builtins.expression2 = expression2;
	return n;
}

Node *newFuncCall(Node *identifier, Node *expressions_opt, int lineno){
	Node *n = newNode(k_NodeKindFuncCall, lineno);
	n->val.func_call.identifier = identifier;
	n->val.func_call.expressions_opt = expressions_opt;
	return n;
}
//+++

