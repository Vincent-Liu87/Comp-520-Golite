#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <memory.h>
#include "symbol.h"
//#include "tree.h"
#include "type.h"


bool compareType(Type* a, Type* b) {//A very good method
	if (a == NULL && b == NULL) {
		return true;
	}
	if ((a == NULL && b != NULL) || (a != NULL && b == NULL)) {
		return false;
	}
	if(a->kind!=b->kind){
		return false;
	}
	
	switch(a->kind){
		k_NodeKindArrayType:
			if(a->val.identifier_type.size!=b->val.identifier_type.size){
				return false;
			}else{
				return compareType(a->val.identifier_type.identifier_type,b->val.identifier_type.identifier_type);
			}
			
		k_NodeKindParType:
		k_NodeKindSliceType:
			return compareType(a->val.identifier_type.identifier_type,b->val.identifier_type.identifier_type);
			
		k_NodeKindIdType:
			if(strcmp(a->val.identifier,b->val.identifier)==0){
				return true;
			} else{
				return false;
			}
			
		k_NodeKindStructType:
			;
			Type *aBody=a->val.identifier_type.identifier_type;
			Type *bBody=b->val.identifier_type.identifier_type;
			return compareType(aBody,bBody);
				
		k_NodeKindStructBody:
			;
			Exp *aFieldNames=a->val.struct_body.identifiers;
			Exp *bFieldNames=b->val.struct_body.identifiers;
			Type *aFieldType=a->val.struct_body.type;
			Type *bFieldType=b->val.struct_body.type;
			if(!compareIdList(aFieldNames,bFieldNames)){
				return false;
			} else if(!compareType(aFieldType,bFieldType)){
				return false;
			} else{
				return compareType(a->val.struct_body.struct_body,b->val.struct_body.struct_body);
			}
		}

}

bool compareIdList(Exp *ids1,Exp *ids2){
	if(ids1==NULL && ids2!=NULL){
		return false;
	}else if(ids1!=NULL&&ids2==NULL){
		return false;
	}else if(ids1==NULL&&ids2==NULL){
		return true;
	}else if(strcmp(ids1->val.identifiers.identifier->val.identifier,ids2->val.identifiers.identifier->val.identifier)!=0){
		return false;
	}else{
		return compareIdList(ids1->val.identifiers.identifiers,ids2->val.identifiers.identifiers);
	}
	
}

Type *inferType_Exp(SymbolTable* t, Exp* n){//TODO:
	if(n!=NULL){
		switch(n->kind){
			case k_NodeKindIdentifiers: // won't reach this case.
			{
				break;
			}
				
			case k_NodeKindIdentifier: // 		TODO: doesn't support _ (blank identifier)
			{
				;
				SYMBOL* symbol = getSymbol(t, n->val.identifier);
				Type* inferred_type = symbol->typelit.type;
				n->type = inferred_type;
				return inferred_type;
			}
				
			case k_NodeKindExpressions:
			case k_NodeKindExpressionsOpt:	// won't reach these cases.
				break;
				
			case k_NodeKindExpressionOpt:
			case k_NodeKindExpression:
				return inferType_Exp(t, n->val.expression.expression);
				
			/* --------------------------- Binary expressions: ---------------------------*/
			case k_NodeKindExpressionBinaryPlus:		// +
			{
				;
				Type* lhs = inferType_Exp(t, n->val.binary.lhs);
				Type* rhs = inferType_Exp(t, n->val.binary.rhs);
				if (compareType(lhs, rhs) && (isNumeric(resolveType(t, lhs)) || isString(resolveType(t, lhs)))) {
					n->type = lhs;
					return lhs;
				} else {
					fprintf(stderr, "Error: (line %d) The operands in binary expression don't type check.", n->lineno);
					exit(1);
				}
			}
					
			case k_NodeKindExpressionBinaryMinus:	// -
			case k_NodeKindExpressionBinaryMultiply:		// *
			case k_NodeKindExpressionBinaryDivide:	// /
			{
				;
				Type* lhs = inferType_Exp(t, n->val.binary.lhs);
				Type* rhs = inferType_Exp(t, n->val.binary.rhs);
				if (compareType(lhs, rhs) && isNumeric(resolveType(t, lhs)) ) {
					n->type = lhs;
					return lhs;
				} else {
					fprintf(stderr, "Error: (line %d) The operands in binary expression don't type check.", n->lineno);
					exit(1);
				}
			}
					
			case k_NodeKindExpressionBinaryModulo:	// %
			case k_NodeKindExpressionBinaryBitAnd:	// &
			case k_NodeKindExpressionBinaryBitOr:	// |
			case k_NodeKindExpressionBinaryBitXor:	// ^
			case k_NodeKindExpressionBinarybitClear:		  // &^
			case k_NodeKindExpressionBinaryLeftShift:	//  <<
			case k_NodeKindExpressionBinaryRightShift:	// >>
			{
				Type* lhs = inferType_Exp(t, n->val.binary.lhs);
				Type* rhs = inferType_Exp(t, n->val.binary.rhs);
				if (compareType(lhs, rhs) && isInteger(resolveType(t, lhs))) {
					n->type = lhs;
					return lhs;
				} else {
					fprintf(stderr, "Error: (line %d) The operands in binary expression don't type check.", n->lineno);
					exit(1);
				}
			}
				
			case k_NodeKindExpressionBinaryIsEqual:	// ==
			case k_NodeKindExpressionBinaryIsNotEqual:	// !=
			{
				Type* lhs = inferType_Exp(t, n->val.binary.lhs);
				Type* rhs = inferType_Exp(t, n->val.binary.rhs);
				if (compareType(lhs, rhs) && isComparable(resolveType(t, lhs))) {
					Type* inferred_type = newIdType("bool", n->lineno);
					n->type = inferred_type;
					return inferred_type;
				} else {
					fprintf(stderr, "Error: (line %d) The operands in binary expression don't type check.", n->lineno);
					exit(1);
				}
			}
				
			case k_NodeKindExpressionBinaryLessThan:		// <
			case k_NodeKindExpressionBinaryGreaterThan:		// >
			case k_NodeKindExpressionBinaryLessThanEqual:	// <=
			case k_NodeKindExpressionBinaryGreaterThanEqual:		// >=
			{
				Type* lhs = inferType_Exp(t, n->val.binary.lhs);
				Type* rhs = inferType_Exp(t, n->val.binary.rhs);
				if (compareType(lhs, rhs) && isOrdered(resolveType(t, lhs))) {
					Type* inferred_type = newIdType("bool", n->lineno);
					n->type = inferred_type;
					return inferred_type;
				} else {
					fprintf(stderr, "Error: (line %d) The operands in binary expression don't type check.", n->lineno);
					exit(1);
				}
			}
				
			case k_NodeKindExpressionBinaryAnd:		// ||		bool || bool = bool
			case k_NodeKindExpressionBinaryOr:		// &&		bool && bool = bool
			{
				Type* lhs = inferType_Exp(t, n->val.binary.lhs);
				Type* rhs = inferType_Exp(t, n->val.binary.rhs);
				if (compareType(lhs, rhs) && isBool(resolveType(t, lhs))) {
					n->type = lhs;
					return lhs;
				} else {
					fprintf(stderr, "Error: (line %d) The operands in binary expression don't type check.", n->lineno);
					exit(1);
				}
			}
			
			/* --------------------------- Unary expressions: ---------------------------*/
			case k_NodeKindUMinus:
			case k_NodeKindUPlus:	// -, + : numeric type
			{
				Type* operand_type = inferType_Exp(t, n->val.unary.operand);
				if(isNumeric(resolveType(t, operand_type))) {
					n->type = operand_type;
					return operand_type;
				} else {
					fprintf(stderr, "Error: (line %d) The operand in unary expression doesn't type check.", n->lineno);
					exit(1);
				}
			}
			case k_NodeKindUNot:		// ! : bool type
			{
				Type* operand_type = inferType_Exp(t, n->val.unary.operand);
				if(isBool(resolveType(t, operand_type))) {
					n->type = operand_type;
					return operand_type;
				} else {
					fprintf(stderr, "Error: (line %d) The operand in unary expression doesn't type check.", n->lineno);
					exit(1);
				}
			}
			case k_NodeKindUXor:		// ^ : int, rune
			{
				Type* operand_type = inferType_Exp(t, n->val.unary.operand);
				if(isInt(resolveType(t, operand_type))
					|| isRune(resolveType(t, operand_type))) {
					n->type = operand_type;
					return operand_type;
				} else {
					fprintf(stderr, "Error: (line %d) The operand in unary expression doesn't type check.", n->lineno);
					exit(1);
				}
			}
			/* ----------------------------------- Literals: -----------------------------------*/
			case k_NodeKindIntLiteral:
			{
				Type* inferred_type = newIdType("int", n->lineno);
				n->type = inferred_type;
				return inferred_type;
			}
			case k_NodeKindRuneLiteral:
			{
				Type* inferred_type = newIdType("rune", n->lineno);
				n->type = inferred_type;
				return inferred_type;
			}
			case k_NodeKindFloatLiteral:
			{
				Type* inferred_type = newIdType("float64", n->lineno);
				n->type = inferred_type;
				return inferred_type;
			}
			case k_NodeKindStringLiteral:
			{
				Type* inferred_type = newIdType("string", n->lineno);
				n->type = inferred_type;
				return inferred_type;
			}

			/* ----------------------------- Primary Expressions: -------------------------------*/
			case k_NodeKindExpressionsPrimary:	// TODO: Won't reach this case
			{
				break;
			}
				
			case k_NodeKindExpressionPrimary:
			{
				if(n->val.primary_expression.primary_expression != NULL){
					//n is a selector or an index
					if(n->val.primary_expression.index!=NULL){
						//n is an index
						Type* index_type = inferType_Exp(t, n->val.primary_expression.index);
						if(!isInt(index_type)){
							fprintf(stderr, "Error: (line %d) The index should be an integer.", n->lineno);
							exit(1);
						}
						Type* inferred_type = inferType_Exp(t, n->val.primary_expression.primary_expression);
						Type* resolved_type = resolveType(t, inferred_type);
						if (resolved_type->kind != k_NodeKindArrayType && resolved_type->kind != k_NodeKindSliceType) {
							fprintf(stderr, "Error: (line %d) The expression should be resolved to array or slice type.", n->lineno);
							exit(1);
						}
						n->type = resolved_type->val.identifier_type.identifier_type;
						return resolved_type->val.identifier_type.identifier_type;
					}else if(n->val.primary_expression.selector!=NULL){
						//n is a selector
						Type* inferred_type = inferType_Exp(t, n->val.primary_expression.primary_expression);
						Type* resolved_type = resolveType(t, inferred_type);
						if (resolved_type->kind != k_NodeKindStructType) {
							fprintf(stderr, "Error: (line %d) The expression should be resolved to struct type in field selection.", n->lineno);
							exit(1);
						}
						// Check if struct type contains a field called id.
						Type* id_type = checkStructId(resolved_type, n->val.primary_expression.selector->val.selector.identifier);
						if (id_type == NULL) {
							fprintf(stderr, "Error: (line %d) The struct doesn't contain the given identifier.", n->lineno);
							exit(1);
						}
						return id_type;
					}
					return inferType_Exp(t, n->val.primary_expression.primary_expression);
				} else if (n->val.primary_expression.identifier_type != NULL){
					// n is a type cast node.
					Type* identifier_type = resolveType(t, n->val.primary_expression.identifier_type);
					if (!isBasic(identifier_type)) {
						fprintf(stderr, "Error: (line %d) expression can only be casted to basic type.", n->lineno);
						exit(1);
					}
					Type* inferred_type = inferType_Exp(t, n->val.primary_expression.expression);
					Type* resolved_type = resolveType(t, inferred_type);
					if (compareType(identifier_type, resolved_type)
							|| (isNumeric(identifier_type) && isNumeric(resolved_type))
							|| (isString(identifier_type) && isInteger(resolved_type))) {
						n->type = n->val.primary_expression.identifier_type;
						return n->val.primary_expression.identifier_type;
					} else {
						fprintf(stderr, "Error: (line %d) the expression can't be casted to the given type.", n->lineno);
						exit(1);
					}
				} else {
					//n is a function call or identifier, get the type from symbol table
					Type* inferred_type = inferType_Exp(t, n->val.primary_expression.expression);
					n->type = inferred_type;
					return inferred_type;
				}
			}
				
			case k_NodeKindSelector:	// Won't reach this case.
			{
				break;
			}

			case k_NodeKindIndex:
				return inferType_Exp(t, n->val.index.expression);

			case k_NodeKindAppend://append []T T = []T
			{
				Type* type1 = inferType_Exp(t, n->val.builtins.expression1);
				Type* resolved_type1 = resolveType(t, type1);
				if (resolved_type1->kind != k_NodeKindArrayType) {
					fprintf(stderr, "Error: (line %d) The first expression in append should be resolved to array type.", n->lineno);
					exit(1);
				}
				Type* type2 = inferType_Exp(t, n->val.builtins.expression2);
				if(compareType(resolved_type1->val.identifier_type.identifier_type, type2)){
					n->type = type1;
					return type1;
				}else{
					fprintf(stderr, "Error: (line %d) Type checking of builtin function call failed", n->lineno);
					exit(1);
				}
			}
			case k_NodeKindLen://len []T/[N]T = int
			{
				Type* type1 = inferType_Exp(t, n->val.builtins.expression1);
				Type* resolved_type1 = resolveType(t, type1);
				if (resolved_type1->kind != k_NodeKindArrayType && resolved_type1->kind != k_NodeKindSliceType
						&& !isString(resolved_type1)) {
					fprintf(stderr, "Error: (line %d) The first expression in len function call should be resolved to array, slice, or string type.", n->lineno);
					exit(1);
				}
				n->type = newIdType("int", 0);
				return n->type;
			}
			case k_NodeKindCap://cap []T/[N]T = int
			{
				Type* type1 = inferType_Exp(t, n->val.builtins.expression1);
				Type* resolved_type1 = resolveType(t, type1);
				if (resolved_type1->kind != k_NodeKindArrayType && resolved_type1->kind != k_NodeKindSliceType) {
					fprintf(stderr, "Error: (line %d) The first expression in cap function call should be resolved to array or slice type.", n->lineno);
					exit(1);
				}
				n->type = newIdType("int", 0);
				return n->type;
			}

			case k_NodeKindFuncCall:// TODO: check func_type
			{
				SYMBOL* s = getSymbol(t, n->val.func_call.identifier->val.identifier);
				Type* return_type = s->typelit.functiondec.returnType;
				Exp* parameters = n->val.func_call.expressions_opt;
				TYPELIST* target_types = s->typelit.functiondec.typelist;

				if (parameter == NULL) { // Function with no params.
					if (target_types!= NUL)
					return return_type;
				}

				while(parameters!=NULL && target_types!=NULL){
					 Type* inferred_type = inferType_Exp(t, parameters->val.expressions.expression);
					 Type* target_type = target_types.currType;
					 if(compareType(inferred_type, target_type)){
						  parameters = parameters->val.expressions.expressions;
						  target_types = target_types.next;
					 }else{
						  fprintf(stderr, "Error: (line %d) Parameter types don't match.", n->lineno);
						  exit(1);
					 }
				}
				if(parameters!=NULL || target_types!=NULL){
					 fprintf(stderr, "Error: (line %d) Parameter numbers don't match.", n->lineno);
					 exit(1);
				}
				return return_type;
			}
		}
	}
	return NULL;
}

// Check if struct type contains a field called id.
// Retrun id type if found. Otherwise return NULL.
Type* checkStructId(Type* n, Exp* id) {
	if (n != NULL) {
		switch (n->kind) {
			case k_NodeKindStructType:
				return checkStructId(n->val.identifier_type.identifier_type, id);
			case k_NodeKindStructBody:
			{
				Exp* ids = n->val.struct_body->identifiers;
				while(ids != NULL) {
					if (strcmp(id->val.identifier, ids->val.identifiers.identifier->val.identifier) == 0) {
						return n->val.struct_body.type;
					}
					ids = ids->val.identifiers.identifiers;
				}
				return checkStructId(n->val.struct_body.struct_body, id);
			}
			default:	// Won't reach this case.
				return NULL;
		}
	} else {
		return NULL;
	}
}

// Find and return the underlying type 
Type* resolveType(SymbolTable* t, Type* type) {
	switch (type->kind) {
		case k_NodeKindArrayType:
		case k_NodeKindSliceType:	// Array & slice type doesn't resolve for inner.
			return type;
		case k_NodeKindIdType:
			if (strcmp(type->val.identifier, "int") == 0
					|| strcmp(type->val.identifier, "float64") == 0
					|| strcmp(type->val.identifier, "rune") == 0
					|| strcmp(type->val.identifier, "string") == 0) {
				return type;
			} else {
				SYMBOL* s = getSymbol(t, type->val.identifier);
				if (s->kind != symkind_type) {
					printf("error");
					exit(1);
				}
				return resolveType(t, s->typelit.type);
			}
		case k_NodeKindParType:
			return resolveType(t, type->val.identifier_type.identifier_type);
		case k_NodeKindStructType:	// Not sure.
			return type;
		case k_NodeKindStructBody:	// won't come here!
			return type;
	}
} 

// Check if the type is basic type
bool isBasic(Type* type) {
	return isInt(type) || isFloat64(type) || isBool(type) || isRune(type) || isString(type);
}

// Check if the type = float64
bool isFloat64(Type* type) {
	return compareType(type, newIdType("float64", 0));
}

// Check if the type = string
bool isString(Type* type) {
	return compareType(type, newIdType("string", 0));
}

// Check if the type = bool
bool isBool(Type* type) {
	return compareType(type, newIdType("bool", 0));
}

// Check if the type = int
bool isInt(Type* type) {
	return compareType(type, newIdType("int", 0));
}

// Check if the type = rune
bool isRune(Type* type) {
	return compareType(type, newIdType("rune", 0));
}
// Check if the type is numeric
bool isNumeric(Type* type) {
	return compareType(type, newIdType("int", 0)) || compareType(type, newIdType("float64", 0))
			|| compareType(type, newIdType("rune", 0));
}
// Check if the type is integer
bool isInteger(Type* type) {
	return compareType(type, newIdType("int", 0)) || compareType(type, newIdType("rune", 0));
}
/*	TODO: not sure what is pointer???
	comparable type: bool, int, float64, string, pointer, 
					 struct(if all their fields are comparable), 
					 array(if values of array element type are comparable)
*/
bool isComparable(Type* type) {
	if (type != NULL) {
		switch (type->kind) {
			case k_NodeKindArrayType:
			case k_NodeKindSliceType:
				return isComparable(type->val.identifier_type.identifier_type);
			case k_NodeKindIdType:
				if (strcmp(type->val.identifier, "int") == 0
						|| strcmp(type->val.identifier, "float64") == 0
						|| strcmp(type->val.identifier, "bool") == 0
						|| strcmp(type->val.identifier, "rune") == 0
						|| strcmp(type->val.identifier, "string") == 0) {
					return true;
				} else {
					return false;
				}
			case k_NodeKindParType:
				return isComparable(t, type->val.identifier_type.identifier_type);
			case k_NodeKindStructType:
				return isComparable(type->val.identifier_type.identifier_type);
			case k_NodeKindStructBody:
				if (!isComparable(n->val.struct_body.type)) {
					return false;
				}
				return isComparable(n->val.struct_body.struct_body);
		}
	} else {
		return true;
	}
}

/*  ordered type: int, float64, string  */
bool isOrdered(Type* type) {
	return compareType(type, newIdType("int", 0)) || compareType(type, newIdType("float64", 0))
				|| compareType(type, newIdType("string", 0));
}


