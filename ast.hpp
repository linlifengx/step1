#ifndef AST_HPP
#define AST_HPP
#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <llvm/Value.h>
#include <llvm/Module.h>
#include <llvm/LLVMContext.h>
#include <llvm/Type.h>
#include <llvm/DerivedTypes.h>
#include <llvm/Function.h>
#include <llvm/BasicBlock.h>
#include <llvm/Argument.h>
#include <llvm/Instructions.h>
#include <llvm/IRBuilder.h>

using namespace std;
using namespace llvm;

class AstFunction;
class AstContext;
class Node;
class Program;
class SimpleVarDecl;
class VarInit;
class Ident;

class GlobalStatement;
class GlobalVarDecl;
class FuncDecl;
class FuncDecl2;

class Statement;
class VarDecl;
class VarAssi;
class MultiVarAssi;
class ExprStmt;
class SimpleStmtList;
class NullStmt;
class ReturnStmt;

class Expression;
class BinaryExpr;
class PrefixExpr;
class IdentExpr;
class CallExpr;
class Long;
class Double;


extern LLVMContext &context;
extern IRBuilder<> builder;
extern Module module;
extern Function *startFunc;
extern string errorMsg;
extern Program *program;

extern Value* createCast(Value *value,Type *type);
extern Constant* getInitial(Type *type);
extern void throwError(Node *node);
extern void throwWarning(Node *node,string msg);
extern string getOperatorName(int op);
extern string getTypeName(Type *type);

class AstFunction{
public:
	string name;
	Function *llvmFunction;
	Type *returnType;
	bool isReturnSingle;
	bool isReturnVoid;
	vector<Type*> returnTypes;
	vector<Type*> argTypes;
	int style;
	vector<Value*> returnVars;
	AstFunction(string name,Function *llvmFunction,vector<Type*> &returnTypes,vector<Type*> &argTypes,int style=1)
		:name(name),llvmFunction(llvmFunction),returnTypes(returnTypes),argTypes(argTypes),style(style){
		isReturnSingle = (returnTypes.size() ==  1);
		isReturnVoid = (returnTypes.size() == 0);
		returnType = llvmFunction->getReturnType();
	}
};

class AstContext{
	AstContext *parent;
	map<string,Type*> typeTable;
	map<string,AstFunction*> functionTable;
	map<string,Value*> varTable;
public:
	AstFunction *currentFunc;

	AstContext(AstContext *parent=NULL):parent(parent){
		if(parent != NULL){
			currentFunc = parent->currentFunc;
		}else{
			currentFunc = NULL;
		}
	}

	Type* getType(string name);
	AstFunction* getFunction(string name);
	Value* getVar(string name);
	bool addFunction(string name,AstFunction *astFunction);
	bool addVar(string name,Value *var);
	bool addType(string name,Type *type);
};

class Node{
public:
	int firstLine;
	int firstColumn;
	int lastLine;
	int lastColumn;
};

class Program : public Node{
public:
	vector<GlobalStatement*> &stmts;
	Program(vector<GlobalStatement*> &stmts):stmts(stmts){}
	void codeGen(AstContext &astContext);
};

class Ident : public Node{
public:
	string &str;
	Ident(string &str):str(str){}
	operator string(){return str;}
};

class VarInit : public Node{
public:
	Ident &varName;
	Expression *expr;
	VarInit(Ident &varName,Expression *expr):varName(varName),expr(expr){}
};

class SimpleVarDecl : public Node{
public:
	Ident &typeName;
	Ident &varName;
	SimpleVarDecl(Ident &typeName,Ident &varName)
		:typeName(typeName),varName(varName){}
};

class GlobalStatement : public Node{
public:
	virtual void declGen(AstContext &astContext)=0;
	virtual void codeGen(AstContext &astContext)=0;
	virtual bool isFuncDecl()=0;
};

class GlobalVarDecl : public GlobalStatement{
public:
	Ident &typeName;
	vector<VarInit*> &varInitList;
	
	GlobalVarDecl(Ident &typeName,vector<VarInit*> &varInitList)
		:typeName(typeName),varInitList(varInitList){}
	void codeGen(AstContext &astContext);
	void declGen(AstContext &astContext);
	bool isFuncDecl(){return false;}
};

class FuncDecl : public GlobalStatement{
public:
	vector<Ident*> &retTypeNameList;
	Ident &funcName;
	vector<SimpleVarDecl*> &argDeclList;
	vector<Statement*> &stmtList;
	FuncDecl(vector<Ident*> &retTypeNameList,Ident &funcName,
			 vector<SimpleVarDecl*> &argDeclList,vector<Statement*> &stmtList)
				 :retTypeNameList(retTypeNameList),funcName(funcName),
				 argDeclList(argDeclList),stmtList(stmtList){}
	void declGen(AstContext &astContext);
	void codeGen(AstContext &astContext);
	bool isFuncDecl(){return true;};
};

class FuncDecl2 : public GlobalStatement{
public:
	vector<SimpleVarDecl*> &retDeclList;
	Ident &funcName;
	vector<SimpleVarDecl*> &argDeclList;
	vector<Statement*> &stmts;
	FuncDecl2(vector<SimpleVarDecl*> &retDeclList,Ident &funcName,
			 vector<SimpleVarDecl*> &argDeclList,vector<Statement*> &stmts)
				 :retDeclList(retDeclList),funcName(funcName),
				 argDeclList(argDeclList),stmts(stmts){}
	void declGen(AstContext &astContext);
	void codeGen(AstContext &astContext);
	bool isFuncDecl(){return true;}
};

class Statement : public Node{
public:
	virtual void codeGen(AstContext &astContext)=0;
};

class VarDecl : public Statement{
public:
	Ident &typeName;
	vector<VarInit*> &varInitList;
	
	VarDecl(Ident &typeName,vector<VarInit*> &varInitList)
		:typeName(typeName),varInitList(varInitList){}
	void codeGen(AstContext &astContext);
};

class VarAssi : public Statement{
public:
	Ident &varName;
	Expression &expr;
	VarAssi(Ident &varName,Expression &expr):varName(varName),expr(expr){}
	void codeGen(AstContext &astContext);
};

class MultiVarAssi : public Statement{
public:
	vector<Ident*> &varNameList;
	CallExpr &callExpr;
	MultiVarAssi(vector<Ident*> &varNameList,CallExpr &callExpr)
		:varNameList(varNameList),callExpr(callExpr){}
	void codeGen(AstContext &astContext);
};

class SimpleStmtList : public Statement{
public:
	vector<Statement*> stmtList;
	SimpleStmtList(){}
	void add(Statement *stmt);
	void codeGen(AstContext &astContext);
};

class ExprStmt : public Statement{
public:
	Expression &expr;
	ExprStmt(Expression &expr):expr(expr){}
	void codeGen(AstContext &astContext);
};

class NullStmt : public Statement{
public:
	NullStmt(){}
	void codeGen(AstContext &astContext){}
};

class ReturnStmt : public Statement{
public:
	vector<Expression*> &exprList;
	ReturnStmt(vector<Expression*> &exprList):exprList(exprList){}
	void codeGen(AstContext &astContext);
};

class Expression : public Node{
public:
	virtual Value* codeGen(AstContext &astContext)=0;
};

class BinaryExpr : public Expression{
public:
	Expression &lexpr;
	Expression &rexpr;
	int op;
	BinaryExpr(Expression &lexpr,int op,Expression &rexpr)
		:lexpr(lexpr),rexpr(rexpr),op(op){}
	Value* codeGen(AstContext &astContext);
};

class PrefixExpr : public Expression{
public:
	int op;
	Expression &expr;
	PrefixExpr(int op,Expression &expr):op(op),expr(expr){}
	Value* codeGen(AstContext &astContext);
};

class IdentExpr : public Expression{
public:
	Ident &ident;
	IdentExpr(Ident &ident):ident(ident){}
	Value* codeGen(AstContext &astContext);
	operator string();
};

class CallExpr : public Expression{
public:
	Ident &funcName;
	vector<Expression*> &exprList;
	CallExpr(Ident &funcName,vector<Expression*> &exprList)
		:funcName(funcName),exprList(exprList){}
	Value* codeGen(AstContext &astContext);
	vector<Value*> multiCodeGen(AstContext &astContext);
};

class Long : public Expression{
public:
	string *valStr;
	Long(string *valStr):valStr(valStr){}
	Value* codeGen(AstContext &astContext);
};

class Double : public Expression{
public:
	string *valStr;
	Double(string *valStr):valStr(valStr){}
	Value* codeGen(AstContext &astContext);
};

#endif // AST_HPP
