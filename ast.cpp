#include "ast.hpp"

void throwError(Node *node){
	cout<<node->firstLine<<":"<<node->firstColumn<<": error: "<<errorMsg<<endl;
	exit(1);
}

void throwWarning(Node *node,string msg){
	cout<<node->firstLine<<":"<<node->firstColumn<<": warning: "<<msg<<endl;
}

string getOperatorName(int op){
	switch(op){
	case '+':
		return "+";
	case '-':
		return "-";
	case '*':
		return "*";
	case '/':
		return "/";
	defalut:
		return "unknow";
	}
}

string getTypeName(Type *type){
	if(type->isDoubleTy()){
		return "double";
	}else if(type->isIntegerTy(64)){
		return "long";
	}if(type->isVoidTy()){
		return "void";
	}else{
		return "unknow";
	}
}

Value* createCast(Value *value,Type *type){
	Type *valType = value->getType();
	if(valType == type){
		return value;
	}else if(type->isDoubleTy() && valType->isDoubleTy()){
		return value;
	}else if(type->isIntegerTy(64) && valType->isIntegerTy(64)){
		return value;
	}else if(type->isDoubleTy() && valType->isIntegerTy(64)){
		return builder.CreateSIToFP(value,type);
	}else if(type->isIntegerTy(64) && valType->isDoubleTy()){
		return builder.CreateFPToSI(value,type);
	}else{
		errorMsg = "no viable conversion from '"+getTypeName(valType)
					  +"' to '"+getTypeName(type)+"'";
		return NULL;
	}
}

Constant* getInitial(Type *type){
	if(type->isDoubleTy()){
		return ConstantFP::get(builder.getDoubleTy(),0);
	}else if(type->isIntegerTy(64)){
		return builder.getInt64(0);
	}else{
		errorMsg = "no initializer for '"+getTypeName(type)+"'";
		return NULL;
	}
}

Type* AstContext::getType(string name){
	Type *type = typeTable[name];
	if(type == NULL && parent != NULL){
		type = parent->getType(name);
	}
	if(type == NULL){
		if(name == "void"){
			errorMsg = "variable has incomplete type 'void'";
		}else{
			errorMsg = "undeclared type '"+name+"'";
		}
	}
	return type;
}

AstFunction* AstContext::getFunction(string name) throw(string){
	AstFunction *function = functionTable[name];
	if(function == NULL && parent != NULL){
		return parent->getFunction(name);
	}
	if(function == NULL){
		errorMsg = "undeclared function '"+name+"'";
	}
	return function;
}

Value* AstContext::getVar(string name){
	Value *var = varTable[name];
	if(var == NULL && parent != NULL){
		return parent->getVar(name);
	}
	if(var == NULL){
		errorMsg = "undeclared identifier '"+name+"'";
	}
	return var;
}

bool AstContext::addFunction(string name, AstFunction *function){
	if(functionTable[name] != NULL){
		errorMsg = "redefine function named '"+name+"'";
		return false;
	}
	functionTable[name] = function;
	return true;
}

bool AstContext::addVar(string name, Value *value){
	if(varTable[name] != NULL){
		errorMsg = "redefine variable named '"+name+"'";
		return false;
	}
	varTable[name] = value;
	return true;
}

bool AstContext::addType(string name, Type *type){
	if(typeTable[name] != NULL){
		errorMsg =  "redefine type named '"+name+"'";
		return false;
	}
	typeTable[name] = type;
	return true;
}

void Program::codeGen(AstContext &astContext){
	for(unsigned i=0; i<stmts.size(); i++){
		GlobalStatement *stmt = stmts[i];
		stmt->declGen(astContext);
	}
	
	//create init func
	FunctionType *initFuncType = FunctionType::get(builder.getVoidTy(),false);
	Function *initFunc = Function::Create(initFuncType,Function::ExternalLinkage,"main",&module);
	builder.SetInsertPoint(BasicBlock::Create(context,"entry",initFunc));
	for(unsigned i=0;i<stmts.size();i++){
		GlobalStatement *stmt = stmts[i];
		if(!stmt->isFuncDecl()){
			stmt->codeGen(astContext);
		}
	}

	AstFunction *mainFunc = astContext.getFunction("main");
	if(mainFunc == NULL){
		cout<<errorMsg<<endl;
	}else{
		builder.CreateCall(mainFunc->llvmFunction);
		builder.CreateRetVoid();
	}

	startFunc = initFunc;

	for(unsigned i = 0; i < stmts.size(); i++){
		GlobalStatement *stmt = stmts[i];
		if(stmt->isFuncDecl()){
			stmt->codeGen(astContext);
		}
	}
}

void GlobalVarDecl::declGen(AstContext &astContext){
	Type *type = astContext.getType(typeName);
	if(type == NULL){
		throwError(&typeName);
	}
	Constant *initial = getInitial(type);
	if(initial == NULL){
		throwError(this);
	}
	for(unsigned i = 0; i < varInitList.size(); i++){
		VarInit *varInit = varInitList[i];
		Value *var = new GlobalVariable(module,type,false,GlobalValue::ExternalLinkage,initial);
		astContext.addVar(varInit->varName,var);
	}
}

void GlobalVarDecl::codeGen(AstContext &astContext){
	Type *type = astContext.getType(typeName);
	if(type == NULL){
		throwError(&typeName);
	}
	for(unsigned i = 0; i < varInitList.size(); i++){
		VarInit *varInit = varInitList[i];
		if(varInit->expr != NULL){
			Value *var = astContext.getVar(varInit->varName);
			Value *v = varInit->expr->codeGen(astContext);
			v = createCast(v,type);
			if(v == NULL){
				throwError(varInit->expr);
			}
			builder.CreateStore(v,var);
		}
	}
}

void FuncDecl::declGen(AstContext &astContext){
	vector<Type*> returnTypes;
	if(retTypeNameList.size() > 1 || retTypeNameList[0]->str != "void"){
		for(unsigned i = 0; i < retTypeNameList.size(); i++){
			Type *type = astContext.getType(*retTypeNameList[i]);
			if(type == NULL){
				throwError(retTypeNameList[i]);
			}
			returnTypes.push_back(type);
		}
	}
	
	Type *returnType = NULL;
	if(returnTypes.size() == 0){
		returnType = builder.getVoidTy();
	}else if(returnTypes.size() == 1){
		returnType =  returnTypes[0];
	}else{
		ArrayRef<Type*> typesArray(returnTypes);
		returnType = StructType::create(context,typesArray);
	}
	
	vector<Type*> argTypes;
	for(unsigned i = 0; i < argDeclList.size(); i++){
		SimpleVarDecl *argDecl = argDeclList[i];
		Type *type = astContext.getType(argDecl->typeName);
		if(type == NULL){
			throwError(&argDecl->typeName);
		}
		argTypes.push_back(type);
	}
	
	FunctionType *functionType = NULL;
	if(argTypes.size() == 0){
		functionType = FunctionType::get(returnType,false);
	}else{
		ArrayRef<Type*> argTypeArrayRef(argTypes);
		functionType = FunctionType::get(returnType,argTypeArrayRef,false);
	}
	Function *function = Function::Create(functionType,Function::ExternalLinkage,funcName.str+"_sp",&module);
	AstFunction *astFunction = new AstFunction(funcName,function,returnTypes,argTypes);
	if(!astContext.addFunction(funcName,astFunction)){
		throwError(&funcName);
	}
}

void FuncDecl::codeGen(AstContext &astContext){
	AstFunction *astFunction = astContext.getFunction(funcName);
	Function* function = astFunction->llvmFunction;
	vector<Type*> &argTypes = astFunction->argTypes;
	vector<Type*> &returnTypes = astFunction->returnTypes;
	AstContext newContext(&astContext);
	builder.SetInsertPoint(BasicBlock::Create(context,"entry",function));
	unsigned i = 0;
	for(Function::arg_iterator ai = function->arg_begin();ai != function->arg_end(); ai++,i++){
		SimpleVarDecl *argDecl = argDeclList[i];
		Value *alloc = builder.CreateAlloca(argTypes[i]);
		builder.CreateStore(ai,alloc);
		if(!newContext.addVar(argDecl->varName,alloc)){
			throwError(&argDecl->varName);
		}
	}
	newContext.currentFunc = astFunction;
	for(i = 0; i < stmtList.size(); i++){
		stmtList[i]->codeGen(newContext);
	}
	if(astFunction->isReturnVoid){
		builder.CreateRetVoid();
	}else if(astFunction->isReturnSingle){
		Value *retVal = getInitial(astFunction->returnType);
		if(retVal == NULL){
			throwError(retTypeNameList[0]);
		}
		builder.CreateRet(retVal);
	}else{
		Value *alloc = builder.CreateAlloca(astFunction->returnType);
		for(i = 0; i < returnTypes.size(); i++){
			Value *element = builder.CreateStructGEP(alloc,i);
			Value *elemVal = getInitial(returnTypes[i]);
			if(elemVal == NULL){
				throwError(retTypeNameList[i]);
			}
			builder.CreateStore(elemVal,element);
		}
		builder.CreateRet(builder.CreateLoad(alloc));
	}
}

void FuncDecl2::declGen(AstContext &astContext){
	vector<Type*> returnTypes;
	for(unsigned i = 0; i < retDeclList.size(); i++){
		SimpleVarDecl *retDecl = retDeclList[i];
		Type *type = astContext.getType(retDecl->typeName);
		if(type == NULL){
			throwError(&retDecl->typeName);
		}
		returnTypes.push_back(type);
	}
	
	Type *returnType = NULL;
	if(returnTypes.size() == 0){
		returnType = builder.getVoidTy();
	}else if(returnTypes.size() == 1){
		returnType =  returnTypes[0];
	}else{
		ArrayRef<Type*> typesArray(returnTypes);
		returnType = StructType::create(context,typesArray);
	}
	
	vector<Type*> argTypes;
	for(unsigned i = 0; i < argDeclList.size(); i++){
		SimpleVarDecl *argDecl = argDeclList[i];
		Type *type = astContext.getType(argDecl->typeName);
		if(type == NULL){
			throwError(&argDecl->typeName);
		}
		argTypes.push_back(type);
	}
	
	FunctionType *functionType = NULL;
	if(argTypes.size() == 0){
		functionType = FunctionType::get(returnType,false);
	}else{
		ArrayRef<Type*> argTypeArrayRef(argTypes);
		functionType = FunctionType::get(returnType,argTypeArrayRef,false);
	}
	Function *function = Function::Create(functionType,Function::ExternalLinkage,funcName.str+"_sp",&module);
	AstFunction *astFunction = new AstFunction(funcName,function,returnTypes,argTypes,2);
	if(!astContext.addFunction(funcName,astFunction)){
		throwError(&funcName);
	}
}

void FuncDecl2::codeGen(AstContext &astContext){
	AstFunction *astFunction = astContext.getFunction(funcName);
	Function* function = astFunction->llvmFunction;
	vector<Type*> &argTypes = astFunction->argTypes;
	vector<Type*> &retTypes = astFunction->returnTypes;
	AstContext newContext(&astContext);
	builder.SetInsertPoint(BasicBlock::Create(context,"entry",function));
	unsigned i = 0;
	for(Function::arg_iterator ai = function->arg_begin();ai != function->arg_end(); ai++,i++){
		SimpleVarDecl *argDecl = argDeclList[i];
		Value *alloc = builder.CreateAlloca(argTypes[i]);
		builder.CreateStore(ai,alloc);
		if(!newContext.addVar(argDecl->varName,alloc)){
			throwError(&argDecl->varName);
		}
	}
	vector<Value*> retVarList;
	for(i = 0; i < retDeclList.size(); i++){
		SimpleVarDecl *retDecl = retDeclList[i];
		Value *alloc = builder.CreateAlloca(retTypes[i]);
		Value *initial = getInitial(retTypes[i]);
		if(initial == NULL){
			throwError(&retDecl->typeName);
		}
		if(!newContext.addVar(retDecl->varName,alloc)){
			throwError(&retDecl->varName);
		}
		retVarList.push_back(alloc);
	}
	astFunction->returnVars = retVarList;
	newContext.currentFunc = astFunction;
	for(i = 0; i < stmts.size(); i++){
		stmts[i]->codeGen(newContext);
	}
	if(astFunction->isReturnVoid){
		builder.CreateRetVoid();
	}else if(astFunction->isReturnSingle){
		builder.CreateRet(builder.CreateLoad(retVarList[0]));
	}else{
		Value *alloc = builder.CreateAlloca(astFunction->returnType);
		for(i = 0; i < retVarList.size(); i++){
			Value *element = builder.CreateStructGEP(alloc,i);
			builder.CreateStore(builder.CreateLoad(retVarList[i]),element);
		}
		builder.CreateRet(builder.CreateLoad(alloc));
	}
}

void VarDecl::codeGen(AstContext &astContext){
	Type *type = astContext.getType(typeName);
	if(type == NULL){
		throwError(&typeName);
	}
	
	for(unsigned i = 0; i < varInitList.size(); i++){
		VarInit *varInit = varInitList[i];
		Value *var = NULL;
		Value *v = NULL;
		if(varInit->expr != NULL){
			v = varInit->expr->codeGen(astContext);
			v = createCast(v,type);
			if(v == NULL){
				throwError(varInit->expr);
			}
		}else{
			v = getInitial(type);
			if(v == NULL){
				throwError(&typeName);
			}
		}
		var = builder.CreateAlloca(type);
		builder.CreateStore(v,var);
		if(!astContext.addVar(varInit->varName,var)){
			throwError(&varInit->varName);
		}
	}
}

void VarAssi::codeGen(AstContext &astContext){
	Value *var = astContext.getVar(varName);
	if(var == NULL){
		throwError(&varName);
	}
	
	Value *value = expr.codeGen(astContext);
	PointerType *pt = static_cast<PointerType*>(var->getType());
	value = createCast(value,pt->getElementType());
	if(value == NULL){
		throwError(&expr);
	}
	builder.CreateStore(value,var);
}

void MultiVarAssi::codeGen(AstContext &astContext){
	vector<Value*> vars;
	for(unsigned i=0; i < varNameList.size(); i++){
		Ident *varName = varNameList[i];
		if(varName == NULL){
			vars.push_back(NULL);
		}else{
			Value *var = astContext.getVar(*varName);
			if(var == NULL){
				throwError(varName);
			}
			vars.push_back(var);
		}
	}
	
	vector<Value*> values = callExpr.multiCodeGen(astContext);
	if(values.size() < vars.size()){
		errorMsg = "too few values returned from function '"+callExpr.funcName.str+"'";
		throwError(&callExpr);
	}
	for(unsigned i=0; i < vars.size(); i++){
		if(vars[i] == NULL){
			continue;
		}
		Value *v = values[i];
		PointerType *pt = static_cast<PointerType*>(vars[i]->getType());
		v = createCast(v,pt->getElementType());
		if(v == NULL){
			throwError(&callExpr);
		}
		builder.CreateStore(v,vars[i]);
	}
}

void SimpleStmtList::codeGen(AstContext &astContext){
	for(unsigned i = 0; i < stmtList.size(); i++){
		stmtList[i]->codeGen(astContext);
	}
}

void SimpleStmtList::add(Statement *stmt){
	stmtList.push_back(stmt);
}

void ExprStmt::codeGen(AstContext &astContext){
	expr.codeGen(astContext);
}

void ReturnStmt::codeGen(AstContext &astContext){	
	AstFunction *currentFunc = astContext.currentFunc;
	if(currentFunc->style == 1){
		vector<Type*> &returnTypes = currentFunc->returnTypes;
		if(exprList.size() < returnTypes.size()){
			errorMsg = "too few values to return in function '"+currentFunc->name+"'";
			throwError(this);
		}else if(exprList.size() > returnTypes.size()){
			errorMsg = "too many values to return in function '"+currentFunc->name+"'";
			throwError(this);
		}
		
		vector<Value*> exprListValues;
		for(unsigned i=0; i < exprList.size(); i++){
			Expression *expr = exprList[i];
			exprListValues.push_back(expr->codeGen(astContext));
		}
		if(returnTypes.size() == 0){
			builder.CreateRetVoid();
		}else if(returnTypes.size() == 1){
			Value *v = createCast(exprListValues[0],returnTypes[0]);
			if(v == NULL){
				throwError(exprList[0]);
			}
			builder.CreateRet(v);
		}else{
			Value *alloc = builder.CreateAlloca(currentFunc->returnType);
			for(unsigned i=0; i < returnTypes.size(); i++){
				Value *element = builder.CreateStructGEP(alloc,i);
				Value *v = createCast(exprListValues[i],returnTypes[i]);
				if(v == NULL){
					throwError(exprList[i]);
				}
				builder.CreateStore(v,element);
			}
			builder.CreateRet(builder.CreateLoad(alloc));
		}
	}else{
		if(exprList.size() > 0){
			errorMsg = "needn't declare any expression behind 'return' in style 2 function";
			throwError(exprList[0]);
		}
		if(currentFunc->isReturnVoid){
			builder.CreateRetVoid();
		}else if(currentFunc->isReturnSingle){
			Value *v = builder.CreateLoad(currentFunc->returnVars[0]);
			builder.CreateRet(v);
		}else{
			Value *alloc = builder.CreateAlloca(currentFunc->returnType);
			for(unsigned i = 0; i < currentFunc->returnVars.size(); i++){
				Value *element = builder.CreateStructGEP(alloc,i);
				Value *v = builder.CreateLoad(currentFunc->returnVars[i]);
				builder.CreateStore(v,element);
			}
			builder.CreateRet(builder.CreateLoad(alloc));
		}
	}
	BasicBlock *anonyBB = BasicBlock::Create(context,"after_return",currentFunc->llvmFunction);
	builder.SetInsertPoint(anonyBB);
}

Value* BinaryExpr::codeGen(AstContext &astContext){
	Value *lv = lexpr.codeGen(astContext);
	Value *rv = rexpr.codeGen(astContext);
	if( (lv->getType()->isDoubleTy() || lv->getType()->isIntegerTy(64))
		&& (lv->getType()->isDoubleTy() || lv->getType()->isIntegerTy(64)) ){
		if(lv->getType()->isDoubleTy()){
			rv = createCast(rv,lv->getType());
			if(rv == NULL){
				throwError(&rexpr);
			}
		}else{
			lv = createCast(lv,rv->getType());
			if(lv == NULL){
				throwError(&lexpr);
			}
		}
		if(lv->getType()->isDoubleTy()){
			switch(op){
			case '+':
				return builder.CreateFAdd(lv,rv);
			case '-':
				return builder.CreateFSub(lv,rv);
			case '*':
				return builder.CreateFMul(lv,rv);
			case '/':
				return builder.CreateFDiv(lv,rv);
			default:
				;
			}
		}else{
			switch(op){
			case '+':
				return builder.CreateAdd(lv,rv);
			case '-':
				return builder.CreateSub(lv,rv);
			case '*':
				return builder.CreateMul(lv,rv);
			case '/':
				return builder.CreateSDiv(lv,rv);
			default:
				;
			}
		}
	}
	errorMsg = "invalid operands to binary expression ("+getTypeName(lv->getType())+
				 " "+getOperatorName(op)+" "+getTypeName(rv->getType())+")";
	throwError(this);
}

Value* PrefixExpr::codeGen(AstContext &astContext){
	Value *val = expr.codeGen(astContext);
	if(op == '-'){
		if(val->getType()->isDoubleTy()){
			return builder.CreateFNeg(val);
		}else if(val->getType()->isIntegerTy(64)){
			return builder.CreateNeg(val);
		}
	}
	errorMsg = "invalid argument type '"+getTypeName(val->getType())+
				 "' to unary '"+getOperatorName(op)+"'expression";
	throwError(this);
}

Value* IdentExpr::codeGen(AstContext &astContext){
	Value *var = astContext.getVar(ident);
	if(var == NULL){
		throwError(this);
	}
	return builder.CreateLoad(var);
}

vector<Value*> CallExpr::multiCodeGen(AstContext &astContext){
	AstFunction *myfunc = astContext.getFunction(funcName);
	if(myfunc == NULL){
		throwError(this);
	}
	vector<Type*> &argTypes = myfunc->argTypes;
	vector<Value*> exprListValues;
	for(unsigned i=0; i < exprList.size(); i++){
		Expression *expr = exprList[i];
		exprListValues.push_back(expr->codeGen(astContext));
	}
	if(exprListValues.size() < argTypes.size()){
		errorMsg = "too few arguments to function '"+funcName.str+"''";
		throwError(this);
	}else if(exprListValues.size() > argTypes.size()){
		cout<<"too many arguments to function '"<<funcName.str<<"'"<<endl;
	}
	
	Value *callResult = NULL;
	if(argTypes.size() == 0){
		callResult = builder.CreateCall(myfunc->llvmFunction);
	}else{
		vector<Value*> argValues;
		for(unsigned i=0; i < argTypes.size(); i++){
			Value *v = createCast(exprListValues[i],argTypes[i]);
			if(v == NULL){
				throwError(exprList[i]);
			}
			argValues.push_back(v);
		}
		ArrayRef<Value*> args(argValues);
		callResult = builder.CreateCall(myfunc->llvmFunction,args);
	}
	
	vector<Value*> resultValues;
	vector<Type*> &resultTypes = myfunc->returnTypes;
	if(myfunc->isReturnVoid){
		resultValues.push_back(callResult);
	}else if(myfunc->isReturnSingle){
		resultValues.push_back(callResult);
	}else{
		Value *alloc = builder.CreateAlloca(myfunc->returnType);
		builder.CreateStore(callResult,alloc);
		for(unsigned i=0; i < resultTypes.size(); i++){
			Value *element = builder.CreateStructGEP(alloc,i);
			resultValues.push_back(builder.CreateLoad(element));
		}
	}
	return resultValues;
}

Value* CallExpr::codeGen(AstContext &astContext){
	vector<Value*> resultValues = multiCodeGen(astContext);
	return resultValues[0];
}

Value* Long::codeGen(AstContext &astContext){
	return builder.getInt64(atol(valStr->c_str()));
}

Value* Double::codeGen(AstContext &astContext){
	return ConstantFP::get(builder.getDoubleTy(),atof(valStr->c_str()));
}
