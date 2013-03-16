%{
#include <stdio.h>
#include "ast.hpp"
#include "parser.hpp"

extern int yylex();
extern int yylineno,charno,yyleng;
extern FILE *yyin;
extern char *yytext;

void yyerror(const char *msg){
	cout<<yylineno<<":"<<(charno-yyleng)<<": error: "<<msg<<endl;
	if(yyin != NULL){
	fclose(yyin);
	}
	exit(1);
}

void setLocation(Node *node,YYLTYPE *loc,YYLTYPE *firstLoc,YYLTYPE *lastLoc){
	loc->first_line = firstLoc->first_line;
	loc->first_column = firstLoc->first_column;
	loc->last_line = lastLoc->last_line;
	loc->last_column = lastLoc->last_column;
	if(node != NULL){
	node->firstLine = loc->first_line;
	node->firstColumn = loc->first_column;
	node->lastLine = loc->last_line;
	node->lastColumn = loc->last_column;
	}
}

void setLocation(Node *node,YYLTYPE *loc){
	loc->first_line = yylineno;
	loc->first_column = charno;
	loc->last_line = yylineno;
	loc->last_column = charno-1;
	if(node != NULL){
	node->firstLine = loc->first_line;
	node->firstColumn = loc->first_line;
	node->lastLine = loc->last_line;
	node->lastColumn = loc->last_column;
	}
}

%}

%error-verbose
%debug

%union{
	int token;
	Ident *ident;
	Program *program;
	Statement *stmt;
	Expression *expr;
	VarInit *varInit;
	SimpleVarDecl *spvarDecl;
	SimpleStmtList *spstmtList;
	CallExpr *callExpr;
	GlobalStatement *globalStmt;
	
	vector<Ident*> *identList;
	vector<Statement*> *stmtList;
	vector<Expression*> *exprList;
	vector<VarInit*> *varInitList;
	vector<SimpleVarDecl*> *spvarDeclList;
	vector<GlobalStatement*> *globalStmtList;
}

%token <token> IDENT RETURN LEX_ERROR DOUBLE LONG 

%type <program> program
%type <ident> ident
%type <stmt> stmt simple_stmt var_decl var_assi return_stmt
%type <expr> expr
%type <varInit> var_init
%type <spvarDecl> simple_var_decl
%type <spstmtList> simple_stmt_list
%type <callExpr> call_expr
%type <globalStmt> global_stmt func_decl global_var_decl

%type <stmtList> stmt_list
%type <identList> ident_list ident_list_allow_null
%type <exprList> expr_list
%type <varInitList> var_init_list
%type <spvarDeclList> simple_var_decl_list
%type <globalStmtList> global_stmt_list

%left '+' '-'
%left '*' '/'
%nonassoc UMINUS

%start program

%%
program:
	global_stmt_list {program=new Program(*$1);$$=program;setLocation($$,&@$,&@1,&@1);}
	;
global_stmt_list:
	global_stmt {$$=new vector<GlobalStatement*>();$$->push_back($1);setLocation(NULL,&@$,&@1,&@1);}
	|global_stmt_list global_stmt {$1->push_back($2);$$=$1;setLocation(NULL,&@$,&@1,&@2);}
	;
global_stmt:
	global_var_decl ';' {$$=$1;setLocation($$,&@$,&@1,&@1);}
	|func_decl {$$=$1;setLocation($$,&@$,&@1,&@1);}
	;
global_var_decl:
	ident var_init_list {$$=new GlobalVarDecl(*$1,*$2);setLocation($$,&@$,&@1,&@2);}
	;
ident:
	IDENT {$$=new Ident(*(new string(yytext,yyleng)));setLocation($$,&@$,&@1,&@1);}
	;
var_init_list:
	var_init {$$=new vector<VarInit*>();$$->push_back($1);setLocation(NULL,&@$,&@1,&@1);}
	|var_init_list ',' var_init {$1->push_back($3);$$=$1;setLocation(NULL,&@$,&@1,&@3);}
	;
var_init:
	ident {$$=new VarInit(*$1,NULL);setLocation($$,&@$,&@1,&@1);}
	|ident '=' expr {$$=new VarInit(*$1,$3);setLocation($$,&@$,&@1,&@3);}
	;
func_decl:
	ident ident '(' simple_var_decl_list ')' '{' stmt_list '}'
	{vector<Ident*> *types = new vector<Ident*>();types->push_back($1);
	$$=new FuncDecl(*types,*$2,*$4,*$7);setLocation($$,&@$,&@1,&@8);}
	|ident_list ident '(' simple_var_decl_list ')' '{' stmt_list '}'
	{$$=new FuncDecl(*$1,*$2,*$4,*$7);setLocation($$,&@$,&@1,&@8);}
	|'[' simple_var_decl_list ']' ident '(' simple_var_decl_list ')' '{' stmt_list '}'
	{$$=new FuncDecl2(*$2,*$4,*$6,*$9);setLocation($$,&@$,&@1,&@10);}
	;
simple_var_decl_list:
	/*blank*/ {$$=new vector<SimpleVarDecl*>();setLocation(NULL,&@$);}
	|simple_var_decl {$$=new vector<SimpleVarDecl*>();$$->push_back($1);setLocation(NULL,&@$,&@1,&@1);}
	|simple_var_decl_list ',' simple_var_decl {$1->push_back($3);$$=$1;setLocation(NULL,&@$,&@1,&@3);}
	;
simple_var_decl:
	ident ident {$$=new SimpleVarDecl(*$1,*$2);setLocation($$,&@$,&@1,&@2);}
	;
ident_list:
	ident {$$=new vector<Ident*>();$$->push_back($1);setLocation(NULL,&@$,&@1,&@1);}
	|ident_list ',' ident {$1->push_back($3);$$=$1;setLocation(NULL,&@$,&@1,&@3);}
	;
stmt_list:
	/*blank*/ {$$=new vector<Statement*>();setLocation(NULL,&@$);}
	|stmt {$$=new vector<Statement*>();$$->push_back($1);setLocation(NULL,&@$,&@1,&@1);}
	|stmt_list stmt {$1->push_back($2);$$=$1;setLocation(NULL,&@$,&@1,&@2);}
	;
stmt:
	';' {$$=new NullStmt();setLocation($$,&@$,&@1,&@1);}
	|var_decl ';' {$$=$1;setLocation($$,&@$,&@1,&@2);}
	|return_stmt ';' {$$=$1;setLocation($$,&@$,&@1,&@2);}
	|simple_stmt_list ';' {$$=$1;setLocation($$,&@$,&@1,&@2);}
	;
var_decl:
	ident var_init_list {$$=new VarDecl(*$1,*$2);setLocation($$,&@$,&@1,&@2);}
	;
return_stmt:
	RETURN expr_list {$$=new ReturnStmt(*$2);setLocation($$,&@$,&@1,&@2);}
	;
simple_stmt_list:
	simple_stmt {$$=new SimpleStmtList();$$->add($1);setLocation($$,&@$,&@1,&@1);}
	|simple_stmt_list ',' simple_stmt {$1->add($3);$$=$1;setLocation($$,&@$,&@1,&@3);}
	;
simple_stmt:
	var_assi {$$=$1;setLocation($$,&@$,&@1,&@1);}
	|expr {$$=new ExprStmt(*$1);setLocation($$,&@$,&@1,&@1);}
	;
var_assi:
	ident '=' expr {$$=new VarAssi(*$1,*$3);setLocation($$,&@$,&@1,&@3);}
	|'[' ident_list_allow_null ']' '=' call_expr {$$=new MultiVarAssi(*$2,*$5);setLocation($$,&@$,&@1,&@5);}
	;
ident_list_allow_null:
	/*blank*/ {$$=new vector<Ident*>();$$->push_back(NULL);setLocation(NULL,&@$);}
	|ident {$$=new vector<Ident*>();$$->push_back($1);setLocation(NULL,&@$,&@1,&@1);}
	|ident_list_allow_null ',' ident {$1->push_back($3);$$=$1;setLocation(NULL,&@$,&@1,&@3);}
	|ident_list_allow_null ',' {$1->push_back(NULL);$$=$1;setLocation(NULL,&@$,&@1,&@2);}
	;
expr_list:
	/*blank*/ {$$=new vector<Expression*>();setLocation(NULL,&@$);}
	|expr {$$=new vector<Expression*>();$$->push_back($1);setLocation(NULL,&@$,&@1,&@1);}
	|expr_list ',' expr {$1->push_back($3);$$=$1;setLocation(NULL,&@$,&@1,&@3);}
	;
expr:
	expr '+' expr {$$=new BinaryExpr(*$1,'+',*$3);setLocation($$,&@$,&@1,&@3);}
	|expr '-' expr {$$=new BinaryExpr(*$1,'-',*$3);setLocation($$,&@$,&@1,&@3);}
	|expr '*' expr {$$=new BinaryExpr(*$1,'*',*$3);setLocation($$,&@$,&@1,&@3);}
	|expr '/' expr {$$=new BinaryExpr(*$1,'/',*$3);setLocation($$,&@$,&@1,&@3);}
	|'(' expr ')' {$$=$2;setLocation($$,&@$,&@1,&@3);}
	|'-' expr %prec UMINUS {$$=new PrefixExpr('-',*$2);setLocation($$,&@$,&@1,&@2);}
	|ident {$$=new IdentExpr(*$1);setLocation($$,&@$,&@1,&@1);}
	|LONG {$$=new Long(new string(yytext,yyleng));setLocation($$,&@$,&@1,&@1);}
	|DOUBLE {$$=new Double(new string(yytext,yyleng));setLocation($$,&@$,&@1,&@1);}
	|call_expr {$$=$1;setLocation($$,&@$,&@1,&@1);}
	;
call_expr:
	ident '(' expr_list ')' {$$=new CallExpr(*$1,*$3);setLocation($$,&@$,&@1,&@4);}
	;
