#include "rvcc.h"

// 在解析时，所有的变量实例都会被累加到这个列表里
Obj *Locals;

// 方法前置声明 
// 语法树规则
// 越往下优先级越高
// program = { 多个代码块语句(compoundStmt)
// compoundStmt = stmt* }
// stmt = return语句返回;隔开的expr表达式 或
//			if ( 表达式expr ) stmt else stmt 或
//			for ( exprStmt expr?; expr? ) stmt
//			{ 代码块语句(compoundStmt) 或
//			exprStmt(表达式语句) 后续会支持其他类型的语句
// exprStmt = 分号隔开的expr 或空语句;
// expr = assign 赋值表达式 或 递归的assign赋值表达式
// assign = equality 相等比较结果
// equality = 关系运算符的比较结果
// relational = 多个加数的比较结果
// add = 多个乘数的加减结果
// mul = 多个基数(primary)相乘除得到的
// unary = 一元运算符与基数(primary)
// primary = 括号内的算式(expr)或数字(num)或标识符(ident)
static Node *compoundStmt(Token **Rest, Token *Tok);
static Node *stmt(Token **Rest, Token *Tok);
static Node *exprStmt(Token **Rest, Token *Tok);
static Node *expr(Token **Rest, Token *Tok);
static Node *assign(Token **Rest, Token *Tok);
static Node *equality(Token **Rest, Token *Tok);
static Node *relational(Token **Rest, Token *Tok);
static Node *add(Token **Rest, Token *Tok);
static Node *mul(Token **Rest, Token *Tok);
static Node *unary(Token **Rest, Token *Tok);
static Node *primary(Token **Rest, Token *Tok);

// 通过名称，查找一个本地变量
static Obj *findVar(Token *Tok) {
	// 查找Locals变量中是否存在同名变量
	for (Obj *Var = Locals; Var; Var = Var->Next) {
		// 判断变量名是否和终结符名长度一直，然后逐字比较
		if(strlen(Var->Name) == Tok->Len && !strncmp(Tok->Loc, Var->Name, Tok->Len)) {
			return Var;
		}
	}
	return NULL;
}

// 新建一个节点
static Node *newNode(NodeKind Kind) {
	Node *Nod = calloc(1, sizeof(Node));
	Nod->Kind = Kind;
	return Nod;
}

// 在链表中新建一个变量
static Obj *newLVar(char *Name) {
	Obj *Var = calloc(1, sizeof(Obj));
	Var->Name = Name;
	// 将变量插入头部
	Var->Next = Locals;
	Locals = Var;
	return Var;
}

// 新建一个数字二叉树叶子
static Node *newNum(int Val) {
	Node *Nod = newNode(ND_NUM);
	Nod->Val = Val;
	return Nod;
}

// 新建一个变量叶子节点
static Node *newVarNode(Obj *Var) {
	Node *Nod = newNode(ND_VAR);
	Nod->Var = Var;
	return Nod;
}

// 新建一个单叉树节点
static Node *newUnary(NodeKind Kind, Node *Expr) {
	Node *Nod = newNode(Kind);
	Nod->LHS = Expr;
	return Nod;
}

// 新建一个二叉树节点
static Node *newBinary(NodeKind Kind, Node *LHS, Node *RHS) {
	Node *Nod = newNode(Kind);
	Nod->LHS = LHS;
	Nod->RHS = RHS;
	return Nod;
}

// 解析复合语句(代码块)
// compoundStmt = stmt* }
static Node *compoundStmt(Token **Rest, Token *Tok) {
	// 这里使用了和词法分析类似的单向链表结构
	// 来表示多个表达式语句
  Node Head = {};
  Node *Cur = &Head;
  // stmt* }
	// 匹配最近的}来结束{代码块，用于支持{}嵌套
  while (!equal(Tok, "}")) {
    Cur->Next = stmt(&Tok, Tok);
		// 处理下个表达式语句
    Cur = Cur->Next;
  }
	
	// Nod的Body中存储了{}内解析的语句
	// Body指的是当前{}中的所有语句构成的链表
	// } 在这里直接被忽略，没有进入语法树的构建
	Node *Nod = newNode(ND_BLOCK);
	Nod->Body = Head.Next;
	*Rest = Tok->Next;
	return Nod;
}

static Node *stmt(Token **Rest, Token *Tok) {
	// return expr ;
	if(equal(Tok, "return")) {
		Node *Nod = newUnary(ND_RETURN, expr(&Tok, Tok->Next));
		*Rest = skip(Tok, ";");
		return Nod;
	}

	// 解析if语句
	if (equal(Tok, "if")) {
		Node *Nod = newNode(ND_IF);
		// ( expr ) 条件内语句
		Tok = skip(Tok->Next, "(");
		Nod->Cond = expr(&Tok, Tok);
		Tok = skip(Tok, ")");
		// stmt 符合条件后执行的语句
		Nod->Then = stmt(&Tok, Tok);
		// else stmt? 不符合条件的语句
		if (equal(Tok, "else")) {
			Nod->Else = stmt(&Tok, Tok->Next);
		}
		*Rest = Tok;
		return Nod;
	}

	// 解析for语句
	if (equal(Tok, "for")) {
		Node *Nod = newNode(ND_FOR);

		Tok = skip(Tok->Next, "(");

		// for 的初始化语句
		Nod->Init = exprStmt(&Tok, Tok);

		// for 的条件语句
		if (!equal(Tok, ";")) {
			Nod->Cond = expr(&Tok, Tok);
		}
		Tok = skip(Tok, ";");

		// for 的递增语句
		if (!equal(Tok, ")")) {
			Nod->Inc = expr(&Tok, Tok);
		}
		Tok = skip(Tok, ")");

		// for 循环代码块
		Nod->Then = stmt(Rest, Tok);

		return Nod;
	}

	// 如果还有{，说明有{}嵌套了
	// 将子级{}作为一个新的复合语句生成语法树
	if(equal(Tok, "{")) {
		return compoundStmt(Rest, Tok->Next);
	}
	
	// exprStmt
	return exprStmt(Rest, Tok);
}

static Node *exprStmt(Token **Rest, Token *Tok) {

	// 空语句;
	if(equal(Tok, ";")) {
		*Rest = Tok->Next;
		// 将空语句设置为ND_BLOCK
		// 在codegen部分，当Nod为空时会直接结束循环和当前递归
		return newNode(ND_BLOCK);
	}

	// expr? ;
	Node *Nd = newUnary(ND_EXPR_STMT, expr(&Tok, Tok));
  *Rest = skip(Tok, ";");
  return Nd;
}

static Node *expr(Token **Rest, Token *Tok) {
	return assign(Rest, Tok);
	/* return equality(Rest, Tok); */
}

static Node *assign(Token **Rest, Token *Tok) {
	Node *Nod = equality(&Tok, Tok);

	// 可能存在递归赋值 a=b=1
	if(equal(Tok, "=")) {
		Nod = newBinary(ND_ASSIGN, Nod, assign(&Tok, Tok->Next));
	}

	*Rest = Tok;
	return Nod;
}

static Node *equality(Token **Rest, Token *Tok) {
	Node *Nod = relational(&Tok, Tok);

	while(true) {
		if(equal(Tok, "==")) {
			Nod = newBinary(ND_EQ, Nod, relational(&Tok, Tok->Next));
			continue;
		}

		if(equal(Tok, "!=")) {
			Nod = newBinary(ND_NEQ, Nod, relational(&Tok, Tok->Next));
			continue;
		}

		*Rest = Tok;
		return Nod;
	}
}

static Node *relational(Token **Rest, Token *Tok) {
	Node *Nod = add(&Tok, Tok);

	while(true) {
		if(equal(Tok, "<")) {
			Nod = newBinary(ND_LT, Nod, add(&Tok, Tok->Next));
			continue;
		}

		if(equal(Tok, "<=")) {
			Nod = newBinary(ND_LTEQ, Nod, add(&Tok, Tok->Next));
			continue;
		}
		// X > Y 相当于 Y < X
		// >= 同
		if(equal(Tok, ">")) {
			Nod = newBinary(ND_LT, add(&Tok, Tok->Next), Nod);
			continue;
		}

		if(equal(Tok, ">=")) {
			Nod = newBinary(ND_LTEQ, add(&Tok, Tok->Next), Nod);
			continue;
		}

		*Rest = Tok;
		return Nod;
	}
}

static Node *add(Token **Rest, Token *Tok) {
	Node *Nod = mul(&Tok, Tok);

	while(true) {
		if(equal(Tok, "+")) {
			Nod = newBinary(ND_ADD, Nod, mul(&Tok, Tok->Next));
			continue;
		}

		if(equal(Tok, "-")) {
			Nod = newBinary(ND_SUB, Nod, mul(&Tok, Tok->Next));
			continue;
		}

		*Rest = Tok;
		return Nod;
	}
}

static Node *mul(Token **Rest, Token *Tok) {

	Node *Nod = unary(&Tok, Tok);

	while(true) {
		if(equal(Tok, "*")) {
			Nod = newBinary(ND_MUL, Nod, unary(&Tok, Tok->Next));
			continue;
		}

		if(equal(Tok, "/")) {
			Nod = newBinary(ND_DIV, Nod, unary(&Tok, Tok->Next));
			continue;
		}

		*Rest = Tok;
		return Nod;
	}
}

static Node *unary(Token **Rest, Token *Tok) {

	// 一元运算符可以看作生成一个单叉树
	if(equal(Tok, "+")) {
		return unary(Rest, Tok->Next);
	}

	// 对于负号，我们仅将数字后面的第一个负号看作减号，之后如果还有负号则看作一元负号
	if(equal(Tok, "-")) {
		return newUnary(ND_NEG, unary(Rest, Tok->Next));
	}

	return primary(Rest, Tok);
}

static Node *primary(Token **Rest, Token *Tok) {

	// 如果是(expr)
	if(equal(Tok, "(")) {
		Node *Nod = expr(&Tok, Tok->Next);
		*Rest = skip(Tok, ")");
		return Nod;
	}

	// 如果是变量
	if(Tok->Kind == TK_IDENT) {
		Obj *Var = findVar(Tok);
		// 如果变量不存在，就在链表中新增一个变量
		if(!Var) {
			// strndup复制N个字符
			Var = newLVar(strndup(Tok->Loc, Tok->Len));
		}
		*Rest = Tok->Next;
		return newVarNode(Var);
	}

	// 如果是数字
	if(Tok->Kind == TK_NUM) {
		Node *Nod = newNum(Tok->Val);
		*Rest = Tok->Next;
		return Nod;
	}

	errorTok(Tok, "expected an expression");
	return NULL;
}

// 语法分析入口函数
Function *parse(Token *Tok){
	// 首先匹配{
	Tok = skip(Tok, "{");

	Function *Prog = calloc(1, sizeof(Function));
	Prog->Body = compoundStmt(&Tok, Tok);
	Prog->Locals = Locals;

  return Prog;
}
