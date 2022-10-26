#include "rvcc.h"

// 局部和全局变量的域
typedef struct VarScope VarScope;
struct VarScope {
	VarScope *Next;
	char *Name;
	Obj *Var;
};

// 结构体和联合体标签的域
typedef struct TagScope TagScope;
struct TagScope {
	TagScope *Next;		// 下一标签域
	char *Name;				// 域名称
	Type *Ty;					// 域类型
};

// 表示一个块域
typedef struct Scope Scope;
struct Scope {
	// 指向上一级的域
	Scope *Next;

	// C有两个域: 变量域和结构体标签域
	// 指向当前域的变量
	VarScope *Vars;
	// 指向当前域内的结构体标签
	TagScope *Tags;
};

// 指向所有域的链表
static Scope *Scp = &(Scope){};

// 在解析时，所有的变量实例都会被累加到这个列表里
Obj *Locals;		// 局部变量
Obj *Globals;		// 全局变量

// 方法前置声明 
// 语法树规则
// 越往下优先级越高
// program = functionDefinition* | global-variable)*
// functionDefinition = declspec declarator? ident "(" ")" "{" compoundStmt*
// declspec = "char" | "int" | "long" | "short" | structDecl | unionDecl
// declarator = "*"* ident typeSuffix
// typeSuffix = typeSuffix = "(" funcParams | "[" num "]" typeSuffix | ε
// funcParams = (param ("," param)*)? ")"
// param = declspec declarator
// compoundStmt = (declaration | stmt)* "}"
// declaration =
//    declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
// stmt = return语句返回;隔开的expr表达式 或
//			if ( 表达式expr ) stmt else stmt 或
//			for ( exprStmt expr?; expr? ) stmt
//			while ( expr ) stmt
//			{ 代码块语句(compoundStmt) 或
//			exprStmt(表达式语句) 后续会支持其他类型的语句
// exprStmt = 分号隔开的expr 或空语句;
// expr = assign 赋值表达式 或 递归的assign赋值表达式
// assign = equality 相等比较结果
// equality = 关系运算符的比较结果
// relational = 多个加数的比较结果
// add = 多个乘数的加减结果
// mul = 多个基数(primary)相乘除得到的
// unary = ("+" | "-" | "*" | "&") unary | postfix
// structMembers = (declspec declarator (","  declarator)* ";")*
// structDecl = structUnionDecl
// unionDecl = structUnionDecl
// structUnionDecl = ident? ("{" structMembers)?
// postfix = primary ("[" expr "]" | "." ident)* | "->" ident)*
// primary = "(" "{" stmt+ "}" ")"
//         | "(" expr ")"
//         | "sizeof" unary
//         | ident funcArgs?
//         | str
//         | num
// funcall = ident ( assign , assign, * ) )
static Type *declspec(Token **Rest, Token *Tok);
static Type *declarator(Token **Rest, Token *Tok, Type *Ty);
static Node *declaration(Token **Rest, Token *Tok);
static Node *compoundStmt(Token **Rest, Token *Tok);
static Node *stmt(Token **Rest, Token *Tok);
static Node *exprStmt(Token **Rest, Token *Tok);
static Node *expr(Token **Rest, Token *Tok);
static Node *assign(Token **Rest, Token *Tok);
static Node *equality(Token **Rest, Token *Tok);
static Node *relational(Token **Rest, Token *Tok);
static Node *add(Token **Rest, Token *Tok);
static Node *mul(Token **Rest, Token *Tok);
static Type *structDecl(Token **Rest, Token *Tok);
static Type *unionDecl(Token **Rest, Token *Tok);
static Node *unary(Token **Rest, Token *Tok);
static Node *postfix(Token **Rest, Token *Tok);
static Node *primary(Token **Rest, Token *Tok);

// 进入域
static void enterScope(void) {
	Scope *S = calloc(1, sizeof(Scope));
	// 后来的域在链表头部
	// 类似栈的结构，栈顶对应最近的域
	S->Next = Scp;
	Scp = S;
}

// 离开域
static void leaveScope() {
	Scp = Scp->Next;
}

// 通过名称，查找一个本地变量
static Obj *findVar(Token *Tok) {

	// 此处越先匹配到的域，越深层
	for (Scope *S = Scp; S; S = S->Next) {
		// 遍历域内的所有变量
		for (VarScope *VarScp = S->Vars; VarScp; VarScp = VarScp->Next) {
			if (equal(Tok, VarScp->Name)) {
				return VarScp->Var;
			}
		}
	}

	return NULL;
}

// 通过Token查找标签
static Type *findTag(Token *Tok) {
	for (Scope *S = Scp; S; S = S->Next) {
		for (TagScope *S2 = S->Tags; S2; S2 = S2->Next) {
			if (equal(Tok, S2->Name)) {
				return S2->Ty;
			}
		}
	}

	return NULL;
}

// 新建一个节点
static Node *newNode(NodeKind Kind, Token *Tok) {
	Node *Nod = calloc(1, sizeof(Node));
	Nod->Kind = Kind;
	Nod->Tok = Tok;
	return Nod;
}

// 将变量存入当前域中
static VarScope *pushScope(char *Name, Obj *Var) {
	VarScope *S = calloc(1, sizeof(VarScope));
	S->Name = Name;
	S->Var = Var;
	// 后来的在链表头部
	S->Next = Scp->Vars;
	Scp->Vars = S;
	return S;
}

// 新建一个变量
static Obj *newVar(char *Name, Type *Ty) {
	Obj *Var = calloc(1, sizeof(Obj));
	Var->Name = Name;
	Var->Ty = Ty;
	pushScope(Name, Var);
	return Var;
}

// 在链表中新增一个局部变量
static Obj *newLVar(char *Name, Type *Ty) {
	Obj *Var = newVar(Name, Ty);
	Var->isLocal = true;
	// 将变量插入头部
	Var->Next = Locals;
	Locals = Var;
	return Var;
}

// 在链表中新增一个全局变量
static Obj *newGVar(char *Name, Type *Ty) {
	Obj *Var = newVar(Name, Ty);
	// 将变量插入头部
	Var->Next = Globals;
	Globals = Var;
	return Var;
}

// 获取标识符
static char *getIdent(Token *Tok) {
	if (Tok->Kind != TK_IDENT) {
		errorTok(Tok, "expected an identifier");
	}
	return strndup(Tok->Loc, Tok->Len);
}

// 新建一个数字二叉树叶子
static Node *newNum(int64_t Val, Token *Tok) {
	Node *Nod = newNode(ND_NUM, Tok);
	Nod->Val = Val;
	return Nod;
}

// 新建一个变量叶子节点
static Node *newVarNode(Obj *Var, Token *Tok) {
	Node *Nod = newNode(ND_VAR, Tok);
	Nod->Var = Var;
	return Nod;
}

// 新建一个单叉树节点
static Node *newUnary(NodeKind Kind, Node *Expr, Token *Tok) {
	Node *Nod = newNode(Kind, Tok);
	Nod->LHS = Expr;
	return Nod;
}

// 新建一个二叉树节点
static Node *newBinary(NodeKind Kind, Node *LHS, Node *RHS, Token *Tok) {
	Node *Nod = newNode(Kind, Tok);
	Nod->LHS = LHS;
	Nod->RHS = RHS;
	return Nod;
}

// 解析各种加法
static Node *newAdd(Node *LHS, Node *RHS, Token *Tok) {
	// 为左右部添加类型
	addType(LHS);
	addType(RHS);

	// num + num
	if (isInteger(LHS->Ty) && isInteger(RHS->Ty))  {
		return newBinary(ND_ADD, LHS, RHS, Tok);
	}

	// 不能解析 ptr + ptr
	if (LHS->Ty->Base && RHS->Ty->Base)  {
		errorTok(Tok, "invalid operands");
	}

	// num + ptr 转换为 ptr + num
	if (!LHS->Ty->Base && RHS->Ty->Base) {
		Node *Tmp = LHS;
		LHS = RHS;
		RHS = Tmp;
	}

	// ptr + num
	// 指针加法 ptr+1 指的是+一个元素的空间
	// 所以需要一个 *8 的操作即 *一个元素大小 的操作
	RHS = newBinary(ND_MUL, RHS, newNum(LHS->Ty->Base->Size, Tok), Tok);
	return newBinary(ND_ADD, LHS, RHS, Tok);
}

// 解析各种减法
static Node *newSub(Node *LHS, Node *RHS, Token *Tok) {
	// 为左右部添加类型
	addType(LHS);
	addType(RHS);

	// num - num
	if (isInteger(LHS->Ty) && isInteger(RHS->Ty))  {
		return newBinary(ND_SUB, LHS, RHS, Tok);
	}

	// ptr - num
  if (LHS->Ty->Base && isInteger(RHS->Ty)) {
    RHS = newBinary(ND_MUL, RHS, newNum(LHS->Ty->Base->Size, Tok), Tok);
    addType(RHS);
    Node *Nd = newBinary(ND_SUB, LHS, RHS, Tok);
    // 节点类型为指针
    Nd->Ty = LHS->Ty;
    return Nd;
  }

  // ptr - ptr，返回两指针间有多少元素
  if (LHS->Ty->Base && RHS->Ty->Base) {
    Node *Nd = newBinary(ND_SUB, LHS, RHS, Tok);
    Nd->Ty = TyInt;
    return newBinary(ND_DIV, Nd, newNum(LHS->Ty->Base->Size, Tok), Tok);
  }

  errorTok(Tok, "invalid operands");
  return NULL;
}

// 获取数字
static int64_t getNumber(Token *Tok) {
	if (Tok->Kind != TK_NUM) {
		errorTok(Tok, "expected a number");
	}
	return Tok->Val;
}

// 判断是否为类型名
static bool isTypename(Token *Tok) {
	return equal(Tok, "char") || equal(Tok, "int") 
		|| equal(Tok, "struct") || equal(Tok, "union") 
		|| equal(Tok, "long") || equal(Tok, "short");
}

// 解析复合语句(代码块)
// compoundStmt = stmt* }
static Node *compoundStmt(Token **Rest, Token *Tok) {
	Node *Nod = newNode(ND_BLOCK, Tok);

	// 这里使用了和词法分析类似的单向链表结构
	// 来表示多个表达式语句
  Node Head = {};
  Node *Cur = &Head;

	// 按照{}匹配，每个{}作为一个新的域
	// 进入新的域
	enterScope();

  // (declaration | stmt)* "}"
	// 匹配声明 或
	// 匹配最近的}来结束{代码块，用于支持{}嵌套
  while (!equal(Tok, "}")) {
		// declaration
		if (isTypename(Tok)) {
			Cur->Next = declaration(&Tok, Tok);
		}else {
			Cur->Next = stmt(&Tok, Tok);
		}
		// 处理下个表达式语句
    Cur = Cur->Next;

		// 构造完语法树后为节点添加类型信息
		addType(Cur);
  }

	// 结束当前域
	leaveScope();
	
	// Nod的Body中存储了{}内解析的语句
	// Body指的是当前{}中的所有语句构成的链表
	// } 在这里直接被忽略，没有进入语法树的构建
	Nod->Body = Head.Next;
	*Rest = Tok->Next;
	return Nod;
}

// 将标签存入当前的域中
static void pushTagScope(Token *Tok, Type *Ty) {
	TagScope *S = calloc(1, sizeof(TagScope));
	S->Name = strndup(Tok->Loc, Tok->Len);
	S->Ty = Ty;
	S->Next = Scp->Tags;
	Scp->Tags = S;
}

static Type *declspec(Token **Rest, Token *Tok) {
	// char
	if (equal(Tok, "char")) {
		*Rest = Tok->Next;
		return TyChar;
	}

	// int
	if (equal(Tok, "int")) {
		*Rest = Tok->Next;
		return TyInt;
	}

	// long
	if (equal(Tok, "long")) {
		*Rest = Tok->Next;
		return TyLong;
	}

	// short
	if (equal(Tok, "short")) {
		*Rest = Tok->Next;
		return TyShort;
	}

	// structDecl
	if (equal(Tok, "struct")) {
		return structDecl(Rest, Tok->Next);
	}

	// unionDecl
	if (equal(Tok, "union")) {
		return unionDecl(Rest, Tok->Next);
	}
	errorTok(Tok, "typename expected");
	return NULL;
}

static Type *funcParams(Token **Rest, Token *Tok, Type *Ty) {
	Type Head = {};
	Type *Cur = &Head;

	while (!equal(Tok, ")")) {
		if (Cur != &Head) {
			Tok  = skip(Tok, ",");
		}
		Type *Basety = declspec(&Tok, Tok);
		Type *DeclarTy = declarator(&Tok, Tok, Basety);
		// 将类型复制到形参链表一份
		Cur->Next = copyType(DeclarTy);
		Cur = Cur->Next;
	}

	// 封装一个函数节点
	Ty = funcType(Ty);
	// 传递形参
	Ty->Params = Head.Next;
	*Rest = Tok->Next;
	return Ty;
}

static Type *typeSuffix(Token **Rest, Token *Tok, Type *Ty) {

	if (equal(Tok, "(")) {
		return funcParams(Rest, Tok->Next, Ty);
	}

	if (equal(Tok, "[")) {
		int Size = getNumber(Tok->Next);
		Tok = skip(Tok->Next->Next, "]");
		Ty = typeSuffix(Rest, Tok, Ty);
		return arrayOf(Ty, Size);
	}

	*Rest = Tok;
	return Ty;
}

static Type *declarator(Token **Rest, Token *Tok, Type *Ty) {
	// 构建所有的(多重)指针
	while(consume(&Tok, Tok, "*")) {
		Ty = pointerTo(Ty);
	}

	if(Tok->Kind != TK_IDENT) {
		errorTok(Tok, "expected a variable name");
	}

	// typeSuffix
	Ty = typeSuffix(Rest, Tok->Next, Ty);
	// ident
	// 变量名 或 函数名
	Ty->Name = Tok;
	return Ty;
}
static Node *declaration(Token **Rest, Token *Tok) {
  // declspec
  // 声明的 基础类型
  Type *Basety = declspec(&Tok, Tok);

  Node Head = {};
  Node *Cur = &Head;
  // 对变量声明次数计数
  int I = 0;

  // (declarator ("=" expr)? ("," declarator ("=" expr)?)*)?
  while (!equal(Tok, ";")) {
    // 第1个变量不必匹配 ","
    if (I++ > 0)
      Tok = skip(Tok, ",");

    // declarator
    // 声明获取到变量类型，包括变量名
    Type *Ty = declarator(&Tok, Tok, Basety);
    Obj *Var = newLVar(getIdent(Ty->Name), Ty);

    // 如果不存在"="则为变量声明，不需要生成节点，已经存储在Locals中了
    if (!equal(Tok, "="))
      continue;

    // 解析“=”后面的Token
    Node *LHS = newVarNode(Var, Ty->Name);
    // 解析递归赋值语句
    Node *RHS = assign(&Tok, Tok->Next);
    Node *Node = newBinary(ND_ASSIGN, LHS, RHS, Tok);
    // 存放在表达式语句中
    Cur->Next = newUnary(ND_EXPR_STMT, Node, Tok);
    Cur = Cur->Next;
  }

  // 将所有表达式语句，存放在代码块中
  Node *Nd = newNode(ND_BLOCK, Tok);
  Nd->Body = Head.Next;
  *Rest = Tok->Next;
  return Nd;
}

static Node *stmt(Token **Rest, Token *Tok) {
	// return expr ;
	if(equal(Tok, "return")) {
		Node *Nod = newNode(ND_RETURN, Tok);
		Nod->LHS = expr(&Tok, Tok->Next);
		*Rest = skip(Tok, ";");
		return Nod;
	}

	// 解析if语句
	if (equal(Tok, "if")) {
		Node *Nod = newNode(ND_IF, Tok);
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
		Node *Nod = newNode(ND_FOR, Tok);

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

	// 解析while语句
	if (equal(Tok, "while")) {
		Node *Nod = newNode(ND_FOR, Tok);

		Tok = skip(Tok->Next, "(");

		// while 的条件语句
		if (!equal(Tok, ")")) {
			Nod->Cond = expr(&Tok, Tok);
		}
		Tok = skip(Tok, ")");

		// while 循环代码块
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
		return newNode(ND_BLOCK, Tok);
	}

	// expr? ;
	Node *Nod = newNode(ND_EXPR_STMT, Tok);
	Nod->LHS = expr(&Tok, Tok);
  *Rest = skip(Tok, ";");
  return Nod;
}

static Node *expr(Token **Rest, Token *Tok) {
	Node *Nd = assign(&Tok, Tok);

	if (equal(Tok, ","))
    return newBinary(ND_COMMA, Nd, expr(Rest, Tok->Next), Tok);

  *Rest = Tok;
  return Nd;
}

static Node *assign(Token **Rest, Token *Tok) {
	Node *Nod = equality(&Tok, Tok);

	// 可能存在递归赋值 a=b=1
	if(equal(Tok, "=")) {
		return Nod = newBinary(ND_ASSIGN, Nod, assign(Rest, Tok->Next), Tok);
	}

	*Rest = Tok;
	return Nod;
}

static Node *equality(Token **Rest, Token *Tok) {
	Node *Nod = relational(&Tok, Tok);

	while(true) {
		Token *Start = Tok;
		if(equal(Tok, "==")) {
			Nod = newBinary(ND_EQ, Nod, relational(&Tok, Tok->Next), Start);
			continue;
		}

		if(equal(Tok, "!=")) {
			Nod = newBinary(ND_NEQ, Nod, relational(&Tok, Tok->Next), Start);
			continue;
		}

		*Rest = Tok;
		return Nod;
	}
}

static Node *relational(Token **Rest, Token *Tok) {
	Node *Nod = add(&Tok, Tok);

	while(true) {
		Token *Start = Tok;
		if(equal(Tok, "<")) {
			Nod = newBinary(ND_LT, Nod, add(&Tok, Tok->Next), Start);
			continue;
		}

		if(equal(Tok, "<=")) {
			Nod = newBinary(ND_LTEQ, Nod, add(&Tok, Tok->Next), Start);
			continue;
		}
		// X > Y 相当于 Y < X
		// >= 同
		if(equal(Tok, ">")) {
			Nod = newBinary(ND_LT, add(&Tok, Tok->Next), Nod, Start);
			continue;
		}

		if(equal(Tok, ">=")) {
			Nod = newBinary(ND_LTEQ, add(&Tok, Tok->Next), Nod, Start);
			continue;
		}

		*Rest = Tok;
		return Nod;
	}
}

static Node *add(Token **Rest, Token *Tok) {
	Node *Nod = mul(&Tok, Tok);

	while(true) {
		Token *Start = Tok;
		if(equal(Tok, "+")) {
			Nod = newAdd(Nod, mul(&Tok, Tok->Next), Start);
			continue;
		}

		if(equal(Tok, "-")) {
			Nod = newSub(Nod, mul(&Tok, Tok->Next), Start);
			continue;
		}

		*Rest = Tok;
		return Nod;
	}
}

static Node *mul(Token **Rest, Token *Tok) {

	Node *Nod = unary(&Tok, Tok);

	while(true) {
		Token *Start = Tok;
		if(equal(Tok, "*")) {
			Nod = newBinary(ND_MUL, Nod, unary(&Tok, Tok->Next), Start);
			continue;
		}

		if(equal(Tok, "/")) {
			Nod = newBinary(ND_DIV, Nod, unary(&Tok, Tok->Next), Start);
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
		return newUnary(ND_NEG, unary(Rest, Tok->Next), Tok);
	}

	// 取地址
	if(equal(Tok, "&")) {
		return newUnary(ND_ADDR, unary(Rest, Tok->Next), Tok);
	}

	// 解引用
	if(equal(Tok, "*")) {
		return newUnary(ND_DEREF, unary(Rest, Tok->Next), Tok);
	}

	return postfix(Rest, Tok);
}

static void structMembers(Token **Rest, Token *Tok, Type *Ty) {
	Member Head = {};
	Member *Cur = &Head;

	while (!equal(Tok, "}")) {
		// declspec
		Type *BaseTy = declspec(&Tok, Tok);
		int I = 0;

		while (!consume(&Tok, Tok, ";")) {
			if (I++) {
				Tok = skip(Tok, ",");
			}

			Member *Mem = calloc(1, sizeof(Member));
			// declarator
			Mem->Ty = declarator(&Tok, Tok, BaseTy);
			Mem->Name = Mem->Ty->Name;
			Cur->Next = Mem;
			Cur = Cur->Next;
		}
	}

	*Rest = Tok->Next;
	Ty->Mems = Head.Next;
}

static Type *structUnionDecl(Token **Rest, Token *Tok) {
	// 读取结构体的标签
  Token *Tag = NULL;
  if (Tok->Kind == TK_IDENT) {
    Tag = Tok;
    Tok = Tok->Next;
  }

  if (Tag && !equal(Tok, "{")) {
    Type *Ty = findTag(Tag);
    if (!Ty)
      errorTok(Tag, "unknown struct type");
    *Rest = Tok;
    return Ty;
  }

  // 构造一个结构体
  Type *Ty = calloc(1, sizeof(Type));
  Ty->Kind = TY_STRUCT;
  structMembers(Rest, Tok->Next, Ty);
  Ty->Align = 1;

  // 如果有名称就注册结构体类型
  if (Tag)
    pushTagScope(Tag, Ty);
  return Ty;
}

static Type *structDecl(Token **Rest, Token *Tok) {
	Type *Ty = structUnionDecl(Rest, Tok);

	int Offset = 0;
	for (Member *Mem = Ty->Mems; Mem; Mem = Mem->Next) {
		//对齐成员变量的偏移量
		Offset = alignTo(Offset, Mem->Ty->Align);
		Mem->Offset = Offset;
		Offset += Mem->Ty->Size;

		// 偏移量为结构体成员的最大偏移量
		if (Ty->Align < Mem->Ty->Align) {
			Ty->Align = Mem->Ty->Align;
		}
	}
	Ty->Size = alignTo(Offset, Ty->Align);

	return Ty;
}

static Type *unionDecl(Token **Rest, Token *Tok) {
  Type *Ty = structUnionDecl(Rest, Tok);
  Ty->Kind = TY_UNION;

  // 联合体需要设置为最大的对齐量与大小，变量偏移量都默认为0
  for (Member *Mem = Ty->Mems; Mem; Mem = Mem->Next) {
    if (Ty->Align < Mem->Ty->Align)
      Ty->Align = Mem->Ty->Align;
    if (Ty->Size < Mem->Ty->Size)
      Ty->Size = Mem->Ty->Size;
  }
  // 将大小对齐
  Ty->Size = alignTo(Ty->Size, Ty->Align);
  return Ty;
}

// 获取结构体成员
static Member *getStructMember(Type *Ty, Token *Tok) {
	for (Member *Mem = Ty->Mems; Mem; Mem = Mem->Next) {
		if (Mem->Name->Len == Tok->Len && !strncmp(Mem->Name->Loc, Tok->Loc, Tok->Len)) {
			return Mem;
		}
	}
	errorTok(Tok, "no such member");
	return NULL;
}

// 构建结构体成员的节点
static Node *structRef(Node *LHS, Token *Tok) {
	addType(LHS);
	if (LHS->Ty->Kind != TY_STRUCT && LHS->Ty->Kind != TY_UNION) {
		errorTok(LHS->Tok, "not a struct nor a union");
	}

	Node *Nod = newUnary(ND_MEMBER, LHS, Tok);
	Nod->Mem = getStructMember(LHS->Ty, Tok);
	return Nod;
}

static Node *postfix(Token **Rest, Token *Tok) {
	Node *Nod = primary(&Tok, Tok);

	while (true) {
		if (equal(Tok, "[")) {
			// x[y] == *(x+y)
			Token *Start = Tok;
			Node *Idx = expr(&Tok, Tok->Next);
			Tok = skip(Tok, "]");
			Nod = newUnary(ND_DEREF, newAdd(Nod, Idx, Start), Start);
			continue;
		}

		if (equal(Tok, ".")) {
			Nod = structRef(Nod, Tok->Next);
			Tok = Tok->Next->Next;
			continue;
		}

		if (equal(Tok, "->")) {
			// x->y ==== (*x).y
			Nod = newUnary(ND_DEREF, Nod, Tok);
			Nod = structRef(Nod, Tok->Next);
			Tok = Tok->Next->Next;
			continue;
		}

		*Rest = Tok;
		return Nod;
	}

}

static Node *funCall(Token **Rest, Token *Tok) {
	Token *Start = Tok;
	Tok = Tok->Next->Next;

	Node Head = {};
	Node *Cur = &Head;

	while (!equal(Tok, ")")) {
		if (Cur != &Head) {
			Tok = skip(Tok, ",");
		}

		Cur->Next = assign(&Tok, Tok);
		Cur = Cur->Next;
	}

	*Rest = skip(Tok, ")");

	Node *Nod = newNode(ND_FUNCALL, Start);
	Nod->FuncName = strndup(Start->Loc, Start->Len);
	Nod->Args = Head.Next;
	return Nod;
}

// 新增唯一姓名
static char *newUniqueName() {
	static int Id = 0;
	return format(".L..%d", Id++);
}

// 新增全局匿名变量
static Obj *newAnonGVar(Type *Ty) {
	return newGVar(newUniqueName(), Ty);
}

// 新增字符串字面值
static Obj *newStringLiteral(char *Str, Type *Ty) {
	Obj *Var = newAnonGVar(Ty);
	Var->InitData = Str;
	return Var;
}

static Node *primary(Token **Rest, Token *Tok) {

	// if is statement expression
	// ({ statement })
	if (equal(Tok, "(") && equal(Tok->Next, "{")) {
		// this is GNU statement expression
		Node *Nod = newNode(ND_STMT_EXPR, Tok);
		// treate inner statement as compound statement
		Nod->Body = compoundStmt(&Tok, Tok->Next->Next)->Body;
		*Rest = skip(Tok, ")");
		return Nod;
	}

	// 如果是(expr)
	if(equal(Tok, "(")) {
		Node *Nod = expr(&Tok, Tok->Next);
		*Rest = skip(Tok, ")");
		return Nod;
	}

	if(equal(Tok, "sizeof")) {
		Node *Nod = unary(Rest, Tok->Next);
		addType(Nod);
		return newNum(Nod->Ty->Size, Tok);
	}

	// 如果是变量
	if(Tok->Kind == TK_IDENT) {
		// 如果是零参数函数
		if (equal(Tok->Next, "(")) {
			return funCall(Rest, Tok);
		}


		Obj *Var = findVar(Tok);
		// 如果变量不存在，就在链表中新增一个变量
		if(!Var) {
			errorTok(Tok, "undefined variable");
		}
		*Rest = Tok->Next;
		return newVarNode(Var, Tok);
	}

	// 如果是字符串字面值
	if (Tok->Kind == TK_STR) {
		Obj *Var = newStringLiteral(Tok->Str, Tok->Ty);
		*Rest = Tok->Next;
		return newVarNode(Var, Tok);
	}

	// 如果是数字
	if(Tok->Kind == TK_NUM) {
		Node *Nod = newNum(Tok->Val, Tok);
		*Rest = Tok->Next;
		return Nod;
	}

	errorTok(Tok, "expected an expression");
	return NULL;
}

// 将形参添加到Locals
static void createParamLVars(Type *Param) {
	if (Param) {
		// 递归到形参最底部
		// 先将最低部的加入Locals中之后的逐个加入到顶部，保持顺序不变
		createParamLVars(Param->Next);
		// 添加到Locals中
		newLVar(getIdent(Param->Name), Param);
	}
}

static Token *function(Token *Tok, Type *BaseTy) {
	Type *Ty = declarator(&Tok, Tok, BaseTy);

	Obj *Fn = newGVar(getIdent(Ty->Name), Ty);
	Fn->isFunction = true;

  // 清空全局变量Locals
	Locals = NULL;

	// 进入新的域
	enterScope();

	createParamLVars(Ty->Params);
	Fn->Params = Locals;

	Tok = skip(Tok, "{");
	Fn->Body = compoundStmt(&Tok, Tok);
	Fn->Locals = Locals;

	// 离开域
	leaveScope();

	return Tok;
}

// 构造全局变量 
static Token *globalVariable(Token *Tok, Type *BaseTy) {
	bool First = true;

	while (!consume(&Tok, Tok, ";")) {
		if (!First) {
			Tok = skip(Tok, ",");
		}
		First = false;

		Type *Ty = declarator(&Tok, Tok, BaseTy);
		newGVar(getIdent(Ty->Name), Ty);
	}
	return Tok;
}

// 区分函数还是全局变量
static bool isFunction(Token *Tok) {
	if (equal(Tok, ";")) {
		return false;
	}

	// 虚拟变量，用于调用declarator
	Type Dummy = {};
	Type *Ty = declarator(&Tok, Tok, &Dummy);
	return Ty->Kind == TY_FUNC;
}

// 语法分析入口函数
Obj *parse(Token *Tok){
	Globals = NULL;

	while (Tok->Kind != TK_EOF) {
		Type *Basety = declspec(&Tok, Tok);

		// 函数
		if (isFunction(Tok)) {
			Tok = function(Tok, Basety);
			continue;
		}

		// 全局变量
		Tok = globalVariable(Tok, Basety);
	}

	return Globals;
}
