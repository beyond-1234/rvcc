#include "rvcc.h"

// 局部和全局变量或是typedef, enum常量的域
typedef struct VarScope VarScope;
struct VarScope {
	VarScope *Next;
	char *Name;
	Obj *Var;
	Type *Typedef;		// 别名
	Type *EnumTy;			// 枚举的类型
	int EnumVal;			// 枚举的值
};

// 结构体和联合体标签, 枚举标签的域
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

	// C有两个域: 变量(或类型别名)域和结构体(或联合体, 标签)标签域
	// 指向当前域的变量
	VarScope *Vars;
	// 指向当前域内的结构体标签
	TagScope *Tags;
};

// 变量属性
typedef struct {
	// 是否为类型别名
	bool IsTypedef;
	// 是否为文件域内
	bool IsStatic;
} VarAttr;

// 可变的初始化器。此处为树状结构。
// 因为初始化器可以是嵌套的，
// 类似于 int x[2][2] = {{1, 2}, {3, 4}}}
typedef struct Initializer Initializer;
struct Initializer {
	Initializer *Next;
	Type *Ty;					// 原始类型
	Token *Tok;				// 终结符
	bool IsFlexible;	// 可调整的，表示需要重新构造

	// 如果不是一个聚合类型，并且有一个初始化器，Expr 有对应的初始化表达式
	Node *Expr;

	// 如果是聚合类型(数组或结构体) Children有子节点对应的初始化器
	Initializer **Children;
};


// 指派初始化，用于局部变量的初始化器
typedef struct InitDesig InitDesig;
struct InitDesig {
	InitDesig *Next;
	int Ind;					// 数组中的下一个索引
	Obj *Var;					// 对应的变量
};

// 指向所有域的链表
static Scope *Scp = &(Scope){};

// 在解析时，所有的变量实例都会被累加到这个列表里
Obj *Locals;		// 局部变量
Obj *Globals;		// 全局变量

// 当前函数内的goto和标签列表
static Node *Gotos;
static Node *Labels;

// 全局break标签
static char *BrkLabel;
// 全局continue跳转的目标
static char *ContLabel;

// 如果我们正在解析switch语句，则指向表示case的节点
// 否则为空
static Node *CurrentSwitch;

// 当前正在解析的函数
static Obj *CurrentFn;

static bool isTypename(Token *Tok);

// 方法前置声明
// 语法树规则
// 越往下优先级越高
// program = (typedef | functionDefinition* | global-variable)*
// functionDefinition = declspec declarator? ident "(" ")" "{" compoundStmt*
// declspec = ("void" | "_Bool" | "char" | "short" | "int" | "long"
//             | "typedef" | "static"
//             | structDecl | unionDecl | typedefName
//             | enumSpecifier)+
// enumSpecifier = ident? "{" enumList? "}"
//                 | ident ("{" enumList? "}")?
// enumList = ident ("=" constExpr)? ("," ident ("=" constExpr)?)*
// declarator = "*"* ("(" ident ")" | "(" declarator ")" | ident) typeSuffix
// typeSuffix = "(" funcParams | "[" arrayDimensions | ε
// arrayDimensions = constExpr? "]" typeSuffix
// funcParams = (param ("," param)*)? ")"
// param = declspec declarator
// compoundStmt = (typedef | declaration | stmt)* "}"
// declaration = declspec (declarator ("=" initializer)?
//                         ("," declarator ("=" initializer)?)*)? ";"
// initializer = stringInitializer | arrayInitializer | assign
// stringInitializer = stringLiteral
// arrayInitializer = "{" initializer ("," initializer)* "}"
// stmt = return语句返回;隔开的expr表达式 或
//        | "if" "(" expr ")" stmt ("else" stmt)?
//        | "switch" "(" expr ")" stmt
//        | "case" constExpr ":" stmt
//        | "default" ":" stmt
//        | "for" "(" exprStmt expr? ";" expr? ")" stmt
//        | "while" "(" expr ")" stmt
//        | "goto" ident ";"
//        | "break" ";"
//        | "continue" ";"
//        | ident ":" stmt
//        | "{" compoundStmt
//        | exprStmt
// exprStmt = 分号隔开的expr 或空语句;
// expr = assign 赋值表达式 或 递归的assign赋值表达式
// assign = conditional (assignOp assign)?
// conditional = logOr ("?" expr ":" conditional)?
// logOr = logAnd ("||" logAnd)*
// logAnd = bitOr ("&&" bitOr)*
// bitOr = bitXor ("|" bitXor)*
// bitXor = bitAnd ("^" bitAnd)*
// bitAnd = equality ("&" equality)*
// assignOp = "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^="
//					| "<<=" | ">>="
// equality = 关系运算符的比较结果
// relational = shift ("<" shift | "<=" shift | ">" shift | ">=" shift)*
// shift = add ("<<" add | ">>" add)*
// add = 多个乘数的加减结果
// mul = cast ("*" cast | "/" cast | "%" cast)*
// cast = "(" typeName ")" cast | unary
// unary = ("+" | "-" | "*" | "&" | "!" | "~") cast | ("++" | "--") unary | postfix
// structMembers = (declspec declarator (","  declarator)* ";")*
// structDecl = structUnionDecl
// unionDecl = structUnionDecl
// structUnionDecl = ident? ("{" structMembers)?
// postfix = primary ("[" expr "]" | "." ident)* | "->" ident | "++" | "--")*
// primary = "(" "{" stmt+ "}" ")"
//         | "(" expr ")"
//         | "sizeof" "(" typeName ")"
//         | "sizeof" unary
//         | ident funcArgs?
//         | str
//         | num
// typeName = declspec abstractDeclarator
// abstractDeclarator = "*"* ("(" abstractDeclarator ")")? typeSuffix
// funcall = ident ( assign , assign, * ) )
static Type *declspec(Token **Rest, Token *Tok, VarAttr *Attr);
static Type *enumSpecifier(Token **Rest, Token *Tok);
static Type *typeSuffix(Token **Rest, Token *Tok, Type *Ty);
static Type *declarator(Token **Rest, Token *Tok, Type *Ty);
static Node *declaration(Token **Rest, Token *Tok, Type *BaseTy);
static void initializer2(Token **Rest, Token *Tok, Initializer *Init);
static Initializer *initializer(Token **Rest, Token *Tok, Type *Ty, Type **NewTy);
static Node *LVarInitializer(Token **Rest, Token *Tok, Obj *Var);
static Node *compoundStmt(Token **Rest, Token *Tok);
static Node *stmt(Token **Rest, Token *Tok);
static Node *exprStmt(Token **Rest, Token *Tok);
static Node *expr(Token **Rest, Token *Tok);
static int64_t constExpr(Token **Rest, Token *Tok);
static Node *assign(Token **Rest, Token *Tok);
static Node *conditional(Token **Rest, Token *Tok);
static Node *logOr(Token **Rest, Token *Tok);
static Node *logAnd(Token **Rest, Token *Tok);
static Node *bitOr(Token **Rest, Token *Tok);
static Node *bitXor(Token **Rest, Token *Tok);
static Node *bitAnd(Token **Rest, Token *Tok);
static Node *equality(Token **Rest, Token *Tok);
static Node *relational(Token **Rest, Token *Tok);
static Node *shift(Token **Rest, Token *Tok);
static Node *add(Token **Rest, Token *Tok);
static Node *newAdd(Node *LHS, Node *RHS, Token *Tok);
static Node *newSub(Node *LHS, Node *RHS, Token *Tok);
static Node *mul(Token **Rest, Token *Tok);
static Node *cast(Token **Rest, Token *Tok);
static Type *structDecl(Token **Rest, Token *Tok);
static Type *unionDecl(Token **Rest, Token *Tok);
static Node *unary(Token **Rest, Token *Tok);
static Node *postfix(Token **Rest, Token *Tok);
static Node *primary(Token **Rest, Token *Tok);
static Token *parseTypedef(Token *Tok, Type *BaseTy);

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
static VarScope *findVar(Token *Tok) {

	// 此处越先匹配到的域，越深层
	for (Scope *S = Scp; S; S = S->Next) {
		// 遍历域内的所有变量
		for (VarScope *VarScp = S->Vars; VarScp; VarScp = VarScp->Next) {
			if (equal(Tok, VarScp->Name)) {
				return VarScp;
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
static VarScope *pushScope(char *Name) {
	VarScope *S = calloc(1, sizeof(VarScope));
	S->Name = Name;

	// 后来的在链表头部
	S->Next = Scp->Vars;
	Scp->Vars = S;
	return S;
}

// 新建初始化器
static Initializer *newInitializer(Type *Ty, bool IsFlexible) {
	Initializer *Init = calloc(1, sizeof(Initializer));
	// 存储原始类型
	Init->Ty = Ty;

	// 处理数组类型
	if (Ty->Kind == TY_ARRAY) {
		// 判断是否需要调整数组元素并且数组不完整
		if (IsFlexible && Ty->Size < 0) {
			// 设置初始化器为可调整的，之后进行完数组元素的计算后，再构造初始化器
			Init->IsFlexible = true;
			return Init;
		}


		// 为数组最外层的每个元素分配空间
		Init->Children = calloc(Ty->ArrayLen, sizeof(Initializer *));
		// 遍历解析数组最外层每个元素
		for (int I = 0; I < Ty->ArrayLen; I++) {
			Init->Children[I] = newInitializer(Ty->Base, false);
		}
	}

	return Init;
}

// 新建一个变量
static Obj *newVar(char *Name, Type *Ty) {
	Obj *Var = calloc(1, sizeof(Obj));
	Var->Name = Name;
	Var->Ty = Ty;
	pushScope(Name)->Var = Var;
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

// 查找类型别名
static Type *findTypedef(Token *Tok) {
	// 类型别名是个标识符
	if (Tok->Kind == TK_IDENT) {
		// 查找是否存在于变量域内
		VarScope *S = findVar(Tok);
		if (S) {
			return S->Typedef;
		}
	}

	return NULL;
}

// 新建一个数字二叉树叶子
static Node *newNum(int64_t Val, Token *Tok) {
	Node *Nod = newNode(ND_NUM, Tok);
	Nod->Val = Val;
	return Nod;
}

// 新建一个长整型节点
static Node *newLong(int64_t Val, Token *Tok) {
	Node *Nod = newNode(ND_NUM, Tok);
	Nod->Val = Val;
	Nod->Ty = TyLong;
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

// 新转换
Node *newCast(Node *Expr, Type *Ty) {
	addType(Expr);

	Node *Nod = calloc(1, sizeof(Node));
	Nod->Kind = ND_CAST;
	Nod->Tok = Expr->Tok;
	Nod->LHS = Expr;
	Nod->Ty = copyType(Ty);
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
	// 指针用long类型存储
	RHS = newBinary(ND_MUL, RHS, newLong(LHS->Ty->Base->Size, Tok), Tok);
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
		// 指针用long类型存储
    RHS = newBinary(ND_MUL, RHS, newLong(LHS->Ty->Base->Size, Tok), Tok);
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


// 获取数字
/* static int64_t getNumber(Token *Tok) { */
/* 	if (Tok->Kind != TK_NUM) { */
/* 		errorTok(Tok, "expected a number"); */
/* 	} */
/* 	return Tok->Val; */
/* } */

// 判断是否为类型名
static bool isTypename(Token *Tok) {
	static char *Kw[] = {
      "void", "_Bool", "char", "short",
			"int", "long", "struct", "union",
			"typedef", "enum", "static"
  };

  for (int I = 0; I < sizeof(Kw) / sizeof(*Kw); ++I) {
    if (equal(Tok, Kw[I]))
      return true;
  }
	// 查找是否为类型别名
  return findTypedef(Tok);
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
		// 处理同名typedef和标签的冲突
		// 即对typename进行判断时判断一下格式是否为标签格式 "label:"
		// typename 要求不是这个格式即可
		if (isTypename(Tok) && !equal(Tok->Next, ":")) {

			VarAttr Attr = {};
			Type *BaseTy = declspec(&Tok, Tok, &Attr);

			// 解析typedef的语句
			if (Attr.IsTypedef) {
				Tok = parseTypedef(Tok, BaseTy);
				continue;
			}

			// 解析变量声明语句
			Cur->Next = declaration(&Tok, Tok, BaseTy);
		} else {
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

static Type *declspec(Token **Rest, Token *Tok, VarAttr *Attr) {

	// 类型的组合
	// 为什么要左移偶数位，比如遇到long long就会有两次1<<8，下面向加就合成为1<<9
	enum {
		VOID  = 1 << 0,
		BOOL  = 1 << 2,
		CHAR  = 1 << 4,
		SHORT = 1 << 6,
		INT   = 1 << 8,
		LONG  = 1 << 10,
		OTHER = 1 << 12,
	};

	Type *Ty = TyInt;
	int Counter = 0; // 记录类型向加的数值

	while (isTypename(Tok)) {

		// 处理typedef关键字
		if (equal(Tok, "typedef") || equal(Tok, "static")) {
			if (!Attr) {
				errorTok(Tok, "storage class specifier is not allowed in the context");
			}

			if (equal(Tok, "typedef")) {
				Attr->IsTypedef = true;
			} else {
				Attr->IsStatic = true;
			}

			// typedef 与 static 不该一起使用
			if (Attr->IsStatic && Attr->IsTypedef) {
				errorTok(Tok, "typedef and static my not be used together");
			}

			Tok = Tok->Next;
			continue;
		}

		// 处理用户定义的类型
		Type *Ty2 = findTypedef(Tok);

		if (equal(Tok, "struct") || equal(Tok, "union")  || equal(Tok, "enum")
				|| Ty2) {

			if (Counter)
				break;

			if (equal(Tok, "struct")) {
				Ty = structDecl(&Tok, Tok->Next);
			} else if (equal(Tok, "union")){
				Ty = unionDecl(&Tok, Tok->Next);
			} else if (equal(Tok, "enum")){
				Ty = enumSpecifier(&Tok, Tok->Next);
			} else {
				// 将类型设置为类型别名指向的类型
				Ty = Ty2;
				Tok = Tok->Next;
			}

			Counter += OTHER;
			continue;
		}

		if (equal(Tok, "void")) {
			Counter += VOID;
		} else if (equal(Tok, "_Bool")) {
			Counter += BOOL;
		} else if (equal(Tok, "char")) {
			Counter += CHAR;
		} else if (equal(Tok, "short")) {
			Counter += SHORT;
		} else if (equal(Tok, "int")) {
			Counter += INT;
		} else if (equal(Tok, "long")) {
			Counter += LONG;
		} else {
			unreachable();
		}

		switch (Counter) {
			case VOID:
				Ty = TyVoid;
				break;
			case BOOL:
				Ty = TyBool;
				break;
			case CHAR:
				Ty = TyChar;
				break;
			case SHORT:
			case SHORT + INT:
				Ty = TyShort;
				break;
			case INT:
				Ty = TyInt;
				break;
			case LONG:
			case LONG + LONG:
			case LONG + INT:
			case LONG + LONG + INT:
				Ty = TyLong;
				break;
			default:
				errorTok(Tok, "invalid type");
		}

		Tok = Tok->Next;
	}

	*Rest = Tok;
	return Ty;
}

static Type *enumSpecifier(Token **Rest, Token *Tok) {
	Type *Ty = enumType();

	// 读取标签
	// ident?
	Token *Tag = NULL;
	if (Tok->Kind == TK_IDENT) {
		Tag = Tok;
		Tok = Tok->Next;
	}

	// 处理没有{}的情况
	// 即使用enum的情况
	if (Tag && !equal(Tok, "{")) {
		Type *Ty = findTag(Tag);
		if (!Ty) {
			errorTok(Tag, "unknown enum type");
		}
		if (Ty->Kind != TY_ENUM) {
			errorTok(Tag, "not an enum tag");
		}

		*Rest = Tok;
		return Ty;
	}

	// 处理{}的情况
	Tok = skip(Tok, "{");

	// enumList 读取enum列表
	int I = 0;
	int Val = 0;
	while (!equal(Tok, "}")) {
		if (I++ > 0) {
			Tok = skip(Tok, ",");
		}

		char *Name = getIdent(Tok);
		Tok = Tok->Next;

		// 判断是否赋值
		if (equal(Tok, "=")) {
			Val = constExpr(&Tok, Tok->Next);
		}

		// 存入枚举常量
		VarScope *S = pushScope(Name);
		S->EnumTy = Ty;
		// 如果个别枚举值有单独赋值，则基于单独赋值递增
		// 但是此处并没对重复赋值的情况做检查
		S->EnumVal = Val++;
	}

	*Rest = Tok->Next;

	if (Tag) {
		pushTagScope(Tag, Ty);
	}
	return Ty;
}

static Type *funcParams(Token **Rest, Token *Tok, Type *Ty) {
	Type Head = {};
	Type *Cur = &Head;

	while (!equal(Tok, ")")) {
		if (Cur != &Head) {
			Tok  = skip(Tok, ",");
		}
		Type *DeclarTy = declspec(&Tok, Tok, NULL);
		DeclarTy = declarator(&Tok, Tok, DeclarTy);

		// T类型的数组被转换为T*
		if (DeclarTy->Kind == TY_ARRAY) {
			Token *Name = DeclarTy->Name;
			DeclarTy = pointerTo(DeclarTy->Base);
			DeclarTy->Name = Name;
		}

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

// 数组维数
// arrayDimensions = num? "]" typeSuffix
static Type *arrayDimensions(Token **Rest, Token *Tok, Type *Ty) {
	// "]" 无数组维数的 "[]"
	if (equal(Tok, "]")) {
		// 再解析一下用于多维数组的情况
		Ty = typeSuffix(Rest, Tok->Next, Ty);
		// 传入Size = -1 跟正常有维度数组区分开
		return arrayOf(Ty, -1);
	}

	// 有数组维数的情况
	int Sz = constExpr(&Tok, Tok);
  Tok = skip(Tok, "]");
	Ty = typeSuffix(Rest, Tok, Ty);
	return arrayOf(Ty, Sz);
}

static Type *typeSuffix(Token **Rest, Token *Tok, Type *Ty) {

	if (equal(Tok, "(")) {
		return funcParams(Rest, Tok->Next, Ty);
	}

	if (equal(Tok, "[")) {
		return arrayDimensions(Rest, Tok->Next, Ty);
	}

	*Rest = Tok;
	return Ty;
}

static Type *declarator(Token **Rest, Token *Tok, Type *Ty) {
	// 构建所有的(多重)指针
	while(consume(&Tok, Tok, "*")) {
		Ty = pointerTo(Ty);
	}

	// 解析嵌套类型
	if (equal(Tok, "(")) {
		// 记录(的位置
		Token *Start = Tok;
		Type Dummy = {};
		// 是Tok前进到)后面的位置
		declarator(&Tok, Start->Next, &Dummy);
		Tok = skip(Tok, ")");
		// 获取到括号后面的类型后缀，Ty为解析完的类型，Rest指向分号
		Ty = typeSuffix(Rest, Tok, Ty);
		// 解析Ty整体作为Base去构造，返回Type的值
		return declarator(&Tok, Start->Next, Ty);
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

static Type *abstractDeclarator(Token **Rest, Token *Tok, Type *Ty) {
	// "*"*
	while (equal(Tok, "*")) {
		Ty = pointerTo(Ty);
		Tok = Tok->Next;
	}

	// ("(" abstractDeclarator ")")?
	if (equal(Tok, "(")) {
		Token *Start = Tok;
		Type Dummy = {};
		// 使tok前进到) 后面的位置
		abstractDeclarator(&Tok, Start->Next, &Dummy);
		Tok = skip(Tok, ")");
		// 获取到括号后面的类型后缀，Ty为解析完的类型，Rest指向分号
		Ty = typeSuffix(Rest, Tok, Ty);
		// 解析Ty整体作为Base去构造，返回Type的值
		return abstractDeclarator(&Tok, Start->Next, Ty);
	}

	// type suffix
	return typeSuffix(Rest, Tok, Ty);
}

// 获取类型的相关信息
static Type *typename(Token **Rest, Token *Tok) {
	// declspec
	Type *Ty = declspec(&Tok, Tok, NULL);
	// abstractDeclarator
	return abstractDeclarator(Rest, Tok, Ty);
}

static Node *declaration(Token **Rest, Token *Tok, Type *BaseTy) {

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
    Type *Ty = declarator(&Tok, Tok, BaseTy);
		if (Ty->Kind == TY_VOID) {
			errorTok(Tok, "variable declared void");
		}
    Obj *Var = newLVar(getIdent(Ty->Name), Ty);

    // 如果不存在"="则为变量声明，不需要生成节点，已经存储在Locals中了
		if (equal(Tok, "=")) {
			// 解析变量的初始化器
			Node *Expr = LVarInitializer(&Tok, Tok->Next, Var);
			// 存放在表达式语句中
			Cur->Next = newUnary(ND_EXPR_STMT, Expr, Tok);
			Cur = Cur->Next;
		}

		if (Var->Ty->Size < 0) {
			errorTok(Ty->Name, "variable has incomplete type");
		}
		if (Var->Ty->Kind == TY_VOID) {
			errorTok(Ty->Name, "variable declared void");
		}
  }

  // 将所有表达式语句，存放在代码块中
  Node *Nd = newNode(ND_BLOCK, Tok);
  Nd->Body = Head.Next;
  *Rest = Tok->Next;
  return Nd;
}

// 跳过多余元素
static Token *skipExcessElement(Token *Tok) {
	if (equal(Tok, "{")) {
		Tok = skipExcessElement(Tok->Next);
		return skip(Tok, "}");
	}

	// 解析并舍弃多余元素
	assign(&Tok, Tok);
	return Tok;
}


// initializer = "{" initializer ("," initializer)* "}" | assign
// stringInitializer = stringLiteral
static void stringInitializer(Token **Rest, Token *Tok, Initializer *Init) {

	// 如果是可调整的，就构造一个包含数组的初始化器
	// 字符串字面值在词法解析部分已经增加了 '\0'
	if (Init->IsFlexible) {
		*Init = *newInitializer(arrayOf(Init->Ty->Base, Tok->Ty->ArrayLen), false);
	}

	// 取数组和字符串的最短长度
	int Len = MIN(Init->Ty->ArrayLen, Tok->Ty->ArrayLen);
	// 遍历赋值
	for (int I = 0; I < Len; I++) {
		// 字符是以数字格式存储的
		Init->Children[I]->Expr = newNum(Tok->Str[I], Tok);
	}

	*Rest = Tok->Next;
}

// 计算数组初始化元素个数
static int countArrayInitElement(Token *Tok, Type *Ty) {
	Initializer *Dummy = newInitializer(Ty->Base, false);
	// 项数
	int I = 0;

	// 遍历所有匹配的项
	for (; !equal(Tok, "}"); I++) {
		if (I > 0) {
			Tok = skip(Tok, ",");
		}

		initializer2(&Tok, Tok, Dummy);
	}

	return I;
}

// arrayInitializer = "{" initializer ("," initializer)* "}"
static void arrayInitializer(Token **Rest, Token *Tok, Initializer *Init) {
	Tok = skip(Tok, "{");

	// 如果数组是可调整的，那么计算数组的元素数，然后进行初始化器的构造
	if (Init->IsFlexible) {
		int Len = countArrayInitElement(Tok, Init->Ty);
		// 在这里Ty也被重新构造为了数组
		*Init = *newInitializer(arrayOf(Init->Ty->Base, Len), false);
	}

	// 遍历变量
	for (int I = 0; !consume(Rest, Tok, "}"); I++) {
	/* for (int I = 0; I < Init->Ty->ArrayLen && !equal(Tok, "}"); I++) { */
		if (I > 0) {
			Tok = skip(Tok, ",");
		}
		if (I < Init->Ty->ArrayLen) {
			initializer2(&Tok, Tok, Init->Children[I]);
		} else {
			// 跳过多余元素
			Tok = skipExcessElement(Tok);
		}
	}
}



// initializer = "{" initializer ("," initializer)* "}" | assign
static void initializer2(Token **Rest, Token *Tok, Initializer *Init) {

	if (Init->Ty->Kind == TY_ARRAY && Tok->Kind == TK_STR) {
		stringInitializer(Rest, Tok, Init);
		return;
	}

	if (Init->Ty->Kind == TY_ARRAY) {
		arrayInitializer(Rest, Tok, Init);
		return;
	}

	// assign
	// 为节点存储对应的表达式
	Init->Expr = assign(Rest, Tok);
}

// 初始化器
static Initializer *initializer(Token **Rest, Token *Tok, Type *Ty, Type **NewTy) {
	// 新建一个解析了类型的初始化器
	Initializer *Init = newInitializer(Ty, true);
	// 解析需要赋值到Init中
	initializer2(Rest, Tok, Init);
	// 将新类型传回变量
	*NewTy = Init->Ty;
	return Init;
}

// 指派初始化表达式
static Node *InitDesigExpr(InitDesig *Desig, Token *Tok) {
	// 返回Desig中的变量
	if (Desig->Var) {
		return newVarNode(Desig->Var, Tok);
	}

	// 需要赋值的变量名
	// 递归到次外层Design，有此时最外层有Desig->Var
	// 然后逐层计算偏移量
	Node *LHS = InitDesigExpr(Desig->Next, Tok);
	// 偏移量
	Node *RHS = newNum(Desig->Ind, Tok);
	// 返回偏移后的变量地址
	return newUnary(ND_DEREF, newAdd(LHS, RHS, Tok), Tok);
}

// 创建局部变量的初始化
static Node *createLVarInit(Initializer *Init, Type *Ty, InitDesig *Desig, Token *Tok) {
	if (Ty->Kind == TY_ARRAY) {
		// 预备表达式为空的情况
		Node *Nod = newNode(ND_NULL_EXPR, Tok);
		for (int I = 0; I < Ty->ArrayLen; I++) {
			// 这里next指向了上一级Desig的信息，以及在这其中的偏移量
			InitDesig Desig2 = {Desig, I};
			// 局部变量初始化
			Node *RHS = createLVarInit(Init->Children[I], Ty->Base, &Desig2, Tok);
			// 构造一个形如: NULL_EXPR, EXPR1, EXPR2 的二叉树
			Nod = newBinary(ND_COMMA, Nod, RHS, Tok);
		}

		return Nod;
	}

	// 如果需要作为右值的表达式为空，则设空表达式
	if (!Init->Expr) {
		return newNode(ND_NULL_EXPR, Tok);
	}

	// 变量可以直接赋值的左值
	Node *LHS = InitDesigExpr(Desig, Tok);
	return newBinary(ND_ASSIGN, LHS, Init->Expr, Tok);
}

static Node *LVarInitializer(Token **Rest, Token *Tok, Obj *Var) {
	// 获取初始化器，将值与数据结构一一对应
	Initializer *Init = initializer(Rest, Tok, Var->Ty, &Var->Ty);
	// 指派初始化
	InitDesig Desig = {NULL, 0, Var};

	// 我们首先要为所有元素赋0，然后有指定值的再进行赋值
	Node *LHS = newNode(ND_MEMZERO, Tok);
	LHS->Var = Var;

	// 创建局部变量的初始化
	Node *RHS = createLVarInit(Init, Var->Ty, &Desig, Tok);
	// 左部为全部清零，右部为需要赋值的部分
	return newBinary(ND_COMMA, LHS, RHS, Tok);
}

static Node *stmt(Token **Rest, Token *Tok) {
	// return expr ;
	if(equal(Tok, "return")) {
		Node *Nod = newNode(ND_RETURN, Tok);
		Node *Exp = expr(&Tok, Tok->Next);
		*Rest = skip(Tok, ";");

		addType(Exp);
		// 对于返回值的类型进行类型转换
		Nod->LHS = newCast(Exp, CurrentFn->Ty->ReturnTy);
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

	// switch
	if (equal(Tok, "switch")) {
		Node *Nod = newNode(ND_SWITCH, Tok);
		Tok = skip(Tok->Next, "(");
		Nod->Cond = expr(&Tok, Tok);
		Tok = skip(Tok, ")");

		// 记录之前的CurrentSwitch
		Node *Sw = CurrentSwitch;
		// 设置当前的CurrentSwitch
		CurrentSwitch = Nod;

		// 存储之前的break标签的信息
		char *Brk = BrkLabel;
		// 设置break标签的名称
		BrkLabel = Nod->BrkLabel = newUniqueName();

		// 进入解析各个case
		Nod->Then = stmt(Rest, Tok);

		// 恢复之前的CurrentSwitch
		CurrentSwitch = Sw;
		// 恢复之前break标签的名称
		BrkLabel = Brk;
		return Nod;
	}

	// case
	if (equal(Tok, "case")) {
		// 说明不在switch里面
		if (!CurrentSwitch)
			errorTok(Tok, "stray case");

		Node *Nod = newNode(ND_CASE, Tok);
		// case 后面的数值
    int Val = constExpr(&Tok, Tok->Next);
    Tok = skip(Tok, ":");
		Nod->Label = newUniqueName();
		// case 中的语句
		Nod->LHS = stmt(Rest, Tok);
		// case 对应的数值
		Nod->Val = Val;

		// 更新当前switch的全局CurrentSwitch
		// 将旧的CurrentSwitch链表头部存入Nod的CaseNext
		Nod->CaseNext = CurrentSwitch->CaseNext;
		// 将Nod存入CurrentSwitch的CaseNext
		CurrentSwitch->CaseNext = Nod;
		return Nod;
	}

	// default
	if (equal(Tok, "default")) {
		// 说明不在switch里面
		if (!CurrentSwitch)
			errorTok(Tok, "stray default");

		Node *Nod = newNode(ND_CASE, Tok);
		Tok = skip(Tok->Next, ":");
		Nod->Label = newUniqueName();
		Nod->LHS = stmt(Rest, Tok);
		// 存入CurrentSwitch->DefaultCase的默认标签
		CurrentSwitch->DefaultCase = Nod;
		return Nod;
	}


	// 解析for语句
	if (equal(Tok, "for")) {
		Node *Nod = newNode(ND_FOR, Tok);

		Tok = skip(Tok->Next, "(");

		// 进入for循环域以便支持局部的循环变量
		enterScope();

		// 在每层循环嵌套中设置break标签的名称
		char *Brk = BrkLabel; // 暂存当前break标签
		BrkLabel = Nod->BrkLabel = newUniqueName(); // 为当前全局break语句生成新的唯一名称

		// 在每层循环嵌套中设置continue标签的名称
		char *Cont = ContLabel; // 暂存当前continue标签
		ContLabel = Nod->ContLabel = newUniqueName(); // 为当前全局continue语句生成新的唯一名称

		if (isTypename(Tok)) {
			// 初始化循环变量
			Type *BaseTy = declspec(&Tok,	Tok, NULL);
			Nod->Init = declaration(&Tok, Tok, BaseTy);
		} else {
			// for 的初始化语句
			Nod->Init = exprStmt(&Tok, Tok);
		}

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

		// 离开for循环域
		leaveScope();

		// 离开一层循环时恢复全局break标签名称
		BrkLabel = Brk;

		// 离开一层循环时恢复全局continue标签名称
		ContLabel = Cont;

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

		// 在每层循环嵌套中设置break标签的名称
		char *Brk = BrkLabel; // 暂存当前break标签
		BrkLabel = Nod->BrkLabel = newUniqueName(); // 为当前全局break语句生成新的唯一名称

		// 在每层循环嵌套中设置continue标签的名称
		char *Cont = ContLabel; // 暂存当前continue标签
		ContLabel = Nod->ContLabel = newUniqueName(); // 为当前全局continue语句生成新的唯一名称

		// while 循环代码块
		Nod->Then = stmt(Rest, Tok);

		// 离开一层循环时恢复全局break标签名称
		BrkLabel = Brk;

		// 离开一层循环时恢复全局continue标签名称
		ContLabel = Cont;

		return Nod;
	}

	if (equal(Tok, "goto")) {
		Node *Nod = newNode(ND_GOTO, Tok);
		Nod->Label = getIdent(Tok->Next);
		// 将Nod同时存入Gotos，最后用于解析UniqueLabel
		// UniqueLabel 用于codegen, 给跳转指令一个唯一的标签
		Nod->GotoNext = Gotos;
		Gotos = Nod;

		*Rest = skip(Tok->Next->Next, ";");
		return Nod;
	}

	if (equal(Tok, "break")) {
		if (!BrkLabel)
			errorTok(Tok, "stray break");
		// 生成一个break节点
		// 跳转到break标签的位置
		Node *Nod = newNode(ND_GOTO, Tok);
		Nod->UniqueLabel = BrkLabel;
		*Rest = skip(Tok->Next, ";");
		return Nod;
	}

	if (equal(Tok, "continue")) {
		if (!ContLabel)
			errorTok(Tok, "stray continue");
		// 生成一个continue节点
		// 跳转到continue标签的位置
		Node *Nod = newNode(ND_GOTO, Tok);
		Nod->UniqueLabel = ContLabel;
		*Rest = skip(Tok->Next, ";");
		return Nod;
	}

	if (Tok->Kind == TK_IDENT && equal(Tok->Next,	":")) {
		Node *Nod = newNode(ND_LABEL, Tok);
		Nod->Label = strndup(Tok->Loc, Tok->Len);
		Nod->UniqueLabel = newUniqueName();
		Nod->LHS = stmt(Rest, Tok->Next->Next);
		// 将Nod同时存入Labels，最后用goto解析UniqueLabel
		Nod->GotoNext = Labels;
		Labels = Nod;
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

// 计算给定节点的常量表达式计算
static int64_t eval(Node *Nd) {
  addType(Nd);

  switch (Nd->Kind) {
  case ND_ADD:
    return eval(Nd->LHS) + eval(Nd->RHS);
  case ND_SUB:
    return eval(Nd->LHS) - eval(Nd->RHS);
  case ND_MUL:
    return eval(Nd->LHS) * eval(Nd->RHS);
  case ND_DIV:
    return eval(Nd->LHS) / eval(Nd->RHS);
  case ND_NEG:
    return -eval(Nd->LHS);
  case ND_MOD:
    return eval(Nd->LHS) % eval(Nd->RHS);
  case ND_BITAND:
    return eval(Nd->LHS) & eval(Nd->RHS);
  case ND_BITOR:
    return eval(Nd->LHS) | eval(Nd->RHS);
  case ND_BITXOR:
    return eval(Nd->LHS) ^ eval(Nd->RHS);
  case ND_SHL:
    return eval(Nd->LHS) << eval(Nd->RHS);
  case ND_SHR:
    return eval(Nd->LHS) >> eval(Nd->RHS);
  case ND_EQ:
    return eval(Nd->LHS) == eval(Nd->RHS);
  case ND_NEQ:
    return eval(Nd->LHS) != eval(Nd->RHS);
  case ND_LT:
    return eval(Nd->LHS) < eval(Nd->RHS);
  case ND_LTEQ:
    return eval(Nd->LHS) <= eval(Nd->RHS);
  case ND_COND:
    return eval(Nd->Cond) ? eval(Nd->Then) : eval(Nd->Else);
  case ND_COMMA:
    return eval(Nd->RHS);
  case ND_NOT:
    return !eval(Nd->LHS);
  case ND_BITNOT:
    return ~eval(Nd->LHS);
  case ND_LOGAND:
    return eval(Nd->LHS) && eval(Nd->RHS);
  case ND_LOGOR:
    return eval(Nd->LHS) || eval(Nd->RHS);
  case ND_CAST:
    if (isInteger(Nd->Ty)) {
      switch (Nd->Ty->Size) {
      case 1:
        return (uint8_t)eval(Nd->LHS);
      case 2:
        return (uint16_t)eval(Nd->LHS);
      case 4:
        return (uint32_t)eval(Nd->LHS);
      }
    }
    return eval(Nd->LHS);
  case ND_NUM:
    return Nd->Val;
  default:
    break;
  }

  errorTok(Nd->Tok, "not a compile-time constant");
  return -1;
}

// 解析常量表达式
static int64_t constExpr(Token **Rest, Token *Tok) {
  // 进行常量表达式的构造
  Node *Nd = conditional(Rest, Tok);
  // 进行常量表达式的计算
  return eval(Nd);
}

// A op= B ===> TMP = &A, *TMP = *TMP op B
static Node *toAssign(Node *Binary) {
	// A
	addType(Binary->LHS);
	// B
	addType(Binary->RHS);
	Token *Tok = Binary->Tok;

	// TMP
	Obj *Var = newLVar("", pointerTo(Binary->LHS->Ty));

	// TMP = &A
	Node *Expr1 = newBinary(ND_ASSIGN, newVarNode(Var, Tok),
												newUnary(ND_ADDR, Binary->LHS, Tok), Tok);

	// *TMP = *TMP op B
	Node *Expr2 = newBinary(
			ND_ASSIGN, newUnary(ND_DEREF, newVarNode(Var, Tok), Tok),
			newBinary(Binary->Kind, newUnary(ND_DEREF, newVarNode(Var, Tok), Tok),
								Binary->RHS, Tok),
			Tok);

	// TMP = &A, *TMP = *TMP op B
	return newBinary(ND_COMMA, Expr1, Expr2, Tok);
}

static Node *assign(Token **Rest, Token *Tok) {
	/* Node *Nod = equality(&Tok, Tok); */
	// conditional
	Node *Nod = conditional(&Tok, Tok);

	// 可能存在递归赋值 a=b=1
	if(equal(Tok, "=")) {
		return Nod = newBinary(ND_ASSIGN, Nod, assign(Rest, Tok->Next), Tok);
	}

	// += -= *= /=
	if (equal(Tok, "+="))
		return toAssign(newAdd(Nod, assign(Rest, Tok->Next), Tok));
	if (equal(Tok, "-="))
		return toAssign(newSub(Nod, assign(Rest, Tok->Next), Tok));
	if (equal(Tok, "*="))
		return toAssign(newBinary(ND_MUL, Nod, assign(Rest, Tok->Next), Tok));
	if (equal(Tok, "/="))
		return toAssign(newBinary(ND_DIV, Nod, assign(Rest, Tok->Next), Tok));
	if (equal(Tok, "%="))
		return toAssign(newBinary(ND_MOD, Nod, assign(Rest, Tok->Next), Tok));
	if (equal(Tok, "&="))
		return toAssign(newBinary(ND_BITAND, Nod, assign(Rest, Tok->Next), Tok));
	if (equal(Tok, "|="))
		return toAssign(newBinary(ND_BITOR, Nod, assign(Rest, Tok->Next), Tok));
	if (equal(Tok, "^="))
		return toAssign(newBinary(ND_BITXOR, Nod, assign(Rest, Tok->Next), Tok));
	if (equal(Tok, "<<="))
		return toAssign(newBinary(ND_SHL, Nod, assign(Rest, Tok->Next), Tok));
	if (equal(Tok, ">>="))
		return toAssign(newBinary(ND_SHR, Nod, assign(Rest, Tok->Next), Tok));

	*Rest = Tok;
	return Nod;
}

static Node *conditional(Token **Rest, Token *Tok) {
	Node *Cond = logOr(&Tok, Tok);

	if (!equal(Tok, "?")) {
		*Rest = Tok;
		return Cond;
	}

	Node *Nod = newNode(ND_COND, Tok);
	Nod->Cond = Cond;
	// expr ":"
	Nod->Then = expr(&Tok, Tok->Next);
	Tok = skip(Tok, ":");
	// conditional 这里不能解析为赋值式，因为c没有左值
	Nod->Else = conditional(Rest, Tok);
	return Nod;
}

// logOr = logAnd ("||" logAnd)*
static Node *logOr(Token **Rest, Token *Tok) {
	Node *Nod = logAnd(&Tok, Tok);
	while (equal(Tok, "||")) {
		Token *Start = Tok;
		Nod = newBinary(ND_LOGOR, Nod, logAnd(&Tok, Tok->Next), Start);
	}

	*Rest = Tok;
	return Nod;
}

// logAnd = bitOr ("&&" bitOr)*
static Node *logAnd(Token **Rest, Token *Tok) {
	Node *Nod = bitOr(&Tok, Tok);
	while (equal(Tok, "&&")) {
		Token *Start = Tok;
		Nod = newBinary(ND_LOGAND, Nod, bitOr(&Tok, Tok->Next), Start);
	}

	*Rest = Tok;
	return Nod;
}

// 位操作是有顺序的递归调用
// 按位或
// bitOr = bitXor ("|" bitXor)*
static Node *bitOr(Token **Rest, Token *Tok) {
	Node *Nod = bitXor(&Tok, Tok);
	while (equal(Tok, "|")) {
		Token *Start = Tok;
		Nod = newBinary(ND_BITOR, Nod, bitXor(&Tok, Tok->Next), Start);
	}

	*Rest = Tok;
	return Nod;
}

// 按位异或
// bitXor = bitAnd ("^" bitAnd)*
static Node *bitXor(Token **Rest, Token *Tok) {
	Node *Nod = bitAnd(&Tok, Tok);
	while (equal(Tok, "^")) {
		Token *Start = Tok;
		Nod = newBinary(ND_BITXOR, Nod, bitAnd(&Tok, Tok->Next), Start);
	}

	*Rest = Tok;
	return Nod;
}

// 按位与
// bitAnd = equality ("&" equality)*
static Node *bitAnd(Token **Rest, Token *Tok) {
	Node *Nod = equality(&Tok, Tok);
	while (equal(Tok, "&")) {
		Token *Start = Tok;
		Nod = newBinary(ND_BITAND, Nod, equality(&Tok, Tok->Next), Start);
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
	Node *Nod = shift(&Tok, Tok);

	while(true) {
		Token *Start = Tok;
		if(equal(Tok, "<")) {
			Nod = newBinary(ND_LT, Nod, shift(&Tok, Tok->Next), Start);
			continue;
		}

		if(equal(Tok, "<=")) {
			Nod = newBinary(ND_LTEQ, Nod, shift(&Tok, Tok->Next), Start);
			continue;
		}
		// X > Y 相当于 Y < X
		// >= 同
		if(equal(Tok, ">")) {
			Nod = newBinary(ND_LT, shift(&Tok, Tok->Next), Nod, Start);
			continue;
		}

		if(equal(Tok, ">=")) {
			Nod = newBinary(ND_LTEQ, shift(&Tok, Tok->Next), Nod, Start);
			continue;
		}

		*Rest = Tok;
		return Nod;
	}
}

static Node *shift(Token **Rest, Token *Tok) {
	Node *Nod = add(&Tok, Tok);

	while(true) {
		Token *Start = Tok;

		if(equal(Tok, "<<")) {
			Nod = newBinary(ND_SHL, Nod, add(&Tok, Tok->Next), Start);
			continue;
		}

		if(equal(Tok, ">>")) {
			Nod = newBinary(ND_SHR, Nod, add(&Tok, Tok->Next), Start);
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
	// cast
	Node *Nod = cast(&Tok, Tok);

	while(true) {
		Token *Start = Tok;

		if(equal(Tok, "*")) {
			Nod = newBinary(ND_MUL, Nod, cast(&Tok, Tok->Next), Start);
			continue;
		}

		if(equal(Tok, "/")) {
			Nod = newBinary(ND_DIV, Nod, cast(&Tok, Tok->Next), Start);
			continue;
		}

		if(equal(Tok, "%")) {
			Nod = newBinary(ND_MOD, Nod, cast(&Tok, Tok->Next), Start);
			continue;
		}

		*Rest = Tok;
		return Nod;
	}
}

static Node *cast(Token **Rest, Token *Tok) {
	// cast = "(" typeName ")" cast
	if (equal(Tok, "(") && isTypename(Tok->Next)) {
		Token *Start = Tok;
		Type *Ty = typename(&Tok, Tok->Next);
		Tok = skip(Tok, ")");
		// 解析嵌套类型
		Node *Nod = newCast(cast(Rest, Tok), Ty);
		Nod->Tok = Start;
		return Nod;
	}

	// unary
	return unary(Rest, Tok);
}

static Node *unary(Token **Rest, Token *Tok) {

	// 一元运算符可以看作生成一个单叉树
	if(equal(Tok, "+")) {
		return cast(Rest, Tok->Next);
	}

	// 对于负号，我们仅将数字后面的第一个负号看作减号，之后如果还有负号则看作一元负号
	if(equal(Tok, "-")) {
		return newUnary(ND_NEG, cast(Rest, Tok->Next), Tok);
	}

	// 取地址
	if(equal(Tok, "&")) {
		return newUnary(ND_ADDR, cast(Rest, Tok->Next), Tok);
	}

	// 解引用
	if(equal(Tok, "*")) {
		return newUnary(ND_DEREF, cast(Rest, Tok->Next), Tok);
	}

	// 非
	if(equal(Tok, "!")) {
		return newUnary(ND_NOT, cast(Rest, Tok->Next), Tok);
	}

	// 按位取反
	if(equal(Tok, "~")) {
		return newUnary(ND_BITNOT, cast(Rest, Tok->Next), Tok);
	}

	// 前置++ 改写为+=1
	if (equal(Tok, "++")) {
		return toAssign(newAdd(unary(Rest, Tok->Next), newNum(1, Tok), Tok));
	}

	// 前置-- 改写为-=1
	if (equal(Tok, "--")) {
		return toAssign(newSub(unary(Rest, Tok->Next), newNum(1, Tok), Tok));
	}

	return postfix(Rest, Tok);
}

static void structMembers(Token **Rest, Token *Tok, Type *Ty) {
	Member Head = {};
	Member *Cur = &Head;

	while (!equal(Tok, "}")) {
		// declspec
		Type *BaseTy = declspec(&Tok, Tok, NULL);
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

	// 构造不完整结构体 没有大括号
  if (Tag && !equal(Tok, "{")) {
		*Rest = Tok;

		// 有可能仅是声明，所以先找tag
    Type *Ty = findTag(Tag);
		if (Ty)
			return Ty;

		// 如果没有找到则构造一个不完整的结构体
		Ty = structType();
		Ty->Size = -1;
		pushTagScope(Tag, Ty);
    return Ty;
  }

	// 匹配正常结构体
	Tok = skip(Tok, "{");

  // 构造一个结构体
	Type *Ty = structType();
	structMembers(Rest, Tok, Ty);
	Ty->Align = 1;

  // 如果是重复定义则覆盖原有定义
	// 否则有名称就注册结构体类型
  if (Tag) {
		for (TagScope *S = Scp->Tags; S; S = S->Next) {
			if (equal(Tag, S->Name)) {
				*S->Ty = *Ty;
				return S->Ty;
			}
		}
		pushTagScope(Tag, Ty);
	}
  return Ty;
}

static Type *structDecl(Token **Rest, Token *Tok) {
	Type *Ty = structUnionDecl(Rest, Tok);
	Ty->Kind = TY_STRUCT;

	// 不完整结构体
	if (Ty->Size < 0) {
		return Ty;
	}

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

// 后置++ --
// 后置++  ===> 先A += 1, 再返回 A - 1 (-1不存储)
static Node *newIncDec(Node *Nod, Token *Tok, int Addend) {
	addType(Nod);
	return newCast(newAdd(toAssign(newAdd(Nod, newNum(Addend, Tok), Tok)),
												newNum(-Addend, Tok), Tok),
								 Nod->Ty);
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

		if (equal(Tok, "++")) {
			Nod = newIncDec(Nod, Tok, 1);
			Tok = Tok->Next;
			continue;
		}

		if (equal(Tok, "--")) {
			Nod = newIncDec(Nod, Tok, -1);
			Tok = Tok->Next;
			continue;
		}

		*Rest = Tok;
		return Nod;
	}

}

static Node *funCall(Token **Rest, Token *Tok) {
	Token *Start = Tok;
	Tok = Tok->Next->Next;

	// 查找函数名
	VarScope *S = findVar(Start);

	if (!S) {
		errorTok(Start, "implicit declaration of a function");
	}

	if (!S->Var || S->Var->Ty->Kind != TY_FUNC) {
		errorTok(Start, "not a function");
	}

	// 函数名的类型
	Type *Ty = S->Var->Ty;
	// 函数形参的类型
	Type *ParamTy = Ty->Params;

	Node Head = {};
	Node *Cur = &Head;

	while (!equal(Tok, ")")) {
		if (Cur != &Head) {
			Tok = skip(Tok, ",");
		}

		// assign
		Node *Arg = assign(&Tok, Tok);
		addType(Arg);

		if (ParamTy) {
			if (ParamTy->Kind == TY_STRUCT || ParamTy->Kind == TY_UNION) {
				errorTok(Arg->Tok, "passing struct or union is not supported yet");
			}

			// 将参数的节点类型进行转换
			Arg = newCast(Arg, ParamTy);
			// 前进到下一个参数
			ParamTy = ParamTy->Next;
		}
		// 对参数进行存储
		Cur->Next = Arg;
		Cur = Cur->Next;
		addType(Cur);
	}

	*Rest = skip(Tok, ")");

	Node *Nod = newNode(ND_FUNCALL, Start);
	Nod->FuncName = strndup(Start->Loc, Start->Len);
	// 函数类型
	Nod->FuncType = Ty;
	// 读取的返回类型
	Nod->Ty = Ty->ReturnTy;
	Nod->Args = Head.Next;
	return Nod;
}

static Node *primary(Token **Rest, Token *Tok) {
	Token *Start = Tok;

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

	// sizeof "(" typeName ")"
	if (equal(Tok, "sizeof") && equal(Tok->Next, "(") && isTypename(Tok->Next->Next)) {
		Type *Ty = typename(&Tok, Tok->Next->Next);
		*Rest = skip(Tok, ")");
		return newNum(Ty->Size, Start);
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
    // ident
    // 查找变量
    // 查找变量（或枚举常量）
		VarScope *S = findVar(Tok);
		// 如果变量(或枚举类型)不存在，就在链表中新增一个变量
		if(!S || (!S->Var && !S->EnumTy)) {
			errorTok(Tok, "undefined variable");
		}

		Node *Nd;
		if (S->Var) {
			Nd = newVarNode(S->Var, Tok);
		} else {
			Nd = newNum(S->EnumVal, Tok);
		}

		*Rest = Tok->Next;
		return Nd;
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

// 解析类型别名
static Token *parseTypedef(Token *Tok, Type *BaseTy) {
	bool First = true;

	while (!consume(&Tok, Tok, ";")) {
		if (!First) {
			Tok = skip(Tok, ",");
		}
		First = false;

		Type *Ty = declarator(&Tok, Tok, BaseTy);
		// 类型别名的变量名存入变量域中，并设置类型
		pushScope(getIdent(Ty->Name))->Typedef = Ty;
	}

	return Tok;
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

// 匹配goto和标签
// 因为标签可能会出现在goto后面，所以要在解析完函数后再进行goto和标签的解析
static void resolveGotoLabels(void) {
	// 遍历goto对应上label
	for (Node *X = Gotos; X; X = X->GotoNext) {
		for (Node *Y = Labels; Y; Y = Y->GotoNext) {
			if (!strcmp(X->Label, Y->Label)) {
				X->UniqueLabel = Y->UniqueLabel;
				break;
			}
		}

		if (X->UniqueLabel == NULL) {
			errorTok(X->Tok->Next, "use of undeclared label");
		}
	}

	Gotos = NULL;
	Labels = NULL;
}

static Token *function(Token *Tok, Type *BaseTy, VarAttr *Attr) {
	Type *Ty = declarator(&Tok, Tok, BaseTy);

	Obj *Fn = newGVar(getIdent(Ty->Name), Ty);
	Fn->IsFunction = true;
	// 如果函数签名后面没有方法体，则为函数声明
	Fn->IsDefinition = !consume(&Tok, Tok, ";");
	Fn->IsStatic = Attr->IsStatic;

	// 判断是否仅为函数签名
	if (!Fn->IsDefinition) {
		return Tok;
	}

	CurrentFn = Fn;
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
	// 处理goto和标签
	resolveGotoLabels();

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
		VarAttr Attr = {};
		Type *Basety = declspec(&Tok, Tok, &Attr);

		if (Attr.IsTypedef) {
			Tok = parseTypedef(Tok, Basety);
			continue;
		}

		// 函数
		if (isFunction(Tok)) {
			Tok = function(Tok, Basety, &Attr);
			continue;
		}

		// 全局变量
		Tok = globalVariable(Tok, Basety);
	}

	return Globals;
}
