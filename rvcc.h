// 使用POSIX.1 标准
// 因为使用了strndup函数，它需要使用这个标准
#define _POSIX_C_SOURCE 200809L

#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <strings.h>

// 共用头文件，定义了多个文件共同使用的函数和数据结构

// 语法树节点种类
typedef enum {
	ND_ADD,
	ND_SUB,
	ND_MUL,
	ND_DIV,
	ND_MOD,
	ND_BITAND,
	ND_BITOR,
	ND_BITXOR,
	ND_NEG,
	ND_LT,
	ND_LTEQ,
	ND_EQ,
	ND_NEQ,
	ND_NUM,
	ND_CAST,			// 类型转换
	ND_ASSIGN,		// 赋值
	ND_COMMA,		  // 逗号
	ND_MEMBER,		// 结构体成员
	ND_ADDR,			// 取地址 &
	ND_DEREF,			// 解引用 *
	ND_NOT,				// 非, !
	ND_BITNOT,		// 按位取反, ~
	ND_IF,				// IF语句
	ND_FOR,				// FOR语句 和 while 语句
	ND_RETURN,		// 返回
	ND_BLOCK,			// { ... } 代码块
	ND_FUNCALL,		// 函数调用
	ND_VAR,				// 变量
	ND_EXPR_STMT,	// 表达式语句
	ND_STMT_EXPR	// 语句表达式
} NodeKind;

typedef struct Type Type;
// 语法树二叉树节点
typedef struct Node Node;
// 结构体成员
typedef struct Member Member;

// 变量 或 函数
typedef struct Obj Obj;
struct Obj {
	Obj *Next;				// 指向下一对象
	char *Name;				// 变量名
	Type *Ty;					// 变量的类型
	bool isLocal;			// 是局部变量或全局变量
	int Offset;				// fp的偏移量
	bool IsFunction;	// 函数或全局变量
	bool IsDefinition;// 是否为函数签名
	bool IsStatic;		// 是否为文件域内的
	Obj *Params;			// 函数形参

	// 全局变量
	char *InitData;

  Node *Body;    // 函数体
  Obj *Locals;   // 本地变量
  int StackSize; // 栈大小
};


// 词法分析的终结符类型
typedef enum {
  TK_IDENT,			// 标记符，可以为变量名、函数名等
  TK_PUNCT,			// 操作符如： + -
  TK_NUM,				// 数字
  TK_STR,				// 字符串字面值
  TK_KEYWORD,   // 关键字
  TK_EOF,				// 文件终止符，即文件的最后
} TokenKind;

// 终结符结构体
typedef struct Token Token;
struct Token {
  TokenKind Kind; // 种类
  Token *Next;    // 指向下一终结符
  int64_t Val;        // 值
  char *Loc;      // 在解析的字符串内的位置
  int Len;        // 长度

	Type *Ty;					// TK_STR使用
	char *Str;				// 字符串字面值，包括'\0'

	int LineNo;     // 行号
};

struct Node {
	NodeKind Kind;		// 节点种类
	Node *Next;				// 指向下一节点，表示下一语句
	Token *Tok;				// 节点对应的终结符
	Type *Ty;					// 节点中数据的类型

	Node *LHS;				// 左子树
	Node *RHS;				// 右子树

	// if 语句 或 for 语句
	Node *Cond;				// 条件
	Node *Then;				// 符合条件的代码体
	Node *Else;				// 不符合条件的代码体
	Node *Init;				// for循环的初始化语句
	Node *Inc;				// for循环的递增语句

	// 代码块 or stmt expr
	Node *Body;

	// 结构体成员
	Member *Mem;

	char *FuncName;		// 函数名
	Type *FuncType;		// 函数类型
	Node *Args;				// 函数参数

	Obj *Var;					// 存储ND_VAR的变量
	int64_t Val;					// 存储ND_NUM种类的值
};

typedef enum {
	TY_VOID,	// void 类型
	TY_CHAR,	// char 字符型
	TY_BOOL,	// _Bool 类型
	TY_INT,		// int 整型
	TY_LONG,	// long 长整型
	TY_SHORT,	// short 短整型
	TY_ENUM,	// 枚举类型
	TY_PTR,		// 指针
	TY_FUNC,	// 函数
	TY_ARRAY,	// 数组
	TY_STRUCT,// 结构体
	TY_UNION	// 联合体
} TypeKind;

struct Type {
	TypeKind Kind;		// 种类
	int Size;					// 大小，sizeof返回的值
	int Align;				// 对其量

	Type *Base;				// 指向的类型
	Token *Name;			// 变量名

	// 数组
	int ArrayLen;			// 数组长度，元素总个数

	// 结构体成员
	Member *Mems;

	Type *ReturnTy;		// 函数返回的类型
	Type *Params;			// 形参列表
	Type *Next;				// 下一类型
};

// 结构体成员
struct Member {
	Member *Next;			// 下一成员
	Type *Ty;					// 类型
	Token *Name;			// 名称
	int Offset;				// 成员在结构体内的偏移量
};

extern Type *TyVoid;
extern Type *TyBool;
extern Type *TyChar;
extern Type *TyInt;
extern Type *TyEnum;
extern Type *TyLong;
extern Type *TyShort;

// 判断是否为整型
bool isInteger(Type *TY);
// 为节点内的所有节点添加类型
void addType(Node *Nod);
// 构建一个指针类型，并指向基类
Type *pointerTo(Type *Base);
// 枚举类型
Type *enumType(void);
// 函数类型
Type *funcType(Type *ReturnTy);
// 复制类型
Type *copyType(Type *Ty);
// 数组类型
Type *arrayOf(Type *base, int Size);
// 字符串解析
char *format(char *Fmt, ...);

// 报错函数
void error(char *Fmt, ...);
void errorAt(char *Loc, char *Fmt, ...);
void errorTok(Token *Tok, char *Fmt, ...);

// 判断token与str关系的函数
bool equal(Token *Tok, char *Str);
Token *skip(Token *Tok, char *Str);
bool consume(Token **Rest, Token *Tok, char *Str);

// 词法分析函数
Token *tokenizeFile(char *Path);

// 类型转换，将表达式的值转换为另一种类型
Node *newCast(Node *Expr, Type *Ty);

// 语法分析入口函数
Obj *parse(Token *Tok);

// 代码生成入口函数
void codegen(Obj *Prog, FILE *Out);
int alignTo(int N, int Align);

// 如果出错了，打印文件名和行号
#define unreachable() error("internal error at %s:%d", __FILE__, __LINE__);
