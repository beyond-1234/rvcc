// 使用POSIX.1 标准
// 因为使用了strndup函数，它需要使用这个标准
#define _POSIX_C_SOURCE 200809L

#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// 共用头文件，定义了多个文件共同使用的函数和数据结构

// 语法树节点种类
typedef enum {
	ND_ADD,
	ND_SUB,
	ND_MUL,
	ND_DIV,
	ND_NEG,
	ND_LT,
	ND_LTEQ,
	ND_EQ,
	ND_NEQ,
	ND_NUM,
	ND_ASSIGN,		// 赋值
	ND_IF,				// IF语句
	ND_FOR,				// FOR语句
	ND_RETURN,		// 返回
	ND_BLOCK,			// { ... } 代码块
	ND_VAR,				// 变量
	ND_EXPR_STMT	// 表达式语句
} NodeKind;

// 语法树二叉树节点
typedef struct Node Node;

// 本地变量
typedef struct Obj Obj;
struct Obj {
	Obj *Next;		// 指向下一对象
	char *Name;		// 变量名
	int Offset;		// fp的偏移量
};

// 函数
typedef  struct Function Function;
struct Function {
	Node *Body;				// 函数体
	Obj *Locals;			// 本地变量
	int StackSize;		// 栈大小
};

struct Node {
	NodeKind Kind;		// 节点种类
	Node *Next;				// 指向下一节点，表示下一语句
	Node *LHS;				// 左子树
	Node *RHS;				// 右子树

	// if 语句 或 for 语句
	Node *Cond;				// 条件
	Node *Then;				// 符合条件的代码体
	Node *Else;				// 不符合条件的代码体
	Node *Init;				// for循环的初始化语句 
	Node *Inc;				// for循环的递增语句

	// 代码块
	Node *Body;

	Obj *Var;					// 存储ND_VAR的变量
	int Val;					// 存储ND_NUM种类的值
};

// 词法分析的终结符类型
typedef enum {
  TK_IDENT,			// 标记符，可以为变量名、函数名等
  TK_PUNCT,			// 操作符如： + -
  TK_NUM,				// 数字
  TK_KEYWORD,   // 关键字
  TK_EOF,				// 文件终止符，即文件的最后
} TokenKind;

// 终结符结构体
typedef struct Token Token;
struct Token {
  TokenKind Kind; // 种类
  Token *Next;    // 指向下一终结符
  int Val;        // 值
  char *Loc;      // 在解析的字符串内的位置
  int Len;        // 长度
};

// 报错函数
void error(char *Fmt, ...);
void errorAt(char *Loc, char *Fmt, ...);
void errorTok(Token *Tok, char *Fmt, ...);

// 判断token与str关系的函数
bool equal(Token *Tok, char *Str);
Token *skip(Token *Tok, char *Str);

// 词法分析函数
Token *tokenize(char *Input);

// 语法分析入口函数
Function *parse(Token *Tok);

// 代码生成入口函数
void codegen(Function *Prog);
