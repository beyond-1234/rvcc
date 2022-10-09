#include "rvcc.h"

static char *CurrentInput;

// 输出错误信息
// static文件内可以访问的函数
// Fmt为传入的字符串， ... 为可变参数，表示Fmt后面所有的参数
void error(char *Fmt, ...) {
  // 定义一个va_list变量
  va_list VA;
  // VA获取Fmt后面的所有参数
  va_start(VA, Fmt);
  // vfprintf可以输出va_list类型的参数
  vfprintf(stderr, Fmt, VA);
  // 在结尾加上一个换行符
  fprintf(stderr, "\n");
  // 清除VA
  va_end(VA);
  // 终止程序
  exit(1);
}

static void verrorAt(char *Loc, char *Fmt, va_list VA) {
	fprintf(stderr, "%s\n", CurrentInput);

	// 计算出错位置，Loc是出错位置的指针
	int Pos = Loc - CurrentInput;
	// 将字符串补齐Pos位，因为是空字符串，所以填充Pos个空格
	fprintf(stderr, "%*s", Pos, "");
	fprintf(stderr, "^ ");
	vfprintf(stderr, Fmt, VA);
	fprintf(stderr, "\n");
	va_end(VA);
	exit(1);
}

// 字符解析出错
void errorAt(char *Loc, char *Fmt, ...) {
	va_list VA;
	va_start(VA, Fmt);
	verrorAt(Loc, Fmt, VA);
}

// Tok解析出错
void errorTok(Token *Tok, char *Fmt, ...) {
	va_list VA;
	va_start(VA, Fmt);
	verrorAt(Tok->Loc, Fmt, VA);
}

// 判断Tok的值是否等于指定值，没有用char，是为了后续拓展
bool equal(Token *Tok, char *Str) {
  // 比较字符串LHS（左部），RHS（右部）的前N位，S2的长度应大于等于N.
  // 比较按照字典序，LHS<RHS回负值，LHS=RHS返回0，LHS>RHS返回正值
  // 同时确保，此处的Op位数=N
  return memcmp(Tok->Loc, Str, Tok->Len) == 0 && Str[Tok->Len] == '\0';
}

// 跳过指定的Str
Token *skip(Token *Tok, char *Str) {
  if (!equal(Tok, Str)) {
		errorTok(Tok, "expect '%s'", Str);
	}
  return Tok->Next;
}

// 消耗指定的Str
bool consume(Token **Rest, Token *Tok, char *Str) {
  if (equal(Tok, Str)) {
		*Rest = Tok->Next;
		return true;
	}

	*Rest = Tok;
  return false;
}

// 返回TK_NUM的值
static int getNumber(Token *Tok) {
  if (Tok->Kind != TK_NUM) {
		errorTok(Tok, "expect a number");
	}
	return Tok->Val;
}

// 生成新的Token
static Token *newToken(TokenKind Kind, char *Start, char *End) {
  // 分配1个Token的内存空间
  Token *Tok = calloc(1, sizeof(Token));
  Tok->Kind = Kind;
  Tok->Loc = Start;
  Tok->Len = End - Start;
  return Tok;
}

// 判断P是否以Str开头
static bool startsWith(char *P, char *Str) {
	return strncmp(P, Str, strlen(Str)) == 0;
}

// 读取符号的长度
static int readPunct(char *P) {
	if(startsWith(P, "==") || startsWith(P, "!=")
	|| startsWith(P, ">=") || startsWith(P, "<=")) {
		return 2;
	}

	return ispunct(*P) ? 1 : 0;
}

static bool isKeyword(Token *T) {
	static char *Keywords[] = {"return", "if", "else", "for", "while", "int", "sizeof", "char"};	

	for(int i = 0; i < sizeof(Keywords) / sizeof(*Keywords); ++i) {
		if(equal(T, Keywords[i])) {
			return true;
		}
	}
	return false;
}

// 将名为"return"的终结符转换为关键字
static void convertKeyWords(Token  *Tok) {
	for (Token *T = Tok; T->Kind != TK_EOF; T = T->Next) {
		if (isKeyword(T)) {
			T->Kind = TK_KEYWORD;
		}
	}
}

// 判断变量首字母是否合法
// [a-zA-z_]
static bool isIdent1(char C) {
	return ('a' <= C && C <= 'z') || ('A' <= C && C <= 'Z') || (C == '_');
}

// 判断变量其他字母是否合法
// [a-zA-z_0-9]
static bool isIdent2(char C) {
	return isIdent1(C) || ('1' <= C && C <= '9');
}

// 终结符解析
Token *tokenize(char *P) {
	CurrentInput = P;
  Token Head = {};
  Token *Cur = &Head;

  while (*P) {
    // 跳过所有空白符如：空格、回车
    if (isspace(*P)) {
      ++P;
      continue;
    }

    // 解析数字
    if (isdigit(*P)) {
      // 初始化，类似于C++的构造函数
      // 我们不使用Head来存储信息，仅用来表示链表入口，这样每次都是存储在Cur->Next
      // 否则下述操作将使第一个Token的地址不在Head中。
      Cur->Next = newToken(TK_NUM, P, P);
      // 指针前进
      Cur = Cur->Next;
      const char *OldPtr = P;
      Cur->Val = strtoul(P, &P, 10);
      Cur->Len = P - OldPtr;
      continue;
    }

		// 解析标记符=解析变量
		// 或解析关键字
		// [a-zA-Z_][a-zA-Z0-9_]*
		if(isIdent1(*P)) {
			char *Start = P;
			do {
				++P;
			} while(isIdent2(*P));
			// 循环最后会多++一次，所以这里不用P+1
			Cur->Next = newToken(TK_IDENT, Start, P);
			Cur = Cur->Next;
			continue;
		}

    // 解析操作符
    /* if (*P == '+' || *P == '-') { */
		int PunctLen = readPunct(P);
    if (PunctLen) {
      Cur->Next = newToken(TK_PUNCT, P, P + PunctLen);
      Cur = Cur->Next;
      P += PunctLen;
      continue;
    }

    // 处理无法识别的字符
    errorAt(P, "invalid token");
  }

  // 解析结束，增加一个EOF，表示终止符。
  Cur->Next = newToken(TK_EOF, P, P);
	// 由于解析标记符和解析关键字没有区别，所以我们最后判断以下关键字
	convertKeyWords(Head.Next);	

  // Head无内容，所以直接返回Next
  return Head.Next;
}
