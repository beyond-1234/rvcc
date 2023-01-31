#include "rvcc.h"

static char *CurrentFileName;
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

// 输出例如下面的错误，并退出
// foo.c:10: x = y + 1;
//               ^ <错误信息>
static void verrorAt(int LineNo, char *Loc, char *Fmt, va_list VA) {
	// find the line including Loc
	char *Line = Loc;
	// Line goes to the beginning of the actual line
	// if Line < CurrentInput, then is the beginning of file
	// Line[-1] != '\n' if the one before the first char of current line is the ending of the last line
	while (CurrentInput < Line && Line[-1] != '\n') {
		Line--;
	}

	// End goes to the ending of the line which is \n
	char *End = Loc;
	while (*End != '\n') {
		End++;
	}

	// print filename:line
	// Indent records how many words printed
	int Indent = fprintf(stderr, "%s:%d ", CurrentFileName, LineNo);
	fprintf(stderr, "%.*s\n", (int)(End - Line), Line);

	// 计算出错位置，Loc是出错位置的指针 + how many words printed
	int Pos = Loc - CurrentInput + Indent;
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
	int LineNo = 1;
	for (char *P = CurrentInput; P < Loc; P++) {
		// LineNo++ if counters \n
		if (*P == '\n') {
			LineNo++;
		}
	}

	va_list VA;
	va_start(VA, Fmt);
	verrorAt(LineNo, Loc, Fmt, VA);
}

// Tok解析出错
void errorTok(Token *Tok, char *Fmt, ...) {
	va_list VA;
	va_start(VA, Fmt);
	verrorAt(Tok->LineNo, Tok->Loc, Fmt, VA);
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
	static char *KW[] = {"==", "!=", "<=", ">=", "->",
											 "+=", "-=", "*=", "/=",
											 "++", "--", "%=", "&=", "|=", "^=",
											 "&&", "||"};

	for (int I = 0; I < sizeof(KW) / sizeof(*KW); I++) {
		if (startsWith(P, KW[I])) {
			return strlen(KW[I]);
		}
	}

	// 判断1字节的操作符
	return ispunct(*P) ? 1 : 0;
}

static bool isKeyword(Token *T) {
	static char *Keywords[] = {
		"return", "if", "else", "for",
		"while", "int", "sizeof", "char",
		"struct", "union", "long", "short",
		"void", "typedef", "_Bool", "enum",
		"static", "goto", "break"
	};

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

// 返回一位十六进制转十进制
// hexDigit = [0-9a-fA-F]
// 16: 0 1 2 3 4 5 6 7 8 9  A  B  C  D  E  F
// 10: 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
static int fromHex(char C) {
  if ('0' <= C && C <= '9')
    return C - '0';
  if ('a' <= C && C <= 'f')
    return C - 'a' + 10;
  return C - 'A' + 10;
}

// 读取转义字符
// 不带第一个反斜杠
static int readEscapedChar(char **NewPos, char *P) {
	if ('0' <= *P && *P <= '7') {
		// 读取一个八进制数字，不能长于3位
		// \abc = (a*8+b)*8+c
		// ++ 是为了跳过斜杠
		int C = *P++ - '0';
		if ('0' <= *P && *P <= '7') {
			C = (C << 3) + (*P++ - '0');
			if ('0' <= *P && *P <= '7') {
				C = (C << 3) + (*P++ - '0');
			}
		}
		*NewPos = P;
		return C;
	}

	// 读取十六进制转义字符
	// 十六进制转义字符没有字符数限制，只有读取到非十六进制字符时才会停止
	if (*P == 'x') {
		P++;
		if (!isxdigit(*P)) {
			errorAt(P, "invalid hex escape sequence");
		}

		int C = 0;
		// 读取一位或多位十六进制数字
		// \xWXYZ = ((W*16+X)*16+Y)*16+Z
		for (; isxdigit(*P); P++) {
			C = (C << 4) + fromHex(*P);
		}
		*NewPos = P;
		return C;
	}

	*NewPos = P + 1;

  switch (*P) {
  case 'a': // 响铃（警报）
    return '\a';
  case 'b': // 退格
    return '\b';
  case 't': // 水平制表符，tab
    return '\t';
  case 'n': // 换行
    return '\n';
  case 'v': // 垂直制表符
    return '\v';
  case 'f': // 换页
    return '\f';
  case 'r': // 回车
    return '\r';
  // 属于GNU C拓展
  case 'e': // 转义符
    return 27;
  default: // 默认将原字符返回
    return *P;
  }
}

// 读取到字符串字面量结尾
static char *stringLiteralEnd(char *P) {
  char *Start = P;
  for (; *P != '"'; P++) {
    if (*P == '\n' || *P == '\0')
      errorAt(Start, "unclosed string literal");
    if (*P == '\\')
      P++;
  }
  return P;
}

static Token *readCharLiteral(char *Start) {
	char *P = Start + 1;
	// 解析字符为 \0 的情况
	if (*P == '\0')
		errorAt(Start, "unclosed char literal");

	// 解析字符
	char C;
	// 转义
	if (*P == '\\')
		C = readEscapedChar(&P, P + 1);
	else
		C = *P++;

	// strchr返回以 ' 开头的字符串，若无则为NULL
	char *End = strchr(P, '\'');
	if (!End) {
		errorAt(P, "unclosed char literal");
	}

	// 构造一个NUM的终结符，值为C的数值
	Token *Tok = newToken(TK_NUM, Start, End + 1);
	Tok->Val = C;
	return Tok;
}

// 读取数字字面值
static Token *readIntLiteral(char *Start) {
	char *P = Start;

	// 读取二、八、十、十六进制
	// 默认为十进制
	int Base = 10;

	if (!strncasecmp(P, "0x", 2) && isxdigit(P[2])) {
		// 十六进制
		P += 2;
		Base = 16;
	} else if (!strncasecmp(P, "0b", 2) && (P[2] == '0' || P[2] == '1')) {
		// 二进制
		P += 2;
		Base = 2;
	} else if (*P == '0') {
		// 八进制
		Base = 8;
	}

	// 将字符串转换为Base进制的数字
	long Val = strtoul(P, &P, Base);
	// 数字理应处理完了，如果还有字符说明数字有问题
	if (isalnum(*P)) {
		errorAt(P, "invalid digit");
	}

	// 构造NUM的终结符
	Token *Tok = newToken(TK_NUM, Start, P);
	Tok->Val = Val;
	return Tok;
}

static Token *readStringLiteral(char *Start) {
  // 读取到字符串字面量的右引号
  char *End = stringLiteralEnd(Start + 1);
  // 定义一个与字符串字面量内字符数+1的Buf
  // 用来存储最大位数的字符串字面量
  char *Buf = calloc(1, End - Start);
  // 实际的字符位数，一个转义字符为1位
  int Len = 0;

  // 将读取后的结果写入Buf
  for (char *P = Start + 1; P < End;) {
    if (*P == '\\') {
      Buf[Len++] = readEscapedChar(&P, P + 1);
    } else {
      Buf[Len++] = *P++;
    }
  }

  // Token这里需要包含带双引号的字符串字面量
  Token *Tok = newToken(TK_STR, Start, End + 1);
  // 为\0增加一位
  Tok->Ty = arrayOf(TyChar, Len + 1);
  Tok->Str = Buf;
  return Tok;
}

// 为所有Token计算行号
static void addLineNo(Token *Tok) {
	char *P = CurrentInput;
	int N = 1;

	do {
		if (P == Tok->Loc) {
			Tok->LineNo = N;
			Tok = Tok->Next;
		}

		if (*P == '\n') {
			N++;
		}
	} while(*P++);
}

// 终结符解析
Token *tokenize(char *FileName, char *P) {
	CurrentFileName = FileName;
	CurrentInput = P;
  Token Head = {};
  Token *Cur = &Head;

  while (*P) {
		// 跳过行注释
    if (startsWith(P, "//")) {
      P += 2;
      while (*P != '\n')
        P++;
      continue;
    }

    // 跳过块注释
    if (startsWith(P, "/*")) {
      // 查找第一个"*/"的位置
      char *Q = strstr(P + 2, "*/");
      if (!Q)
        errorAt(P, "unclosed block comment");
      P = Q + 2;
      continue;
    }


    // 跳过所有空白符如：空格、回车
    if (isspace(*P)) {
      ++P;
      continue;
    }

    // 解析数字
    if (isdigit(*P)) {
      /* // 初始化，类似于C++的构造函数 */
      /* // 我们不使用Head来存储信息，仅用来表示链表入口，这样每次都是存储在Cur->Next */
      /* // 否则下述操作将使第一个Token的地址不在Head中。 */
      /* Cur->Next = newToken(TK_NUM, P, P); */

			// 读取数字字面值
			Cur->Next = readIntLiteral(P);

      // 指针前进
      Cur = Cur->Next;
			P += Cur->Len;
      /* const char *OldPtr = P; */
      /* Cur->Val = strtoul(P, &P, 10); */
      /* Cur->Len = P - OldPtr; */
      continue;
    }

		if (*P == '"') {
			Cur->Next = readStringLiteral(P);
			Cur = Cur->Next;
			P += Cur->Len;
			continue;
		}

		if (*P == '\'') {
			Cur->Next = readCharLiteral(P);
			Cur = Cur->Next;
			P += Cur->Len;
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

	addLineNo(Head.Next);

	// 由于解析标记符和解析关键字没有区别，所以我们最后判断以下关键字
	convertKeyWords(Head.Next);

  // Head无内容，所以直接返回Next
  return Head.Next;
}

static char *readFile(char *Path) {
	FILE *FP;

	if (strcmp(Path, "-") == 0) {
		// if filename is - , then read from stream
		FP = stdin;
	} else {
		FP = fopen(Path, "r");
		if (!FP) {
			// errno is the last error code within system
			// strerror function print error message
			error("can not open %s: %s", Path, strerror(errno));
		}
	}

	char *Buf;
	size_t BufLen;
	FILE *Out = open_memstream(&Buf, &BufLen);

	// read the whole file
	while (true) {
		char Buf2[4096];
		// fread function reads from file stream to byte array
		// params: byte array, array element size, array max len, file pointer
		int N = fread(Buf2, 1, sizeof(Buf2), FP);
		if (N == 0) {
			break;
		}
		// params: byte array, array element size, actual array len, file pointer
		fwrite(Buf2, 1, N, Out);
	}

	// finished reading file
	if (FP != stdin) {
		fclose(FP);
	}

	fflush(Out);

	// make sure last line ended by \n
	// keep consistency with previous code
	if (BufLen == 0 || Buf[BufLen - 1] != '\n') {
		fputc('\n', Out);
	}
	fputc('\0', Out);
	fclose(Out);
	return Buf;
}

Token *tokenizeFile(char *Path) {
	return tokenize(Path, readFile(Path));
}
