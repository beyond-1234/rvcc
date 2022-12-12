#include "rvcc.h"

static void genExpr(Node *Nod);
static void genStmt(Node *Nod);

// 输出文件
static FILE *OutputFile;
// 记录栈的深度
static int Depth;
// 用于函数参数的寄存器
// RICSV中函数的前6个寄存器就是用这几个寄存器来存
static char *ArgReg[] = {"a0", "a1", "a2", "a3", "a4", "a5"};
// 当前的函数
static Obj *CurrentFn;

// 输出字符串并换行
static void printLine(char *Fmt, ...) {
	va_list VA;

	va_start(VA, Fmt);
	vfprintf(OutputFile, Fmt, VA);
	va_end(VA);

	fprintf(OutputFile, "\n");
}


// 类型枚举
enum { I8, I16, I32, I64 };

// 获取类型对应的枚举值
static int getTypeId(Type *Ty) {
	switch (Ty->Kind) {
		case TY_CHAR:
			return I8;
		case TY_SHORT:
			return I16;
		case TY_INT:
			return I32;
		default:
			return I64;
	}
}

// 类型映射表
// 先逻辑左移N位，再算术右移N位，就实现了将64位有符号数转换为64-N位的有符号数
// 只需要大类型转小类型
static char i64i8[] =		"  # 转换为i8类型\n"
												"  slli a0, a0, 56\n"
												"  srai a0, a0, 56";
static char i64i16[] =	"  # 转换为i16类型\n"
												"  slli a0, a0, 48\n"
												"  srai a0, a0, 48";
static char i64i32[] =	"  # 转换为i32类型\n"
												"  slli a0, a0, 32\n"
												"  srai a0, a0, 32";
// 所有类型转换表
static char *castTable[10][10] = {
    // clang-format off

    // 被映射到
    // {i8,  i16,    i32,    i64}
    {NULL,   NULL,   NULL,   NULL}, // 从i8转换
    {i64i8,  NULL,   NULL,   NULL}, // 从i16转换
    {i64i8,  i64i16, NULL,   NULL}, // 从i32转换
    {i64i8,  i64i16, i64i32, NULL}, // 从i64转换

    // clang-format on
};

static void cast(Type *From, Type *To) {
	if (To->Kind == TY_VOID) {
		return;
	}

	if (To->Kind == TY_BOOL) {
		printLine("  # 转为Bool类型: 为0置0，非0置1");
		printLine("  snez a0, a0");
		return;
	}

	int T1 = getTypeId(From);
	int T2 = getTypeId(To);
	if (castTable[T1][T2]) {
		printLine("  # 转换函数");
		printLine("%s", castTable[T1][T2]);
	}
}


// 代码段计数
static int count(void) {
	static int I = 1;
	return I++;
}

// 将结果压栈
// sp为栈顶指针，RISCV栈向下增长，64位下，整数8个字节
// 当前栈顶指针就是sp，将a0的值压入栈
// 不使用寄存器的原因是 需要存储的值的数量未知
static void push() {
	printLine("  # 压栈，将a0的值存入栈顶");
	printLine("	addi sp, sp, -8");
	printLine("	sd a0, 0(sp)");
	Depth++;
}

// 将栈顶，sp指向的地址的值弹出到a1
static void pop(char *Reg) {
	printLine(" # 弹栈，将栈顶的值存入%s", Reg);
	printLine("	ld %s, 0(sp)", Reg);
	printLine("	addi sp, sp, 8");
	Depth--;
}

// 加载a0指向的值
static void load(Type *Ty) {
	if (Ty->Kind == TY_ARRAY || Ty->Kind == TY_STRUCT || Ty->Kind == TY_UNION) {
		return;
	}

	printLine("  # 读取a0中存放的地址，得到的值存入a0");
	if (Ty->Size == 1) {
		// b = byte = 1字节
		printLine("  lb a0, 0(a0)");
	} else if (Ty->Size == 2) {
		// h = half word = 2字节
		printLine("  lh a0, 0(a0)");
	} else if (Ty->Size == 4) {
		// w = word = 4字节
		printLine("  lw a0, 0(a0)");
	} else {
		// ld 的d=double word=8字节
		printLine("  ld a0, 0(a0)");
	}
}

// 将栈顶值(为一个地址)存入a0
static void store(Type *Ty) {
	pop("a1");

	if (Ty->Kind == TY_STRUCT || Ty->Kind == TY_UNION) {
		printLine("  # 对%s进行赋值", Ty->Kind == TY_STRUCT ? "结构体" : "联合体");
		for (int I = 0; I < Ty->Size; ++I) {
			printLine("  lb a2, %d(a0)", I);
			printLine("  sb a2, %d(a1)", I);
		}

		return;
	}

	printLine("  # 读取a0的值，写入到a1存放的地址");
	if (Ty->Size == 1) {
		printLine("  sb a0, 0(a1)");
	} else if (Ty->Size == 2) {
		// h === half word
		printLine("  sh a0, 0(a1)");
	} else if (Ty->Size == 4) {
		printLine("  sw a0, 0(a1)");
	} else {
		printLine("  sd a0, 0(a1)");
	}
}

// 把N对其到离Align最近的整数倍的值
// ex: N=17，Align=8，则得到24
// 在创建变量时，fp先移动Size个大小，比如int就得到了32
//
// 注意栈是向下的，从高地址向低地址增长的
// 用于栈的对齐
int alignTo(int N, int Align) {
	// (0,Align] 返回Align
	return (N + Align - 1) / Align * Align;
}

// 根据变量的链表计算出偏移量
static void assignLVarOffsets(Obj *Prog) {
	
	// 为每个函数计算其变量所用的栈空间
	for (Obj *Fn = Prog; Fn; Fn = Fn->Next) {
		if (!Fn->IsFunction) {
			continue;
		}
		int Offset = 0;
		for (Obj *Var = Fn->Locals; Var; Var = Var->Next) {

			/* 对于栈来说，栈顶指针从高地址向低地址移动 */
			/* 在创建变量时，fp指针会先向低地址移动相应大小，腾出变量的空间 */
			/* 我们这里在腾出空间之前先对齐，对齐之后再腾出变量的空间 */
			/* 比如若char 后面是 int */
			/* 则会先为char计算偏移量，创建char(指针先移动1个字节，再填充1个字节)，占用1个字节； */
			/* 再为int计算偏移量，此时将计算出对齐到最近的8的倍数字节位置，再创建int(指针先移动8个字节，再填充8个字节)，占用8个字节 */
			/* 这样就以8的倍数对齐了变量空间，char和int中间隔了7个字节的空间 */

			// 每个变量分配相应字节
			Offset += Var->Ty->Size;
			// 对齐变量
			Offset = alignTo(Offset, Var->Ty->Align);
			// 为每个变量赋一个编译量，或者说是栈中地址
			Var->Offset = -Offset;
		}

		// 将栈对其到16字节
		Fn->StackSize = alignTo(Offset, 16);
	}

}

// 计算给定节点的绝对地址
// 如果报错，说明节点不在内存里
static void genAddr(Node *Nod) {
	switch (Nod->Kind) {
		case ND_VAR:
			if (Nod->Var->isLocal) {
				// 编译量是相对于fp的
				// a0在这里存的是地址而不是数值
				printLine("  # 获取局部变量%s的栈内地址为%d(fp)", Nod->Var->Name,
					 Nod->Var->Offset);
				printLine("  addi a0, fp, %d", Nod->Var->Offset);
			} else {
				printLine("  # 获取全局变量%s的", Nod->Var->Name);
				// 获取全局变量的地址	
				// 高地址(高20位，31~20位)
				printLine("  lui a0, %%hi(%s)", Nod->Var->Name);
				// 低地址(低12位，19~0位)
				printLine("  addi a0, a0, %%lo(%s)", Nod->Var->Name);
			}
			return;
		// 解引用
		case ND_DEREF:
			genExpr(Nod->LHS);
			return;
		case ND_COMMA:
			genExpr(Nod->LHS);
			genAddr(Nod->RHS);
			return;
		case ND_MEMBER:
			genAddr(Nod->LHS);
			printLine("  addi a0, a0, %d", Nod->Mem->Offset);
			return;
		default:
			break;
	}

	errorTok(Nod->Tok, "not an lvalue");
}

// 对AST二叉树进行中序遍历处理
static void genExpr(Node *Nod) {
	// .loc 文件编号 行号
	printLine("  .loc 1 %d", Nod->Tok->LineNo);

  // 这里我们将算式分解为 num (op num) (op num)...的形式
  // 所以先将第一个num传入a0
	switch(Nod->Kind) {
		// li为addi别名指令，加载一个立即数到寄存器中
		// 加载数字到a0
		case ND_NUM: 
			printLine("  # 将%d加载到a0中", Nod->Val);
			printLine("  li a0, %ld", Nod->Val);
			return;
		// 识别到负号对寄存器取反
		case ND_NEG: 
			// 我们生成单叉树时只使用左子树
			genExpr(Nod->LHS);
			// neg a0, a0 是 sub a0, x0 的别名
			// 即 a0 = 0 - a0
			printLine("  # 对a0的值进行取反");
			printLine("  neg a0, a0");
			return;
		// 变量
		case ND_VAR:
		case ND_MEMBER:
			// 计算出变量的地址，然后存入a0
			genAddr(Nod);
			// 访问a0地址中存储的数据，存入a0中
			// 当前变量写死为8字节，所以用ld
			load(Nod->Ty);
			return;
		// 解引用
		case ND_DEREF:
			genExpr(Nod->LHS);
			load(Nod->Ty);
			return;
		// 取地址
		case ND_ADDR:
			genAddr(Nod->LHS);
			return;
		case ND_ASSIGN:
			// 左部是左值，保存值到的地址
			genAddr(Nod->LHS);
			push();
			// 右部是右值，为表达式的值
			genExpr(Nod->RHS);
			store(Nod->Ty);
			return;
		case ND_STMT_EXPR:
			for (Node *N = Nod->Body; N; N = N->Next) {
				genStmt(N);
			}
			return;
		case ND_COMMA:
			genExpr(Nod->LHS);
			genExpr(Nod->RHS);
			return;
		case ND_CAST:
			genExpr(Nod->LHS);
			cast(Nod->LHS->Ty, Nod->Ty);
			return;
		case ND_FUNCALL: {
			// 记录参数个数
			int NArgs = 0;
			// 计算所有参数的值，正向压栈
			for (Node *Arg = Nod->Args; Arg; Arg = Arg->Next) {
				genExpr(Arg);
				push();
				NArgs++;
			}

			// 反向弹栈 a0=参数1，a1=参数2.....
			for (int i = NArgs - 1; i >= 0; i--) {
				pop(ArgReg[i]);
			}

			// 调用函数
			printLine("\n  # 调用函数%s", Nod->FuncName);
			printLine("  call %s", Nod->FuncName);
			return;
		}
		default:
			break;
	}

	// 递归到最右节点
	genExpr(Nod->RHS);
	// 将a0结果压栈
	push();
	// 递归到左节点
	genExpr(Nod->LHS);
	// 将栈顶结果返回给a1寄存器
	pop("a1");

	// 生成各个二叉树节点
	// 判断是否是long类型或指针类型，这两种类型用64位计算，其他类型32位计算便可以
	char *Suffix = Nod->LHS->Ty->Kind == TY_LONG || Nod->LHS->Ty->Base ? "" : "w";
	switch(Nod->Kind) {
		case ND_ADD: 
			printLine("  # a0 + a1，结果写入a0");
      printLine("  add%s a0, a0, a1", Suffix);
			return;
		case ND_SUB: 
			printLine("  # a0 - a1，结果写入a0");
      printLine("  sub%s a0, a0, a1", Suffix);
			return;
		case ND_MUL: 
			printLine("  # a0 * a1，结果写入a0");
      printLine("  mul%s a0, a0, a1", Suffix);
			return;
		case ND_DIV: 
			printLine("  # a0 / a1，结果写入a0");
      printLine("  div%s a0, a0, a1", Suffix);
			return;
		// RICSV 没有相等性指令
		// 相等性需要两条指令来判断
		case ND_EQ: 
			// a0 = a0 ^ a1 异或指令
			// 如果相等则=0，如果不相等则不为0
			printLine("  # 判断是否a0 %s a1", Nod->Kind == ND_EQ ? "=" : "≠");
      printLine("  xor a0, a0, a1");
			// 汇编指令介绍
			// SLTI：如果rs小于立即数(都是有符号整数),将rd置1,否则置0
			// SLTIU：和SLTI一致，不过都是无符号数
			// SLT/SLTU: 如果rs1<rs2，rd写1; 否则rd为0
			// 伪指令SEQZ："SEQZ rd, rs" 实际上是 "SLTIU rd, rs1, 1"
			// 
			// 
			// sltiu a0, a0, 1 异或后跟1比较
			// 相等时为0，与1比较为小于；不等时一定不小于1
			// 异或结果等于0则置1
      printLine("  seqz a0, a0");
			return;
		case ND_NEQ: 
			// a0 = a0 ^ a1 异或指令
			printLine("  # 判断是否a0 %s a1", Nod->Kind == ND_EQ ? "=" : "≠");
      printLine("  xor a0, a0, a1");
			// 异或结果不等于0则置1
      printLine("  snez a0, a0");
			return;
		case ND_LT:
			printLine(" # 判断a0 < a1");
      printLine("  slt a0, a0, a1");
			return;
		case ND_LTEQ:
			// X <= Y 等价于 Y < X 再取反
			printLine(" # 判断a0 ≤ a1");
      printLine("  slt a0, a1, a0");
      printLine("  xori a0, a0, 1");
			return;
		default:
			break;
	}

	errorTok(Nod->Tok, "invalid expression");
}

static void genStmt(Node *Nod) {
	// .loc 文件编号 行号
	printLine("  .loc 1 %d", Nod->Tok->LineNo);

	switch(Nod->Kind) {
		case ND_FOR: {
			// 使用{}包裹，这样C的生命周期仅存在于这个case内
			// 代码段计数
			int C = count();
			printLine("\n# =====循环语句%d===============", C);
			// 生成初始化语句
			// 此处加判断是为了兼while循环
			if (Nod->Init) {
				printLine("\n# Init语句%d", C);
				genStmt(Nod->Init);
			}
			// 生成循环头部标签
			printLine("\n# 循环%d的.L.begin.%d段标签", C, C);
			printLine(".L.begin.%d:", C);
			// 生成循环条件语句
			printLine("# Cond表达式%d", C);
			if (Nod->Cond) {
				// 生成条件循环语句
				genExpr(Nod->Cond);
				// 判断结果是否为0，为0则跳转到结束部分
				printLine("  # 若a0为0，则跳转到循环%d的.L.end.%d段", C, C);
				printLine("  beqz a0, .L.end.%d", C);
			}
			// 生成循环体的语句
			printLine("\n# Then语句%d", C);
			genStmt(Nod->Then);
			// 处理循环递增语句
			if (Nod->Inc) {
				// 生成循环递增语句
				printLine("\n# Inc语句%d", C);
				genExpr(Nod->Inc);
			}
			// 跳转到循环头部
			printLine("  # 跳转到循环%d的.L.begin.%d段", C, C);
			printLine("  j .L.begin.%d", C);
			// 输出循环尾部标签
			printLine("\n# 循环%d的.L.end.%d段标签", C, C);
			printLine(".L.end.%d:", C);
			
			return;
		}
		case ND_IF: {
			// 使用{}包裹，这样C的生命周期仅存在于这个case内
			// 代码段计数
			int C = count();
			printLine("\n# =====分支语句%d==============", C);
			// 生成条件内语句
			printLine("\n# Cond表达式%d", C);
			genExpr(Nod->Cond);
			// 判断结果是否为0，为0则跳转到else标签
			// 对标签标号，防止重复，不会跳转到错误代码段
			printLine(" # 若a0为0，则跳转到分支%d的.L.else.%d段", C, C);
			printLine("  beqz a0, .L.else.%d", C);
			// 生成符合条件后的语句
			printLine("\n# Then语句%d", C);
			genStmt(Nod->Then);
			// 执行完后跳转到if语句后面的语句
			printLine(" # 跳转到分支%d的.L.end.%d段", C, C);
			printLine("  j .L.end.%d", C);
			// else 代码块，可能为空，故输出标签
			printLine("\n# Else语句%d", C);
			printLine("# 分支%d的.L.else.%d段标签", C, C);
			printLine(".L.else.%d:", C);
			// 生成else代码块
			if (Nod->Else) {
				genStmt(Nod->Else);
			}
			// 结束if语句，继续执行后面的语句
			printLine("\n# 分支%d的.L.end.%d段标签", C, C);
			printLine(".L.end.%d:", C);
			return;
		}
		case ND_BLOCK:
			// 遍历语法树生成汇编
			for (Node *N = Nod->Body; N;  N = N->Next) {
				genStmt(N);
			}
			return;
		case ND_RETURN:
			printLine("# 返回语句");
			genExpr(Nod->LHS);
			// 无条件跳转语句，跳转到.L.return段
			// j offset是 jal x0, offset的别名指令
			printLine("  # 跳转到.L.return.%s段", CurrentFn->Name);
			printLine("	j .L.return.%s", CurrentFn->Name);
			return;
		case ND_EXPR_STMT:
			genExpr(Nod->LHS);
			return;
		default:
			break;
	}

  errorTok(Nod->Tok, "invalid statement");
}

// 生成全局变量的汇编代码
static void emitData(Obj *Prog) {
	for (Obj *Var = Prog; Var; Var = Var->Next) {
		if (Var->IsFunction) {
			continue;
		}

		printLine("  # 数据段标签");
    printLine("  .data");
    printLine("%s:", Var->Name);

		if (Var->InitData) {
			printLine("  # 字符串字面量");
			for (int I = 0; I < Var->Ty->Size; ++I) {
				char C = Var->InitData[I];
				if (isprint(C)) {
					printLine("  .byte %d\t# %c", C, C);
				} else {
					printLine("  .byte %d", C);
				}
			}
		} else {
				printLine("  # 全局段%s", Var->Name);
				printLine("  .globl %s", Var->Name);
				printLine("%s:", Var->Name);
				printLine("  # 全局变量零填充%d位", Var->Ty->Size);
				printLine("  .zero %d", Var->Ty->Size);
		}
	}
}

// 将整形寄存器的值存入栈中
static void storeGeneral(int Reg, int Offset, int Size) {
	printLine("  # 将%s寄存器的值存入%d(fp)的栈地址", ArgReg[Reg], Offset);

	switch (Size) {
	case 1:
		printLine("  sb %s, %d(fp)", ArgReg[Reg++], Offset);
		return;
	case 2:
		printLine("  sh %s, %d(fp)", ArgReg[Reg++], Offset);
		return;
	case 4:
		printLine("  sw %s, %d(fp)", ArgReg[Reg++], Offset);
		return;
	case 8:
		printLine("  sd %s, %d(fp)", ArgReg[Reg++], Offset);
		return;
	}	

	unreachable();
}

static void emitText(Obj *Prog) {

	for (Obj *Fn = Prog; Fn; Fn = Fn->Next) {
		if (!Fn->IsFunction || !Fn->IsDefinition) {
			continue;
		}

		if (Fn->IsStatic) {
			// 定义局部%s函数段
			printLine("  # 定义局部%s段", Fn->Name);
			printLine("  .local %s", Fn->Name);
		} else {
			// 为每一个方法声明一个全局方法段
			printLine("  # 定义全局%s段", Fn->Name);
			printLine("  .globl %s", Fn->Name);
		}

		printLine("  # 代码段标签");
		printLine("  .text");
		// 每个方法段标签
		printLine("\n# =====%s段开始===============", Fn->Name);
		printLine("# %s段标签", Fn->Name);
		printLine("%s:", Fn->Name);
		CurrentFn = Fn;

		// 栈布局
		//-------------------------------// sp
		//              ra                  
		//-------------------------------// ra = sp-8
		//              fp                  
		//-------------------------------// fp = sp-8
		//						 变量
		//-------------------------------// sp=sp-8-StackSize
		//           表达式计算
		//-------------------------------//
		//
		// Prologue 前言
		// 将ra寄存器压栈，保存ra的值，ra是保存方法的当前地址的寄存器，当调用其他方法结束时可以返回原来的地址
		printLine("  # 将ra寄存器压栈，保存ra的值");
		printLine("  addi sp, sp, -16");
		printLine("  sd ra, 8(sp)");
		// 将fp压入栈，保存fp的值
		printLine("  # 将fp压栈，fp属于“被调用者保存”的寄存器，需要恢复原值");
		printLine("  addi sp, sp, -8");
		printLine("  sd fp, 0(sp)");
		// 将sp写入fp
		printLine("  # 将sp的值写入fp");
		printLine("  mv fp, sp");

		// 偏移量为实际变量所用的栈大小
		printLine("  # sp腾出StackSize大小的栈空间");
		printLine("  addi sp, sp, -%d", Fn->StackSize);

		// 由于方法参数与a0,a1...寄存器自动对应
		// 所以我们要将形参跟寄存器对应起来
		int I = 0;
		for (Obj *Var = Fn->Params; Var; Var = Var->Next) {
			storeGeneral(I++, Var->Offset, Var->Ty->Size);
		}

		printLine("\n# =====%s段主体===============", Fn->Name);
		genStmt(Fn->Body);
		assert(Depth == 0);

		// Epilogue 后语
		// 输出return段标签
		printLine("\n# =====%s段程序结束===============", Fn->Name);
		printLine("# return段标签");
		printLine("	.L.return.%s:", Fn->Name);
		// 将fp的值改写回sp
		printLine("  # 将fp的值写回sp");
		printLine("  mv sp, fp");
		// 将最早fp保存的值弹栈，恢复fp
		printLine("  # 将最早fp保存的值弹栈，恢复fp和sp");
		printLine("  ld fp, 0(sp)");
		printLine("  addi sp, sp, 8");
		// 将ra保存的值弹栈，恢复ra的值
		printLine("  # 将ra保存的值弹栈，恢复ra的值");
		printLine("  ld ra, 8(sp)");
		printLine("  addi sp, sp, 16");

		// ret为jalr x0, x1, 0别名指令，用于返回子程序
		// 返回的为a0的值
		printLine("  # 返回a0值给系统调用");
		printLine("  ret");
	}
}

void codegen(Obj *Prog, FILE *Out) {
	OutputFile = Out;

	// 计算局部变量的偏移量
	assignLVarOffsets(Prog);
	// 生成数据
	emitData(Prog);
	// 生成代码
	emitText(Prog);

}
