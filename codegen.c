#include "rvcc.h"
#include <stdio.h>

static void genExpr(Node *Nod);

// 记录栈的深度
static int Depth;
// 用于函数参数的寄存器
// RICSV中函数的前6个寄存器就是用这几个寄存器来存
static char *ArgReg[] = {"a0", "a1", "a2", "a3", "a4", "a5"};
// 当前的函数
static Obj *CurrentFn;

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
	printf("  # 压栈，将a0的值存入栈顶\n");
	printf("	addi sp, sp, -8\n");
	printf("	sd a0, 0(sp)\n");
	Depth++;
}

// 将栈顶，sp指向的地址的值弹出到a1
static void pop(char *Reg) {
	printf(" # 弹栈，将栈顶的值存入%s\n", Reg);
	printf("	ld %s, 0(sp)\n", Reg);
	printf("	addi sp, sp, 8\n");
	Depth--;
}

// 加载a0指向的值
static void load(Type *Ty) {
	if (Ty->Kind == TY_ARRAY) {
		return;
	}

	printf("  # 读取a0中存放的地址，得到的值存入a0\n");
	if (Ty->Size == 1) {
		// b = byte = 1字节
		printf("  lb a0, 0(a0)");
	} else {
		// ld 的d=double word=8字节
		printf("  ld a0, 0(a0)");
	}
}

// 将栈顶值(为一个地址)存入a0
static void store(Type *Ty) {
	pop("a1");
	printf("  # 读取a0的值，写入到a1存放的地址\n");
	if (Ty->Size == 1) {
		printf("  sb a0, 0(a1)");
	} else {
		printf("  sd a0, 0(a1)");
	}
}

// 对其到Align的整数倍
// 用于栈的对齐
static int alignTo(int N, int Align) {
	// (0,Align] 返回Align
	return (N + Align - 1) / Align * Align;
}

// 根据变量的链表计算出偏移量
static void assignLVarOffsets(Obj *Prog) {
	
	// 为每个函数计算其变量所用的栈空间
	for (Obj *Fn = Prog; Fn; Fn = Fn->Next) {
		if (!Fn->isFunction) {
			continue;
		}
		int Offset = 0;
		for (Obj *Var = Fn->Locals; Var; Var = Var->Next) {
			// 每个变量分配8字节
			Offset += Var->Ty->Size;
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
				printf("  # 获取局部变量%s的栈内地址为%d(fp)\n", Nod->Var->Name,
					 Nod->Var->Offset);
				printf("  addi a0, fp, %d\n", Nod->Var->Offset);
			} else {
				printf("  # 获取全局变量%s的\n", Nod->Var->Name);
				// 获取全局变量的地址	
				// 高地址(高20位，31~20位)
				printf("  lui a0, %%hi(%s)\n", Nod->Var->Name);
				// 低地址(低12位，19~0位)
				printf("  addi a0, a0, %%lo(%s)\n", Nod->Var->Name);
			}
			return;
		// 解引用
		case ND_DEREF:
			genExpr(Nod->LHS);
			return;
	}

	errorTok(Nod->Tok, "not an lvalue");
}

// 对AST二叉树进行中序遍历处理
static void genExpr(Node *Nod) {
  // 这里我们将算式分解为 num (op num) (op num)...的形式
  // 所以先将第一个num传入a0
	switch(Nod->Kind) {
		// li为addi别名指令，加载一个立即数到寄存器中
		// 加载数字到a0
		case ND_NUM: 
			printf("  # 将%d加载到a0中\n", Nod->Val);
			printf("  li a0, %d\n", Nod->Val);
			return;
		// 识别到负号对寄存器取反
		case ND_NEG: 
			// 我们生成单叉树时只使用左子树
			genExpr(Nod->LHS);
			// neg a0, a0 是 sub a0, x0 的别名
			// 即 a0 = 0 - a0
			printf("  # 对a0的值进行取反\n");
			printf("  neg a0, a0\n");
			return;
		// 变量
		case ND_VAR:
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
			printf("\n  # 调用函数%s\n", Nod->FuncName);
			printf("  call %s\n", Nod->FuncName);
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
	switch(Nod->Kind) {
		case ND_ADD: 
			printf("  # a0 + a1，结果写入a0\n");
      printf("  add a0, a0, a1\n");
			return;
		case ND_SUB: 
			printf("  # a0 - a1，结果写入a0\n");
      printf("  sub a0, a0, a1\n");
			return;
		case ND_MUL: 
			printf("  # a0 * a1，结果写入a0\n");
      printf("  mul a0, a0, a1\n");
			return;
		case ND_DIV: 
			printf("  # a0 / a1，结果写入a0\n");
      printf("  div a0, a0, a1\n");
			return;
		// RICSV 没有相等性指令
		// 相等性需要两条指令来判断
		case ND_EQ: 
			// a0 = a0 ^ a1 异或指令
			// 如果相等则=0，如果不相等则不为0
			printf("  # 判断是否a0 %s a1\n", Nod->Kind == ND_EQ ? "=" : "≠");
      printf("  xor a0, a0, a1\n");
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
      printf("  seqz a0, a0\n");
			return;
		case ND_NEQ: 
			// a0 = a0 ^ a1 异或指令
			printf("  # 判断是否a0 %s a1\n", Nod->Kind == ND_EQ ? "=" : "≠");
      printf("  xor a0, a0, a1\n");
			// 异或结果不等于0则置1
      printf("  snez a0, a0\n");
			return;
		case ND_LT:
			printf(" # 判断a0 < a1\n");
      printf("  slt a0, a0, a1\n");
			return;
		case ND_LTEQ:
			// X <= Y 等价于 Y < X 再取反
			printf(" # 判断a0 ≤ a1\n");
      printf("  slt a0, a1, a0\n");
      printf("  xori a0, a0, 1\n");
			return;
		default:
			break;
	}

	errorTok(Nod->Tok, "invalid expression");
}

static void genStmt(Node *Nod) {
	switch(Nod->Kind) {
		case ND_FOR: {
			// 使用{}包裹，这样C的生命周期仅存在于这个case内
			// 代码段计数
			int C = count();
			printf("\n# =====循环语句%d===============\n", C);
			// 生成初始化语句
			// 此处加判断是为了兼while循环
			if (Nod->Init) {
				printf("\n# Init语句%d\n", C);
				genStmt(Nod->Init);
			}
			// 生成循环头部标签
			printf("\n# 循环%d的.L.begin.%d段标签\n", C, C);
			printf(".L.begin.%d:\n", C);
			// 生成循环条件语句
			printf("# Cond表达式%d\n", C);
			if (Nod->Cond) {
				// 生成条件循环语句
				genExpr(Nod->Cond);
				// 判断结果是否为0，为0则跳转到结束部分
				printf("  # 若a0为0，则跳转到循环%d的.L.end.%d段\n", C, C);
				printf("  beqz a0, .L.end.%d\n", C);
			}
			// 生成循环体的语句
			printf("\n# Then语句%d\n", C);
			genStmt(Nod->Then);
			// 处理循环递增语句
			if (Nod->Inc) {
				// 生成循环递增语句
				printf("\n# Inc语句%d\n", C);
				genExpr(Nod->Inc);
			}
			// 跳转到循环头部
			printf("  # 跳转到循环%d的.L.begin.%d段\n", C, C);
			printf("  j .L.begin.%d\n", C);
			// 输出循环尾部标签
			printf("\n# 循环%d的.L.end.%d段标签\n", C, C);
			printf(".L.end.%d:\n", C);
			
			return;
		}
		case ND_IF: {
			// 使用{}包裹，这样C的生命周期仅存在于这个case内
			// 代码段计数
			int C = count();
			printf("\n# =====分支语句%d==============\n", C);
			// 生成条件内语句
			printf("\n# Cond表达式%d\n", C);
			genExpr(Nod->Cond);
			// 判断结果是否为0，为0则跳转到else标签
			// 对标签标号，防止重复，不会跳转到错误代码段
			printf(" # 若a0为0，则跳转到分支%d的.L.else.%d段\n", C, C);
			printf("  beqz a0, .L.else.%d\n", C);
			// 生成符合条件后的语句
			printf("\n# Then语句%d\n", C);
			genStmt(Nod->Then);
			// 执行完后跳转到if语句后面的语句
			printf(" # 跳转到分支%d的.L.end.%d段\n", C, C);
			printf("  j .L.end.%d\n", C);
			// else 代码块，可能为空，故输出标签
			printf("\n# Else语句%d\n", C);
			printf("# 分支%d的.L.else.%d段标签\n", C, C);
			printf(".L.else.%d:\n", C);
			// 生成else代码块
			if (Nod->Else) {
				genStmt(Nod->Else);
			}
			// 结束if语句，继续执行后面的语句
			printf("\n# 分支%d的.L.end.%d段标签\n", C, C);
			printf(".L.end.%d:", C);
			return;
		}
		case ND_BLOCK:
			// 遍历语法树生成汇编
			for (Node *N = Nod->Body; N;  N = N->Next) {
				genStmt(N);
			}
			return;
		case ND_RETURN:
			printf("# 返回语句\n");
			genExpr(Nod->LHS);
			// 无条件跳转语句，跳转到.L.return段
			// j offset是 jal x0, offset的别名指令
			printf("  # 跳转到.L.return.%s段\n", CurrentFn->Name);
			printf("	j .L.return.%s\n", CurrentFn->Name);
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
		if (Var->isFunction) {
			continue;
		}

		printf("  # 数据段标签\n");
    printf("  .data\n");
    printf("%s:\n", Var->Name);

		if (Var->InitData) {
			printf("  # 字符串字面量\n");
			for (int I = 0; I < Var->Ty->Size; ++I) {
				char C = Var->InitData[I];
				if (isprint(C)) {
					printf("  .byte %d\t# %c\n", C, C);
				} else {
					printf("  .byte %d\n", C);
				}
			}
		} else {
				printf("  # 全局段%s\n", Var->Name);
				printf("  .globl %s\n", Var->Name);
				printf("%s:\n", Var->Name);
				printf("  # 全局变量零填充%d位\n", Var->Ty->Size);
				printf("  .zero %d\n", Var->Ty->Size);
		}
	}
}

static void emitText(Obj *Prog) {

	for (Obj *Fn = Prog; Fn; Fn = Fn->Next) {
		if (!Fn->isFunction) {
			continue;
		}

		// 为每一个方法声明一个全局方法段
		printf("  # 定义全局%s段\n", Fn->Name);
		printf("  .globl %s\n", Fn->Name);

		printf("  # 代码段标签\n");
		printf("  .text\n");
		// 每个方法段标签
		printf("\n# =====%s段开始===============\n", Fn->Name);
		printf("# %s段标签\n", Fn->Name);
		printf("%s:\n", Fn->Name);
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
		printf("  # 将ra寄存器压栈，保存ra的值\n");
		printf("  addi sp, sp, -16\n");
		printf("  sd ra, 8(sp)\n");
		// 将fp压入栈，保存fp的值
		printf("  # 将fp压栈，fp属于“被调用者保存”的寄存器，需要恢复原值\n");
		printf("  addi sp, sp, -8\n");
		printf("  sd fp, 0(sp)\n");
		// 将sp写入fp
		printf("  # 将sp的值写入fp\n");
		printf("  mv fp, sp\n");

		// 偏移量为实际变量所用的栈大小
		printf("  # sp腾出StackSize大小的栈空间\n");
		printf("  addi sp, sp, -%d\n", Fn->StackSize);

		// 由于方法参数与a0,a1...寄存器自动对应
		// 所以我们要将形参跟寄存器对应起来
		int I = 0;
		for (Obj *Var = Fn->Params; Var; Var = Var->Next) {
			printf("  # 将%s寄存器的值存入%s的栈地址\n", ArgReg[1], Var->Name);
			
			if (Var->Ty->Size == 1) {
				printf("  sb %s, %d(fp)\n", ArgReg[I++], Var->Offset);
			} else {
				printf("  sd %s, %d(fp)\n", ArgReg[I++], Var->Offset);
			}
		}

		printf("\n# =====%s段主体===============\n", Fn->Name);
		genStmt(Fn->Body);
		assert(Depth == 0);

		// Epilogue 后语
		// 输出return段标签
		printf("\n# =====%s段程序结束===============\n", Fn->Name);
		printf("# return段标签\n");
		printf("	.L.return.%s:\n", Fn->Name);
		// 将fp的值改写回sp
		printf("  # 将fp的值写回sp\n");
		printf("  mv sp, fp\n");
		// 将最早fp保存的值弹栈，恢复fp
		printf("  # 将最早fp保存的值弹栈，恢复fp和sp\n");
		printf("  ld fp, 0(sp)\n");
		printf("  addi sp, sp, 8\n");
		// 将ra保存的值弹栈，恢复ra的值
		printf("  # 将ra保存的值弹栈，恢复ra的值\n");
		printf("  ld ra, 8(sp)\n");
		printf("  addi sp, sp, 16\n");

		// ret为jalr x0, x1, 0别名指令，用于返回子程序
		// 返回的为a0的值
		printf("  # 返回a0值给系统调用\n");
		printf("  ret\n");
	}
}

void codegen(Obj *Prog) {
	// 计算局部变量的偏移量
	assignLVarOffsets(Prog);
	// 生成数据
	emitData(Prog);
	// 生成代码
	emitText(Prog);

}
