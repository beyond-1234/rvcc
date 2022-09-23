#include "rvcc.h"
#include <stdio.h>

// 记录栈的深度
static int Depth;

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
	printf("	addi sp, sp, -8\n");
	printf("	sd a0, 0(sp)\n");
	Depth++;
}

// 将栈顶，sp指向的地址的值弹出到a1
static void pop(char *Reg) {
	printf("	ld %s, 0(sp)\n", Reg);
	printf("	addi sp, sp, 8\n");
	Depth--;
}

// 对其到Align的整数倍
// 用于栈的对齐
static int alignTo(int N, int Align) {
	// (0,Align] 返回Align
	return (N + Align - 1) / Align * Align;
}

// 根据变量的链表计算出偏移量
static void assignLVarOffsets(Function *Prog) {
	int Offset = 0;
	for (Obj *Var = Prog->Locals; Var; Var = Var->Next) {
		// 每个变量分配8字节
		Offset += 8;
		// 为每个变量赋一个编译量，或者说是栈中地址
		Var->Offset = -Offset;
	}

	// 将栈对其到16字节
	Prog->StackSize = alignTo(Offset, 16);
}

// 计算给定节点的绝对地址
// 如果报错，说明节点不在内存里
static void genAddr(Node *Nod) {
	if(Nod->Kind == ND_VAR) {
		// 编译量是相对于fp的
		// a0在这里存的是地址而不是数值
		printf("  addi a0, fp, %d\n", Nod->Var->Offset);
		return;
	}

	error("not an lvalue");
}

static void genExpr(Node *Nod) {
  // 这里我们将算式分解为 num (op num) (op num)...的形式
  // 所以先将第一个num传入a0
	switch(Nod->Kind) {
		// li为addi别名指令，加载一个立即数到寄存器中
		// 加载数字到a0
		case ND_NUM: 
			printf("  li a0, %d\n", Nod->Val);
			return;
		// 识别到负号对寄存器取反
		case ND_NEG: 
			// 我们生成单叉树时只使用左子树
			genExpr(Nod->LHS);
			// neg a0, a0 是 sub a0, x0 的别名
			// 即 a0 = 0 - a0
			printf("  neg a0, a0\n");
			return;
		// 变量
		case ND_VAR:
			// 计算出变量的地址，然后存入a0
			genAddr(Nod);
			// 访问a0地址中存储的数据，存入a0中
			// 当前变量写死为8字节，所以用ld
			printf("  ld a0, 0(a0)\n");
			return;
		case ND_ASSIGN:
			// 左部是左值，保存值到的地址
			genAddr(Nod->LHS);
			push();
			// 右部是右值，为表达式的值
			genExpr(Nod->RHS);
			pop("a1");
			// 当前变量写死为8位，所以用sd
			printf("  sd a0, 0(a1)\n");
			return;
		default:
			break;
	}

	// 递归到最右节点
	genExpr(Nod->RHS);
	// 将结果压栈
	push();
	// 递归到左节点
	genExpr(Nod->LHS);
	// 将栈顶结果返回给a1寄存器
	pop("a1");

	// 生成各个二叉树节点
	switch(Nod->Kind) {
		case ND_ADD: 
      printf("  add a0, a0, a1\n");
			return;
		case ND_SUB: 
      printf("  sub a0, a0, a1\n");
			return;
		case ND_MUL: 
      printf("  mul a0, a0, a1\n");
			return;
		case ND_DIV: 
      printf("  div a0, a0, a1\n");
			return;
		// RICSV 没有相等性指令
		// 相等性需要两条指令来判断
		case ND_EQ: 
			// a0 = a0 ^ a1 异或指令
			// 如果相等则=0，如果不相等则不为0
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
      printf("  xor a0, a0, a1\n");
			// 异或结果不等于0则置1
      printf("  snez a0, a0\n");
			return;
		case ND_LT:
      printf("  slt a0, a0, a1\n");
			return;
		case ND_LTEQ:
			// X <= Y 等价于 Y < X 再取反
      printf("  slt a0, a1, a0\n");
      printf("  xori a0, a0, 1\n");
			return;
		default:
			break;
	}

	error("invalid expression");
}

static void genStmt(Node *Nod) {
	switch(Nod->Kind) {
		case ND_IF: {
			// 使用{}包裹，这样C的生命周期仅存在于这个case内
			// 代码段计数
			int C = count();
			// 生成条件内语句
			genExpr(Nod->Cond);
			// 判断结果是否为0，为0则跳转到else标签
			// 对标签标号，防止重复，不会跳转到错误代码段
			printf("  beqz a0, .L.else.%d\n", C);
			// 生成符合条件后的语句
			genStmt(Nod->Then);
			// 执行完后跳转到if语句后面的语句
			printf("  j .L.end.%d\n", C);
			// else 代码块，可能为空，故输出标签
			printf(".L.else.%d:\n", C);
			// 生成else代码块
			if (Nod->Else) {
				genStmt(Nod->Else);
			}
			// 结束if语句，继续执行后面的语句
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
			genExpr(Nod->LHS);
			printf("	j .L.return\n");
			return;
		case ND_EXPR_STMT:
			genExpr(Nod->LHS);
			return;
		default:
			break;
	}

  error("invalid statement");
}


void codegen(Function *Prog) {
	assignLVarOffsets(Prog);

  // 声明一个全局main段，同时也是程序入口段
  printf("  .globl main\n");
  // main段标签
  printf("main:\n");

  // 栈布局
  //-------------------------------// sp
  //              fp                  fp = sp-8
  //-------------------------------// fp
	//						 变量
  //-------------------------------// sp=sp-8-StackSize
  //           表达式计算
  //-------------------------------//
	//
	// Prologue 前言
	// 将fp压入栈，保存fp的值
	printf("  addi sp, sp, -8\n");
	printf("  sd fp, 0(sp)\n");
	// 将sp写入fp
	printf("  mv fp, sp\n");

	// 偏移量为实际变量所用的栈大小
	printf("  addi sp, sp, -%d\n", Prog->StackSize);

	genStmt(Prog->Body);
	assert(Depth == 0);

	// Epilogue 后语
	// 输出return段标签
	printf("	.L.return:\n");
	// 将fp的值改写回sp
	printf("  mv sp, fp\n");
	// 将最早fp保存的值弹栈，恢复fp
	printf("  ld fp, 0(sp)\n");
	printf("  addi sp, sp, 8\n");
	

  // ret为jalr x0, x1, 0别名指令，用于返回子程序
  // 返回的为a0的值
  printf("  ret\n");
}
