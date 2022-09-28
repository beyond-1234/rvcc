#include "rvcc.h"

// {TY_INT} 构造了一个数据结构，(Type) 强制转换为struct
// 全局变量TyInt，用来将Type赋值为int类型
Type *TyInt = &(Type){TY_INT};

// 判断Type是否为int类型
bool isInteger(Type *Ty) { return Ty->Kind == TY_INT; };

// 生成指针指向的类型
Type *pointerTo(Type *Base) {
	Type *Ty = calloc(1, sizeof(Type));
	Ty->Kind = TY_PTR;
	Ty->Base = Base;
	return Ty;
}

// 为节点添加类型
void addType(Node *Nod) {
	// 判断节点是否为空 或者 节点类型已经有值，那么就直接返回
	if (!Nod || Nod->Ty) {
		return;
	}

	// 递归访问所有节点以增加类型
	addType(Nod->LHS);
	addType(Nod->RHS);
	addType(Nod->Cond);
	addType(Nod->Then);
	addType(Nod->Else);
	addType(Nod->Init);
	addType(Nod->Inc);

	// 访问链表内的所有节点以增加类型
	for (Node *N = Nod->Body; N; N = N->Next) {
		addType(N);
	}

	switch (Nod->Kind) {
		// 将节点类型设定为节点左部的类型
		case ND_ADD:
		case ND_SUB:
		case ND_MUL:
		case ND_DIV:
		case ND_NEG:
		case ND_ASSIGN:
			Nod->Ty = Nod->LHS->Ty;
			return;
		case ND_EQ:
		case ND_NEQ:
		case ND_LT:
		case ND_LTEQ:
		case ND_VAR:
		case ND_NUM:
			// 暂将类型设为int
			Nod->Ty = TyInt;
			return;
		case ND_ADDR:
			// 将节点类型设为指针，并指向左部的类型
			Nod->Ty = pointerTo(Nod->LHS->Ty);
			return;
		case ND_DEREF:
			// 节点类型：如果解引用指向的是指针，则为指针指向的类型；否则为int
			if (Nod->LHS->Ty->Kind == TY_PTR) {
				Nod->Ty = Nod->LHS->Ty->Base;
			} else {
				Nod->Ty = TyInt;
			}
			return;
		default:
			break;
	}
}
