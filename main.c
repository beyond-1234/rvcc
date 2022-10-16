#include "rvcc.h"
#include <stdio.h>

// 输出文件路径
static char *OptO;

// 输入文件路径
static char *InputPath;

static void usage(int Status) {
	fprintf(stderr, "rvcc [ -o <path> ] <file>\n");
	exit(Status);
}

static void parseArgs(int Argc, char **Argv) {
	for (int I = 1; I < Argc; I++) {

		// --help
		if (!strcmp(Argv[I], "--help")) {
			usage(0);
		}

		// -o XXXXXX
		if (!strcmp(Argv[I], "-o")) {
			if (!Argv[++I]) {
				usage(1);
			}
			OptO = Argv[I];
			continue;	
		}

		// -oXXXXXX
		if (!strncmp(Argv[I], "-o", 2)) {
			OptO = Argv[I] + 2;
			continue;	
		}

		// 解析为-的参数
		if (Argv[I][0] == '-' && Argv[I][1] != '\0') {
			error("unknown argument: %s", Argv[I]);
		}

		// 其他情况则匹配为输入文件
		InputPath = Argv[I];
	}

	if (!InputPath) {
		error("no input files");
	}
}

// 以写入方式打开文件流
static FILE *openFile(char *Path) {
	if (!Path || strcmp(Path, "-") == 0) {
		return stdout;
	}

	FILE *Out = fopen(Path, "w");
	if (!Out) {
		error("cannot open output file: %s: %s", Path, strerror(errno));
	}

	return Out;
}

int main(int Argc, char **Argv) {

	parseArgs(Argc, Argv);

	// 解析输入的算式，生成终结符流
  Token *Tok = tokenizeFile(InputPath);

	// 解析终结符流
	Obj *Prog = parse(Tok);

	FILE *Out = openFile(OptO);
	// 生成汇编代码
	codegen(Prog, Out);

	return 0;
}
