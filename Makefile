CFLAGS=-std=c11 -g -fno-common
CC=gcc
# c源码文件
SRCS=$(wildcard *.c)
# c源码文件生成的未链接的可重定向文件，将所有的.c文件替换为.o文件
OBJS=$(SRCS:.c=.o)

# rvcc 标签表示如何构建最终的二进制文件，依赖所有的.o文件
# $@表示目标文件，此处为rvcc；$^表示依赖文件，此处为$(OBJS)
rvcc: $(OBJS)
	$(CC) $(CFLAGS) -o $@  $^

# 所有的可重定位文件依赖于rvcc.h的头文件
$(OBJS): rvcc.h

test: rvcc
	./test.sh
	./test-driver.sh
	# this is to test read code from file
	#./test.sh tmp.c

clean:
	rm -f rvcc *.o *.s tmp* a.out

# 伪目标，没有实际的依赖文件
.PHONY: test clean
