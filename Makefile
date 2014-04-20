defaulttarget: ubcompiler

.PHONY: clean defaulttarget test doc

CC=gcc
CXX=g++
CPPFLAGS=-Illvminc
CFLAGS=-Wall -g -O0 -DYYERROR_VERBOSE
LDFLAGS=
LIBS=$(shell llvm-config --libs) -ldl -lpthread -lncurses
LEXFLAGS=
YACCFLAGS=-v

-include ../config.mk

scanner.c: scanner.l
parser.c parser.h: parser.y

%.c: %.l
	flex $(LEXFLAGS) -o $@ $^

%.c %.h: %.y
	bison $(YACCFLAGS) -o $(basename $@).c --defines=$(basename $@).h $^

%.o: %.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -c -o $@ $^

%.o: %.cpp
	$(CXX) $(CPPFLAGS) $(CFLAGS) -c -o $@ $^

ubcompiler: codegen-llvm.o parser.o scanner.o main.o ast.o ast-codeprinter.o
	$(CXX) $(LDFLAGS) -o $@ $^ $(LIBS)

doc:
	doxygen

clean:
	rm -f parser.c parser.h parser.output scanner.c
	rm -f codegen-llvm.o parser.o scanner.o main.o ast.o ast-codeprinter.o
	rm -f ubcompiler
	rm -Rf gendoc
