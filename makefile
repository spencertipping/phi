all: phi phi-debug

phi: phi.c makefile
	c99 phi.c -DCONS_STACKS -DPROF -Wall -Wno-unused-value -g -O1 -lm -lgc -o $@

phi-debug: phi.c makefile
	c99 phi.c -DCONS_STACKS -DDEBUG -Wall -Wno-unused-value -O0 -lm -g -o $@
