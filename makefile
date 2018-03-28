all: phi phi-debug

phi: phi.c makefile
	c99 phi.c -Wall -g -O3 -lm -lgc -o $@

phi-debug: phi.c makefile
	c99 phi.c -DDEBUG -Wall -O0 -lm -g -o $@
