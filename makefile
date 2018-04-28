all: phi phi-debug phiml

phi: phi.c makefile
	c99 phi.c -DCONS_STACKS -DPROF -Wall -Wno-unused-value -g -O3 -lm -lgc -o $@

phi-debug: phi.c makefile
	c99 phi.c -DCONS_STACKS -DDEBUG -Wall -Wno-unused-value -O0 -lm -g -lgc -o $@

phiml: phi.ml makefile
	ocamlopt -g -o phiml phi.ml
