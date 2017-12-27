all: vtable call-byte

.PHONY: run
run: all
	./vtable

vtable: vtable.c
	c99 $< -o $@

call-byte: call-byte.c
	c99 $< -o $@
