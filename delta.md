# Next steps
**The big one:** convert the fuzz to be a concatenative compiler instead of an
interpreter. This should resolve all of the weirdness currently going on around
strictness and timelines.

1. Build a program that tests all instructions and will verify that a VM is
   implemented correctly
2. Write a formal-ish spec for the [frontend language](phifront.pm)
3. Fix repl tests (right now a bunch fail from the phifront port)
