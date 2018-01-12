# Compilation notes
## Threading and list literals
Every concatenative program can be reduced to a mixture of list literals and
integers, possibly with circular references created with `mut`-related opcodes.
Ordinarily the list-literal aspect of it would complicate stuff, but we can just
allocate backend opcodes that push specific list constants; then we're compiling
to ordinary threaded code.
