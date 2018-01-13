## Static friction force
If I write something like `myfn = fn`, aliasing `fn` for some reason, then the
static friction on `myfn` should be proportional to the number of uses of that
word elsewhere in my code. In other words, I should be able to understand ahead
of time what depends on my definition when I hover over it. (And possibly have a
view where defined words are colored by friction.)

## Threading and list literals
Every concatenative program can be reduced to a mixture of list literals and
integers, possibly with circular references created with `mut`-related opcodes.
Ordinarily the list-literal aspect of it would complicate stuff, but we can just
allocate backend opcodes that push specific list constants; then we're compiling
to ordinary threaded code.
