# `phi`: my final programming language
If this works, I doubt I'll need anything else. That's kind of a big "if".

## If you're reading this, here's where to start
- [phi0.pm](phi0.pm): low-level bytecodes and interpreter structure, internally
  similar to Jonesforth
- [phi1/asm.pm](phi1/asm.pm): bytecode assembler
- [phi1/fn.pm](phi1/fn.pm): thin wrapper over asm to provide functions
- [phi1/oop.pm](phi1/oop.pm): receiver-polymorphic calling convention
- [phi1/class.pm](phi1/class.pm): classes and metaclasses (incomplete)
- [phi1/frame.pm](phi1/frame.pm): stack-allocated frame classes for function
  locals
- [phi1/sexp.pm](phi1/sexp.pm): Scheme-flavored S-expression compiler (no `eval`
  or macros yet; those will be in phi2)

**Work in progress:** I expect to ship this by end of year 2018.
