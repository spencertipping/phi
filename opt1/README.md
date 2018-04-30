# First-pass optimization
phi uses a tracing compiler with representational abstraction as a self-hosted
optimization layer before we emit backend code. Before I get into the details,
let's talk about some design constraints.

## Lying convincingly
Unlike non-politicians, compilers are allowed and encouraged to be dishonest.
Most languages are predicated on the maintenance of some set of fictions:

- Smalltalk and Ruby: everything is an object, including classes
- Haskell: you really always wanted to represent mutable operations as values
  but didn't realize it, laziness is a good idea, and knowing about monads makes
  you a superior human being
- Perl: you have no linguistic standards, and a sufficiently expressive regular
  expression is marriage material
- C: pointers, stacks, values, and functions are things
- Java: you can't be trusted to use a real language, and you're too much of a
  philistine to be bothered by GC overhead, UTF-16 overhead, or multi-second
  startup time
- Forth: stacks are a thing and POSIX is for wimps
- Machine code: all memory addresses are created equal, registers are a thing,
  and you're drunk enough to read the Intel manuals

These fictions are as real as we believe they are -- and if they simplify our
world without being demonstrably false, we're usually willing to accept them as
canon. The language economy doesn't demand or value honesty; it just demands
believability.

## The performance problem
Any language with an optimizer demands some knowledge of how that optimizer
works. The more complex the optimizer, the more likely you are to inadvertently
observe it; for example, [Haskell's stream
fusion](https://stackoverflow.com/questions/578063/what-is-haskells-stream-fusion)
isn't universally applicable; if you wrote your own list function, it would
probably break the optimization and start allocating intermediate cons cells. In
this case, the language has unconvincingly claimed that it can optimize through
cons-allocating functions. The dishonesty comes with an asterisk.

phi's goal is jointly to be fast and to minimize the number of asterisks.

## How it works
- [Basic blocks](basic-blocks.md)
- [Block composition](block-composition.md)
