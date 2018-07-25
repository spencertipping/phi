# Stuff to fix
Some background: [OOP delta](oop-delta.md), a redesign of the method calling
convention.

Here's what I'm planning to change:

1. Convert method calls to self-standing symbol/LUT
2. Continuations on frame stack, not data stack?
3. Variable-width nop bytecodes (1-16 bytes?)

I've thought about some arithmetic changes like adding carry/overflow and
unsigned mul/div, but that goes against the strategy of keeping the bytecode set
minimal. The strategically correct approach is to specialize in machine code and
use low-level JIT.

## Self-standing methods
This comes down to hashing stuff and being optimistic about no overlaps. We can
get more elaborate down the line, but for now I think it's fine to hash into 64
bit ints and drop the strings. Collisions will be rare enough for bootstrapping
if we have a halfway competent hashing algorithm.

If we persist this into phi2, we'll need to tag these hashed values so we can
store the original strings in a proper symbol table -- and that means a new
"reference" type for bytecode constants. That too should be fine. We can't go
with memory addresses-as-hashes because it doesn't give us a way to canonicalize
the string -> symbol constructor, or at least not without some extra machinery.

Method calls really need to be function calls to a code-hereptr on the class.
This allows classes to own their optimization.
