# Block composition
A program is a graph of basic blocks. Most languages don't provide runtime code
generation or a fast `eval` function, so the total set of basic blocks tends to
be known or at least bounded for most programs. (And even when it isn't, e.g.
for Javascript/V8 or LuaJIT, the backend isn't compressing composed objects very
much.)

phi complicates things by equating `eval` and function calls, which minimizes
the overhead of JIT within the language at the cost of my personal happiness and
spiritual fulfillment while I contemplate what the hell I signed myself up for.
In practice, though, it's less bad than it sounds.

## A bit of background
Let's get a couple of things out of the way:

1. Type-driven polymorphism is _identical_ to condition-driven polymorphism, in
   part because phi's core operators are all monomorphic.
2. JIT is as slow as it is indeterminate: since interpreters are just data
   structures, low-entropy unknowns should correspond to low-entropy outputs.

I personally care about (1) because it is indefensibly idiotic for OOP
polymorphism and conditional structures to have different performance
characteristics. It's also an important concept on its own: in phi, types are
entirely library-driven. The optimizer/interpreter/etc don't really understand
anything about types other than the primitive set.

(2) is the Futamura Projection Everything-Is-Amazing Guarantee (TM).
Essentially, we're saying that we should be able apply an interpreter to a
constant and get a compiled program for it; put differently, since we're willing
to trace for basic blocks and flatten conditionals in the process, our notion of
basic blocks commutes across `eval`.

## Polymorphic vs megamorphic
...where polymorphic means "enumerable" and megamorphic means "not even gonna
try." In abstract-interpreter terms, polymorphic things are `union` values and
megamorphic things are grounded out in `unknown`s -- although there are some
exceptions. Before I get to those, let's talk about why we even care.

**TODO**
