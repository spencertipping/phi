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

### Lying convincingly
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

### The performance problem
Any language with an optimizer demands some knowledge of how that optimizer
works. The more complex the optimizer, the more likely you are to inadvertently
observe it; for example, [Haskell's stream
fusion](https://stackoverflow.com/questions/578063/what-is-haskells-stream-fusion)
isn't universally applicable; if you wrote your own list function, it would
probably break the optimization and start allocating intermediate cons cells. In
this case, the language has unconvincingly claimed that it can optimize through
cons-allocating functions. The dishonesty comes with an asterisk.

phi's goal is jointly to be fast, and to minimize the number of asterisks.

### Why we care about enumerability
Let's suppose we're doing something simple like this:

```
[cons <unknown> [uncons 1 +] [uncons 77 *] if]
```

The unknown exists within a state space that is presumably impractical to
enumerate, but `if` reduces it to a single two-way decision. The program above
is strictly easier to optimize than one in which we call into an unknown
quantity:

```
[cons [<unknown>] .]
```

**TODO:** this section sucks
