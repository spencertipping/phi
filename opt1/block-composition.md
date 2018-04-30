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
exceptions. Enumerable values are useful because we can speculate to turn them
into constants -- and that often means we can constant-fold much more
efficiently. Here's an example:

```
[<stuff>  [f]   [g]  if]                # program A
[<stuff> [[f]] [[g]] if .]              # program B
```

These two programs are semantically identical; each will end up applying either
`f` or `g`. Here's the difference between polymorphic and megamorphic modeling:

```
[<stuff> <cond>]                        # megamorphic unknown
[<stuff> <cond> [f] [g] if]
  == [<stuff> <cond> not not [f] [g] if]# reduces megamorphic to polymorphic
  == either(<cond>,                     # this node is polymorphic
       [<stuff> f],                     # speculative branch
       [<stuff> g])                     # speculative branch
```

Notice that we've moved the unknown-frontier rightwards; that is, we've inlined
each branch of the conditional. Here's an example where that matters:

```
[<stuff> cons <cond> [uncons] [] if]
  == either(<cond>,
       [<stuff> cons uncons],           # this can be simplified
       [<stuff> cons])
  == either(<cond>,
       [<stuff>],                       # ...into this
       [<stuff> cons])
```

This isn't quite free, of course, because we duplicate code each time we lift
things into an `either`. But it gives us some real advantages, particularly in
object-oriented code. For instance:

```
[<some-object> [.foo() .bar()] .]
```

If `some-object` is megamorphic, we have no options; we have to emulate the
object lookup using the resolver. But if `some-object` is a polymorphic value,
we can constant-fold each possibility into the method call loop -- and
crucially, _we can inline both method calls_. This means we get cross-method
optimization: `.foo()` can end with `cons` and `.bar()` can start with `uncons`
and that allocation will be elided.
