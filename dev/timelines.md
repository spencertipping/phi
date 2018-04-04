# Timelines
Op graphs are DAGs of expressions, which internally provide some latitude about
instruction ordering. Timelines are partial linearizations of those DAGs.

For example, `(+ (* 1 2) (* 3 4))` has no preference for the order of
multiplication instructions, nor do you. But `(+ (print "hi ") (print "there"))`
is strictly required to print `hi there` in that order. This happens because
`print` is marked as modifying timelines, and `+` (and every binary operator)
has the property that, inasmuch as timelines are involved, the left operand is
always evaluated before the right.

phi mixes lazy and strict evaluation depending on how expressions use timelines.
For example:

```
let x = print "hi " in
let y = x :: y in
let z = print("there") :: y in
z.h
```

The defined behavior of this program is to print `hi there` and return whatever
`print("there")` returned. `let` sets up relative timeline orderings to create
the following equation:

```
timeline(let x = <xdef> in <body>) = timeline(<xdef>) ++ timeline(<body>)
```

This can be achieved using strict evaluation, but it doesn't have to be. The
definition of `y`, for instance, can't be strictly evaluated because it would
never return. `y` would fail if it modified timelines, for instance:

```
let y = print("hi") :: y in ...         # this will fail
```

The reason is that `timeline(y = print("hi") :: y)` is indefinite; we can't
force `y`'s timeline modifications because they continue forever. Put
differently, we can't linearize a self-referential quantity; the binary node
must be order-ambivalent.

## Sequence points
A sequence point forces timelines to be flattened before proceeding. Crucially,
sequence points _don't_ require full expression evaluation; we just need to
prove that timelines are finalized.
