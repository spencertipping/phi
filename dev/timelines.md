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

## A pathological example
Before I get too far into this, what happens here?

```
let all_ones = 1 :: map x + 1, all_ones in
let stuff = map x < 5 ? print(x) : x + 1, all_ones in
stuff.h
```

Do we seriously expect phi to figure out that `x < 5` is a convergent
subexpression, and that timeline modifications end at some point? (Which
technically isn't even true if we consider numeric overflow.)

Haskell is a bit more disciplined in that you'd have to lift `x + 1` into `IO`
to get the types to work out. Then you would have a fully serialized expression,
which would loop forever. In other words, the only way to make this work at all
is to have conditionally-dependent types, which Haskell doesn't. And that's
probably fine.

## Sequence points
A sequence point forces timelines to be flattened before proceeding. Crucially,
sequence points _don't_ require full expression evaluation; we just need to
prove that timelines are finalized.

Timeline evaluation and "real" evaluation are related. For example, `print(3+4)`
creates a timeline event with a dependent subexpression `3+4`; in order to
advance time, we'll have to resolve `3+4` before calling `print`. Aha, so what
happens for this:

```
# suppose for the sake of argument that print() ignores its parameter
let all_ones = 1::all_ones in print(all_ones)
```

`all_ones` as a name is timeline-free, so we're in the clear there. So we now
have a revised contract for function args in general: _they can arrive in a
partially-evaluated state; the only guarantee is that timelines have been
collapsed._

## `eval` instantiation
Node expansion happens 1:1 against op nodes except for function calls. Those are
represented as quoted quantities but resolve into subexpressions, and they
inherit a separate `arg, capture` pair in the parse state. So if we wanted to
identify call frames as a way to namespace node instantiations, we could use a
counter within the parse state and increment it for every function call.

This isn't quite right, though: function calls aren't a linear function of
timelines.

## Mutable values
```
let s = "foo" in
let x = (s[0] = 65) :: x in             # this should fail
print x.h
```

Mutable assignment is a timeline modification, but it's not the same as `print`
because it's purely local. It would be like using `IO` to represent some local
state and then collapsing it with `unsafePerformIO`. Now we have to care about
aliasing.

## Object identity
The evaluator's parse state can maintain a counter to track individual object
IDs; then we have a way to identify everything. We'll need this if we want to do
alias analysis.

We care about object identities only for mutable things. Cons cells, integers,
symbols, etc can be anonymous. Object IDs == timeline IDs.

Oh god, it's worse:

```
let xs = "foo" :: xs in xs.h            # elements of xs should be distinct?

let newstr _ = "foo" in
let xs = newstr() :: xs in xs.h         # these should definitely be distinct
```

_Maybe_ we say that creating a timeline isn't the same as modifying one. Or
maybe we represent timeline modification as a set of object/timeline IDs and we
ask whether the intersection is provably null.

## Timeline compression
```
let f = \x -> let s = "foo" in
              let _ = s[0] = x in
              s.to_sym in
f 65                                    # this uses no timelines
```

We don't need any timelines to think about `f` because the reference can be
collected inline.

## Timelines and GC
We need a datatype that can be compared for equality but not for ordering. Then
heap allocations can be commutative (good) while maintaining object identities
and supporting indefinite allocation series.

...so interactions with the heap are not linear; therefore the heap is not a
timeline. If the guarantee is that the heap is indifferent to allocation
ordering, then observable object lifetime must also be invariant to allocation
ordering: so any object with a finalizer must be eagerly reclaimed _iff_ that
finalizer is entangled with the global timeline.
