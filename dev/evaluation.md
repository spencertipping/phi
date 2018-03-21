# Node-based evaluation
phi evaluates while it parses, so it's worth thinking about this a bit.

Let's start with what the parsers produce normally. As operators fold up, we
have various combinations of LHS/RHS with named ops:

```
lhs rhs <some-combiner> -> <some-value>
```

...so parsers need to lift everything into roughly the same space of values.

## Simple case: no partial evaluation
There are two types of expressions: things we can evaluate immediately and
things that are blocked on an `arg`. We can't short-circuit `capture` entirely,
but we can cache the resolved value for parsing purposes.

If we break this down into cases:

1. `const(x)`: parse continuation from `x`
2. `pending(v, fn, x, ...)`: parse continuation from `v`
3. `captured(v, n)`: capture nth, but parse continuation from `v`
4. `arg(v)`: the function arg, parse continuation from `v`

Case (1) is really just a value on its own; there's no reason to wrap it in a
`const` marker. We can have a simple object protocol that requires everything to
implement an `is_const` method -- then non-const things can have an open-ended
API to enable arbitrarily complex forms of partial and speculative evaluation,
while constants aren't obligated to provide anything beyond their native values.

So, at a minimum, every value needs to implement this much:

```
v.is_const()                    -> 1|0
v.postfix_modify(op, v)         -> v'
v.parse_continuation(op, vself) -> parser
```

From an implementation standpoint we should probably use a flag mask, so we'd
get something like this:

```
v.flags()                       -> bitmask (0 = const)
v.postfix_modify(op, v)         -> v'
v.parse_continuation(op, vself) -> parser
```

## Possible object models
Right now I have an awkward hybrid going on: ops encode low-level "values",
whereas syntax nodes wrap ops/constants and provide parse continuations. Ops
should be primary and should optionally delegate to a constant to provide a
parse continuation.

This also opens the door to ops that provide fictitious parsing alternatives to
display inline help. There's no reason parsers need to all operate over strings.
