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
v.postfix_modify(op, lhs)       -> v'
v.parse_continuation(op, vself) -> parser
```

## Possible object models
Right now I have an awkward hybrid going on: ops encode low-level "values",
whereas syntax nodes wrap ops/constants and provide parse continuations. Ops
should be primary and should optionally delegate to a constant to provide a
parse continuation.

This also opens the door to ops that provide fictitious parsing alternatives to
display inline help. There's no reason parsers need to all operate over strings.

So we really have three distinct responsibilities:

1. Encode a value for evaluation purposes
2. Encode a process to get to a value (also for evaluation purposes)
3. Specify parse behavior

And there are a few different ways we could split them up.

### 1. Ops outside syntax (the strategy above)
```
op|arg|capture {
  syntax_node parse_delegate;
  ...;
}
```

`const` would be just a syntax node on its own, so syntax nodes participate
minimally in the AST representation by specifying three methods:

```
v.flags() -> 0
v.type()  -> 'int|'str|...|'object
v.val()   -> primitive-val
```

**Q:** suppose we're evaluating things and reducing to const nodes. How do we
decide which syntax delegates to use?

### 2. Syntax outside ops
```
syntax_node {
  op val;
  ...
}
```

This fails because `capture` won't know the correct syntax node _type_ to
create. In strategy (1), this isn't a problem because the capture node is just
an op type that wraps the logical value it resolves to (and stores no extra
data; there's no caching silliness beyond that).

### 3. Explicit linkage
```
op          { ... }
syntax_node { ... }

interactive {
  op          val;
  syntax_node parse_delegate;
}
```

This isn't great because it doesn't provide any real help for things like
argument substitution ... although maybe that's getting at the real problem
here. [Let's talk about that.](dialects.md)

### 4. Dialectal linkage
```
op { ... }
syntax_node = state.context.dialect.syntax_for(state.context, op)
```

This solves the language emulation problem, but what about custom
language-independent extensions? Computed values need a dialect-independent way
to indicate how they want to augment the grammar. Either that, or dialects all
need to recognize a common protocol to let nodes do this.

I like this dialect-driven approach. It's valuable because there are some cases
within a dialect where, for whatever reason, you don't want an open-ended syntax
dropped in. I think it's fine for the dialect to have the ultimate say about
when this happens.