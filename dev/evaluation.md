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
here. Let's talk about that.

## Core problem: dialects and values
Ops can't generate syntax delegates from their values directly because they
would be unaware of the inflection of the local environment. For example, I
might be writing a section of code in the Python dialect, which should locally
change the parse continuations and in some cases the behavior of the language.

...now, I also want something which at this point is incompatible: as I type
expressions, I want the parse continuation to reflect the fully-evaluated state
of the values I'm working with. Because values transcend dialects, there's no
clear way for the value/evaluator to know which dialect is appropriate for the
given context. It's a lexical/dynamic scoping problem.

Some ways to solve it:

### 1. Replace `op` with `context` in `parse_continuation` and `postfix_modify`
Then the context would provide dialect information:

```
context {
  op ...            # NB: this is a syntactic op, not a value-op
  dialect ...
}
```

Then values are always cast into the lexically-appropriate dialect, and the
dialect becomes part of the scope chain.

This is nice because it generalizes the `op` stuff, which needs to happen
anyway, and it follows the intuition that semantics are consistent within a
scope.

Do all values respond to dialects? I suppose they can specify their preference
and the dialect can take that into account. For example, if we have a value
resident within a Perl runtime, we'll want to continue to apply Perl semantics
to it; it doesn't necessarily behave like a local value because phi can't
guarantee behavior the same way. (Is this true?)

...basically, though, dialects should give us enough flexibility to make
everything work, one way or another.

### What is a dialect, structurally?
It's an object, but let's figure out what it's responsible for. At a high level,
a dialect's job is to modify a value's parser behavior to convincingly model a
target language. For example:

```
# python dialect
x = {}
if 'foo' in x: print "uh oh"

# ocaml-ish dialect
let x = {} in
if Hashtbl.contains x "foo" then
  print_string "uh oh\n"

# perl dialect
my %x;
print "uh oh" if exists $x{foo};
```

I threw ocaml in there because it and Python conflict about the meaning of the
word `in` -- so dialects need to have enough force to resolve things like this.
On the other hand, there are cases where dialects have no business trying to
override things:

```
# python dialect
x = 5mm * 6mm * 7mm
print x.volume()                        # prints "210mm³"

# ocaml-ish dialect
let x = 5mm * 6mm * 7mm in
print_string (
  string_of_float Dimension.volume(x)
  ^ Dimension.unit_string(x))           # prints "210.0mm³" or something

# perl dialect
my $x = 5mm * 6mm * 7mm;
print $x->volume;                       # prints "210mm³"
```

From the syntactic perspective, `mm` is a grammar extension while `.volume()` is
a semantic one. This forces a few aspects of our design:

1. Normal operators are owned by the dialect and translated into semantics
2. Extensions are owned by the value and translated into semantics
3. Do "semantics" correspond to op nodes?

Before I get into (3), I think a simple way to get (1) and (2) is just to have
the dialect add an alternative to each value's parse continuation. So any
regular value with no exceptional operators simply fails directly into the
dialect. (Also, the dialect should answer questions about operator precedence to
help exceptional operators integrate well.)

Ok, let's talk about (3) because it's nontrivial. It's actually quite awkward
thanks to Python, which provides this beautiful syntax:

```py
if 2 < 3 < 4 > 3 > 2: print "aw yeah"
```

#### Semantics and evaluation
We have two degrees of nonlinearity, or at least nonlocality:

1. Syntax -> semantics
2. Semantics -> evaluation

(2) is required by expressions like `2 < 3 < 4` in Python: the semantic stream
is `((< 3), (< 4))`, but the evaluation behavior is `((< 3) && (< 4))`. And this
is also, to some extent, dictated by the dialect -- which is a big can of worms
due to pathological situations like this:

```
# C dialect
int y = ...;
int x = y < 4;

# python dialect
if x < 5: print "..."
```

Python's semantic folding of multiple `<` happens only under two conditions:
first, every expression involved needs to be in the Python dialect; and second,
every expression also needs to be non-parenthetically present in the same
reduction. In other words, the `<`-collapsing stuff behaves like syntax and not
semantics.

From a source-parsing point of view we would approach this as a continuation
that matched multiple `'<' expr` or `'>' expr` followers. Python's dialect is at
liberty to provide this behavior for general expressions, and on a syntactic
basis.
