# phi language frontend
One thing I haven't really been decisive about here is where the delegation is
between "stuff written in concatenative" and "stuff written in real phi" -- so
let's fix that. First, let's take some inventory.

1. Parse continuations are fully implemented in concatenative
2. Extensible infix/grouping are also fully implemented
3. `phifront` doesn't implement destructuring grammars yet

The question, then, is whether we want to fix (3). It shouldn't be difficult at
all, actually, but it does impact the nature of the language because it's
parser-based: if we have `alt` parsers to work with, then why not use those
instead of conditional nodes? Also, who hosts the parsers -- are they optrees,
or do the optrees delegate to concatenative parsers?

Before I get into that, though, I need to talk about node/concatenative interop.

## Concatenative interop
Right now we have two evaluation contexts: one built around objects that encode
expressions, and one hosted by phi itself that provides concatenative execution.
Expressions are easier to generate from parsers and they're straightforward
enough to evaluate, so there's no real issue using them to host the frontend
language. What we _do_ need, though, is a way to write `phifront` expressions
that interoperate with concatenative primitives, including `phiobj` values.

This is tricky for a couple of reasons:

1. Not all operators can be represented as expressions, e.g. `uncons`, `i>`
2. Expression-functions need wrapping to be used as concatenative lists

(2) is simple enough: we just have closures convert to things which call
`thefuzz` (or some other evaluator) on an expr node, with some input/result
conversion. The extra stack slots from this process will be gone by return-time,
so we should be in good shape.

(1) is harder, although we have some options that mitigate the worst of it. The
big thing we need to do is support variadic/multary functions and returns, which
enables a bunch of stuff like primitive `cons`/`uncons`, etc. We have a few
options, ranked from least to most aggressive:

1. Define expr-functions as having a declared input arity, then returning a list
   of results
2. Define expr-functions as taking and returning the whole data stack
3. Define expr-functions as taking and returning the whole interpreter

### Declared arity, using lists
This is simple and aligns well with the amount of abstraction we want. I don't
believe I've defined any truly variadic concatenative-level APIs so far;
everything is fixed-arity. Returning a list of results is trivial.

### Data stack -> data stack
We get variadic support here, so we could theoretically implement `restack`
using opnodes if we do this. Is there a reason we would want to? It seems like
if we do, we should just go all out and...

### Interpreter -> interpreter
Despite the obvious idiocy of this design, I do like what it gives us. We can
implement efficient `call/cc` with interpreter quoting (which is absolutely the
wrong way to do it, I suspect), and ultimately we can extend the interpreter's
instruction set if we want to -- including with complex things like lists.

The adapter would need to be very careful about managing the continuation stack
correctly, and this might create problems with things like tail-call
optimization. That said, though, the calling convention is incredibly
straightforward: `i>` for input, and `i<` for output. Then it's up to libraries
to indicate slicing. (And to make sure we handle the continuation stack the
right way -- always a risk when we're interpreter-quoting.)

This design makes op nodes into first-class things. If we then were to implement
an abstract interpreter within op-node space, it could become a self hosting
optimizer. The _data_ is still the interpreter, but the computational mechanics
are much more structured. I have to admit, I really like this idea.

### Composability
If I've got two functions `f` and `g`, when+how can they be composed?

- If they're list -> list, they can be composed _against a real stack_
- If they're stack -> stack, they can be composed abstractly, but this will miss
  optimizations that happen with real stacks
- If they're interpreter -> interpreter, I have no idea what composition even
  means anymore

...so in no world do we really want to transform interpreters all the time. We
want the ability to for specific purposes -- but then we can just emit a magic
function that's been composed with `i<` and `i>` at the concatenative level.
Really we're about transforming stuff on the data stack, probably by
transforming the stack itself.

Let's do this then: functions are stack -> stack, and we need enough
destructuring ability to work in a world where the interpreter itself is the top
stack item.

## Destructuring/conditional mechanics
There's a lot of advantage to having destructuring parsers as builtins when we
boot up `phifront`:

1. We get a parsing context for LHS templates, which should generate
   parsers/bind lists
2. `let` bindings and function args require no real modification to be fully
   destructurable
3. We might be able to use the concatenative language basis forever if it
   handles all of the base cases correctly

(3) is interesting because it changes the equation for bootstrapping: we no
longer need to do the self-hosting rewrite. If there's nothing wrong with the
concatenative layer, then why rewrite it using infix?

### Concatenative re-hosting
We might as well use the phifront mechanics to replicate the concatenative
development layer we have in Perl right now -- particularly if those op nodes
then become interpreter -> interpreter functions. If we have an op-node
implementation of an interpreter, then we can compile a new concatenative
backend. At that point we're fully self-hosted.

### Stack destructuring and function signatures
From a usability point of view, the big issue here is that we need implicit
"rest-of-stack" passthrough. For example, it should be possible to write a
unary-ish function like `f(x) = x + 1` and have that do the right thing with the
unmentioned remainder of data stack entries. I think this can be done by ending
the main list parser with a catch-all rather than a constant `nil`.

### Parsers as data
If we have a function that destructures the stack, how do we compile that
destructuring operation? We can abstract interpret the parsers it's using
(parsers-as-process, execution-as-data), or we can directly translate the
parsers into functional operations. I think we want the latter in some cases
unless our abstract interpreter is good at optimizing for entropy per unit
runtime -- which it should be, but that's a tall order.

Regardless of how we describe what's going on, though, it's clear enough that
we'll end up with what amounts to a parser of parsers; _something_ ends up
destructuring our pattern matchers and producing imperative code from it. I'm
not sure I have a direction I'm going with this other than to mention that
parsers are data too, and that we might care at some point.

### A minimal set of destructuring parsers
Enough stuff to implement the phi concatenative backend strictly using pattern
matching and restructuring.

- `cons(p1, p2)`
- `nil`
- `guard(p, fn)`
- `bind(p, name)`
- `any`
- `rest` (this is a bit magical for syntactic convenience)

Parsers emit a scope transformation (I think).

So, for example, the pattern `([x], y:int)` would be expressed like this:

```
cons(                                   # [x] :: y:int :: rest
  cons(                                 # [x]
    bind(any, 'x),                      # x
    nil),                               # []
  cons(                                 # y:int :: rest
    cons(                               # [y:int]
      bind(                             # y:int
        guard(any,                      # y
              [type, 'int, symeq]),     # :int
        'y),                            # y
      nil),                             # []
    rest))                              # rest
```

One interesting aspect of the implicit-`rest` design is that we can never assert
that the stack is _at most_ some length, which I think is fine. (If we really
care about that, we can just use an interpreter-quoting function.)

Parsers are first-class objects, so there's no lock-in to primitive
destructuring. We could later destructure phi objects or other complex datatypes
by writing new grammar elements and new syntactic bindings to generate them.

## Writing patterns
We want to support variable shadowing within destructuring patterns, which
creates ambiguity around things like `x -> x + 1`. There are a couple of ways we
could resolve this:

1. Explicitly enter pattern context, e.g. with `\` or `let`
2. Do something _very_ interesting with alternative scope resolution

### The very interesting alternative thing
This isn't just insane, it's impossible -- but the reasons are pretty cool.
Let's suppose we have a binding `x = 5`, and that `x` can alternatively resolve
into either an int or an unbound quantity. We'll prefer the int until it gets
rejected, at which point we switch over.

```
x = alt(int-value, unbound-value)
```

So far so good. What happens if we parse `(1 + x)`? The close-paren will kick
the `alt` for `x` down all the cases to see if any of them will parse the close
paren. Then we'll accept a continuation-free parse, which must be an `int`. This
case works correctly. (This is actually an optimistic lie: `int` will be willing
to take the continuation-free parse immediately, so we'd never fail to the
pattern case regardless of what followed.)

OK, how about `[x] -> x + 1`, in which `->` is a function constructor? By the
time we parse `[x]`, we'll have committed to one alternative or another because
`[` incorporates a _value_ into its parse continuation and `]` commits that
value. But that commit happens before we encounter `->`, so we'll choose the
integer and end up with a function that matches a constant list.

Egregious though it is to add the extra byte, we really do need `\` or some
other value to break into pattern context.

### Syntax
There are two basic things we want to do with patterns:

1. Make a function
2. Bind local values

Unlike in [most functional
languages](https://stackoverflow.com/questions/3416475/haskell-guards-on-lambda-functions),
though, functions in phi can be parsers -- and that creates some interesting
options. Structurally speaking, a function is a parser that consumes a data
stack and emits a new one. So let's go through some examples.

```
fn x -> x + 1                           # x :: rest -> (x + 1) :: rest
fn [x] -> x                             # [x] :: rest -> x :: rest
```

Simple enough. But since we're dealing with parsers anyway, we might as well
support alternatives:

```
fn [x] -> 1                             # [x] :: rest -> 1 :: rest
 | []  -> 0                             # []  :: rest -> 0 :: rest
```

Functions-as-parsers aren't allowed to fail, just like you shouldn't be allowed
to apply an `int`-only function to a string. So one of the alternatives must
match.

#### `let` and `match`
`let` -> `fn` conversion on the above will make it obvious that you can't use
alternatives in a `let` binding, but you can use them in `match`:

```
let [x] = foo in ...                    # this is a bit silly
  | []  = foo in ...

match foo                               # more standard
  | [x] -> 1
  | []  -> 0
```

Of course, there's no reason `match` has to be any special syntactic thing; it
makes more sense for it to be an unowned infix operator:

```
foo match
  | [x] -> 1
  | []  -> 0
```

...and that converts to `fn` in the predictable way:

```
x match <stuff> = (fn <stuff>) (x)
```

#### First-class patterns
Pattern context binds names to `bind` matchers, so we need a different way to
refer to existing values. I think it makes sense to use a consistent
context-independent interpolation syntax for this, sort of like Lisp's `,` and
`,@` for quasiquoted values.

**NB:** it's tempting to use undefined edges of pattern syntax to infer
interpolation, but we hit exactly the same set of problems that we do trying to
infer pattern syntax to begin with. Interpolation needs to be explicit.
