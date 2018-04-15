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
to indicate slicing.

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

We still have some unresolved issues, though, so let's talk about those before I
get further into the weeds here.
