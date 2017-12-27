- bootup interpreter that consumes a stream like FORTH (input is data)
- single-byte builtin functions that later become ref IDs
  - bootstrap is bytecode
- data stack isn't global: it's a transient context for evaluating stuff
  - something like `[f] 'name eval-and-bind`
  - builtin `eval` uses a stack, but compiled backends can be different
  - no toplevel stack persistence
- resolver is a single value, not a stack
- continuation stack is a global+persistent thing
  - technically not so much a stack as a list form
  - `next` == `(ip, cs) = uncons(cs)`, give or take (whatever canard does)
  - monomorphic evaluator; parsers _compile_ downwards
- phi base imperative, so mutability is fine (no need for nice math)
  - this means phi can manage its heap, e.g. in C, and do stuff in other langs

OK wait a moment. If the evaluator is monomorphic, how do we get type-based
polymorphism? There are plenty of cases where we can't ask the runtime for the
type of an object, but we're still expecting polymorphic behavior.

Do we ever delegate type-handling to the backend? Ideally the backend sees a
bunch of monomorphic functions or something, although some backends are
specialized for polymorphic object handling.

Well ... one important thing to remember is that we don't have to generate
uber-fast code all the time. We just need to make it work and be reasonably
flexible about the idioms we compile to.

phi is a _data_ language, not a _program_ language.

OK let's back out of a bunch of stuff. Let's suppose phi's whole architecture is
just parsers that translate data into backend code, with a loophole for
self-reference. How bad is that? (Not bad, but we want a way to write those
parsers within phi itself so we get backend portability.)

Ok, if phi is a data language, then let's suppose there are two types of data we
describe with it:

1. A storage class, like a `struct`
2. A reaction, which is a rewrite rule

Then what we normally think of as a "function" is actually just a storage class:
phi doesn't evaluate functions. (2) gives us Turing-completeness, albeit in a
fairly slow way. Then we're back to where we started.

This language design is going in circles and I'm still not quite sure what we're
missing.
