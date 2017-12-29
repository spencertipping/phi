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

## Another try: FORTH with a custom reader/parser/resolver
FORTH isn't suitable because:

1. It uses a global dictionary
2. It doesn't _appear_ to support reader overloading (but of course it does)
3. It doesn't give you a great way to get local scopes

So let's modify it in these ways:

1. Dictionaries/scopes are quoted things that can be called on other quoted
   things. A given scope can implement whatever crazy open-ended parsing logic
   it wants. The default scope binds names to values.
2. We get a default reader that splits on word boundaries, like FORTH, but it's
   trivial to extend that however we'd like. The reader uses some type of sane,
   not-necessarily-bare-minimal first class input buffer. (**Q:** can we do
   type-based parse continuations like this?)
3. Local scopes are easy to create because our reader can bind/produce custom
   functions that consume stack args and bind them to local values. So `|x, y|`
   consumes values and adds new bindings into the current local scope.

This is quite nice. We still get a "compile" pass, which presumably can involve
"immediate" things (macros), but we get enough generality to extend the syntax
in value-specific ways.

**NB:** local scopes can dictate semantics by specifying new "compile" bindings;
everything always gets compiled down to concatenative words.

### **Q:** does any of this give us a way to quote things?
_Maybe._ It's not clear that we have any separation between the reader and the
compiler, even though it's obvious that we must. FORTH does this with an
imperative function that consumes buffered input and returns values onto the
stack, called by the interpreter.

### **Q:** can we do type-based parse continuations?
Sure; it involves some things:

1. We need "abstract" values, which should be fine. This demands low-level
   polymorphism if we want functions to compile against them (**!!**: I'm not
   sure this is a reasonable way to think about this if we're already separating
   into "compile" and "immediate" stages)
2. We need typed parameters: `|x:int, y:int|` for instance. This means we need
   named types.
3. We need some kind of compile-time value introspection and stack replay. We're
   no longer fully compile-generic like FORTH; and in fact the whole idea of a
   compile stage may no longer make sense (i.e. this seems more like Joy than
   like FORTH).

**Q:** if I'm a function, how do I know whether I'm being invoked in a "compile
me" or "run me" context? I like FORTH's "immediate" word flag, but it's too
blunt an instrument here because if we use it, we won't know the type of value
being returned.

So ... we need to commit to the distinction between compiling stuff and running
stuff, and it can't simply be predicated on whether we're working with abstract
values, because some functions like `random` are side-effectful.

Can function application be polymorphic in run/compile intent?

OK, the compile/run distinction is a bit silly. Why bother when we can just
quote stuff and use a trivially simple evaluation model that consumes lists? We
can have partial evaluation for free since we're concatenative: we just make
sure that backend lists are generated mostly in order, and ask about the stack
top type. Memoize the evaluator for performance, use abstracts.

In other words we don't actually have to care about compile/run. We can
constant-fold everything up to impure functions, which must specify their return
types. (I guess we can also type-hint any function's return value.)

**NB:** It's important that we be able to use parse continuations for
non-constant values. Some kind of type inference is required.

**Q:** if we're inferring types, how do we encode abstract behavior? Which is
polymorphic, input values or function application operations? How do we deal
with undefined functions?

OK, the central issue here is around abstract values, and getting enough
machinery to type-infer in realtime. [Some possibilities](abstracts.md)
