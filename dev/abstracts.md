# Abstract values
An "abstract" value is one that is only partially defined. This is in contrast
to a "concrete" value, which is what programs would manipulate at runtime.

Abstracts are used by the compiler to track what we _do_ know about values,
which may include things like its type, or the values of some of its fields. For
example:

```
x;                  # a completely abstract quantity
x:int;              # an abstract int
[x:int, y:int];     # an abstract list
x:(int|float);      # an abstract value, either an int or a float
```

Types are first-class values that specify things like how they're encoded and
how to access various sub-fields.

Abstracts are themselves concrete values, but backend primitives are polymorphic
wrt abstract-ness.

**Q:** is this true? What if a function's return arity differs depending on an
unspecified input quantity? Are we saying it's impossible to define such a
function? (Possibly, and that might be OK.)

## How abstracts work, philosophically
1. As a type system: `x:int + y:int` would return an abstract `int`.
2. As a constant-folding system: `3 + 4` would return `7`.
3. As a way to ask questions about functions: `f(abstract int x)`

Point (3) is the most interesting; what it means is that we're turning the
language compiler into a library. This leads to some questions:

1. Is it correct to implement this as a matter of runtime value polymorphism, as
   opposed to having an "abstract evaluator" that tracked evaluation semantics?
2. How do we handle recursive functions?
3. Suppose a function asks its own questions about abstract values; can it then
   be abstractly reasoned about? (If not, then we need to think hard about (1).)
4. If we're writing a function, do we want to specify partially-constrained
   abstracts as arguments? (Certainly; e.g. `f(x:[int, int])`.) So abstracts
   need to be first-class values.

## Quick proposal: polymorphic backend functions
i.e. we're deciding (1) above in terms of polymorphic runtime values, and we're
making the design decision that anyone producing backend functions should
support abstracts. Everything above backend functions applies normally.

Here's what breaks right away:

1. Recursion: we could easily not have a base case.
2. Anything where the wrong set of conditions send a function off the rails.
3. IO isolation, if an abstract ends up getting constant-folded.
4. IO isolation for any function that doesn't take an argument (like `random`).

## Quick proposal: a "compile" scope
Now we're deciding (1) using a separate evaluator; backend functions should hint
themselves, but don't need to have any awareness of "abstract" values because
abstract values don't exist in the runtime sense.

Here's what we fix from above:

1. Recursion: we can emit a cycle or limit recursive depth.
2. Errors and off-the-rails behavior.
3. IO isolation of all types.

We also get some nice benefits like timeline quoting (distinct from values) and
pre-optimization abstract values -- meaning that the optimizer can itself be a
library.

Here's what's broken now:

1. Type inference that depends on IO? (Nope; we can define functions that allow
   compile-time IO.)
2. Programs that generate their own abstracts for whatever reason? (I'm not sure
   this is a real problem because we'll have some quoted representation of these
   things that can easily be adapted.)
3. We need some quoted form for abstract values, but this is trivial to define.
4. Can we parse a function definition this way?

(4) deserves some discussion. Let's suppose we write `x:int`; this is a sort of
flatmap in compilation terms because we're looking up a runtime value that
should refer to a type. That's fine, though; if there's a runtime IO dependency
for a type value, it becomes generic for parsing purposes.

### So now...
1. The language is self-hosting through the compiler function.
2. A concatenative basis makes perfect sense.
3. The parser is a regular function; we use a boot image to get there.
4. The evaluation model must be trivial (which it is).
5. We have two evaluators: one to host the compiler, and the implied one we get
   if we evaluate the compiler's output. These must be distinct.

**Q:** how do we compile initial code? Without parse continuations, I suppose.