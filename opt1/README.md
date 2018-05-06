# First-pass optimization
phi uses a tracing compiler with representational abstraction as a self-hosted
optimization layer before we emit backend code. Before I get into the details,
let's talk about some design constraints.

## Lying convincingly
Unlike non-politicians, compilers are allowed and encouraged to be dishonest.
Most languages are predicated on the maintenance of some set of fictions:

- Smalltalk and Ruby: everything is an object, including classes
- Haskell: you really always wanted to represent mutable operations as values
  but didn't realize it, laziness is a good idea, and knowing about monads makes
  you a superior human being
- Perl: you have no linguistic standards, and a sufficiently expressive regular
  expression is marriage material
- C: pointers, stacks, values, and functions are things
- Java: you can't be trusted to use a real language, and you're too much of a
  philistine to be bothered by GC overhead, UTF-16 overhead, or multi-second
  startup time
- Forth: stacks are a thing and POSIX is for wimps
- Machine code: all memory addresses are created equal, registers are a thing,
  and you're drunk enough to read the Intel manuals

These fictions are as real as we believe they are -- and if they simplify our
world without being demonstrably false, we're usually willing to accept them as
canon. The language economy doesn't demand or value honesty; it just demands
believability.

## The performance problem
Any language with an optimizer demands some knowledge of how that optimizer
works. The more complex the optimizer, the more likely you are to inadvertently
observe it; for example, [Haskell's stream
fusion](https://stackoverflow.com/questions/578063/what-is-haskells-stream-fusion)
isn't universally applicable; if you wrote your own list function, it would
probably break the optimization and start allocating intermediate cons cells. In
this case, the language has unconvincingly claimed that it can optimize through
cons-allocating functions. The dishonesty comes with an asterisk.

phi's goal is jointly to be fast and to minimize the number of asterisks.

## Optimization strategies
Broadly speaking, we can look at optimization in two ways:

1. Speculatively advance the interpreter and study the output (abstract eval)
2. Profile the interpreter and reactively optimize the trace

It isn't immediately clear which of these will produce better results, so let's
get into that a bit.

### Speculative evaluation
This is how I've been thinking about optimization for most of phi's development.
The basic idea is to reverse-engineer pieces of code using abstract evaluation:
that is, construct some placeholder values, simulate a function being applied to
those values, and then compare input/output. We'll end up with a new
quasi-instruction for the function.

There are some tradeoffs here. First, it's an entropy-unaware solution; we won't
know that a branch is rarely taken just by looking at the code. This means we
might miss opportunities to speculatively unroll loops or recursive invocations.

Second, the effectiveness of abstract evaluation depends a lot on the initial
assumptions we make. If I just send unknowns into a function, I'm unlikely to
get anything very insightful from the result; most of the gains happen when we
compose multiple basic blocks and preserve structure between them.

I think there's a place for speculative evaluation, but it shouldn't be the
front line against the optimization problem.

### Profiling/tracing
I really like this approach for a few reasons:

1. It's dead easy
2. It's profile-guided, so we optimize what we need to
3. We get automatic inlining with assertive bailout (more on this below)
4. It's unsupervised

(3) helps because it lets us short-circuit or batch deoptimization conditions.
For example, let's suppose we have a hot path that takes the `then` case of each
of three `if` branches:

```
cond1...
[ this_is_hot cond2...
  [ this_is_hot cond3...
    [ this_is_hot... ]
    [ this_is_cold... ]
    if ]
  [ this_is_cold... ]
  if ]
[ this_is_cold... ]
if
```

If we're optimizing a trace that runs through all of the hot branches, then we
need to verify that `cond1 && cond2 && cond3` is true. The moment any of those
are false, we bail out of the optimized version and go back to unoptimized
evaluation -- we no longer have an optimized trace that helps us. (The idea
being that this happens rarely enough that we're willing to let it happen
sometimes in exchange for better inlining for the hot trace.)

### ¿Por qué no los dos?
Profiling/tracing tells us _what_ to optimize, but not how we should optimize
it. The event timeline should look something like this:

- Run some code with profiling...
- Identify a hot trace
- Abstract-optimize that trace, yielding a faster version
- ???
- The world is now faster

The ??? step is pretty important. We need to replace the original code with the
optimized version, which isn't completely straightforward unless we have some
type of indirection against cons cells.
