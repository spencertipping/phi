# Applicative compiler
We need a compiler that reduces applicative forms to concatenative ones.
Superficially this is straightforward: we just maintain a scope chain in memory
and use that to reduce everything to abstract references at parse-time. Those
abstract references specify their desired stack positions for primitive
operations.

In other words, the abstract-value layer is properly applicative (and might be
equivalent to SSA now that I think about it). We don't really care which stack
positions are used by various values because every primitive/backend operation
will involve `restack`ing. We just need fixed points for any branching that
happens. (Also, nothing about stack layout impacts performance because the
backend compilers will re-encode the stack into local variables.)

## Quick example
Let's write a simple recursive function.

```
quicksort xs =
  if xs.length <= 1
    then xs
    else
      let pivot:xs' = xs in
      quicksort (xs'.filter |x| x <  pivot) ++ [pivot] ++
      quicksort (xs'.filter |x| x >= pivot)
```

Now let's break it down into concatenative. We know up front that `xs` comes in
on the stack, so we have this:

```
xs quicksort = result
```

`xs.length` is a derivative expression, so it needs its own stack entry. So we
`restack` to allocate a new cell for it, preferring the top of the stack. It's
fine for every `restack` to rewrite the full local scope of the function; the
stack layout is persisted within the flatmapped parser combinators as part of
the parse continuation.

There's some identity between applicative and concatenative linearity:
`xs.length` isn't aliased anywhere, so we don't need to persist it on the stack
beyond its immediate use. This might save some time, but it also might
complicate the parser and not be worth it. (Like, we still need to track the
position of linear expressions, so we might as well treat them as named things I
suppose.)

More discussion + resolution [below](#who-is-managing-object-lifetime).

Ok, so we're pushing lifetime down to the concatenative layer, which simplifies
things. Then the parser just maintains a mapping from expression to stack
position and can basically allocate stuff with impunity. Local scopes end by
specifying a single (or multiple) return value and clearing the stack otherwise.

Actually, we need to think about that: let's suppose a function has two distinct
return cases, one early and one implicit:

```
contains? = |xs target|
  r = return            # capture the continuation?
  xs.each |x| if x == target then r(x)
  0
```

Ok let's step through how this works.

### Compiling early returns, in general
The applicative parser setup controls the continuation stack, so we can
up-propagate the `return` until it hits the function itself. This should do two
things:

1. Rearrange the data stack to remove locals
2. Pop the correct number of entries off the continuation stack

In other words, we have all of the mechanics required to easily handle
early/nonlocal returns -- and exceptions in fact (although those involve
runtime-querying the continuation stack).

### Compiling `contains?`
The parse tree will end up looking like this:

```
function(layout => [xs target],
  forloop(layout => [xs target],
          iterator => xs,

```

## Who is managing object lifetime?
How to track the lifetime of each subexpression? Like, how do we indicate that
`xs.length <= 1` can be reclaimed after the `if` runs? If the parse layer is
doing that, it needs lookforward.

### Option 1: the parser owns it anyway
Pros:

- The concatenative layer provides fine-grained lifetime control
- The concatenative layer has consistent GC semantics -- i.e. quoted vs real

Cons:

- Lookahead breaks local purity
- It's unclear that the concatenative layer needs to commit to lifetimes

### Option 2: push this down to the concatenative backend
This is a no-brainer; let's take option 2.

Pros:

- Parser is compact and simple
- We get "true" lifetime management: no capricious hanging onto stuff
- The concatenative layer no longer promises lifetime identity
- Concatenative compilers get simpler

Cons:

- Less predictable lifetimes for interpreted/quoted things unless we have a
  concatenative -> concatenative recompiler to clear stuff early
