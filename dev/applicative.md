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
