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
      pivot:xs' = xs;
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

## Applicative parse elements
Values can specify their own parse continuations, which should reduce to
primitive constructs:

- `fn [<bindings>] <expr>`: a function that starts a local scope
- `<expr>; <expr>`
- `<expr> ? <expr> : <expr>`: the "just trust me" wrapper around `restack`
- `<expr> = <expr>`: bind or update

`?:` is interesting because both conditional branches need to conform to the
same stack layout. I guess this means we need a "stack layout union" function,
which shouldn't be difficult. Conditional assignments and such are trivial if
the values end up in the same place; the only restriction is that you can't
differentially modify the scope by branch:

```
value ? (x = 10) : (y = 20);  # this will fail unless both x and y already exist
print(x);
```

**Q:** how do type-based parse continuations work?

**Q:** how do we parse for operator precedence?

I think I'm missing some design around parse-time evaluation.

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
