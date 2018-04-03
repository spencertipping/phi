# Next steps
1. Build a program that tests all instructions and will verify that a VM is
   implemented correctly
2. Write a formal-ish spec for the [frontend language](phifront.pm)
3. Fix repl tests (right now a bunch fail from the phifront port)

## The `phieval` problem
Ok, basically the current self-reference strategy is fraught with peril. I
suspect it can be fixed to get the current tests to pass, but even then we'll
have problems with side-effect timing. Let's go through some ways to deal with
this.

1. Implement mutable state cells for variables
2. Implement an "access but don't impact timelines" op type for caching

I think those are the only two options. Let's compare them for the expression
`let x = print(3+4)+1 in x + 1`.

### Mutable state cells
This is the more traditional way to get self-referential quantities. It
corresponds to what we're doing with muts, except in a way that doesn't rely on
strict node aliasing. Most likely we would have something like this:

```
(seqr
  (set-mutable 'gensym (+ (print (+ 3 4)) 1))
  (+ (get-mutable 'gensym) 1))
```

This creates a GC problem, of course -- but more importantly, these `gensym`
quantities aren't as distinct as they need to be: there's a difference between
instantiated (i.e. evaluated) nodes and structural nodes.

### Non-timeline op wrapper
Initially, I really like this approach. Let's see how well it holds up, for
instance on something like `let x = print(3+4)+1 in x + 1`. The optree here
would be:

```
print_expr = (+ (print (+ 3 4)) 1)
(seqr
  print_expr
  (+ (no-timelines print_expr) 1))
```

So far so good. Now, we may have a problem around GC: how do we know the result
value from `print_expr` can't be reclaimed eagerly? (Maybe it's a general GC
problem, then again: trace the nodes' parse-result lifetimes to determine the
right caching strategy.)

### The GC problem
This is common to both solutions above, so let's get into it a bit.

The main issue is that parse states need to cache some values if they have
timeline dependencies. More specifically, we need to cache any value that we
can't, or don't want to, recompute. This involves knowing what we'll need later,
and how bad it is to need it but not have it. (In the case of timelines, it's a
hard dependency: we _can't_ optimistically drop the value or the program will
fail. So degree-of-badness isn't a continuum.)

#### How do existing languages deal with this?
C/OCaml/Haskell/etc use stack frames to hang onto local values; these become GC
roots. So the compiler backend would maintain some frame-related state,
allocating slots for locals (possibly from SSA).

Scheme and SML use CPS, which pushes values forwards through closure
instantiation. This is more similar to how phi works -- and actually, if we were
to do proper closure conversion for `let` bindings we would get this for free.

...so yeah, let's just CPS convert already. FFS.
