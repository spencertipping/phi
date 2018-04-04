# The `phieval` problem
**Update:** The below may be a lie. See [timelines.md](timelines.md) for an
up-to-date discussion of all of this.

Ok, basically the current self-reference strategy is fraught with peril. I
suspect it can be fixed to get the current tests to pass, but even then we'll
have problems with side-effect timing. Let's go through some ways to deal with
this.

1. Implement mutable state cells for variables
2. Implement an "access but don't impact timelines" op type for caching

I think those are the only two options. Let's compare them for the expression
`let x = print(3+4)+1 in x + 1`.

## Update: a third option: forcing
A strict evaluator is required to convincingly _pretend_ to be strict in terms
of IO. So a `(force)` node would inherit flags and a `(reuse (force ...))` would
have no flags.

Hang on. Can we just separate function capture/arg and timelines this way? Sure:
we quote the parse state and defer evaluation.

## Mutable state cells
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

## Non-timeline op wrapper
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

## The GC problem
This is common to both solutions above, so let's get into it a bit.

The main issue is that parse states need to cache some values if they have
timeline dependencies. More specifically, we need to cache any value that we
can't, or don't want to, recompute. This involves knowing what we'll need later,
and how bad it is to need it but not have it. (In the case of timelines, it's a
hard dependency: we _can't_ optimistically drop the value or the program will
fail. So degree-of-badness isn't a continuum.)

**NB:** this comes down to a continuation-reference problem: "does our
continuation refer to node X?" If so, we hang onto a node. I think this is
solvable, possibly recursively if we have a value-lifecycle parser.

### How do existing languages deal with this?
C/OCaml/Haskell/etc use stack frames to hang onto local values; these become GC
roots. So the compiler backend would maintain some frame-related state,
allocating slots for locals (possibly from SSA).

Scheme and SML use CPS, which pushes values forwards through closure
instantiation. This is more similar to how phi works -- and actually, if we were
to do proper closure conversion for `let` bindings we would get this for free.

Ok, is that completely true? How about self-reference?

```
let f = \x -> f(x - 1) in f 5
```

In CPS-world we obviously can't do this easily -- but it becomes easier with a
fixed-point combinator. Let's rewrite this in functional terms using the [Z
combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator#Strict_fixed_point_combinator)
since our evaluation model is semantically equivalent to a strict one:

```
let y = \f -> (\x -> f (\v -> x x v)) (\x -> f (\v -> x x v)) in
let f = y (\f -> (\x -> f(x - 1))) in
f 5
```

This creates a problem, of course: we can't create self-referential infinite
lists. For example:

```
let all_ones = 1::all_ones in all_ones.h      # should be 1 with no fanfare
```

That self-reference isn't functional in nature, so combinators don't fix
anything. And this highlights my thought process being broken: **phi isn't
strict, it's only strict about timelines.** This works because timeline
dependencies behave like monadic IO threading, just with less syntactic
overhead.

OK, so getting back to implementation ... we really do need a recursive graph to
come out of a self-referential expression. This forces `flags` to accurately
indicate any timeline modifications, because the strict evaluator will calculate
the limit of timeline modifications in series. This means `flags` needs to be
accurate, not conservative, for self-reference. I suspect there's a
contradiction in this: if `flags` is eager in any way and we're relying on
laziness, we have a gap that will collect problems.

...do we actually want formal monadic IO? How much lipstick can computed
grammars put onto that situation?

### Let's try something
1. Timelines are library objects
2. The evaluator is non-strict
3. The `IO` wrapper auto-threads through all operations, which it can because
   it's interposing syntax already

Ops already thread timelines implicitly. Maybe the only issue is the way we
calculate `flags`. I can live with that.

### `flags` clearing
Both `let` bindings and function calls will force the timeline updates of their
expressions to happen at a specific time -- phi is timeline-strict after all.
_This means we know a self-reference has already been forced._ Therefore,
`flags` on a self reference indicates purity. We can treat it exactly like a
`mut` retrieval.

Does this work with functions?

```
let our_cons = \x -> \y -> x :: y in
let xs = our_cons 1 xs in               # do we inline our_cons?
...
```

Our hand is forced in cases like this, which I think is ok.
