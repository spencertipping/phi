# phi evaluation model
phi is a strict language with an optimizer that brings a lot of the benefits of
lazy evaluation without introducing the inconveniences (IO monad). For example:

```ocaml
let x = cons(1, x)              (* (1) this is fine *)
let x = cons("hi".print, x)     (* (2) this will infinite-loop *)

let y = "hi".print in
  let x = cons(y, x)            (* (3) this is also fine *)
```

In a lazy language, (2) and (3) would be equivalent: values carry IO. But under
a strict evaluation model, a `let`-binding introduces a sequence point and takes
up the IO from the value. Equationally:

```ocaml
let x = cons(1, x)              (* IO[x] = IO[1] + IO[x] + IO[cons(1, x)] *)
                                (* IO[1] and IO[cons(1, x)] must be 0 *)

let x = cons("hi".print, x)     (* IO[x] = IO["hi".print] + IO[x]
                                         + IO[cons("hi".print, x)] *)
                                (* fails because IO["hi".print] != 0 *)

let y = "hi".print in           (* IO[y] != IO["hi".print] *)
  let x = cons(y, x)            (* IO[y] = 0 so this works *)
```

I was going to say that anyone willing to tolerate the IO monad should just go
and marry Haskell, but now I'm really seeing the virtue of that model. Strict IO
is more complicated than dataflow IO, particularly if the syntax can handle the
dataflow propagation. Dataflow IO also has the benefit of being first-class, so
we can do stuff like custom control flow/parallelism.

**Q:** is it at all practical to have the syntax itself linearize evaluation and
IO? Sure: just ask abstract values about their IO using `.io`; operators and
function calls then compose this, and any unknown-ness about `io` will prevent
code from being compiled/run.

**Q:** Can IO values themselves have IO? Sure; then we evaluate the higher-order
IO to produce the lower-order one. This continues until we end up with a
constant. (And this is how `eval` and `require` must work.)

OK... so:

1. There is no "unevaluated quantity"; instead, we have "IO-dependent
   quantities" and "quantities that can't be parsed". The latter is a fatal
   error if you try to compile it, because we erase scopes at runtime.
2. Quoting a runtime value into a compile-time value yields `(value, type, IO)`,
   each component of which is expressible as a maybe-constant runtime value.
   Anything with unspecified IO will refer to the interpreter, which makes it
   possible to inline IO-independent calls to `eval`. (**NB:** technically we
   don't literally have `value, type, IO`; we have generators for each component
   of it.)
3. Can a program request modifications to its compilation semantics by modifying
   the compiler scope? In theory sure; in practice, we'll need to quote the
   compiler's representation of everything and interpret an additional layer
   down. Is there an in-place transformation that makes it so we don't have to
   quote everything upwards?
4. Destructuring binds and structural parsers are evaluated within the compiler,
   which makes (3) relevant.
5. Gensyms and compile-time file reads are examples of compile-time IO.
   `require`'s IO is itself IO-dependent.
6. Conditions are always IO-evaluatable: the IO of `(if x y z)` is
   `IO[x] + (IO[y] | IO[z])` -- but `IO[x]` specifies `x` so we can then decide.
   If we didn't have this, then `if` would be impossible to evaluate.
7. The runtime can ask about types and IOs inferred from compile-time, and can
   splice these (or derivatives) back into the compiler. This may introduce
   runtime IO dependencies on runtime IO values, but that just becomes an IO
   flatmap.

**Q:** Is it the case that an IO-dependent IO just needs to be flatmapped?
Obviously. That simplifies a lot of stuff!
