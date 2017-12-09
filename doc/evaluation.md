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
IO?
