# phi evaluation model
phi is a lazy language: expressions are evaluated only when they need to be.
Unlike Haskell, though, phi manages IO threading for you so you can live in a
lazy world and get correct ordering for side effects without explicitly creating
data dependencies.

**TODO:** equations for `eval`
