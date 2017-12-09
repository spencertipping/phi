# phi evaluation model
phi is a lazy language: expressions are evaluated only when they need to be.
Unlike Haskell, though, phi manages IO threading for you so you can live in a
lazy world and get correct ordering for side effects without explicitly creating
data dependencies.

**WTF come on spencer, you can't have lazy eval + implicit IO**

**TODO:** equations for `eval`
