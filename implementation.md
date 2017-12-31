# Implementation strategy
1. Spec out the concatenative layer: I think just lists + primitives will work
2. Write a bootstrap applicative -> concatenative compiler
3. Rewrite the bootstrap applicative -> concatenative compiler in its own
   applicative notation
4. Use (2) to compile (3) to concatenative, which will form the self-hosting
   base
5. Write concatenative backends
