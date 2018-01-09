# Applicative compiler
We need a compiler that reduces applicative forms to concatenative ones.
Superficially this is straightforward: we just maintain a scope chain in memory
and use that to reduce everything to abstract references at parse-time. Those
abstract references specify the stack permutations required to load them into
place for operators to be applied.

Alternatively, we can reduce these forms to symbols and rely on the underlying
resolver and a concatenative compiler library. This keeps everything
compartmentalized a bit better, but entangles the resolver into the state update
mechanism for variable updates. The resulting concatenative code is likely to be
slow, but it should be reducible to abstract values that can be compiled to
backends or much faster concatenative code.
