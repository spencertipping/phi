# phi1 spec
phi1 needs to be thin and syntax-compatible with phi2. This makes it possible to
reuse most/all phi1 code even though we can't reuse the implementation of phi1.

There are three big things we need from phi1:

1. Automatic frame management (allocation + class generation)
2. Expressions (e.g. `(3 + 4) * 5`)
3. Block parsing + linking
