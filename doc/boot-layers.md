# Bootstrap layers
We need to get phi to a self-compiling state without writing too much low-level
code by hand, and without throwing away too much code. So let's choose some
layers of astraction that are straightforward to implement and give us a lot of
leverage.

## `phi0`: bytecode
`phi0` is a bytecode interpreter implemented in x86 machine code. It's where phi
bottoms out into something concrete.

## `phi1`: high-level assembly
This is basically a version of object-oriented C with no type safety whatsoever.
The compiler generates bytecode that is GC-safe and correctly uses call frames.

Functionally speaking, `phi1` is a macro assembler that uses a real parser.
`phi1` is a source-compatible subset of `phi2`.

## `phi2`: computed grammar and semantics
TODO
