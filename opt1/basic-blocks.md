# Basic blocks
I dun goofed.

Most languages have a couple of limitations that make basic blocks easier/more
obvious. One is that lists aren't functions: you can't just cons up a new
function out of nowhere and interpret it. Put differently, most languages either
don't support JIT, or if they do, it's through a magic function like `eval` that
is allowed to behave a bit inconsistently.

The other limitation is that most languages don't provide indirect access to the
current frame. phi has the `restack` operator, which supports arbitrary stack
rearrangement, including with computed indexes. This is great and very useful,
but it complicates optimization a bit.

But no matter; the premise of phi was never that optimization was going to be
easy. Just that it's possible, maybe, or something.

## Basic blocks in phi
Let's model this at the interpreter level as opposed to the value level. This
way we can incorporate `i>`, `d<`, and `c<`.

Generally speaking, a basic block is any continuous trace for which the
`next_insn` from the `c` stack is a constant value; and if it's `eval`, `if`, or
`restack`, the relevant arguments are themselves constants. In other words, we
infer basic blocks by stepping an interpreter over abstract values.

This sounds awful but it really isn't. First, we can generate arbitrarily small
basic-block traces by limiting the continuation we give to the interpreter:

```
# this trace will stop after [f] because unknown-c' is indeterminate
[d ([f]::unknown-c') r] basic-block-trace
```

Second, we get a full interpreter -> interpreter transformation -- and this is
pure awesome. For example, we get `call/cc` for free and with minimal overhead.
`i>` also works and is fast. I think we end up with unified stack/heap
allocations and intermediate allocation compression.

## Block trace structure
Basic blocks operate on interpreters-as-values, so our traces can use a smaller
instruction set. In particular I think we need the following:

- Integer ops
- Real ops
- String/sym ops
- Mut ops
- Meta-functions: `version`, `qext`, `crash`
- `restack` over constants
- `uncons`
- `cons`
- `type`
- Extension-defined functions, e.g. `posix_fileio`

We _don't_ support `if`, `eval`, `restack` over computed values, `d<`, `c<`,
`r<`, or `i>`.

## OK, hang on; this depends on values doesn't it?
Yup; this is a strength and a weakness. It's a strength because we can
constant-fold into the optimization phase, which is pure awesomeness; but it
carries the obvious downside that, due to JIT, we need an open-ended runtime.
