# Quoting and literals
The concatenative layer needs to support enough instructions to make it possible
to quote things. I started with `uncons` and `restack` as a basis, where
`restack` took two arguments: a number and an ordering list. `restack`'s
argument order needed to match the result of `uncons`, so a common idiom would
be something like `[0 1 2] uncons restack`.

Obviously there's no reason for `restack` to take two distinct arguments: we can
save a step by having it do the uncons internally. So I made that change.

Now I'm looking at this and realizing that `uncons` itself isn't a great
instruction. If we can `restack` without any preprocessing, then we get `swap`,
`dup`, etc without any dependencies on list functions. So we can get to `uncons`
using `head`, `tail`, `swap`, and `dup`:

```
uncons = dup tail swap head
head   = head
tail   = tail
```

Right now our implementations of `head` and `tail` are more expensive:

```
uncons = uncons
head   = uncons [2 0] restack
tail   = uncons drop
```

**NB:** the above may be true, but I want to table this for now. If there's room
for optimization, we can probably do it in the backend rather than by modifying
the instruction set.
