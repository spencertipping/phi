# Semantics of `quote`
Because phi's scopes allow you to rewrite complex forms, attaching a `quote_op`
to a value will modify the way some other operators work. For example:

```
(head_op (cons_op 1 2)) = 1
(head_op (quote_op (cons_op 1 2))) = (quote_op (cons_op instance cons_t))
```

`quote` doesn't do anything by itself, but it does modify the behavior of
`cons`, `head`, and `tail` to apply to phi's internal representation of various
quantities. `quote` also makes `ref`s transparent, reducing everything to `val`
objects and cons cells. Here are the equations:

**TODO:** this way of defining things won't work. We need to define the
interpreter's behavior in terms of quoted forms, not the other way around.

**Q:** is it ok to have an isolated quote layer, rather than backdooring into
it? Probably.
