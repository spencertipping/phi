# Self hosting
Let's start with FORTH for a minute. FORTH has two modes, "compile" and
"immediate", which can be toggled using `[` and `]`, or by using pre-bracketed
words. This quasiquotes the body of a function:

```fth
: f 1 + ;       \ : kicks into compilation mode for 1 +
                \ ; kicks back into interpretation mode
```

And this is an elegant way to think about what's happening: `:` and `;`
manipulate the interpreter's state concatenatively. In phi terms, `:` delegates
parsing to a different scope, and `;` is implied when that parser is done
consuming input (which may or may not happen with some visible delimiter).

**Q:** can phi be FORTH with a different set of basis definitions?
