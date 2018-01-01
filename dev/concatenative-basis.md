# Concatenative basis design
phi's base concatenative layer has just enough machinery to make things work. We
don't particularly have to worry about performance yet; that will be handled by
backend-specific compilers.

A phi value is one of the following:

- An integer
- A string (written in quotes)
- A symbol (not quoted)
- `nil`
- A cons cell; lists are written `[1 2]` to mean `cons(1, cons(2, nil))`

phi is stack-concatenative, so the interpreter maintains a data stack and a
return stack. I call these `d` and `c` (for "continuation") respectively. These
stacks are themselves phi values; a stack can be represented as a list.

The final part of the interpreter is `r`, the symbol resolver. I'll explain this
in more detail below, but the important things to know for now are that (1)
lists behave as functions, and (2) `r` is a function that turns a symbol into a
function.

## Basic evaluation example
Before I get into the formal equations, let's walk through a quick function.
Let's suppose we have a function called `inc` that adds one to a number (defined
as `[1 +]`) and another function `x*inc` defined as `[dup inc *]`. Now let's
evaluate `5 x*inc` by stepping through:

```
5 x*inc           # d = [],      c = [[5 x*inc]]
x*inc             # d = [5],     c = [[x*inc]]
                  # d = [5],     c = [[dup inc *]]
                  # d = [5 5],   c = [[inc *]]
                  # d = [5 5],   c = [[1 +] [*]]
                  # d = [1 5 5], c = [[+] [*]]
                  # d = [6 5],   c = [[] [*]]   <- end of function
                  # d = [6 5],   c = [[*]]
                  # d = [30],    c = [[]]       <- end of function
                  # d = [30],    c = []         <- end of evaluation
```

This isn't shown above, but it's also ok to have an improper list, e.g.
`cons(+, *)`; if `c = [+ [*]]`, then phi will evaluate `+` and pop it in one
step. This is how you would optimize tail calls.

## Constitutive equations
phi is defined in terms of interpreter state transitions; to simplify the
notation, I represent an interpreter as a phi list of `[d c r]`. I also
sometimes write things like `[x xs...]` as a shorthand for `cons(x, xs)`.

### Self-quoting forms: non-symbols
When `x` is not a symbol:

```
[[d...] [[x c0...] c...] r] -> [[x d...] [[c0...] c...] r]
```

**NB:** this means you can't tail-call optimize a function whose final operation
is to push a list based on its self-quoting behavior; you'd get `[x foo...]`
instead of `[x [foo...]]`.

### Non-native symbols
When `x` is a symbol that has no native behavior:

```
[[d...] [x c...] r] -> [[x d...] [r . c...] r]
```

That is, when we have a symbol that requires resolution, push it onto the data
stack and call the resolver `r` on it. Then take that result and run it using
`.` (eval).

Technically, native symbols are also resolved; they just don't get bound to
anything.

### Native symbols
These symbols fall through the resolver and then end up on the data stack
against an eval continuation; we have `[[x d...] [. c...] r]`. What happens next
depends on the value of `x`:

#### Interpreter quote/unquote
```
[[i> d...]            [. c...] r] -> [[[d c r] d...] [c...] r]
[[i< [d' c' r'] d...] [. c...] r] -> [d' c' r']
```

#### General value operations
You can ask for the type of a value, which phi represents using the symbols
`int`, `string`, `symbol`, `cons`, and `nil`.

phi represents booleans as the integers `0` and `1`.

```
[[type a   d...] [. c...] r] -> [[<type-symbol> d...] [c...] r]
[[==   a b d...] [. c...] r] -> [[<0|1>         d...] [c...] r]
[[=    a b d...] [. c...] r] -> [[<0|1>         d...] [c...] r]
[[<    a b d...] [. c...] r] -> [[<0|1>         d...] [c...] r]
```

#### Conditions
These are written as `[then] [else] if`, which produces the opposite order on
the stack:

```
[[if else then 0         d...] [. c...] r] -> [[d...] [else c...] r]
[[if else then <nonzero> d...] [. c...] r] -> [[d...] [then c...] r]
```

#### List operations
```
[[cons   a b        d...] [. c...] r] -> [[cons(a, b) d...] [c...] r]
[[uncons cons(a, b) d...] [. c...] r] -> [[a b d...]        [c...] r]
```

#### Stack operations
```
[[dup   a     d...] [. c...] r] -> [[a a   d...] [c...] r]
[[drop  a     d...] [. c...] r] -> [[      d...] [c...] r]
[[swap  a b   d...] [. c...] r] -> [[b a   d...] [c...] r]
[[rot3< a b c d...] [. c...] r] -> [[b c a d...] [c...] r]
[[rot3> a b c d...] [. c...] r] -> [[c a b d...] [c...] r]
```
