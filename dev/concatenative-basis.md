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
evaluate `x*inc` applied to `5` by stepping through:

```
x*inc             # d = [5],     c = [[x*inc]]
                  # d = [5],     c = [[dup inc *] []] <- replaced by definition
                  # d = [5 5],   c = [[inc *] []]
                  # d = [5 5],   c = [[1 +] [*] []]
                  # d = [1 5 5], c = [[+] [*] []]
                  # d = [6 5],   c = [[] [*] []]      <- end of function
                  # d = [6 5],   c = [[*] []]         <- ...drop the nil
                  # d = [30],    c = [[] []]
                  # d = [30],    c = [[]]
                  # d = [30],    c = []               <- end of evaluation
```

This isn't shown above, but it's also ok to have an improper list, e.g.
`cons(+, *)`; if `c = [+ [...]]`, then phi will evaluate `+` and pop it in one
step. This is how you would optimize tail calls, but be careful: not everything
can be optimized this way (explained in "self-quoting forms" below).

## Constitutive equations
phi is defined in terms of interpreter state transitions; to simplify the
notation, I represent an interpreter as a phi list of `[d c r]`. I also
sometimes write things like `[x xs...]` as a shorthand for `cons(x, xs)`.

### Self-quoting forms: conses, nil, and strings
When `x` is a cons, nil, or string:

```
[[d...] [[x c0...] c...] r] -> [[x d...] [[c0...] c...] r]
```

**NB:** this means you can't tail-call optimize a function whose final operation
is to push a list based on its self-quoting behavior; you'd get `[x foo...]`
instead of `[x [foo...]]`.

### Symbols: stuff that needs to be resolved
When `x` is a symbol:

```
[[d...] [[x c0...] c...] r] -> [[x d...] [r . [c0...] c...] r]  # normal call
[[d...] [x         c...] r] -> [[x d...] [r .         c...] r]  # tail call
```

That is, when we have a symbol, push it onto the data stack and call the
resolver `r` on it. Then take that result and run it using `.` (eval).

### Natives: integers
Each native function is represented by an integer against an eval continuation:
`[[x d...] [. c...] r]`. What happens next depends on the value of `x`.

The set of native symbols/equations is deliberately minimal because anyone
implementing a phi backend will need to implement each of them. For this reason,
phi assumes you'll provide some operators written in terms of others; for
instance:

```
-     -> neg +
or    -> ~ swap ~ and ~
>     -> swap <
```

Here are the codes for each of the operators below:

| Code   | Name     | Description |
|--------|----------|-------------|
| `0x00` | `i>`     | Quote interpreter   |
| `0x01` | `i<`     | Unquote interpreter |
| `0x02` | `.`      | `eval`              |
| `0x03` | `if`     | If-then-else        |
| `0x04` | `type`   | Get type of a value |
| `0x05` | `==`     | Physical equality   |
| `0x06` | `cons`   | Make a cons         |
| `0x07` | `uncons` | Invert `cons`       |
| `0x08` | `dup`    | |
| `0x09` | `drop`   | |
| `0x0a` | `swap`   | |
| `0x0b` | `rot3<`  | |
| `0x0c` | `rot3>`  | |
| `0x0d` | `cseth`  | Set cons head       |
| `0x0e` | `csett`  | Set cons tail       |
|--------|----------|---------------------|
| `0x10` | `+`      | Integer add         |
| `0x11` | `neg`    | Integer negate      |
| `0x12` | `*`      | Integer multiply    |
| `0x13` | `/%`     | Integer divmod      |
| `0x14` | `<<`     | Integer left-shift  |
| `0x15` | `>>`     | Integer right-shift |
| `0x16` | `and`    | Integer bitwise and |
| `0x17` | `xor`    | Integer bitwise xor |
| `0x18` | `~`      | Integer one's complement |
| `0x19` | `<`      | Integer less-than   |
|--------|----------|---------------------|
| `0x20` | `str`    | Make a string       |
| `0x21` | `slen`   | String length       |
| `0x22` | `sget`   | String byte get     |
| `0x23` | `sset`   | String byte set     |
| `0x24` | `strsym` | String to symbol    |
| `0x25` | `symstr` | Symbol to string    |

#### Interpreter quote/unquote
```
[[i> d...]            [. c...] r] -> [[[d c r] d...] [c...] r]
[[i< [d' c' r'] d...] [. c...] r] -> [d' c' r']
```

`.` is the `eval` function and, for non-native symbols and non-symbols, adds a
continuation like this:

```
[[. x d...] [. c...] r] -> [[d...] [x c...] r]
```

It's ok for `i<` and `i>` to introduce unpredictable performance characteristics
into the resulting program if they're called in the middle, although
implementations should generally perform well if usage is confined to startup.

#### General value operations
You can ask for the type of a value, which phi represents using the symbols
`int`, `string`, `symbol`, `cons`, and `nil`.

phi represents booleans as the integers `0` and `1`.

```
[[type a   d...] [. c...] r] -> [[<type-symbol> d...] [c...] r]
[[==   a b d...] [. c...] r] -> [[<0|1>         d...] [c...] r] # physical
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
[[cons   a b          d...] [. c...] r] -> [[cons(a, b) d...] [c...] r]
[[uncons cons(a, b)   d...] [. c...] r] -> [[a b d...]        [c...] r]

# in-place updates:
[[cseth  h cons(a, b) d...] [. c...] r] -> [[cons(h, b) d...] [c...] r]
[[csett  t cons(a, b) d...] [. c...] r] -> [[cons(a, t) d...] [c...] r]
```

**NB:** A common idiom for quoted values is to place them inside a list and then
`uncons` them: `[5] uncons swap drop` == `5`, but this will prevent `5` from
being executed. This is how literal numbers work internally.

#### Stack operations
```
[[dup   a     d...] [. c...] r] -> [[a a   d...] [c...] r]
[[drop  a     d...] [. c...] r] -> [[      d...] [c...] r]
[[swap  a b   d...] [. c...] r] -> [[b a   d...] [c...] r]
[[rot3< a b c d...] [. c...] r] -> [[b c a d...] [c...] r]
[[rot3> a b c d...] [. c...] r] -> [[c a b d...] [c...] r]
```

`rot3<` and `rot3>` work such that `1 2 3 rot3>` == `3 1 2`.

#### Integer operations
```
[[+   a b d...] [. c...] r] -> [[(a+b)       d...] [c...] r]
[[neg a   d...] [. c...] r] -> [[(-a)        d...] [c...] r]
[[*   a b d...] [. c...] r] -> [[(a*b)       d...] [c...] r]  # signed
[[/%  a b d...] [. c...] r] -> [[(a%b) (a/b) d...] [c...] r]  # maybe signed
[[<<  a b d...] [. c...] r] -> [[(a<<b)      d...] [c...] r]
[[>>  a b d...] [. c...] r] -> [[(a>>b)      d...] [c...] r]  # signed
[[and a b d...] [. c...] r] -> [[(a&b)       d...] [c...] r]
[[xor a b d...] [. c...] r] -> [[(a^b)       d...] [c...] r]
[[~   a   d...] [. c...] r] -> [[~a          d...] [c...] r]
[[<   a b d...] [. c...] r] -> [[<0|1>       d...] [c...] r]  # signed
```

#### String operations
```
[[str  n     d...] [. c...] r] -> [[s d...] [c...] r]   # new string of n bytes
[[slen s     d...] [. c...] r] -> [[n d...] [c...] r]   # string length
[[sget i s   d...] [. c...] r] -> [[n d...] [c...] r]   # byte at i
[[sset x i s d...] [. c...] r] -> [[s d...] [c...] r]   # set byte at i

[[strsym str d...] [. c...] r] -> [[sym d...] [c...] r] # string to symbol
[[symstr sym d...] [. c...] r] -> [[str d...] [c...] r] # symbol to string
```

## How the resolver works
The resolver isn't typically used at runtime for performance reasons; normally
by the time a program is running it will have been reduced to a list of native
integers and list references.
