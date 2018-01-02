# Concatenative basis design
**TODO:** symbols and strings can be merged

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
d = [5],     c = [[x*inc]]
d = [5],     c = [[dup inc *] []] <- replaced by definition
d = [5 5],   c = [[inc *] []]
d = [5 5],   c = [[1 +] [*] []]
d = [1 5 5], c = [[+] [*] []]
d = [6 5],   c = [[] [*] []]      <- end of function
d = [6 5],   c = [[*] []]         <- ...drop the nil
d = [30],    c = [[] []]
d = [30],    c = [[]]
d = [30],    c = []               <- end of evaluation
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
!     -> [0] [1] if
```

Here are the codes for each of the operators below:

| Code   | Name      | Description         |
|--------|-----------|---------------------|
| `0x00` | `i>`      | Quote interpreter   |
| `0x01` | `i<`      | Unquote interpreter |
| `0x02` | `.`       | `eval`              |
| `0x03` | `if`      | If-then-else        |
| `0x04` | `type`    | Get type of a value |
| `0x05` | `==`      | Physical equality   |
| `0x06` | `cons`    | Make a cons         |
| `0x07` | `uncons`  | Invert `cons`       |
| `0x08` | `restack` | Rearrange stack     |
| `0x09` | `mut`     | Create a mutable    |
| `0x0a` | `mset`    | Set a mutable       |
|--------|-----------|---------------------|
| `0x10` | `+`       | Integer add         |
| `0x11` | `neg`     | Integer negate      |
| `0x12` | `*`       | Integer multiply    |
| `0x13` | `/%`      | Integer divmod      |
| `0x14` | `<<`      | Integer left-shift  |
| `0x15` | `>>`      | Integer right-shift |
| `0x16` | `and`     | Integer bitwise and |
| `0x17` | `xor`     | Integer bitwise xor |
| `0x18` | `~`       | Integer bit invert  |
| `0x19` | `<`       | Integer less-than   |
|--------|-----------|---------------------|
| `0x20` | `str`     | Make a string       |
| `0x21` | `slen`    | String length       |
| `0x22` | `sget`    | String byte get     |
| `0x23` | `sset`    | String byte set     |
| `0x24` | `scmp`    | String byte compare |
| `0x25` | `strsym`  | String to symbol    |
| `0x26` | `symstr`  | Symbol to string    |
| `0x27` | `sym=`    | Symbol equality     |

Numbers below `0x100` are reserved for future low-level expansion, and `0x100`
and above are used for backend-specific bindings.

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

#### Mutability
phi doesn't provide general structure mutability, but it does give you a way to
introduce circular references into data structures. There are two operators
involved:

```
[[mut             d...] [. c...] r] -> [[<a cell> d...] [c...] r]
[[mset x <a cell> d...] [. c...] r] -> [[<cell x> d...] [c...] r]
```

Once you've set a cell's value, that cell is a complete passthrough: it behaves
exactly like the value it contains, and you can no longer detect the fact that
you had a cell to start with. A cell can be set only once.

Cells will generate fatal errors if:

- You use an unset cell in any operation other than `type` or `mset`
- You try to `mset` an already-set cell

#### List operations
```
[[cons   a b        d...] [. c...] r] -> [[cons(a, b) d...] [c...] r]
[[uncons cons(a, b) d...] [. c...] r] -> [[a b        d...] [c...] r]
```

**NB:** A common idiom for quoted values is to place them inside a list and then
`uncons` them: `[5] uncons swap drop` == `5`, but this will prevent `5` from
being executed. This is how literal numbers work internally.

#### Stack operations
phi's higher-level compiler generates stack shuffling operations, which means
there's no purpose in having them be especially human-friendly. Instead of the
usual `swap`, `dup`, etc, phi provides a single `restack` operator that takes a
string argument:

```
"201" restack             # drops two entries, pushes them in opposite order
```

If the usual stack operators were builtins, they'd work like this:

```
[[dup   a     d...] [. c...] r] -> [[a a   d...] [c...] r]
[[drop  a     d...] [. c...] r] -> [[      d...] [c...] r]
[[swap  a b   d...] [. c...] r] -> [[b a   d...] [c...] r]
[[rot3< a b c d...] [. c...] r] -> [[b c a d...] [c...] r]
[[rot3> a b c d...] [. c...] r] -> [[c a b d...] [c...] r]
```

With `restack`, they're defined this way:

```
dup   = ["00"   restack]
drop  = ["1"    restack]
swap  = ["201"  restack]
rot3< = ["3102" restack]
rot3> = ["3021" restack]
```

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
[[scmp s1 s2 d...] [. c...] r] -> [[n d...] [c...] r]   # compare bytes

[[strsym str d...] [. c...] r] -> [[sym   d...] [c...] r] # string to symbol
[[symstr sym d...] [. c...] r] -> [[str   d...] [c...] r] # symbol to string
[[sym= s1 s2 d...] [. c...] r] -> [[<0|1> d...] [c...] r] # symbol compare
```

## How the resolver works
The resolver isn't typically used at runtime for performance reasons; normally
by the time a program is running it will have been reduced to a list of native
integers and list references. However, nothing stops you from relying on `r` to
interpret stuff while you're building up an image.

`r` is called like a regular function and phi is concatenative, so it's made up
of basis functions that, when composed, form an alternation structure that
matches symbols. So, if we have symbol `s` bound to list `[l]`, one matcher
element might work like this (where `x` is the stack top being matched):

```
x [dup [s] head sym= [drop [l]] [] if] .
```

`sym=` will return `0` if either argument is a non-symbol, so the above strategy
will work for the fallthrough case as well. There are just two problems with
what we have:

1. It's a pain to build up that specific structure for every binding we want to
   create.
2. It's possible to bind a symbol to a non-list value, which could have
   unintended consequences.

To fix this, let's change the representation a little by getting more leverage
out of list quoting; here's the basic idea:

```
r = [[[sym def...] [sym def...] ...] code...]
```

This is great because it's trivial to add new definitions, and the definitions
will be cons cells unless the lists are improper. So what does the code look
like?

### Resolver loop
We obviously can't rely on the resolver when executing the resolver, so anything
recursive here needs to be implemented as a circular reference. When the code
runs, we'll have a symbol and our list on the stack; here's what we do from
there:

```
# stack = ... mystery-symbol [[sym def...] ...]
# TODO
```
