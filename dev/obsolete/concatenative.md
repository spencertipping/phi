# Concatenative basis design
phi's base concatenative layer has just enough machinery to make things work. We
don't particularly have to worry about performance yet; that will be handled by
backend-specific compilers.

A phi value is one of the following:

- An integer
- A real number
- A string (written in quotes)
- A symbol (not quoted)
- `nil`
- A cons cell; lists are written `[1 2]` to mean `cons(1, cons(2, nil))`
- A mut cell
- An opaque native value

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
-   -> neg +
or  -> ~ swap ~ and ~
>   -> swap <
```

Here are the codes for each of the operators along with the spec version that
introduced each one. The contract is that, like Linux syscalls and x86
instructions, interpreters/compilers at version X will support every instruction
at every version from 0 to X: backwards compatibility is fully guaranteed.

| Code   | Name      | Version | Description         |
|--------|-----------|---------|---------------------|
| `0x00` | `i>`      | 0       | Quote interpreter   |
| `0x01` | `c<`      | 0       | Set continuation    |
| `0x02` | `.`       | 0       | `eval`              |
| `0x03` | `type`    | 0       | Get type of a value |
| `0x04` | `id`      | EXP     | Unique value ID     |
| `0x05` | `cons`    | 0       | Make a cons         |
| `0x06` | `uncons`  | 0       | Invert `cons`       |
| `0x07` | `restack` | 0       | Rearrange stack     |
| `0x08` | `mut`     | 0       | Create a mutable    |
| `0x09` | `mset`    | 0       | Set a mutable       |
| `0x0a` | `d<`      | 0       | Set data stack      |
| `0x0b` | `r<`      | 0       | Set resolver        |
| `0x0c` | `if`      | 0       | Conditional         |

| Code   | Name      | Version | Description         |
|--------|-----------|---------|---------------------|
| `0x10` | `+`       | 0       | Integer add         |
| `0x11` | `neg`     | 0       | Integer negate      |
| `0x12` | `*`       | 0       | Integer multiply    |
| `0x13` | `/%`      | 1       | Integer divmod      |
| `0x14` | `<<`      | 0       | Integer left-shift  |
| `0x15` | `>>`      | 0       | Integer right-shift |
| `0x16` | `and`     | 0       | Integer bitwise and |
| `0x17` | `xor`     | 0       | Integer bitwise xor |
| `0x18` | `~`       | 0       | Integer bit invert  |
| `0x19` | `<`       | 0       | Integer less-than   |
| `0x1a` | `not`     | 0       | Integer 1/0 not     |

| Code   | Name      | Version | Description         |
|--------|-----------|---------|---------------------|
| `0x20` | `str`     | 0       | Make a string       |
| `0x21` | `slen`    | 0       | String length       |
| `0x22` | `sget`    | 0       | String byte get     |
| `0x23` | `sset`    | 0       | String byte set     |
| `0x24` | `scmp`    | 0       | String byte compare |
| `0x25` | `strsym`  | 0       | String to symbol    |
| `0x26` | `symstr`  | 0       | Symbol to string    |
| `0x27` | `sym=`    | 0       | Symbol equality     |
| `0x28` | `strcpy`  | 0       | String copy         |

| Code   | Name      | Version | Description         |
|--------|-----------|---------|---------------------|
| `0x30` | `+.`      | 0       | Real add            |
| `0x31` | `neg.`    | 0       | Real negate         |
| `0x32` | `*.`      | 0       | Real multiply       |
| `0x33` | `/.`      | 0       | Real divide         |
| `0x34` | `i>r`     | 0       | Integer -> real     |
| `0x35` | `r>i`     | 0       | Real -> integer     |
| `0x36` | `b>r`     | 0       | Bits -> real        |
| `0x37` | `r>b`     | 0       | Real -> bits        |
| `0x38` | `log`     | 0       | Natural log         |
| `0x39` | `<.`      | 0       | Real less-than      |
| `0x3a` | `sqrt`    | 0       | Square root         |
| `0x3b` | `exp`     | 0       | Exponential         |

| Code   | Name      | Version | Description         |
|--------|-----------|---------|---------------------|
| `0x40` | `version` | 0       | Version number      |
| `0x41` | `crash`   | 0       | Crash the program   |
| `0x42` | `qext`    | 0       | Query for extension |

Numbers below `0x100` are reserved for future low-level expansion, and `0x100`
and above are used for backend-specific bindings.

`version` is an important instruction that lets your program figure out which
core instructions are defined. It serves the same purpose as x86 `CPUID`.
Versions don't need to be strictly linear, but they do specify which extended
instructions are available. Anytime you use extended instructions (such as a
revised op table spec), you should first check the version to verify, or be
prepared for your thing to crash if the interpreter isn't the right version.

Operators whose version is `EXP` in the table above are experimental and will
probably change. They probably suck in some way.

`crash` is an instruction that is illegal to execute. As such, you can assume
that it will never arise in valid code and use this to optimize things. It's
sometimes used for assertions, so interpreters should provide some indication
that this instruction has been run.

`qext` queries for specific extensions. It takes a symbol and returns `1` to
indicate that the given extensions are available, `0` to indicate that they're
unavailable.

#### POSIX backend functions (extension `posix_fileio`)
If this extension exists, the following instructions will be defined:

| Code    | Name      | Description                           |
|---------|-----------|---------------------------------------|
| `0x110` | `open`    | `fd|-1 = open(filename, flags, mode)` |
| `0x111` | `read`    | `size|-1 = read(fd, str, offset, n)`  |

#### Literals
Numbers behave as functions, so you can't have a number in the middle of a list
and expect for it to behave as data. That is, the list `[1 2 3]`, if you
evaluated it, would run three native functions instead of pushing three numbers
onto the stack.

As a result, literal numbers need to use a cons-quoting mechanism:
`[1] uncons swap drop` will leave the number `1` on the stack without evaluating
it. Symbols work the same way. In `restack` terms we have this:

```
'x = [x] uncons [2 1] restack
```

#### Undefined behavior and crashes
**WARNING:** Native functions have undefined behavior if you misuse them;
examples include:

- Accessing string chars out of bounds
- Restacking beyond end of stack
- Unconsing something that isn't a cons
- Using integer operations on non-integers
- Referring to undefined instructions

In practice, the best-case scenario is that your program dies instantly with an
error. Worst case is, as in C, memory corruption of an unspecified nature.

**phi often assumes illegal operations, including the crash instruction, are
unreachable.** This means phi is allowed to remove any control flow branches
leading to a crash. This means `crash` shouldn't be used for user-facing error
reporting (including debugging assertions); instead, you should exit nonzero and
print to stderr or similar. If the runtime doesn't provide `exit`, you can
simulate it by pushing the exit code and clearing the continuation stack using
`cset`.

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
`int`, `string`, `symbol`, `cons`, `mut`, and `nil`.

phi represents booleans as the integers `0` and `1`.

```
[[type a d...] [. c...] r] -> [[<type-symbol> d...] [c...] r]
```

`id` gives you a symbol for a given value, guaranteed to be unique for the
lifetime of the _runtime that created it_. This operator is experimental.

```
[[id x d...] [. c...] r] -> [[<symbol> d...] [c...] r]
```

#### Mutability
phi doesn't provide general structure mutability, but it does give you a way to
introduce circular references into data structures. There are two operators
involved:

```
[[mut             d...] [. c...] r] -> [[<a cell> d...] [c...] r]
[[mset <a cell> x d...] [. c...] r] -> [[<cell x> d...] [c...] r]
```

Once you've set a cell's value, that cell is a complete passthrough: it behaves
exactly like the value it contains, and you can no longer detect the fact that
you had a cell to start with. A cell can be set only once.

Cells will generate fatal errors if:

- You use an unset cell in any operation other than `type` or `mset`
- You try to `mset` an already-set cell

**TODO:** create an `mget` instruction to dereference exactly one layer; this is
required if we have `mut(unset_mut)`.

#### List operations
```
[[cons   a b        d...] [. c...] r] -> [[cons(a, b) d...] [c...] r]
[[uncons cons(a, b) d...] [. c...] r] -> [[a b        d...] [c...] r]
```

#### Stack operations
phi's higher-level compiler generates stack shuffling operations, which means
there's no purpose in having them be especially human-friendly. Instead of the
usual `swap`, `dup`, etc, phi provides a single `restack` operator that takes a
list argument whose head is the drop count and whose tail is a layout:

```
[2 0 1] restack       # drops two entries, pushes them in opposite order
```

If the usual stack operators were builtins, they'd work like this:

```
[[dup   a     d...] [. c...] r] -> [[a a   d...] [c...] r]
[[drop  a     d...] [. c...] r] -> [[      d...] [c...] r]
[[swap  a b   d...] [. c...] r] -> [[b a   d...] [c...] r]
[[rot3< a b c d...] [. c...] r] -> [[b c a d...] [c...] r]
[[rot3> a b c d...] [. c...] r] -> [[c a b d...] [c...] r]
```

Each of them can be defined in terms of `restack`:

```
dup   = [[0 0]     restack]
drop  = [[1]       restack]
swap  = [[2 1 0]   restack]
rot3< = [[3 2 0 1] restack]
rot3> = [[3 1 2 0] restack]
```

`if` and other conditionals can also be defined in terms of `restack`:

```
# <0|1> [then] [else] rot3<              = [then] [else] <0|1>
# [then] [else] <0|1> [] swap cons       = [then] [else] [<0|1>]
# [then] [else] [<0|1>] 2 cons restack . = [<then|else>] .

if = [rot3< [] swap cons 2 cons restack .]
```

To be safe, `if` should normalize its argument:

```
if = [rot3< not not [] swap cons 2 cons restack .]
```

This definition is equivalent to the builtin `if` instruction, which is provided
for pre-optimization performance.

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
[[<   a b d...] [. c...] r] -> [[(a<b?1:0)   d...] [c...] r]  # signed
[[not a   d...] [. c...] r] -> [[<0|1>       d...] [c...] r]
```

`not` behaves like the C `!` operator, always returning `0` or `1`.

#### Real operations
These are mostly self-explanatory, but `r>b` and `b>r` deserve some discussion.
These functions convert between floats and strings, which allows you to access
and modify the internals of floats. Floats are always stored at double
precision and in native format, whichever endianness that is.

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

`str` initializes each byte to `0`.

The only reason we have both strings and symbols is for optimization: symbols
are immutable and have predictable behavior in the continuation stack. (It's a
lot easier for compilers if their input data structures are all immutable.)

```
[[strcpy len tooffset tostr fromoffset fromstr d...] [. c...] r]
  -> [[tostr d...] [c...] r]
```

`strcpy` does exactly what C's `memcpy` does. All offsets/lengths are in terms
of bytes.

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
# x [[s d...] ...] uncons uncons       = x [...] [d...] s
# x [...] [d...] s [3 0 1 2] 3 restack = x [...] [d...] s x
# x [...] [d...] s x sym=              = x [...] [d...] <1|0>
# x [...] [d...] <1|0> [[0] 3 restack] [drop <resolver-code>] if
#   = [d...]
#   | x [...] <resolver-code>
```

The only remaining problem is that we might go through the whole binding list
without finding a match; in that case we should return the symbol.

```
# x [...] dup type 'nil sym=    = x [...] <0|1>
# x [...] <0|1> [drop] [uncons uncons ...] if
```

Now we can put it all together, complete with the circular reference:

```
<resolver-code> =
  dup type 'nil sym=
    [drop]
    [uncons uncons [3 0 1 2] 3 restack sym=
     [[0] 3 restack] [drop <resolver-code>] if] if
```

## How data types work
Linked lists don't give you fast access to anything, especially when cons cells
are immutable. If phi is going to be at all fast, it needs support for custom
memory layouts and proper data structures -- at the very least, arrays. And phi
does indeed support this as a layer on top of conses.

Data type encoding is managed by optimization parsers, which choose alternatives
depending on various characteristics of backends.

Importantly, data types aren't a special-case optimization phi makes. A
concatenative -> concatenative compiler running over abstract values can do a
bunch of inlining and decision flattening that creates serial instructions that
can apply to whatever model makes sense for the hosting backend (usually
memory-as-strings).

**TODO:** figure out how type indicators work. I think we can use an abstract
projection and compile-time constant folding to remove the type indicator field
when we don't need it.

### Quick example: array of ints
Semantically, int arrays support the following operations:

- `n new -> array`
- _internal:_ `array [markfn] gc_trace ->`
- _internal:_ `array markval oldp newp gc_notify_move ->`
- _internal:_ `array gc_size -> (n*sizeof(int))`
- `array i get`
- `array x i set`
- `array length -> n`

Now we need to specify how these operations work using strings as memory
buffers.

```
# assume i64get/set are defined in terms of sget/sset and bitshifts
[[new             1 + dup 8 * str 0 i64set]   # give or take
 [gc_trace        [] 2 restack]               # drop two
 [gc_notify_move  [] 4 restack]
 [gc_size         0 i64get]
 [get             8 + i64get]
 ...]
```

This list is a type implementation spec, in this case for C and other
flat-memory languages where phi provides the garbage collector. A full type
value contains potentially many implementation specs.

### Quick example: cons cell
Cons cells can refer to other values, so stuff about pointers starts to matter
here. The set of operations looks like this:

- `h t cons -> cell`
- _internal:_ `cell [markfn] gc_trace ->`: `h 0 markfn t 1 markfn`
- _internal:_ `cell markval oldp newp gc_notify_move ->`
  - if `markval == 0`, update head
  - if `markval == 1`, update tail
- _internal:_ `cell gc_size -> 2*sizeof(void*)`
- `cell head`
- `cell tail`

Let's talk about how some of these functions work.

```
[[cons  16 str swap pointer->int 0 i64set swap pointer->int 8 i64set]
 [head  0 i64get int->pointer]
 [tail  8 i64get int->pointer]
 ...]
```

`pointer->int` and `int->pointer` aren't general-purpose functions you can use;
they're specific to flat-memory backends. They also have no specified
definition -- i.e. they don't appear in the resolver. The backend's
implementation parsers find these symbols and generate the appropriate
operations.

Interestingly, it's fine to do weird things with your pointers; just because we
have a GC doesn't mean you have to represent them verbatim. For example, you
could define a doubly linked list node with xor-ed forward/back pointers. Then
the `gc_notify_move` implementation would xor both old and new into the combined
pointer value. (Although I suppose you'd have to do some interesting stuff to
get `gc_trace` set up correctly.)
