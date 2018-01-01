# The problem right now
We've got a concatenative model (awesome), but we need extensible scopes and
parse continuations. Suppose we're parsing a list:

```
[ |x:int y:int| x + y ]
```

In order for `x + y` to have any hope of working, the `+` needs to be a part of
the parse continuation for `x` -- which means `|x:int y:int|` needs to modify
its continuation parser also. If we want this to happen without actually calling
the list, we have a couple of options:

1. `|x:int y:int|` itself parses the continuation `x + y`
2. `|x:int y:int|` defines a scope continuation that then encounters `x` and
   pushes an abstract `int` onto the stack

## Option (1): arg frames parse expressions
Some pros:

1. It's trivially straightforward: `|x:int y:int|` is just a value that either
   specifies a parse continuation, or more likely continues to parse the rest as
   a part of its grammar (using flatmapped parsers)
2. We can specify arbitrary structural expression grammars without requiring
   individual values' parse continuations to handle control flow/statement
   sequencing

Problems with this approach:

1. Can we do nullary functions? (sure, use `do`/`lambda` notation or something)
2. What happens if `int` isn't a fully-specified type? (no parse continuations)
3. How do function calls work for type inference? (still need an abstract-value
   compiler)
4. Is the underlying model still concatenative? (sure)
5. Scopes need to define open-ended parsers, which arguably they do anyway to
   handle syntactic literals.

I think this is the way to go.

OK, so really we're basing the language out in concatenative style, then
immediately writing enough parsers to write applicative/traditional grammars and
have that compile down to concatenative stuff. We need the concatenative base to
be able to flatmap parsers when they're predicated on things like types.

Scope and parse continuations don't really exist; these are encapsulated by
parsers themselves.

## Option (2): scope continuations
This is a terrible idea. Problems with this approach:

1. We're still fundamentally concatenative, which makes the intent of `+`
   ambiguous if a concatenative function of that name exists
2. We require some logic on the part of the language to handle "scope
   continuations" in the first place
3. We're delegating type-based parse continuations to the language, which means
   the language itself is doing abstract-value stuff
4. **We have no distinction between quoted-syntax and executed-syntax**
