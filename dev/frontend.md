# phi frontend syntax + API
Time to engineer a way to use this language aside from writing concatenative
code. We have an [abstract interpreter](../phiabstract.pm), which is useful both
for backend optimization and for hypothetical evaluation, e.g. for IDE features.

phi is an unusual language in that its parser is extensible by runtime values.
This is how the entire language is parsed; for instance, if you write `3`, this
is immediately evaluated into an abstract integer and the parse continuation is
dictated by that value. This coupling makes it possible to write things like
`3mm`: `mm` converts numbers to dimensions.

Under the hood phi's syntax is incredibly simple. We have a series of literals
we can parse (encoded in the parse state), and we delegate to a parse
continuation as soon as we parse a value. The parse state stores information
about lexical scopes, local bindings, and operator precedence.

## Abstract value API
Abstract values provide a `parse-continuation` method that takes an operator
and returns a parser. That parser should provide an empty-succeed as an
alternative unless a suffix is absolutely required. Then every value is parsed
like this using a parser flatmap:

```
value = expr >>= .parse-continuation nil
binop = value <op> (expr >>= .parse-continuation <op>)
```

A phi program, then, is simply this transformation applied to the global
definition of `expr`.

## The parse state in some detail
String parse states always contain two elements, the string and the offset; but
in phi's case they contain a bit more:

```
parse state = [str offset context]
```

The context is an [object](../phiobj.pm) that manages two things:

1. The full lexical scope chain
2. The list of syntactic literals we can parse

### Operator precedence
Let's talk about how this works:

```
1 | 3 + 4 >> 5 * 6
 ^   ^   ^    ^
 A   B   C    D
```

At point `A` the surrounding precedence is undefined/nil, so `1`'s parse
continuation can generate the full range of operators. This changes by parse
point `B`: the surrounding precedence is `|`, so only higher precedence or right
associative same-precedence operators can be generated. Everything to the right
falls into this category.

All of this is managed by the argument passed to `parse-continuation`, which is
just the operator itself. For example, at parse point `B` above we'd have this:

```
3.parse-continuation("|")
```
