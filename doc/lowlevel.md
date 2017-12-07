# Low level design
phi is a concatenative graph language based on combinatory parsers contained
within a _scope_. A scope is structurally a list, but it behaves as a function
by the following logic:

```
apply(scope, val) =
  return apply(scope.h, val) or apply(scope.t, val) if scope is cons
  return destructure_and_apply(scope, val)          otherwise
```

So the scope will end up being flattened and its elements individually applied
to the value until one accepts (in parser terms).

Scopes contain two types of parsers:

- phi string -> phi value (source parsers)
- phi value -> phi value (structural parsers)

## Values
phi understands the following kinds of values:

- `int(n)` primitive
- `string(s)` primitive
- `symbol(name)` primitive: a specific symbol
- `variable(name)` primitive: a name for a value (unlike a symbol)
- `nil`
- `cons(x, y)`
- `rewrite(lhs, rhs)`: a rewrite rule written in phi
- `rewrite_native(f)`: a native function that can rewrite phi values
- `op(x, op, y)`: some type of operation applied against two values
- `object(id)`: a unique object that matches only itself

**Q:** can we represent ops and rewrites in terms of conses against magic
objects?

Internally, the interpreter has another form to handle circular graph
references:

- `forward(v)`: a reference to a value that may or may not yet be defined

phi resolves `forward` values so you don't normally interact with them, but it's
worth knowing that they exist.

## Functions and rewriting
phi doesn't have anything resembling functions in most languages, but you can
emulate function behavior by creating a rewrite rule that consumes "call"
operations and emits an expanded value. Here's the difference:

```
# most languages:
f = function x -> x + 1

# phi (pseudocode):
scope += fn op(symbol("f"), symbol("call"), variable("x")) -> x + 1
```

Everything in phi is implemented this way, including variables, which means that
rewrites are lexically scoped.
