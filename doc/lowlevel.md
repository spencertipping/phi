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
- `object(id)`: a unique object that you can't construct
- `cons(x, y)`: a cons cell; `int(0)` functions as nil
- `rewrite_native(f)`: a native function that can rewrite phi values

phi can give you `object` values, but you can't construct them yourself. This
solves the gensym problem and is used extensively throughout phi to provide
magic tags for values.

Internally, the interpreter has one last form to handle circular graph
references:

- `forward(v)`: a reference to a value that may or may not yet be defined

phi resolves `forward` values so you don't normally interact with them, but it's
worth knowing that they exist.

## Functions and rewriting
phi doesn't have anything resembling functions in most languages, but you can
emulate function behavior by creating a rewrite rule that consumes conses tagged
with the `call_op` object and emits an expanded value. Here's the difference:

```
# most languages:
f = function x -> x + 1

# phi (structural):
scope += cons(rewrite_op_object,
              cons(cons(call_op_object, cons(cons(symbol_op,   string("f"))
                                             cons(variable_op, string("x")))),
                   cons(cons(call_op_object,
                             cons(method_op_object,
                                  cons(cons(quote_op_object, forward(x)),
                                       string("plus")),
                             cons(quote_op_object, int(1)))))))

# phi (syntactic):
f x = x + 1
```

Everything in phi is implemented this way, including variables, which means that
rewrites are lexically scoped. And the mechanism that makes phi even remotely
usable is the _parse continuation_, which allows values to insert custom
parsers.

## Parse continuations
phi evaluates your code at parse-time and asks values if they want to take over
the parse. This allows you to define custom type-specific (or value-specific)
suffixes for things; for instance, you could define a parse continuation for
integers that allowed you to write `4mm` and turn that into a custom operation
like `cons(method_op, cons(int(4), string("mm")))`.

phi uses parse continuations throughout the language to implement things like
locally-extended lexical scopes, variables, method syntax, call syntax, and
overloadable operators. Parse continuations are implemented by the reader asking
the scope to match a value that looks like this:

```
cons(parse_continuation_op,
     cons(v, cons(string("source"), int(offset))))
```

If the scope rewrites that value, it should return `cons(v', cons(s', offset'))`
as the parse result. phi's reader will die if the scope matches and returns a
different type of value.

## Quoted vs unquoted forms
phi values are almost always quoted. For example, writing `5` will produce
`cons(quote_op, int(5))`; that is, phi generates _a description of `5`_ rather
than `5` without context. This makes it possible for you to structurally parse
quoted forms to implement the evaluation model, which is exactly how phi works.
