# Low level design
phi is a concatenative graph language based on combinatory parsers contained
within a _scope_. A scope is structurally a list, but it behaves as a function
by the following logic:

```
apply(scope, val) = apply_one(scope.h, val) || apply(scope.t, val)
apply_one(cons(rewriter_op, cons(lhs, rhs)), val) =
  fail unless lhs matches val
  rewrite rhs, lhs match val
```

## Values
phi understands the following kinds of values:

- `int(n)` primitive
- `string(s)` primitive
- `object(id)`: a unique object that you can't construct
- `cons(x, y)`: a cons cell; `int(0)` functions as nil

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
scope += cons(rewriter_op,
              cons(cons(call_op, cons(cons(symbol_op,   string("f"))
                                      cons(variable_op, string("x")))),
                   cons(cons(call_op,
                             cons(method_op,
                                  cons(cons(variable_op, string("x")),
                                       string("plus")),
                             cons(quote_op, int(1)))))))

# phi (syntactic):
f x = x + 1
```

Everything in phi is implemented this way, including variables, which means that
rewrites are lexically scoped. And the mechanism that makes phi even remotely
usable is the _parse continuation_, which allows values to insert custom
parsers.

### How rewriting works
Let's talk about what happens when you write `f x = x + 1`, and let's simplify
that a bit by using low-level syntax: `f x = x.plus 1`.

When you first write `f`, you'll get `cons(symbol_op, string("f"))`: the scope
parses unknown symbols into these objects. Its parse continuation includes
variables (which is how `x` is consumed) and `=`. Once we hit `=` we get a new
parse continuation, this time one that binds `f` to a forward reference, and `x`
to `cons(variable_op, string("x"))`. Normally, unbound identifiers would be
encoded as `cons(symbol_op, string("x"))`.

Now let's suppose we later write `f 5`. `f` is a symbol and `5` is part of its
parse continuation; we'll get this:

```
cons(call_op, cons(cons(symbol_op, string("f")),
                   cons(quote_op, int(5))))
```

This matches the left-hand side of the rewriter created by `f x = x.plus 1`, so
we destructure it against the call and request the following rewrite:

```
cons(rewrite_op, cons(cons(call_op,
                           cons(cons(method_op,
                                     cons(cons(variable_op, string("x")),
                                          string("plus"))),
                                cons(quote_op, int(1)))),
                      cons(cons(cons(variable_op, string("x")),
                                cons(quote_op, int(5))),
                           int(0))))
```

## Parse continuations
phi evaluates your code at parse-time and asks values if they want to take over
the parse. This allows you to define custom type-specific (or value-specific)
suffixes for things; for instance, you could define a parse continuation for
integers that allowed you to write `4mm` and turn that into a custom operation
like `cons(method_op, cons(cons(quote_op, int(4)), string("mm")))`.

phi uses parse continuations throughout the language to implement things like
locally-extended lexical scopes, variables, method syntax, call syntax, and
overloadable operators. Parse continuations are implemented by the reader asking
the scope to match a value that looks like this:

```
cons(parse_continuation_op,
     cons(v, cons(scope, cons(string("source"), int(offset)))))
```

If the scope rewrites that value, it should return `cons(v', cons(s', offset'))`
as the parse result. phi's reader will die if the scope matches and returns a
different type of value.

## Quoted vs unquoted forms
phi values are almost always quoted. For example, writing `5` will produce
`cons(quote_op, int(5))`; that is, phi generates _a description of `5`_ rather
than `5` without context. This makes it possible for you to structurally parse
quoted forms to implement the evaluation model, which is exactly how phi works.
For example, you can implement rules like `cons(match_op, cons(x, y))` to
describe how to destructure things.
