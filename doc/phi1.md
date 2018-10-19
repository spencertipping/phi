# phi1 spec
phi1 needs to be thin and syntax-compatible with phi2. This makes it possible to
reuse most/all phi1 code even though we can't reuse the implementation of phi1.

There are three big things we need from phi1:

1. Automatic frame management (allocation + class generation)
2. Expressions (e.g. `(3 + 4) * 5`)
3. Block parsing + linking

## Simplified CTTIs
phi1 and phi2 use completely different mechanisms to parse things, but we can
design them to have a syntactic intersection that's usable for bootstrapping. We
just need to keep the feature set small.

Let's start by reducing the world to three types:

1. `ptr`: a pointer to an object
2. `hereptr`: a pointer to something we can `unhere` to get an object
3. `int`: a thing that isn't a pointer

phi2 will need CTTI definitions for most values because assignment is
lvalue-polymorphic. So we want a syntax that provides CTTI for every lvalue,
including arguments and returns.

phi1 and phi2 have different but sometimes-compatible semantics in some
important cases. phi1 implements all casts as copy operations, and there is no
arity or type checking. Pointers are also untyped; all objects and structs are
encoded as opaque `ptr` types. This means every method call goes through runtime
polymorphic dispatch.

## Syntax
phi2 implements syntax using dialects, which own the grammar and delegate to
abstract values for syntactic interpolation. phi1 is basically phi2 without any
interpolating abstracts and with a single fixed dialect.

I also want to keep phi1 as simple as humanly possible, so I'm sidestepping
operator precedence and special casing by going with a Lisp-style grammar. For
example:

```
(def f
  (fn (int a int b)
    (let ((int x a)
          (ptr y (+ x 1)))
      (= x (g64 y))
      (= x (+ x 1))
      (return x))))
```

## Simplifying deviations from phi2
1. Every expression leaves a single value on the stack
2. Every value occupies a single stack cell
3. Every value occupies a single frame slot (64 bits)
4. Lisp macros don't exist
5. Abstracts don't interpolate anything into the grammar
6. Abstracts are semantic passthroughs: no optimizations or reductions
7. There is only one protocol for polymorphic OOP dispatch
