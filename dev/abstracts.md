# Abstract values
The basic idea here is to model phi's state of knowledge about a program,
allowing uncertainties. For example, you know some things about the behavior of
most functions even if you don't know their inputs:

```
f(x) = cons(x, nil)
f(x).tail             # we probably know what this value is
```

There are three basic types of abstracts:

1. Constants, which are fully specified and behave exactly like phi's builtin
   types (note that the `cons` above is considered to be a constant even though
   it refers to unspecified values)
2. Unknowns, which are fully _un_specified
3. Unions, which contain two possibilities and a determinant and ultimately
   resolve to one of those two possibilities once the determinant is known (or
   they compile into a runtime `if`)

Abstract values drive phi's compilation backends and are used to fold constants
and erase most of the interpreter's overhead. Unlike most constant folding
systems, abstract values actually execute your code and simulate a full
interpreter; this means everything is fair game, including functions that
manipulate the continuation stack, things that use the resolver, most
monomorphic method call sites, recursion, etc. Ultimately, abstract values
convert interpreters into compilers because the interpreter's state is itself
abstract.
