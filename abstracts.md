# Abstract values
An "abstract" value is one that is only partially defined. This is in contrast
to a "concrete" value, which is what programs would manipulate at runtime.

Abstracts are used by the compiler to track what we _do_ know about values,
which may include things like its type, or the values of some of its fields. For
example:

```
x;                  # a completely abstract quantity
x:int;              # an abstract int
[x:int, y:int];     # an abstract list
x:(int|float);      # an abstract value, either an int or a float
```

Types are first-class values that specify things like how they're encoded and
how to access various sub-fields.

Abstracts are themselves concrete values, but backend primitives are polymorphic
wrt abstract-ness.

**Q:** is this true? What if a function's return arity differs depending on an
unspecified input quantity? Are we saying it's impossible to define such a
function? (Possibly, and that might be OK.)
