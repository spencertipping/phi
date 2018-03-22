# Core problem: dialects and values
A continuation from [evaluation.md](evaluation.md).

Ops can't generate syntax delegates from their values directly because they
would be unaware of the inflection of the local environment. For example, I
might be writing a section of code in the Python dialect, which should locally
change the parse continuations and in some cases the behavior of the language.

...now, I also want something which at this point is incompatible: as I type
expressions, I want the parse continuation to reflect the fully-evaluated state
of the values I'm working with. Because values transcend dialects, there's no
clear way for the value/evaluator to know which dialect is appropriate for the
given context. It's a lexical/dynamic scoping problem.

Some ways to solve it:

## 1. Replace `op` with `context` in `parse_continuation` and `postfix_modify`
Then the context would provide dialect information:

```
context {
  op ...            # NB: this is a syntactic op, not a value-op
  dialect ...
}
```

Then values are always cast into the lexically-appropriate dialect, and the
dialect becomes part of the scope chain.

This is nice because it generalizes the `op` stuff, which needs to happen
anyway, and it follows the intuition that semantics are consistent within a
scope.

Do all values respond to dialects? I suppose they can specify their preference
and the dialect can take that into account. For example, if we have a value
resident within a Perl runtime, we'll want to continue to apply Perl semantics
to it; it doesn't necessarily behave like a local value because phi can't
guarantee behavior the same way. (Is this true?)

...basically, though, dialects should give us enough flexibility to make
everything work, one way or another.

## What is a dialect, structurally?
It's an object, but let's figure out what it's responsible for. At a high level,
a dialect's job is to modify a value's parser behavior to convincingly model a
target language. For example:

```
# python dialect
x = {}
if 'foo' in x: print "uh oh"

# ocaml-ish dialect
let x = {} in
if Hashtbl.contains x "foo" then
  print_string "uh oh\n"

# perl dialect
my %x;
print "uh oh" if exists $x{foo};
```

I threw ocaml in there because it and Python conflict about the meaning of the
word `in` -- so dialects need to have enough force to resolve things like this.
On the other hand, there are cases where dialects have no business trying to
override things:

```
# python dialect
x = 5mm * 6mm * 7mm
print x.volume()                        # prints "210mm³"

# ocaml-ish dialect
let x = 5mm * 6mm * 7mm in
print_string (
  string_of_float Dimension.volume(x)
  ^ Dimension.unit_string(x))           # prints "210.0mm³" or something

# perl dialect
my $x = 5mm * 6mm * 7mm;
print $x->volume;                       # prints "210mm³"
```

From the syntactic perspective, `mm` is a grammar extension while `.volume()` is
a semantic one. This forces a few aspects of our design:

1. Normal operators are owned by the dialect and translated into semantics
2. Extensions are owned by the value and translated into semantics
3. Do "semantics" correspond to op nodes?

Before I get into (3), I think a simple way to get (1) and (2) is just to have
the dialect add an alternative to each value's parse continuation. So any
regular value with no exceptional operators simply fails directly into the
dialect. (Also, the dialect should answer questions about operator precedence to
help exceptional operators integrate well.)

Ok, let's talk about (3) because it's nontrivial. It's actually quite awkward
thanks to Python, which provides this beautiful syntax:

```py
if 2 < 3 < 4 > 3 > 2: print "aw yeah"
```

### Semantics and evaluation
We have two degrees of nonlinearity, or at least nonlocality:

1. Syntax -> semantics
2. Semantics -> evaluation

(1) is required by expressions like `2 < 3 < 4` in Python: the syntactic stream
is `((< 3), (< 4))`, but the semantic behavior is `((< 3) && (< 4))`. We need
the folding to be syntactic rather than semantic to handle cases like this:

```
# C dialect
int y = ...;
int x = y < 4;

# python dialect
if x < 5: print "..."
```

Python's semantic folding of multiple `<` happens only under two conditions:
first, every expression involved needs to be in the Python dialect; and second,
every expression also needs to be non-parenthetically present in the same
reduction. In other words, the `<`-collapsing stuff behaves like syntax and not
semantics.

From a source-parsing point of view we would approach this as a continuation
that matched multiple `'<' expr` or `'>' expr` followers. Python's dialect is at
liberty to provide this behavior for general expressions, and on a syntactic
basis.

Semantics -> evaluation is nonlinear not because of Python's `<` collapsing, but
rather to handle peephole optimization and semantic dialect conversions.

### Semantic parsers
If we look at expression trees from the LHS's point of view, we end up with a
semantic stream of sorts:

```
x.foo().bar().bif() + 1
```

Here, the stream is `(.foo(), .bar(), .bif(), (+ 1))`. It isn't a strictly
linear quantity, but we can still parse it -- and this is how evaluation and
compilation ultimately happen.

While syntax is lexically scoped, semantics clearly aren't -- and that means
semantic parsers also aren't. This means that semantics and dialects are
distinct abstractions, which is unfortunate.

## Dialects, in the ideal case
A user should be able to look at a dialect as a layer of syntax and semantics
that make things like Python, or Perl, or whatever, Just Work (TM) within your
code. For example, once I've loaded the Python and C++ dialects, I should be
able to do this:

```
use c++
use python

in c++ {
  #include <string>
  #include <unordered_map>
  std::unordered_map<std::string, int> x;
  x.emplace("foo", 1);
};

in python:
  if 'foo' in x:
    print "ok"
```

...and there are three ways it might work:

1. Compile both dialects to phi, and run anywhere (ideal case)
2. Compile Python to C++, then merge the two into the C++ runtime
3. Run C++ and Python in separate processes and use RMI

We don't care at the syntactic level, but it's precisely because the dialects
are defining enough semantics to take care of it for us.
