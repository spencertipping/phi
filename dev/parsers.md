# Parsers
In order to get a sane applicative notation, we're going to need a parsing
expression grammar implemented in the concatenative layer. I thought about
writing the grammar in something like Perl up front, then using it to compile
itself into concatenative, but that's probably more work than concatenative
hand-coding and might result in suboptimal code.

## Parsing expression grammars, normally
There are three basic parts of most PEG systems:

- Terminal parsers: stuff like `str`, `charclass`, etc
- `seq`: a composition of parsers (this includes `repeat`)
- `alt`: a union of parse results, preferring early ones

Because `alt` commits to the first successful result, parsers can return values
as they go; we don't build a parse tree, we just build the results directly.
This does a lot to keep things simple.

## Avoiding final commits by storing continuations
**NB:** I'm not sure I really need this yet, but I'm designing it up front
because it's likely to be useful for interactive stuff like editors.

Parse tree mutability is a workaround for call stack linearity. PEGs can also
bypass final commits by having `alt` store the next continuation; then a parse
failure is just a tail call into a continuation if there is one. Here's an
example where we get backtracking:

```
grammar = ("foo" | "foo" "bar") $
```

This will normally fail for the input `foobar`:

```
(  "foo" -> ("foo", 3)
 | "foo" -> ("foo", 3) "bar" -> ("bar", 6)) = ("foo", 3)
$ -> fail
```

Now let's change it up: just before returning a new parse state, `alt` captures
the continuation that would have continued through alternatives. In Scheme it
would look like this:

```scheme
(call/cc (lambda (return)
  ...
  (let ((result (current-parser input)))
    (call/cc (lambda (next-alternative)
      (return (success result next-alternative)))))))
```

Then a failure is simply a call into the next alternative continuation, which
bottoms out by returning `nil` or something.
