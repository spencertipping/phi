=head1 phi expression nodes and evaluation parsers
Most interpreters assign evaluation semantics to the parse nodes (or bytecode)
directly: evaluation is an intrinsic part of parsed code. phi is different in
that it stores a functional graph up front with no specific evaluation
semantics, then uses structural parsers to evaluate. This makes it possible to
introduce custom evaluation semantics or optimization strategies in libraries,
since evaluation was extrinsic to begin with.


=head2 Structural parsing
Parsing expression grammars are normally applied to strings, but strings aren't
the only things you might want to parse. For example, let's suppose we have a
list of integers -- ASCII codes perhaps -- and we want to use parser combinators
against it. Then the parse state would point to the list itself, and
continuations would be states which pointed to cons cells later down the list.
You could easily port string combinators to operate on lists of char codes
because they're fundamentally the same data structure.

Things get a little more interesting if you want to parse nonlinear structures
-- although this is also something that happens in the string case. Let's start
with something simple like an arithmetic expression tree, for instance:

  binop(+, const(3), binop(*, const(4), const(5)))

The parser to evaluate a structure like this is similar to the one we'd use for
strings:

  evaluated  ::= plus_case | times_case | const_case
  plus_case  ::= binop('+', evaluated, evaluated) -> v[0] + v[1]
  times_case ::= binop('*', evaluated, evaluated) -> v[0] * v[1]
  const_case ::= const(evaluated)

Ok, so what about the parse state? In this case it could be the node being
evaluated. Continuations would be children of that node.

If this seems a lot like destructuring, that's because it really is, just like
parsers are destructuring binds over strings. Parsers as a concept give you much
more flexibility than typical implementations of destructuring binds,
particularly when you can compute grammars, so phi prefers them -- but really,
we're just pattern matching.


=head2 Operation nodes
With the context established that op nodes are just data, what data do we want
to store? It's a nontrivial question because we have dialects, but let's shelve
that first and talk about what this would look like in a dialect-free world.

=head3 Simple case: no dialects
Within phi's runtime we just have a few types of nodes:

  const(x)                              # a quoted phi native
  unop(op, x)                           # a strict unary operator
  binop(op, x, y)                       # a strict binary operator
  fn(capture, body)                     # a function that may capture lexically
  arg()                                 # the current function's argument
  capture()                             # the function's captured value
  seql(x, y)                            # evaluate x then y, then return x
  seqr(x, y)                            # evaluate x then y, then return y
  if(c, x, y)                           # a non-strict conditional

I think that covers the basics. I won't get into strict/lazy yet; lazy operators
would make the set of primitives smaller but introduces its own complexity. The
above is a reasonable place to start for a strict evaluator.

=head3 Inflected semantics
The problem with dialects is that they truly are open-ended. If we wanted to
model Haskell in terms of phi, we would need lazy evaluation nodes. If we wanted
to model C++, we'd need a way to deal with direct memory access. Given that
dialects can't install persistent semantic inflections, we ultimately need to
compile them away into core phi.

So ... what does this mean for our op encodings? Well, if dialects are
translated using parsers, then we'll have arbitrarily many custom nodes -- but
those have to be erased by the time we're ready to have phi evaluate things. We
can't have random dialects floating around the op tree simply because the
translation parsers are lexically bound.

In order to make this work, we have to introduce a translation step for each
dialect barrier:

  in python:
    f = lambda x: x + 1
    print(f(5))

C<in python> parses a block of python code into a python parse tree, then
applies the python dialect translation parsers to reduce it to phi expressions.

Now ... this obviously raises a problem: how do we do sweet stuff like live
value previewing if we're waiting until the end to translate things into phi?
That's up to the dialect. Most languages have highly linear mappings into phi
semantics, so they can use incremental or partial parsers to get there. I'm
willing to defer on this problem for now.


=head2 phi layer and node flags
Let's get back to phi's intermediate evaluation structures. I was being a bit
misleading when I said above that there are no intrinsic evaluation semantics
for them; in fact phi does have a very specific set of rules for evaluating
things, which it uses for a number of purposes including representational
optimization and speculative execution.

Because these semantics are known ahead of time, we can precompute some node
attributes to avoid having (more computationally expensive) parsers to test
various predicates. This is surfaced as a bitmask of node flags that phi's
evaluation parsers can use to quickly reject nodes in various states. Before I
get to that, though, let's talk about a more complete evaluation model.

=head3 phi semantics
TODO: should phi support continuations?

=cut
