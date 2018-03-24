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
  call(f, argval)                       # a strict function call

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
I think it's fine to make some simplifying assumptions for now:

1. No need for C<call/cc> yet
2. No need for lazy cons cells yet

(2) impacts representational optimization: in some cases we can eliminate cons
cells if we know up front that only one half is accessed. The subtlety comes in
when timelines are involved; we have to preserve any side effects accumulated
from the discarded value.

=head3 Node flags
At the most basic level, we care about a few things:

1. Is a node constant?
2. Can we move a node beyond a function boundary?
3. Does a node impact timelines? (i.e. does it create side effects)

(1) seems like it would be obvious, but it isn't entirely. For example, some
things like functions aren't constant all the time; they're only really constant
when the capture value is itself a constant. (That's when we can instantiate the
captured scope for a lambda.)

(2) has to do with references to C<arg>, C<capture>, and side effects. For
example, C<3 + arg> can't be lifted through a function, whereas C<3 + 4> can.
Similarly, C<print("hi")> can't be lifted but C<reverse("hi")> can.

(3) doesn't capture the whole story for a node because nodes can tell you
specifically which timelines they use. But we don't need those details for most
optimizations -- we just need to know whether a node is free of timeline-related
dependencies.

=head3 Node protocol
Nodes are regular phi objects, so we need to define a consistent protocol for
them. The main goal here is to interact with parsers in a straightforward way.
Let's incorporate the base node type into the flags using the low four bits:

  0000 = const native value
  0001 = arg
  0010 = capture
  0011 = fn
  0100 = strict binary op
  0101 = strict unary op
  0110 = seql
  0111 = seqr
  1000 = if
  1001 = call

The next bits are specific attributes:

  bits 0-3: type
  bit 4:    is_const
  bit 5:    is_native
  bit 6:    refers_to_arg
  bit 7:    refers_to_capture
  bit 8:    reads_timelines
  bit 9:    modifies_timelines

Other bits are reserved for future expansion.

OK, so given the above, the only method any node _needs_ to support is C<flags>:

  node.flags() -> int

From there, additional methods are required based on the flag set.

=head4 Const and native protocol
Specifying C<is_const> doesn't obligate a node into any additional
functionality, but C<is_native> does:

  node.native() -> phi-value

=head4 Functions
If a node's type is C<fn>, then it must provide the following:

  node.capture() -> capture node
  node.body()    -> body node

Function nodes delegate most flags to their C<capture> value.

=head4 Unary and binary ops
Strict unary and binary ops both provide two methods:

  node.op()  -> symbol
  node.lhs() -> operand node

If the node is a binary op, then it additionally provides a C<rhs> accessor:

  node.rhs() -> operand node

=head4 Sequence nodes
C<seql> and C<seqr> have the same API:

  node.lhs() -> operand node
  node.rhs() -> operand node

=head4 C<if> nodes
C<if> nodes delegate most flags to C<cond>, although capture and timeline flags
consider both alternatives.

  node.cond() -> node
  node.then() -> node
  node.else() -> node

There's a lot of subtlety involved around conditions, but simple evaluators can
ignore most of it.

=head4 C<call> nodes
Fairly straightforward:

  node.fn()  -> node
  node.arg() -> node

=cut
