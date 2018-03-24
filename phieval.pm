=head1 phi expression nodes and evaluation parsers
Most interpreters assign evaluation semantics to the parse nodes (or bytecode)
directly: evaluation is an intrinsic part of parsed code. phi is different in
that it stores a functional graph up front with no specific evaluation
semantics, then uses structural parsers to evaluate. This makes it possible to
introduce custom evaluation semantics or optimization strategies in libraries,
since evaluation was extrinsic to begin with.


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

=cut

package phieval;
use strict;
use warnings;

use phiboot;
use phibootmacros;
use philist;
use phiobj;
use phiparse;


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
=cut

use constant t_const_native   => 0;
use constant t_arg            => 1;
use constant t_capture        => 2;
use constant t_fn             => 3;
use constant t_strict_binary  => 4;
use constant t_strict_unary   => 5;
use constant t_strict_nullary => 6;
use constant t_seql           => 7;
use constant t_seqr           => 8;
use constant t_if             => 9;
use constant t_call           => 10;
use constant t_flatparse      => 11;

=head3 Node protocol
The next bits are specific markers, each of which is a degree of restriction on
the node. Semantically, bitwise-ORing two flagsets should reflect the aggregated
set of restrictions present on two operations in sequence; that is:

  (seq(x, y).flags & ~typemask) == ((x.flags | y.flags) & ~typemask)

=cut

use constant f_typemask           => 0x0f;
use constant f_bound_to_fn        => 0x10;
use constant f_is_variant         => 0x20;
use constant f_reads_timelines    => 0x40;
use constant f_modifies_timelines => 0x80;


use phi f_impurities => le lit f_typemask, lit bound_to_fn, ior, i_inv;


use phi retype_flags => l               # flags type
  swap,                                 # type flags
  lit f_typemask, i_inv, i_and,         # type flags'
  ior;                                  # flags''


=head3 Node protocol
Other bits are reserved for future expansion.

OK, so given the above, the only method any node _needs_ to support is C<flags>:

  node.flags() -> int

From there, additional methods are required based on the flag set.

=head4 Const and native protocol
Specifying C<is_const> doesn't obligate a node into any additional
functionality, but C<is_native> does:

  node.native() -> phi-value

=cut

use phitype native_const_type =>
  bind(flags  => drop, lit(t_const_native | f_isconst)),
  bind(native => isget 0);

use phi native_const => l               # native
  pnil, swons,                          # [native]
  native_const_type, swons;             # [native]::native_const_type


=head4 Functions
If a node's type is C<fn>, then it must provide the following:

  node.capture() -> capture node
  node.body()    -> body node

Function nodes delegate most flags to their C<capture> value.
=cut

use phitype fn_type =>
  bind(flags   => isget 0),
  bind(capture => isget 1),
  bind(body    => isget 2);

use phi fn =>                           # capture body
  pnil, swons, swap,                    # [body] capture
  dup, mcall"flags",                    # [body] capture cflags
  rot3r, i_cons, swap,                  # [capture body] cflags

  # Functions inherit everything except type from their capture value.
  lit t_fn, retype_flags, i_eval,       # [capture body] flags
  i_cons, fn_type, swons;               # fnval


use phitype arg_type     => bind(flags => drop, lit(t_arg | f_bound_to_fn));
use phitype capture_type => bind(flags => drop, lit(t_capture | f_bound_to_fn));

use phi arg     => pcons pnil, arg_type;
use phi capture => pcons pnil, capture_type;


=head4 Unary and binary ops
Strict unary and binary ops both provide two methods:

  node.op()  -> symbol
  node.lhs() -> operand node

If the node is a binary op, then it additionally provides a C<rhs> accessor:

  node.rhs() -> operand node

=cut

use phitype unop_type =>
  bind(flags => isget 0),
  bind(op    => isget 1),
  bind(lhs   => isget 2);

use phi unop => l                       # lhs op
  swap, dup, mcall"flags",              # op lhs lflags
  lit t_strict_unary, retype_flags,
                      i_eval,           # op lhs flags
  rot3r, pnil, swons, swons, swons,     # [flags op lhs]
  unop_type, swons;


use phitype binop_type =>
  bind(flags => isget 0),
  bind(op    => isget 1),
  bind(lhs   => isget 2),
  bind(rhs   => isget 3);

use phi binop => l                      # lhs rhs op
  rot3r, dup, mcall"flags",             # op lhs rhs rflags
  rot3l, dup, mcall"flags",             # op rhs rflags lhs lflags
  rot3l, ior,                           # op rhs lhs uflags
  lit t_strict_binary, retype_flags,
                       i_eval,          # op rhs lhs flags
  rot3l, pnil, swons,                   # op lhs flags [rhs]
  rot3l, i_cons,                        # op flags [lhs rhs]
  rot3l, i_cons,                        # flags [op lhs rhs]
  swons, binop_type, swons;


=head4 Sequence nodes
C<seql> and C<seqr> have the same API:

  node.lhs() -> operand node
  node.rhs() -> operand node

=cut

use phitype seq_type =>
  bind(flags => isget 0),
  bind(lhs   => isget 1),
  bind(rhs   => isget 2);

use phi seq => l                        # lhs rhs type
  rot3r, dup, mcall"flags",             # type lhs rhs rflags
  rot3l, dup, mcall"flags",             # type rhs rflags lhs lflags
  rot3l, ior,                           # type rhs lhs uflags
  rot3l, pnil, swons,                   # type lhs uflags [rhs]
  rot3l, i_cons,                        # type uflags [lhs rhs]
  rot3r, swap,                          # [lhs rhs] uflags type
  retype_flags, i_eval, i_cons,         # [flags lhs rhs]
  seq_type, swons;


use phi seql => l lit t_seql, seq, i_eval;
use phi seqr => l lit t_seqr, seq, i_eval;


=head4 C<if> nodes
C<if> nodes delegate most flags to C<cond>, although capture and timeline flags
consider both alternatives.

  node.cond() -> node
  node.then() -> node
  node.else() -> node

There's a lot of subtlety involved around conditions, but simple evaluators can
ignore most of it. Basically, all we need to do is constant-fold the condition
when we can. If the condition is unknown, then the flagset becomes pessimistic.

(The reason we constant fold known conditionals isn't because it's appropriate
for op nodes to self-optimize; that's a job for structural parsers. We do it
here because it produces more accurate flags.)
=cut

use phitype if_type =>
  bind(flags => isget 0),
  bind(cond  => isget 1),
  bind(then  => isget 2),
  bind(else  => isget 3);

use phi if => l                         # then else cond
  # Is the condition variant? If constant, we can fold it because the discarded
  # branch of a conditional has no effect (by definition).
  dup, mcall"flags",
  lit t_variant, i_and,                 # then else cond var?

  # Variant case: construct the conditional node.
  l(                                    # then else cond
    rot3r, dup, mcall"flags",           # cond then else eflags
    rot3l, dup, mcall"flags",           # cond else eflags then tflags
    rot3l, ior,                         # cond else then teflags
    rot3r, swap, pnil, swons, swons,    # cond teflags [then else]
    rot3l, dup, mcall"flags",           # teflags [then else] cond cflags
    rot3r, i_cons,                      # teflags cflags [cond then else]
    rot3r, ior,                         # [cond then else] cteflags
    lit t_if, retype_flags, i_eval,     # [cond then else] flags
    i_cons, if_type, swons),

  # Constant case: choose then or else depending on the value of the condition.
  # The condition must be an integer.
  l(                                    # then else cond
    mcall"native",                      # then else n
    dup, i_type, lit"int", i_symeq,     # then else n int?
    l(                                  # then else n
      rot3l, pnil, swons,               # else n [then]
      rot3l, pnil, swons,               # n [then] [else]
      rot3l, if_),                      # then|else
    l(                                  # then else n
      lit constructing_if_node_for_non_int_condition => i_crash),
    if_),                               # then|else

  if_;


=head4 C<call> nodes
Fairly straightforward:

  node.fn()  -> node
  node.arg() -> node

Just as for conditions, we do some work to calculate an accurate flagset when we
can. We don't substitute the argument into the function body, but we do look at
the function body and see whether it is provably pure. (If it isn't, then we
have to assume it impacts timelines.)
=cut

use phitype call_type =>
  bind(flags => isget 0),
  bind(fn    => isget 1),
  bind(arg   => isget 2);

use phi call => l                       # fn arg
  dup, mcall"flags",                    # fn arg aflags
  rot3l, dup, mcall"flags",             # arg aflags fn fflags

  # We can assume there are no side effects if the following things are true:
  #
  # 1. The function is invariant
  # 2. The body of the function has no side effect flags
  # 3. The arg value has no side effect flags
  #
  # If we can't retrieve the function body, then we have to assume every
  # impurity possible.

  stack(0, 0, 2), i_and,                # arg af fn ff aff
  lit f_impurities, i_and, dup, i_not,  # arg af fn ff aff pure-so-far?

  l(                                    # arg aflags fn fflags aff
    stack(0, 2), mcall"body",           # arg af fn ff aff body
    mcall"flags",
    lit f_impurities, i_and,            # arg af fn ff aff bf
    ior),                               # arg af fn ff impurities

  l(                                    # arg af fn ff aff
    drop, lit f_impurities),            # arg af fn ff impurities
  if_,

  # At this point, "impurities" is the correct set of impurities for the
  # function body, capture, and arg. Our flags will be those + the type marker.
  lit t_fn, retype_flags, i_eval,       # arg af fn ff flags
  stack(5, 4, 2, 0), pnil,              # flags fn arg []
  swons, swons, swons,                  # [flags fn arg]
  call_type, swons;


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
=cut
