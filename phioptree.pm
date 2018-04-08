=head1 License
    phi programming language
    Copyright (C) 2018  Spencer Tipping

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

=head1 phi op tree definitions
Most interpreters assign evaluation semantics to the parse nodes (or bytecode)
directly: evaluation is an intrinsic part of parsed code. phi is different in
that it stores a functional graph up front with no specific evaluator
implementation, then uses structural parsers to compile or interpret. This makes
it possible to introduce custom evaluation semantics or optimization strategies
in libraries, since evaluation was extrinsic to begin with.

Importantly, that open-endedness is isolated to backends or frontends; the set
of tree nodes is fixed.


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


=head2 Let bindings, side effects, and IO
Compiling something like C<let x = 3 + 4 in x + 1> is easy: we can just have the
source parsers alias C<x> to the C<3 + 4> node and call it a day. But if the
binding has side effects, for instance C<let x = print("hi") in x + x>,
we need to make sure we evaluate C<x> exactly once and save the result. That is,
C<x> can no longer refer to the C<print("hi")> opnode directly because then we'd
print C<hi> twice.

We have a few options:

1. Use some type of structured, monadic IO
2. Force the evaluation order with a pair of C<allocate> and C<access> nodes
3. Force the evaluation order by converting C<let> to a function call

=head3 Monadic IO
This pulls side effects into the data layer, which does a few things:

1. Expressions become fully mobile (up to arg/capture dependencies)
2. Side effects can be quoted, since they're just data
3. The side effect chain can itself be parsed
4. Monadic bind lambda conversion happens automatically and only when required

=head3 C<allocate> and C<access>
This is a mess despite being a fairly literal representation of what we want.
How do we know when an allocated quantity can be GC'd? How do we thread the cell
ID between allocation and access? This will never work because it's a terrible
idea.

=head3 C<let> -> C<fn>
This is a more ham-fisted version of monadic IO, and it carries some drawbacks:

1. It creates a bunch of closures that serve no real purpose
2. Expressions need to be IO-aware; otherwise they aren't mobile
3. We have no quotable IO list: IO can't be simulated
4. We rely on the evaluator to support commutative IO ordering

...which is a pretty terrible series of compromises. Let's go with structured
IO.


=head2 Syntactic IO flattening
From a phi-syntax point of view, any given subexpression falls into one of two
grammar categories: IO or pure. The difference becomes evident when you
C<let>-bind something:

  let x = pure in ...                   # purely syntactic (no runtime var)
  let x = io in ...                     # monadic bind (lambda+capture)

The pure-syntactic transformation isn't a free lunch: evaluators still need to
be aware of potential node aliasing. For example, it's reasonable to expect each
subexpression to be calculated at most once in a situation like this:

  let x = some_awful_pure_thing in      # this result should be cached...
  let y = x + x + x + x in              # ...so this is fast
  print(y)

...which I think is fine. We can structurally parse the C<+> nodes and observe
that we have a common subexpression (C<let>-bound or not), and rewrite some
terms to create the right functional dependency. And, of course, we're always at
liberty to convert every C<let> expression to a function call initially, then
detect mobile subexpressions and optimize from there.

NB: we may need something more subtle than C<arg>/C<capture> as single-frame
references if we want actual subexpression mobility with this type of conversion
-- or maybe we can just inline any lambda whose invocation site is immediate.

...actually, the whole premise of IO-wrapped expressions is that they _aren't_
mobile. You can't inline an impure function. So the world is split into
before-IO and after-IO based specifically on the C<arg> dependency. That's quite
beautiful actually.

=head3 Purity inference and monadic transformation
Tree node flags take care this for us. If a node is flagged C<f_io>, then it
should be promoted into a syntax node that will monadic-bind its continuation --
which may entail some interesting stuff, so let's talk about that.

Let's start with a simple example: C<io1 + io2>. If we're in a dialect that
forces left-to-right evaluation, then the monadic bind would be
C<< io1.bind(\x -> io2.bind(\y -> return x + y)) >>. C<io2.bind> can be called
only when its lambda is available, which can happen only when we know C<x>; so
we have a strict functional dependency to force the IO ordering.

So far so good: now let's look at something like C<f(io1, io2)>. C<f> needs to
understand that C<(io1, io2)> is impure, so it owns the bind operation and
transforms itself as C<< io1.bind(\x -> io2.bind(\y -> return f(x, y))) >>.

How about C<io1; io2>? Same thing here:
C<< io1.bind(\x -> io2.bind(\y -> x; y)) >>. All binops can be transposed this
way because they all have the same ordering semantics (NB: some frontends may
differ, but it's up to the frontend to do the monadic conversion in a way that
reflects those differences.)

What happens when a function returns an IO? The result is marked as impure and
we bind it.

There may be a syntactic/semantic mapping for this type of transformation.
=cut

package phioptree;
use strict;
use warnings;

use phiboot;
use phibootmacros;
use philist;
use phiobj;
use phiparse;


=head2 phi layer and node flags
Let's get back to phi's intermediate evaluation structures. I was being a bit
misleading when I suggested above that there are no intrinsic evaluation
semantics for them; in fact phi does have a very specific set of rules for
evaluating things, which it uses for a number of purposes including
representational optimization and speculative execution.

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

=head3 Node protocol: types
Nodes are regular phi objects, so we need to define a consistent protocol for
them. The main goal here is to interact with parsers in a straightforward way.
Let's incorporate the base node type into the flags using the low four bits.

C<t_syntax> is a bit of an outlier here in that it doesn't participate in the
evaluation process. It's used to inform dialects about nonstandard values whose
only role is to modify the parse, for instance comments or whitespace, which are
typically bound as locals and can therefore be customized.

Syntax nodes will normally kill an evaluation process; they should resolve to
mut bombs if evaluated. (A mut bomb is a pointer to itself.)
=cut

use constant t_native_const   => 0;
use constant t_arg            => 1;
use constant t_capture        => 2;
use constant t_fn             => 3;
use constant t_strict_binary  => 4;
use constant t_strict_unary   => 5;
use constant t_strict_nullary => 6;
use constant t_if             => 7;
use constant t_call           => 8;
use constant t_syntax         => 9;
use constant t_alias          => 10;


=head3 Node protocol: flags
The next bits are specific markers, each of which is a degree of restriction on
the node. Semantically, bitwise-ORing two flagsets should reflect the aggregated
set of restrictions present on two operations in sequence; that is:

  (seq(x, y).flags & ~typemask) == ((x.flags | y.flags) & ~typemask)

Some bits are reserved. For example, the low 8 bits are reserved for type and
function-related things, for instance the type tag and markers for custom parse
continuations. The next eight bits are allocated for "impurity flags," which
restrict the types of optimizations or evaluation that can be made against a
node.
=cut

use constant f_typemask    => 0x0f;
use constant f_bound_to_fn => 0x10;
use constant f_is_variant  => 0x20;
use constant f_io          => 0x40;

use phi f_impurities => le lit f_typemask, i_inv;

use phi retype_flags => l               # flags type
  swap,                                 # type flags
  lit f_typemask, i_inv, i_and,         # type flags'
  ior;                                  # flags''

# Macros to make life a little easier
sub node_type()     { ( mcall"flags", lit f_typemask, i_and ) }
sub node_type_is($) { ( node_type, lit shift, i_xor, i_not ) }


=head3 Node protocol
Other bits are reserved for future expansion.

OK, so given the above, the only method any node _needs_ to support is C<flags>:

  node.flags() -> int

From there, additional methods are required based on the flag set and node type.

=head4 Const native protocol
C<native_const> nodes implement this method:

  node.native() -> phi-value

=cut

use phitype native_const_type =>
  bind(flags  => drop, lit(t_native_const)),
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

use phi fn_mut => pmut;
use phitype fn_type =>
  bind(flags   => isget 0),
  bind(capture => isget 1),
  bind(body    => isget 2),
  bind(with_capture =>                  # c self
    mcall"body", fn_mut, i_eval);       # f'

use phi fn => l                         # capture body
  pnil, swons, swap,                    # [body] capture
  dup, mcall"flags",                    # [body] capture cflags
  rot3r, i_cons, swap,                  # [capture body] cflags

  # Functions inherit everything except type from their capture value.
  lit t_fn, retype_flags, i_eval,       # [capture body] flags
  i_cons, fn_type, swons;               # fnval

fn_mut->set(fn);


use phitype arg_type     => bind(flags => drop, lit(t_arg     | f_bound_to_fn));
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

use phitype nullop_type =>
  bind(flags => isget 0),
  bind(op    => isget 1);


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
  lit f_is_variant, i_and,              # then else cond var?

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

  stack(0, 0, 2), ior,                  # arg af fn ff aff
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
  lit t_call, retype_flags, i_eval,     # arg af fn ff flags
  stack(5, 4, 2, 0), pnil,              # flags fn arg []
  swons, swons, swons,                  # [flags fn arg]
  call_type, swons;


=head4 Syntax nodes
These are simple: they just store a value and have no impurities because they
are erased by runtime.
=cut

use phitype syntax_type =>
  bind(flags  => drop, lit t_syntax),
  bind(syntax => isget 0);

use phi syntax => l                     # v
  pnil, swons,                          # [v]
  syntax_type, swons;                   # [v]::syntax_type


=head4 Alias nodes
These are used to inform dialects that we have a more useful alias for a
computed value. This situation comes up when you capture something, for
instance.

Aliases are special in that you can use an unresolved mut as the real node, and
they will know not to call C<flags> on that value. If you do this, every
impurity will be set.
=cut

use phitype alias_type =>
  bind(flags =>                         # self
    mcall"real_node",                   # real
    dup, i_type, lit"mut", i_symeq,     # real is-mut?

    l(drop, lit f_impurities),          # vflags
    l(mcall"flags"),                    # rflags
    if_,
    lit t_alias, retype_flags, i_eval), # flags

  bind(can_be_real =>                   # self
    mcall"real_node", i_type,           # realtype
    lit"mut", i_symeq, i_not),          # not-a-mut?

  bind(proxy_node => isget 0),
  bind(real_node  => isget 1);

use phi alias => l                      # real proxy
  dup, node_type_is(t_alias),           # real proxy proxy-is-alias?

  l(lit alias_proxy_should_not_be_alias => i_crash),
  pnil,
  if_,                                  # real proxy

  swap, pnil, swons, swons,             # [proxy real]
  alias_type, swons;


use phi alias_deref_real_mut => pmut;
use phi alias_deref_real => l           # node
  dup, node_type_is(t_alias),           # node alias?
  l(dup, mcall"can_be_real",            # node can-be-real?
    l(mcall"real_node",                 # node.real
      alias_deref_real_mut, i_eval),    # deref(node.real)
    pnil,                               # node
    if_),
  pnil,                                 # node
  if_;

alias_deref_real_mut->set(alias_deref_real);

use phi alias_deref_proxy => l          # node
  dup, node_type_is(t_alias),           # node alias?
  l(mcall"proxy_node"),                 # node.proxy
  pnil,
  if_;


=head2 phi bootstrap language operators
All of the low-level primitives you can invoke. This set of operators shouldn't
expand much because any complexity added here will substantially increase the
number of operator interactions that compilation parsers would need to handle.


=head3 Sequence operators
C<seqr(x, y)> evaluates C<x> before C<y>, then returns C<y>.

C<seql(x, y)> evaluates C<x> before C<y>, then returns C<x>.
=cut

use phi op_seql => l lit"seql", binop, i_eval;
use phi op_seqr => l lit"seqr", binop, i_eval;


=head3 Integer operators
Each of these exactly tracks phi's primitive integer implementations. The LHS is
always evaluated before the RHS.

C<gt> is distinct from C<lt> because it evaluates its arguments in a different
order; that is, C<< x < y >> evaluates C<x> before C<y>, so it would be
incorrect to implement C<< > >> in terms of C<< < >> by swapping the operands
before we construct the op node.
=cut

use phi op_iplus  => l lit"i+",  binop, i_eval;
use phi op_itimes => l lit"i*",  binop, i_eval;
use phi op_ilsh   => l lit"i<<", binop, i_eval;
use phi op_irsh   => l lit"i>>", binop, i_eval;
use phi op_ilt    => l lit"i<",  binop, i_eval;
use phi op_igt    => l lit"i>",  binop, i_eval;

use phi op_iand   => l lit"i&",  binop, i_eval;
use phi op_ior    => l lit"i|",  binop, i_eval;
use phi op_ixor   => l lit"i^",  binop, i_eval;

use phi op_ineg   => l lit"i-",   unop, i_eval;
use phi op_iinv   => l lit"i~",   unop, i_eval;
use phi op_inot   => l lit"i!",   unop, i_eval;


=head3 Cons operators
These are close to phi's builtins. Because the optree doesn't have a way to
indicate multiple return values, we split C<uncons> into two separate accessors,
C<head> and C<tail>.
=cut

use phi op_cons => l lit"cons", binop, i_eval;
use phi op_head => l lit"head",  unop, i_eval;
use phi op_tail => l lit"tail",  unop, i_eval;


# TODO: more ops

use phi c_nil => le pnil, native_const, i_eval;


1;
