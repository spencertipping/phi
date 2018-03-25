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

=head3 Node protocol: types
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
use constant t_if             => 7;
use constant t_call           => 8;


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

Although I'm doing stuff like reserving bits, you shouldn't assume any
consistency over time. This format is just used internally as an optimization.
=cut

use constant f_typemask           => 0x00f;
# flags 0x10, 0x20, 0x40, 0x80 are reserved

use constant f_bound_to_fn        => 0x100;
use constant f_is_variant         => 0x200;
use constant f_reads_timelines    => 0x400;
use constant f_modifies_timelines => 0x800;

use phi f_impurities => le lit 255, i_inv;

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

From there, additional methods are required based on the flag set.

=head4 Const and native protocol
Specifying C<is_const> doesn't obligate a node into any additional
functionality, but C<is_native> does:

  node.native() -> phi-value

=cut

use phitype native_const_type =>
  bind(flags  => drop, lit(t_const_native)),
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

use phi fn => l                         # capture body
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

=head3 Evaluator parse state
Let's start here:

  state.is_error      -> 1|0
  state.value         -> v
  state.with_value(v) -> state'

We're parsing over objects, so we additionally have a pointer to the object
we're focused on:

  state.node          -> node
  state.with_node(n)  -> state'

And we need to carry some information around; in particular, we need to know the
current C<arg> and C<capture> for any function we're evaluating. Before I list
out a couple more properties, let's talk about how parse states interact with
the evaluation process.

=head4 Subexpression continuations
Suppose we have a parse rule that calls a function on a value (which we will).
Then it sets the parse state's C<arg> and C<capture>, then parses the body with
the overall evaluation grammar to get a resulting logical value. So far so good.

Now let's suppose the body of the function contains a fork with two more
function calls, e.g. C<binop(+, f(3), f(4))>. How is the parse state threaded
between C<f(3)> and C<f(4)>, if at all? If we're parsing them in sequence, then
they need to share some state.

...and that's where things start to get interesting. In sequential terms,
C<f(4)> happens strictly after C<f(3)> -- it has to, because timelines need a
coherent ordering. (This example might be more obvious with a C<seqr> node, but
it's the same ordering for binops.) So we need to take the state coming out of
C<f(3)> and then continue it for C<f(4)>.

We could apply modifications to the return from C<f(3)>, but that doesn't really
make sense when we can just make a new state. The only piece we share between
the two evaluations is the set of timelines, which itself is managed as an
object. So internally, we follow a strategy like this:

  let lstate = lhs_parser.parse(state { f(3), incoming.timelines })
  let rstate = rhs_parser.parse(state { f(4), lstate.timelines })
  let result = rstate.with_value(lstate.value + rstate.value)

In object terms, then, we have a few more methods:

  state.timelines         -> timeline-set
  state.with_timelines(t) -> state'

  state.arg               -> arg-node
  state.capture           -> capture-node
  state.with_arg(a)       -> state'
  state.with_capture(c)   -> state'

We inherit fail states from C<phiparse>.

=head4 We're not done yet: let's talk about timelines
So far I've treated timelines as opaque, and I want to continue doing that for
te most part -- but it's important to get a little bit into how we intend to
work with them. (I'll get into the gory details further below.)

The short version for now is that timelines are event chains that log or apply
side effects. So if we wanted to evaluate a sequence of things, for instance
C<seqr(print("hi"), print("there"))>, we'd end up with two events appended to
the C<stdout> timeline. Those events can be committed eagerly iff we're using a
non-alternative parser to evaluate things; otherwise they get consed onto the
timelines.

Committing-vs-consing isn't straightforward in practice. First, parsers aren't
typically aware of (1) whether a fallback exists, and (2) if it does, whether it
will match. Second, there are cases where we want to emulate a program -- so
although normally we would commit things, we instead look at localized event
deltas so we can reverse-engineer for better performance.

For now, the easiest way to think about timelines is that they collect events
and then do an unspecified thing with them:

  timeline.add(event) -> timeline'

=cut

use phitype eval_state_type =>
  bind(is_error => drop, lit 0),

  bind(value     => isget 0),
  bind(node      => isget 1),
  bind(arg       => isget 2),
  bind(capture   => isget 3),
  bind(timelines => isget 4),

  bind(with_value     => isset 0),
  bind(with_node      => isset 1),
  bind(with_arg       => isset 2),
  bind(with_capture   => isset 3),
  bind(with_timelines => isset 4);


=head3 Evaluation parsers
The most important of these is C<const-able>, which sounds like "constable," so
I'm calling it C<thefuzz> for brevity. The idea here is to take a
possibly-complicated expression and match if we can reduce it to a constant. If
so, we return that constant.

The fuzz is a complete parser: that is, it traverses the expression tree all the
way down to leaf nodes. This forces a full evaluation of the thing you apply it
to.

Operator implementations are provided by three objects you give to the fuzz:
C<nullop>, C<unop>, and C<binop>. Each of these objects binds the _symbol_ of
the op as a method over the two parse states. For example, if C<+> is a binary
op, then the fuzz would expect this method to exist:

  binop.+(lhs_state, rhs_state) -> state'

The reason we operate on states, as opposed to values, is that some operators
modify timelines. It's each operator's responsibility to reflect this in the
state it returns.

=head4 Building type-specific parsers
Here's what the const parser for the fuzz looks like:

  use phitype thefuzz_const_parser =>
    bind(parse =>                         # state self
      drop, dup, mcall"node", dup,        # state node node
      node_type_is(t_const),              # state node const?
      l(                                  # state node
        mcall"native", swap,              # native state
        mcall"with_value"),               # state'
      l(stack(2), fail_state),            # fail
      if_);

It gets repetitive type-filtering all of our nodes, though, so let's automate
it. Here's what we want:

  TYPE [FN] type_filtered_parser ->
    [(parse ::                            # state self
       drop, dup, mcall"node", dup,       # state node node
       node_type_is(TYPE),                # state node type?
       [FN],                              # state node -> state'
       [stack(2), fail_state]             # state node -> fail
       if_)]
    make_type

The success function should have this signature:

  state node -> state'

It can, of course, return a failing state.
=cut


# Sub-parsers can recursively refer to the fuzz to evaluate their arguments as
# necessary.
use phi thefuzz_mut => pmut;

use phi type_filtered_parser => l       # type fn -> parser
  l(l(stack(2), phiparse::fail_state),
    if_),                               # type fn tail
  swons,                                # type tail
  swap, pnil, swons, swap,              # [type] tail

  # Now build up the type detector function, which has this signature:
  #
  #   node [type] -> match?
  #
  # We can prepend this to the tail.

  l(                                    # node [type]
    head, swap, mcall"flags",           # type flags
    lit f_typemask, i_and,              # type type
    i_xor, i_not),                      # match?
                                        # [type] tail f
  swap, list_append, i_eval,            # [type] f++tail
  swons,                                # [type]::(f++tail)

  # Now we have a function that takes a node and does the rest. All we need to
  # do is adapt the initial call stack correctly. Specifically, we need this
  # conversion:
  #
  #   state self -> state node node

  l(                                    # state self
    drop, dup, mcall"node", dup),       # state node node
                                        # tail f
  swap, list_append, i_eval,            # f++tail

  lit psym"parse", i_cons,              # bind(parse => ...)
  pnil, swons,                          # [bind(parse => ...)]
  make_type, i_eval;                    # the type


=head4 Simple instantiations
These don't have any delegation like op tables; they're simple values.

The C<fn> parser is a bit unusual in that its value is the op node itself.
=cut

use phi thefuzz_const_parser =>
  le lit t_const_native,
     l(                                 # state node
       mcall"native", swap,
       mcall"with_value"),
     type_filtered_parser, i_eval;

use phi thefuzz_arg_parser =>
  le lit t_arg,
     l(                                 # state node
       drop, dup, mcall"arg",           # state argval
       swap, mcall"with_value"),
     type_filtered_parser, i_eval;

use phi thefuzz_capture_parser =>
  le lit t_capture,
     l(
       drop, dup, mcall"capture",
       swap, mcall"with_value"),
     type_filtered_parser, i_eval;

use phi thefuzz_fn_parser =>
  le lit t_fn,
     l(                                 # state node
       swap, mcall"with_value"),        # state { val = fn_node }
     type_filtered_parser, i_eval;


=head4 Nullary operators
These, unary, and binary operators follow a similar implementation strategy.
Basically, the parser objects are parameterized by two things:

1. The op -> fn dispatch table (an object)
2. The subordinate parser/parsers, if they exist

This makes the pieces mostly-reusable.

The op->fn dispatch table works like this, for successful parses:

  self.operator.<op>(state)             # for nullary
  self.operator.<op>(state, s1)         # for unary
  self.operator.<op>(state, s1, s2)     # for binary

=cut

use phitype thefuzz_nullary_operator_type =>
  bind(say_hi =>                        # state self
    drop, lit hi => i_print,            # state
    lit"hi", swap, mcall"with_value");  # state'

use phitype thefuzz_nullary_parser_type =>
  bind(operator => isget 0),
  bind(parse =>                         # state self
    nip, mcall"node", dup, mcall"flags",# state self node flags
    lit f_typemask, i_and,              # state self node type
    lit t_strict_nullary, i_xor,        # state self node not-match?

    l(stack(3), phiparse::fail_state),
    l(                                  # state self node
      mcall"op",                        # state self op
      swap, mcall"operator", i_eval),   # self.operator.<op>(state)
    if_);

use phi thefuzz_nullary_operator => pcons pnil, thefuzz_nullary_operator_type;
use phi thefuzz_nullary_parser =>
  pcons l(thefuzz_nullary_operator), thefuzz_nullary_parser_type;


=head4 Unary operators
Same as above, just with more fuzz.
=cut

use phitype thefuzz_unary_operator_type =>
  bind("~" =>                           # state s1 self
    stack(3, 1, 1), mcall"value", i_inv,# s1 ~v
    swap, mcall"with_value");           # s1'

use phitype thefuzz_unary_parser_type =>
  bind(parser   => isget 0),
  bind(operator => isget 1),
  bind(parse    =>                      # state self
    nip, mcall"node", dup, mcall"flags",# state self node flags
    lit f_typemask, i_and,              # state self node type
    lit t_strict_unary, i_xor,          # state self node not-match?

    l(stack(3), phiparse::fail_state),
    l(                                  # state self node
      dup, mcall"lhs",                  # state self node node'
      stack(0, 3), mcall"with_node",    # state self node state'
      nip, mcall"parser", mcall"parse", # state self node state''
      dup, mcall"is_error",             # state self node state'' e?
      l(stack(4, 0)),                   # failstate
      l(                                # state self node state''
        stack(4, 1, 2, 0, 3),           # state state'' self node
        mcall"op", swap,                # state state'' op self
        mcall"operator", i_eval),       # opr.<op>(state, state'')
      if_),
    if_);

use phi thefuzz_unary_operator => pcons pnil, thefuzz_unary_operator_type;
use phi thefuzz_unary_parser =>
  pcons l(thefuzz_mut, thefuzz_unary_operator), thefuzz_unary_parser_type;


=head4 Binary operators
Ditto - the only new thing here is that we sequentially evaluate things, and
forward the timelines accordingly.
=cut

use phitype thefuzz_binary_operator_type =>
  bind("+" =>                           # state s1 s2 self
    stack(4, 2, 1), mcall"value",       # s2 v1
    nip, mcall"value",                  # s2 v1 v2
    i_plus, swap, mcall"with_value");

use phitype thefuzz_binary_parser_type =>
  bind(parser   => isget 0),
  bind(operator => isget 1),
  bind(parse    =>                      # state self
    nip, mcall"node", dup, mcall"flags",# state self node flags
    lit f_typemask, i_and,              # state self node type
    lit t_strict_binary, i_xor,         # state self node not-match?

    l(stack(3), phiparse::fail_state),

    # First, parse the LHS and handle failures.
    l(                                  # state self node
      dup, mcall"lhs",                  # state self node node'
      stack(0, 3), mcall"with_node",    # state self node state'
      nip, mcall"parser", mcall"parse", # state self node state''
      dup, mcall"is_error",             # state self node state'' e?
      l(stack(4, 0)),                   # failstate
      l(                                # state self node state''
        # Now parse the RHS. This is where we forward timelines into the parent
        # parse state.
        dup, mcall"timelines",          # state self node state'' ts
        stack(2, 1, 0, 2),              # state self node node ts state''
        mcall"with_timelines",          # state self node node rstate
        swap, mcall"rhs",               # state self node rstate rhs
        stack(0, 1), mcall"with_node",  # state self node rstate rstate'
        stack(0, 2), mcall"parser",     # state self node rstate rstate' p
        mcall"parse",                   # state self node rstate rstate''
        dup, mcall"is_error",           # state self node rstate rstate'' e?
        l(stack(5, 0)),                 # failstate
        l(                              # state self node s1 s2
          stack(4, 2, 3, 0, 1),         # state s1 s2 self node
          mcall"op", swap,              # state s1 s2 op self
          mcall"operator", i_eval),     # opr.<op>(state, s1, s2)
        if_),
      if_),
    if_);

use phi thefuzz_binary_operator => pcons pnil, thefuzz_binary_operator_type;
use phi thefuzz_binary_parser =>
  pcons l(thefuzz_mut, thefuzz_binary_operator), thefuzz_binary_parser_type;


1;
