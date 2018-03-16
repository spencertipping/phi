=head1 Abstract type definitions
Parameterizable objects that become abstract types. At a high level, abstract
types support a few different things:

1. Partial evaluation and evaluation state
2. Postfix modification, if supported
3. Parse continuations, split into postfix and prefix

Abstract values, via (1), form an interpreter -- and that's a bit nontrivial, so
let's talk about what it entails.


=head2 Using abstracts to interpret code
This is actually very, very simple even for lexical closures. Let's work through
a quick example:

  (x -> x + 1)(5)

Here's what this looks like in object terms:

  call(fn(capture=[int(1)], intplus(cons_head(arg()), capture(0))),
       cons(int(5), nil()))

NB: C<cons(x, y)> is an abstract node object, whereas C<[...]> is a native list.
Because capture lists are always fully-specified, we don't need to encode them
as abstract values. (Also note that C<capture(0)> refers to the _rightmost_
capture list entry due to the way capture nodes work.)

Evaluating something like C<intplus(int(3), int(4))> is obviously trivial and
doesn't involve any stack state -- but functions require a bit more machinery.
Specifically, we need to make sure two things are true:

1. All args from parent functions are dereferenced to non-arg values
2. The capture list for the child function is as evaluated as it can be

phi is all about partial evaluation, so both of these things use the same
mechanism: we just call C<eval> on anything we want to resolve.

=head3 Evaluating C<call> nodes
C<call> interacts with C<fn> to move the body into the calling context. Normally
this isn't possible because the function body (and anything that refers to
C<arg> or C<capture>, really) can't move through a C<fn> node. C<call> performs
the motion by substituting the C<capture> and C<arg> values into things anchored
in the parent context. That means the C<eval> method needs to carry C<capture>
and C<arg> as stack state:

  capture-val arg-val node .eval() -> node'


=head2 Abstract value protocol
Abstract values all support the following methods (WARNING: the API below is a
complete lie):

  node.eval(capture, arg) -> node'
  node.is_error()         -> 1|0
  node.is_constant()      -> 1|0
  node.type()             -> abstract symbol
  node.val()              -> phi val (if .is_constant()) | crash

The most important invariant is that every node always exists in its
most-evaluated state. This means that node constructors will constant-fold
whenever possible; for instance, trying to construct C<plus(int(1), int(2))>
would actually produce C<int(3)>.

Obviously, eager partial-evaluation is asking for trouble: first, what happens
when you have an unknowable decision over a recursive function? Eager evaluation
would demand that we fully force the recursion, which might never end. And this
brings us to the final part of C<eval>, the speculation limiter.

=head3 C<eval>, speculative execution, and entropy
As a human, you have an intuitive sense about code that you're analyzing: you
might feel optimistic about something straightforward like a list-map function,
but you would quickly give up on C<sha256>. One reason you might feel this way
is that C<map> is highly structured: any input entropy is confined to a small
region of output; whereas C<sha256> is specifically designed to distribute
entropy throughout its output value and internal state. Put differently, C<map>
is more easily reverse-engineered than C<sha256>.

phi can simulate this intuition by limiting the amount of entropy it's willing
to speculate about. For example, maybe we have a limit of four bits of total
entropy; then we would speculatively evaluate four iterations of C<map>:

  map f xs = match xs with              # two outcomes = one bit of entropy
    | x::xs' -> (f x) :: map f xs'
    | []     -> []
    | _      -> crash                   # this branch is implied

This is a bit of a lie, of course; there are some lists for which we would
speculatively evaluate many more iterations -- and that has to do with
entanglement.

=head3 Entanglement and collapse
Let's take a trivial example:

  x == nil ? (x == nil ? 1 : 2)
           : (x == nil ? 3 : 4)

If we assume C<x> is an unknown (either cons or nil) and we apply the naive
entropy formula to this expression, we'll get two bits -- but the second is
obviously fictitious because it's identical to the first. Both bits are
predicated on the same underlying unknown quantity, which makes them entangled.

One way to model this is to propagate assertions:

  x == nil ? [assume   x == nil ] (x == nil ? 1 : 2) -> 1
           : [assume !(x == nil)] (x == nil ? 3 : 4) -> 4

Not everything is quite so trivial, for instance:

  x xor y ? (x ? (y ? 1 : 2) : (y ? 3 : 4))
          : (x ? (y ? 5 : 6) : (y ? 7 : 8))

Although C<x xor y> doesn't tell us either value, we can still eliminate any
branch for which C<x xor y> differs from our asserted value. This is a
convenient model because it allows us to use arbitrarily complex expressions as
assertions without directly reverse-engineering them.

TODO: decide on a representation for entropy and dependencies


=head2 Abstract value protocol, without the lies
This is basically the same as before; the main change is that now nodes contain
some probabilistic elements:

  node.eval(context, capture, arg) -> node'
  node.p_error(context)            -> float
  node.entropy(context)            -> float
  node.type()                      -> abstract symbol
  node.val()                       -> val | crash

=cut


package phiabstract;
use strict;
use warnings;

use phiboot;
use phibootmacros;
use phiparse;
use phiobj;


=head2 Meta-abstracts
Basically, things that aren't actual values. These are used to force a parse
failure in situations where an abstract, rather than a parser, is being
returned. Because the parse continuation is required to match and invariably
fails, C<abstract_fail> is unparsable and will cause some amount of
backtracking.

NB: C<abstract_fail> nodes will never appear in an AST, and therefore will never
be evaluated. So we don't need to support the full protocol here.

=head3 C<abstract_error>
C<abstract_error> is conceptually similar to C<abstract_fail>, but refers to an
expression that would cause the interpreter to crash if it were evaluated at
runtime. Typically this happens when you do things like pass arguments of
incorrect types to primitive operators, for instance trying to uncons an int.
=cut

use phitype abstract_fail_type =>
  bind(parse_continuation => stack(3), phiparse::fail);

use phi abstract_fail => pcons pnil, abstract_fail_type;


use phitype abstract_error_type =>
  bind(message => isget 0),
  bind(eval => stack(3, 0));            # capture arg self -> self


=head2 Primitive nodes

=cut


1;

__END__

use phitype int_type =>
  bind(val      => isget 0),
  bind(with_val => isset 0),

  # Reject all postfix modifications; ints aren't operators
  bind(postfix_modify => stack(3), abstract_fail),

  bind(parse_continuation =>            # op vself self
    drop,
    pnil,                               # op self []

    # none case (must be last in the list, so first consed)
    stack(0, 1),                        # op self [cases] self
    identity_null_continuation, i_eval, # op self [cases] p
    i_cons,                             # op self [cases']

    # unowned op case
    stack(0, 1, 2),                     # op self [cases] op self
    unowned_as_postfix, i_eval,         # op self [cases] p
    i_cons,                             # op self [cases']

    # Now build up the list of other possibilities, then filter it down by
    # applicable precedence.
    stack(3, 2, 1, 0),                  # [cases] self op
    rot3l,                              # self op [cases]
    l(plus_op, xor_op),                 # self op [cases] +op
    applicable_ops_from, i_eval,        # [cases']

    phiparse::alt, swons);


=head2 Example type: integers
=head3 Integer parsing
The usual radix conversion:

  cs list-int = 0 cs list-int'
  n cs list-int' = match cs with
    []    -> n
    c:cs' -> (n*10 + (c-48)) cs' list-int'

=cut

use phi list_int1_mut => pmut;
use phi list_int1 => l
  dup, nilp,
  l(drop),
  l(i_uncons,                           # n cs' c
    lit 48, i_neg, i_plus, rot3l,       # cs' c-48 n
    lit 10, i_times, i_plus, swap,      # (n*10+(c-48)) cs'
    list_int1_mut, i_eval),
  if_;

list_int1_mut->set(list_int1);

use phi list_int => l lit 0, swap, list_int1, i_eval;


=head3 Ops for testing: C<+>, C<*>, and C<^>
These operate live as opposed to building an expression tree.
=cut

use phi int_type_mut => pmut;

use phi times_fn => l                   # v1 v2
  mcall"val", swap, mcall"val",         # n2 n1
  i_times, pnil, swons,                 # [n2*n1]
  int_type_mut, swons;                  # [n2*n1]::int_type

use phi plus_fn => l                    # rhs lhs
  mcall"val", swap, mcall"val",         # nl nr
  i_plus, pnil, swons,                  # [nl+nr]
  int_type_mut, swons;                  # [nl+nr]::int_type

use phi xor_fn => l                     # rhs lhs
  mcall"val", swap, mcall"val",         # nl nr
  i_xor, pnil, swons,                   # [nl^nr]
  int_type_mut, swons;                  # [nl^nr]::int_type

use phi plus_op_mut  => pmut;
use phi times_op_mut => pmut;
use phi xor_op_mut   => pmut;

use phi plus_op => pcons l(pcons(l(3, 0), op_precedence_type),
                           str_(pstr"+"),
                           l(plus_op_mut, philang::expr, i_eval, i_eval),
                           plus_fn),
                         owned_op_type;

use phi times_op => pcons l(pcons(l(2, 0), op_precedence_type),
                            times_fn,
                            l(times_op_mut, philang::expr, i_eval, i_eval),
                            abstract_fail,
                            pnil),
                          unowned_op_type;

use phi xor_op => pcons l(pcons(l(1, 0), op_precedence_type),
                          str_(pstr"^"),
                          l(xor_op_mut, philang::expr, i_eval, i_eval),
                          xor_fn),
                        owned_op_type;

plus_op_mut->set(plus_op);
times_op_mut->set(times_op);
xor_op_mut->set(xor_op);

use phi timesop_literal => local_ str_(pstr "*"), times_op;


use phitype int_type =>
  bind(val      => isget 0),
  bind(with_val => isset 0),

  # Reject all postfix modifications; ints aren't operators
  bind(postfix_modify => stack(3), abstract_fail),

  bind(parse_continuation =>            # op vself self
    drop,
    pnil,                               # op self []

    # none case (must be last in the list, so first consed)
    stack(0, 1),                        # op self [cases] self
    identity_null_continuation, i_eval, # op self [cases] p
    i_cons,                             # op self [cases']

    # unowned op case
    stack(0, 1, 2),                     # op self [cases] op self
    unowned_as_postfix, i_eval,         # op self [cases] p
    i_cons,                             # op self [cases']

    # Now build up the list of other possibilities, then filter it down by
    # applicable precedence.
    stack(3, 2, 1, 0),                  # [cases] self op
    rot3l,                              # self op [cases]
    l(plus_op, xor_op),                 # self op [cases] +op
    applicable_ops_from, i_eval,        # [cases']

    phiparse::alt, swons);

int_type_mut->set(int_type);

use phi int_literal => l
  rep_ oneof_(pstr join('', 0..9), lit 1),
  l(list_int, i_eval, pnil, swons, int_type, swons),
  phiparse::pmap, i_eval;


1;
