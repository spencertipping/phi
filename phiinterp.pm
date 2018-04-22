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

=head1 phi optree interpreter
An eager interpreter for phi op trees. This works about like you'd expect: we do
a recursive tree reduction using a reducer function.
=cut

package phiinterp;
use strict;
use warnings;

use Exporter qw/import/;
use phiboot;
use phibootmacros;
use philist;
use phiobj;
use phioptree;

our @EXPORT =
our @EXPORT_OK = qw/ interp /;


=head2 Entry point
The main function here is C<interp>, which works like this:

  node argv capturev interp -> val|crash

It's ok (and desirable) to crash the hosting interpreter. We want failures to be
indicated eagerly so an abstract interpreter can eliminate branches as soon as
possible.
=cut

use phi interp_mut => pmut;


=head2 Simple node cases
Most nodes don't require very much work, so let's get those out of the way
early.
=cut

use phi i_native_const => l             # node arg capture
  stack(2),                             # node
  mcall"native";                        # v

use phi i_arg => l                      # node arg capture
  stack(3, 1);                          # arg

use phi i_capture => l                  # node arg capture
  stack(3, 0);                          # capture

use phi i_capture_nth => l              # node arg capture
  # capture_nth is an indexed capture from the end of the list, so we need to
  # subtract the index from the list length, which is stored at the head.
  dup, head,                            # node arg capture cl
  stack(4, 3, 0, 1),                    # capture cl node
  mcall"n", i_neg, i_plus,              # capture cl-n
  lget, i_eval;                         # capture[cl-n]

use phi i_if => l                       # node arg capture
  stack(0, 2), mcall"cond",             # node arg capture cond
  stack(0, 1, 2), interp_mut, i_eval,   # node arg capture condv

  l(rot3l, mcall"then", rot3r),         # then arg capture
  l(rot3l, mcall"else", rot3r),         # else arg capture
  if_,
  interp_mut, i_eval;                   # interp(then|else, arg, capture)


=head3 Syntax/alias nodes
Alias nodes are real values, but syntax ones aren't. These arise as the result
of some dialect operations and handling them is a bit subtle: it isn't strictly
an error to try to evaluate them (even unresolvable); we just need to return a
value that contains no data at all. A self-referential mut works for this and is
detected by interpreters if you try to access it.
=cut

use phi i_syntax => l                   # node arg capture
  stack(3),                             #
  i_mut, dup, i_mset;                   # mutbomb

use phi i_alias => l                    # node arg capture
  rot3l, dup, mcall"can_be_real",       # arg capture node real?
  l(mcall"real_node",                   # arg capture node'
    rot3r, interp_mut, i_eval),         # interp(node', arg, capture)
  l(stack(3),                           #
    i_mut, dup, i_mset),                # mutbomb
  if_;


=head2 Strict fixed-arity operations
These are implemented as method calls against delegate objects. For example, if
we have a binary operator called C<i+> (which we do), then it will be
implemented by the C<i+(v1, v2)> method on the C<interp_binary_ops> object.
=cut

# NB: bindop() just shifts off self, since there's no reason to refer to it
# within operator implementations
sub bindop { bind(shift, drop, @_) }

use phitype interp_nullary_op_type =>
  bindop(crash => i_crash);

use phitype interp_unary_op_type =>
  bindop(head => head),
  bindop(tail => tail),
  bindop('i-' => i_neg),
  bindop('i~' => i_inv),
  bindop('i!' => i_not);

use phitype interp_binary_op_type =>
  bindop(seql => drop),
  bindop(seqr => top),
  bindop(cons => swons),

  bindop('i+'  => i_plus),
  bindop('i*'  => i_times),
  bindop('i<<' => i_lsh),
  bindop('i>>' => i_rsh),
  bindop('i<'  => i_lt),
  bindop('i>'  => swap, i_lt),

  bindop('i&'  => i_and),
  bindop('i|'  => ior),
  bindop('i^'  => i_xor);


use phi interp_nullary_op => pcons pnil, interp_nullary_op_type;
use phi interp_unary_op   => pcons pnil, interp_unary_op_type;
use phi interp_binary_op  => pcons pnil, interp_binary_op_type;


use phi i_strict_nullary => l           # node arg capture
  stack(2), mcall"op",                  # op
  interp_nullary_op, i_eval;            # interp_nullary_op.<op>()

use phi i_strict_unary => l             # node arg capture
  stack(0, 2), mcall"lhs",              # node arg capture lhs
  rot3r, interp_mut, i_eval,            # node vlhs
  swap, mcall"op",                      # vlhs op
  interp_unary_op, i_eval;              # interp_unary_op.<op>(vlhs)

use phi i_strict_binary => l            # node arg capture
  stack(0, 2, 0, 1), mcall"lhs",        # node arg capture arg capture lhs
  rot3r, interp_mut, i_eval,            # node arg capture vlhs
  stack(3, 3, 1, 2, 0), mcall"rhs",     # node vlhs arg capture rhs
  rot3r, interp_mut, i_eval,            # node vlhs vrhs
  rot3l, mcall"op",                     # vlhs vrhs op
  interp_binary_op, i_eval;             # interp_binary_op.<op>(vlhs, vrhs)


=head2 Functions, calls, and concatenative/applicative bridging
OK, this is where things get seriously awesome.

It's fairly trivial to implement a pure op-tree function calling mechanism with
what we have so far; that's why we pass C<arg> and C<capture> through the
evaluator. That strategy would get us an interpreter that seems to serve the
purpose: we can evaluate anything within op-tree space.

We can do better, though. We know that the op tree stuff is hosted within a
concatenative interpreter, so there's a possibility that you'd want to use a
concatenative function from within op-tree code or vice versa. To do this, we
define a consistent translation convention that allows these two function types
to interoperate.

L<phioptree> has no intrinsic awareness of the stack because it's applicative,
not concatenative. So any concatenative interoperability involves crossing the
arity barrier; applicative functions are strictly unary. There are a few ways we
can do this (also mentioned in L<phifront.md>):

1. Have functions declare in/out arities and adapt to the stack
2. Have functions take and return entire data stacks
3. Have functions take and return entire interpreters

Of these, (2) makes the most sense -- and is very simple to implement. If we
have an applicative function object C<f>, we can wrap it in a list like this to
turn it into a concatenative function:

  [                                     # d... f
    i> head tail                        # d... f [d]
    call-node                           # d... call(f, [d])
    nil nil interp                      # d... d'
    d<                                  # d'...
  ]

Applicative can call back into concatenative using a bit of continuation stack
trickery. Specifically, we stash the current data stack (which contains
interpreter state) into the continuation, use C<< d< >> to set everything up for
the function, quote the resulting data stack, and cons it onto the one we
stored. Here's what that looks like:

  [                                     # orig... [dstack] cf
    i> head tail tail                   # orig... [dstack] cf [orig]
    [                                   # dstack'... [orig]
      i> head uncons                    # [dstack'] [orig]
      swons                             # [[dstack'] orig]
      d<                                # orig... [dstack']
    ]                                   # orig... [dstack] cf [orig] f
    swons                               # orig... [dstack] cf [[orig] f.]
    '. cons                             # orig... [dstack] cf [. [orig] f.]
    swons                               # orig... [dstack] [cf . [orig] f.]
    'd< cons                            # orig... [dstack] [d< cf . [orig] f.]
    .                                   # orig... [dstack']
  ]

=head3 The upshot of this
Basically, two things:

1. Interpreting a function node returns a concatenative closure.
2. Every function call is against a concatenative function.

This will often end up doing exactly what would happen if we interpreted the
function body directly, but we'll also be able to use concatenative "native"
functions seamlessly. An added benefit is that you can effectively JIT functions
by having infix expressions that cons up lists, then calling into them.

NB: we assume C<capture> always produces a self-quoting value, in this case a
list of captured quantities. If it doesn't, everything will fail in horrific
ways.
=cut

use phi to_concatenative => l           # d... body capture
  i_quote, head, tail, tail,            # d... body capture [d]
  swap, interp_mut, i_eval,             # d... [d']
  i_dset;                               # d'...

use phi to_applicative => l             # orig... [dstack] cf
  i_quote, head, tail, tail,            # orig... [dstack] cf [orig]
  l(                                    # dstack'... [orig]
    i_quote, head, i_uncons,            # dstack'... [dstack'] [orig]
    swons,                              # dstack'... [[dstack'] orig]
    i_dset                              # orig... [dstack']
  ),                                    # orig... [dstack] cf [orig] f
  swons,                                # orig... [dstack] cf [[orig] f.]
  lit i_eval, i_cons,                   # orig... [dstack] cf [. [orig] f.]
  swons,                                # orig... [dstack] [cf . [orig] f.]
  lit i_dset, i_cons,                   # orig... [dstack] [d< cf . [orig] f.]
  i_eval;                               # orig... [dstack']


use phi i_fn => l                       # node arg capture
  stack(0, 2), mcall"capture",          # node arg capture cnode
  rot3r, interp_mut, i_eval,            # node cval
  swap, mcall"body",                    # cval body
  swap, to_concatenative,               # body cval tc
  swons, swons;                         # [body cval tc...]

use phi i_call => l                     # node arg capture
  stack(0, 2, 0, 1), mcall"fn",         # node arg capture arg capture fnode
  rot3r, interp_mut, i_eval,            # node arg capture f
  stack(4, 3, 1, 2, 0), mcall"arg",     # f arg capture argnode
  rot3r, interp_mut, i_eval,            # f a
  swap, to_applicative, i_eval;         # f(a)


=head2 Main dispatch
Now we have all of the node types implemented, so we just need to write the main
interpreter function to select the node-specific alternative.

Luckily there's an easier way to do this than writing a bunch of if-statements.
=cut

use phi interp_unimplemented => l lit interp_unimplemented => i_crash;

use phi interp_cases => le              #
  l((interp_unimplemented) x 16),       # ilist
  i_native_const,    lit t_native_const,   lset, i_eval,
  i_arg,             lit t_arg,            lset, i_eval,
  i_capture,         lit t_capture,        lset, i_eval,
  i_capture_nth,     lit t_capture_nth,    lset, i_eval,
  i_fn,              lit t_fn,             lset, i_eval,
  i_strict_binary,   lit t_strict_binary,  lset, i_eval,
  i_strict_unary,    lit t_strict_unary,   lset, i_eval,
  i_strict_nullary,  lit t_strict_nullary, lset, i_eval,
  i_if,              lit t_if,             lset, i_eval,
  i_call,            lit t_call,           lset, i_eval,
  i_syntax,          lit t_syntax,         lset, i_eval,
  i_alias,           lit t_alias,          lset, i_eval;


use phi interp => l                     # node arg capture
  stack(0, 2), node_type,               # node arg capture t
  interp_cases, swap, lget, i_eval,     # node arg capture f
  i_eval;

interp_mut->set(interp);


1;
