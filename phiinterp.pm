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

use phitype interp_nullary_op_type =>
  bind(crash => i_crash);

use phitype interp_unary_op_type =>
  bind(head => head),
  bind(tail => tail),
  bind('i-' => i_neg),
  bind('i~' => i_inv),
  bind('i!' => i_not);

use phitype interp_binary_op_type =>
  bind(seql => drop),
  bind(seqr => top),
  bind(cons => swons),

  bind('i+'  => i_plus),
  bind('i*'  => i_times),
  bind('i<<' => i_lsh),
  bind('i>>' => i_rsh),
  bind('i<'  => i_lt),
  bind('i>'  => swap, i_lt),

  bind('i&'  => i_and),
  bind('i|'  => ior),
  bind('i^'  => i_xor);


use phi interp_nullary_op => pcons pnil, interp_nullary_op_type;
use phi interp_unary_op   => pcons pnil, interp_unary_op_type;
use phi interp_binary_op  => pcons pnil, interp_binary_op_type;


use phi interp_nullary => l             # node arg capture
  stack(2), mcall"op",                  # op
  interp_nullary_op, i_eval;            # interp_nullary_op.<op>()

use phi interp_unary => l               # node arg capture
  stack(0, 2), mcall"lhs",              # node arg capture lhs
  rot3r, interp_mut, i_eval,            # node vlhs
  swap, mcall"op",                      # vlhs op
  interp_unary_op, i_eval;              # interp_unary_op.<op>(vlhs)

use phi interp_binary => l              # node arg capture
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
concatenative function from within op-tree code or vice versa.

TODO
=cut


1;
