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


=head1 phi data structures
We could use lists for everything, but it would be horrifically slow. So let's
define some data structures to fix this. The most important of these is
C<array>, which uses bisection lists to get C<O(log n)> access/update time per
element.
=cut


package phidata;
use strict;
use warnings;

use Exporter qw/import/;
use phiboot;
use phibootmacros;
use phiobj;
use philist;

our @EXPORT =
our @EXPORT_OK = qw/ array_type array_from_list /;


=head2 Bisection tree data structure
The idea here is pretty simple: each bit of the index specifies head/tail. We
store the tree depth in the root object and pass that into the bisection
function. These structures are immutable and managed with two functions,
C<bisection_get> and C<bisection_update>.

A bisection tree looks like this in practice:

  # make a bisection tree of length 5 for the list [1, 2, 3, 4, 5]
  tree = ((1 :: 2) :: (3 :: 4)) :: ((5 :: nil) :: nil)

Then each access index is decomposed into bits to bisect the tree in C<log(n)>
time:

  # access tree[4]
  # 4 = 100 in binary, and we know up front that there are three levels
  bisection_get(tree, 4, 3) =
    tree.tail                           # (4 >> 2 & 1) == 1 == tail
        .head                           # (4 >> 1 & 1) == 0 == head
        .head                           # (4 >> 0 & 1) == 0 == head

=cut

use phi bisection_get_mut => pmut;
use phi bisection_get => l              # c n levels -> x
  dup,                                  # c n levels more?
  l(                                    # c n levels
    lit 1, i_neg, i_plus,               # c n levels-1
    stack(0, 1, 0),                     # c n levels-1 levels-1 n
    i_rsh, lit 1, i_and,                # c n levels-1 t|h
    l(rot3l, tail),
    l(rot3l, head),
    if_,                                # n levels-1 c'
    rot3r, bisection_get_mut, i_eval),  # array_get(c', n, levels-1)
  l(                                    # c n 0
    stack(2)),                          # c
  if_;

BEGIN { bisection_get_mut->set(bisection_get) }


use phi bisection_update_mut => pmut;
use phi bisection_update => l           # x c n levels -> c'
  dup,                                  # x c n levels more?
  l(                                    # x c n levels
    lit 1, i_neg, i_plus,               # x c n levels-1
    stack(0, 1, 0),                     # x c n levels-1 levels-1 n
    i_rsh, lit 1, i_and,                # x c n levels-1 bit
    l(                                  # x c n levels-1
      rot3l, i_uncons,                  # x n levels-1 ct ch
      stack(5, 2, 3, 1, 4, 0),          # ch x ct n levels-1
      bisection_update_mut, i_eval,     # ch ct'
      swons),                           # c'
    l(                                  # x c n levels-1
      rot3l, i_uncons,                  # x n levels-1 ct ch
      stack(5, 2, 3, 0, 4, 1),          # ct x ch n levels-1
      bisection_update_mut, i_eval,     # ct ch'
      i_cons),                          # c'
    if_),                               # c'
  l(stack(3)),                          # x
  if_;

BEGIN { bisection_update_mut->set(bisection_update) }


=head3 Bisection tree allocation
I'm afraid this is cheating. Since this is an immutable data structure, we can
preallocate everything using aliased children and then use a bisection list to
store the preallocated things. Then "allocation" is just C<lists[n]>, where C<n>
is the required bit depth. This is C<O(log n)> in the bit depth itself, which is
as fast as we can possibly go.

Now there's one complication here, and it's a dumb one: how do we know how many
integer bits we have for this runtime? I'm end-running this for now by assuming
that we always have 64 bits max, but this isn't a very elegant assumption and at
some point in the future it's likely to be false.
=cut

use phi bisection_level_mut => pmut;
use phi bisection_level => l            # level -> c
  dup,                                  # level bisect?
  l(                                    # level
    lit 1, i_neg, i_plus,               # level-1
    bisection_level_mut, i_eval,        # cnext
    dup, i_cons),                       # cnext::cnext
  l(drop, pnil),                        # nil
  if_;                                  # c

BEGIN { bisection_level_mut->set(bisection_level) }


use phi bisections_preallocated => le   #
  lit 64, bisection_level, i_eval,      # c(64)
  lit 6,  bisection_level, i_eval,      # c(64) r
  l(                                    # recur x r i
    dup,                                # recur x r i nonzero?
    l(                                  # recur x r i
      # Store r[i] = x, then recur with x' = x.head, i' = i-1
      stack(3, 0, 1, 2, 0, 2),          # recur x i x r i
      lit 6,                            # recur x i x r i 6
      bisection_update, i_eval,         # recur x i r'
      swap, lit 1, i_neg, i_plus,       # recur x r' i'
      rot3l, head, rot3r,               # recur x' r' i'
      stack(0, 3), i_eval),             # recur(recur, x', r', i')
    l(                                  # recur x r i
      # There's no such thing as a bisection tree of level 0, so we don't have
      # to do anything here; we can just return r.
      stack(4, 1)),                     # r
    if_),                               # x r f
  rot3r, lit 63,                        # f x r i
  stack(0, 3), i_eval;                  # r'


use phi bisection_new => l              # levels -> c
  bisections_preallocated, swap,        # xs levels
  lit 6, bisection_get, i_eval;         # xs[levels]


=head3 Fast integer log
TODO
=cut

use phi integer_log => l                # n
  ;


=head2 Array data structure
An OOP wrapper around bisection trees. We store the depth on the object to
reduce the amount of overhead involved.
=cut

use phitype array_type =>
  bind(bisection_tree      => isget 0),
  bind(bisection_levels    => isget 1),
  bind(with_bisection_tree => isset 0),

  bind(get =>                           # i self -> self[i]
    dup, mcall"bisection_tree",         # i self c
    rot3r, mcall"bisection_levels",     # c i levels
    bisection_get, i_eval),             # x

  bind(update =>                        # x i self -> self'
    dup, mcall"bisection_tree",         # x i self c
    nip, mcall"bisection_levels",       # x i self c levels
    stack(5, 0, 3, 1, 4, 2),            # self x c i levels
    bisection_update, i_eval,           # self c'
    swap, mcall"with_bisection_tree");  # self'


1;
