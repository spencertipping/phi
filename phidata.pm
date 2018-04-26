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
  tree = ((1 :: 2) :: (3 :: 4)) :: ((5 :: nil) :: (nil :: nil))

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

bisection_get_mut->set(bisection_get);


use phi bisection_update_mut => pmut;
use phi bisection_update => l           # x c n levels -> c'
  dup,                                  # x c n levels more?
  l(                                    # x c n levels
    lit 1, i_neg, i_plus,               # x c n levels-1
    stack(0, 1, 0),                     # x c n levels-1 levels-1 n
    i_rsh, lit 1, i_and,                # x c n levels-1 bit
    # TODO
    ),
  l(stack(3)),                          # x
  if_;

bisection_update_mut->set(bisection_update);


1;
