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


=head2 Integer functions
We can find the log of an integer in C<log(#bits)> steps by bisecting and
comparing. The only complication is the high bit, which may indicate sign -- for
now I'm ignoring this BUT THIS IS HORRIBLE FOR GODS SAKE FIXME.

C<integer_log> returns the one-based index of the highest bit in the integer, so
C<integer_log(4) == 3> and C<integer_log(0) == 0>.
=cut

use phi integer_log => l                # n
  lit 0,                                # n log
  l(                                    # recur n log bit
    dup, lit 0, swap, i_lt,             # recur n log bit bit<0?
    l(stack(4, 1)),                     # log
    l(                                  # recur n log bit
      nip, nip, lit 1, i_lsh, ior,      # recur n log bit log'=log|1<<bit
      dup, lit 1, i_lsh,                # recur n log bit log' 1<<log'
      stack(0, 4), i_lt,                # recur n log bit log' n<(1<<log')?
      l(drop),                          # recur n log  bit
      l(stack(3, 1, 0)),                # recur n log' bit
      if_,                              # recur n log' bit
      lit 1, i_neg, i_plus,             # recur n log' bit'
      stack(0, 3), i_eval),             # recur(recur, n, log', bit')
    if_),                               # log
                                        # n log recur
  rot3r, lit 5,                         # recur n log bit
  stack(0, 3), i_eval,                  # intlog(n)-1
  lit 1, i_plus;                        # intlog(n)


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

The trailing C<nil :: nil> term (as opposed to just C<nil>) doesn't use any
extra space. There's some cool stuff going on to make C<nil> cells free; see
C<bisection_new> for details.
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
    dup, lit 0, swap, i_lt,             # recur x r i i<0?
    l(stack(4, 1)),                     # r
    l(                                  # recur x r i
      # Store r[i] = x, then recur with x' = x.head, i' = i-1
      stack(3, 0, 1, 2, 0, 2),          # recur x i x r i
      lit 6,                            # recur x i x r i 6
      bisection_update, i_eval,         # recur x i r'
      swap, lit 1, i_neg, i_plus,       # recur x r' i'
      rot3l, head, rot3r,               # recur x' r' i'
      stack(0, 3), i_eval),             # recur(recur, x', r', i')
    if_),                               # x r f
  rot3r, lit 63,                        # f x r i
  stack(0, 3), i_eval;                  # r'


use phi bisection_new => l              # levels -> c
  lit 1, i_neg, i_plus,                 # levels-1
  bisections_preallocated, swap,        # xs levels-1
  lit 6, bisection_get, i_eval;         # xs[levels-1]


=head2 Array data structure
An OOP wrapper around bisection trees. We store the depth on the object to make
it easier to use.
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


use phi array_new => l                  # size
  integer_log, i_eval,                  # levels
  dup, bisection_new, i_eval,           # levels tree
  swap, pnil, swons, swons,             # [tree levels]
  array_type, swons;                    # [tree levels]::array


=head2 Associative data structure
Preallocated, aliased level conses means that we can (as we did) allocate every
possible bisection tree up front, which in turn means that we have a full-depth
tree waiting for us already. This is great because we can address the full range
of integers as an array, which makes it trivial to do associative operations.

TODO: we need a way to quickly calculate hashcodes for various values. I have an
C<id> instruction in the design doc, but it's unclear to me how it needs to work
and whether symbols have some special C<id> behavior. Do we want hosted
hashcoding?
=cut

# TODO: write this once I get the above object ID stuff figured out


=head2 Bitset data structure
This one is mutable and optimized for performance. It's backed by a string,
provides constant access time, and has much lower overhead than using an array
of ints.

Bitsets will crash if you address bits outside their configured capacity.
=cut

use phitype mutable_bitset_type =>
  bind(string => isget 0),
  bind(size   => isget 1),              # NB: size is in bits

  bind(get =>                           # i self -> 1|0
    mcall"string", nip, lit 3, swap,    # i s 3 i
    i_rsh, i_sget,                      # i s[i>>3]
    swap, lit 7, i_and,                 # s[i>>3] i&7
    lit 1, i_lsh, i_and, i_not, i_not), # !!(s[i>>3]&1<<(i&7))

  bind(set =>                           # i self -> self
    nip, lit 7, i_and, lit 1, i_lsh,    # i self mask
    nip, mcall"string",                 # i self mask s
    stack(0, 3), lit 3, swap, i_rsh,    # i self mask s i>>3
    stack(0, 0, 1), i_sget,             # i self mask s i>>3 c=s[i>>3]
    stack(4, 3, 0, 1, 2), ior,          # i self s i>>3 c'
    i_sset, stack(3, 1)),               # self

  bind(clear =>                         # i self -> self
    nip, lit 7, i_and, lit 1, i_lsh,    # i self ~mask
    lit 255, i_xor,                     # i self mask
    nip, mcall"string",                 # i self mask s
    stack(0, 3), lit 3, swap, i_rsh,    # i self mask s i>>3
    stack(0, 0, 1), i_sget,             # i self mask s i>>3 c=s[i>>3]
    stack(4, 3, 0, 1, 2), i_and,        # i self s i>>3 c'
    i_sset, stack(3, 1));               # self


use phi mutable_bitset_new => l         # bits
  dup, lit 7, i_plus,                   # bits bits+7
  lit 3, swap, i_rsh, i_str,            # bits s
  swap, pnil, swons, swons,             # [s bits]
  mutable_bitset_type, swons;           # [s bits]::mutable_bitset


1;
