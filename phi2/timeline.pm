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
=cut

package phi;

use strict;
use warnings;


=head2 Timelines
phi is an imperative language, which means that regardless of the paradigm we're
presenting through any given dialect we ultimately need to commit to some side
effect schedule that encodes the semantics we want. We do this with timelines.

Timelines are branching structures; if two events need to happen and we don't
care about their relative ordering, then we have a fork and a join:

                event A
               /       \
  ...timeline .         . timeline...
               \       /
                event B

Backend interpreters or compilers can then choose an arbitrary ordering of A and
B, or can execute both at once (e.g. on different processors or machines).


=head3 Speculation and commits
phi begins with a single timeline C<root> that represents global program actions
that are ultimately visible to the kernel. Anything you merge into this timeline
is guaranteed to happen, which means it's committed: you can't undo these
merges.

Not every timeline ultimately needs to be committed. If you write an expression
that does a bunch of computation but doesn't create any side effects, that
timeline can remain unmerged from C<root> until you entangle its result. For
example:

  int x = 5;                            # no root entanglement
  int y = x + 1;                        # ditto
  int z = y > 5 ? y : 9;                # ditto
  int q = print(z);                     # z is entangled here

Structurally speaking, the above looks like this (if we treat C<print> as being
opaque):


          ;      ;             ;        <- sequence points: timelines are merged

                          [9]           <- [9] is unentangled
                             \
       [5]--[+ 1]--[> 5]?-----.
                         \   / \
                          [y]   \
                                 \
  root ---------------------------[print(z)]---...

C<root> doesn't fork to C<z>'s timeline because C<z> doesn't depend on C<root>
at all. This means we can schedule it anywhere before C<print(z)>, including at
compile time.


=head3 Specialization
Timelines tell us when we can evaluate things. If an expression has no
left-facing links -- i.e. no dependencies -- then its meaning is time-invariant;
C<3 + 4> always means 7, regardless of where we are on the global timeline.

Specialization happens when we transform timelines in a way that removes
left-facing links.


=head3 Functions


=cut


1;
