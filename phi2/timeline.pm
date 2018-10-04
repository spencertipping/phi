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
I'm not quite sure how this works, so I guess I'll start with what's obviously
true:

1. Inlined functions produce timelines computed from arguments
2. Non-inlined functions produce some sort of polymorphic call/return structure
3. Timelines are values, often with no left-links (i.e. they're constant)
4. Timelines can be computed at runtime, in which case we have JIT
5. The compiler is a function from stuff to timelines
6. C<root> can flatmap timelines
7. Optimization is timeline algebra

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


=head3 Speculation
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

...so basically, a link leftwards from B to A should indicate that A is the
earliest moment when B becomes computable.


=head3 Specialization
Timelines tell us when we can evaluate things. If an expression has no
left-facing links -- i.e. no dependencies -- then its meaning is time-invariant;
C<3 + 4> always means 7, regardless of where we are on the global timeline.

Specialization happens when we transform timelines in a way that removes
left-facing links. There are a few ways this can happen:

1. Forward propagation -- i.e. abstract interpretation
2. Speculation, e.g. simulating both directions of an C<if>
3. On demand, e.g. profile-guided JIT

Technically (3) is always happening; left links effectively vanish as
expressions are evaluated.


=head3 Timelines as first-class values
C<root> is objective only from the inside; if there are two phi runtimes, each
will have a C<root> and synchronization/delegation happen over some sort of RPC.

Another way to say it is that timelines are applied functions. Saying "hey
remote, merge this timeline" is exactly the same as saying "hey remote, call
this function" -- except that timelines, being first-class values, can
interleave their execution in various ways, including within the same serial
runtime. This is useful for things like embedded programming, where you can
simulate multithreaded processing within a single thread by having timelines
that interleave instructions and track the cycle counts to guarantee latencies.

Structurally speaking, timelines are defined by their ability to run themselves
against a stack -- that is, flatmap themselves into C<root> or anywhere else.

TODO: is this strictly true? Or do we want the default definition to be virtual?

=cut


1;
