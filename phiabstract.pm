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


=head1 phi abstract interpreter
Simulates the concatenative interpreter used to execute phi programs. In doing
so, we can aggressively fold constants to get partial evaluation, which should
in turn yield specialized/optimized concatenative code. This is a prerequisite
for higher-level abstractions, whose unoptimized versions run far too slowly to
be useful.

I think this concept is the same thing as Futamura projection and partial
evaluation. It may differ a bit because phi is built around immutable objects
(or objects that can easily be modeled in immutable terms), so I use some
different terminology here. I also haven't read the Futamura paper or anything
so my design might differ arbitrarily.

The core idea here is that we reduce the number of concatenative operations
required to do something, in our case by simulating the evaluator applied to
partially-constant expressions. Those degrees of specification allow us to
eliminate certain conditional branches and allocation. For example, most of the
method calls made against objects involve clearly-redundant linear scans to
resolve symbols to method implementations. We can pre-resolve and ultimately
inline any monomorphic call site.

Rather than applying specific optimizations, we simulate the entire interpreter
state as a single abstract value and treat it as a system that needs to be
reverse-engineered. For example, if we have a function C<f> that we know takes
and returns two integers, we create two unknown quantities C<x> and C<y>, push
them on the stack, and then measure the interpreter after applying C<f>. We then
have a (presumably) faster pseudo-instruction that will behave identically to
C<f>.
=cut


package phiabstract;
use strict;
use warnings;

use Exporter qw/import/;
use phiboot;
use phibootmacros;
use philist;
use phiparse;
use phiobj;


=head2 Constant folding
Ultimately, everything we're doing here is predicated on the idea that we can
reduce some intermediate expression to a constant. There are cases where we can
do this without knowing the expression fully; e.g:

  cons(unknown, 5)                      # .type = cons, .tail = 5
  unknown(int)                          # .type = int

From there, we can optimize in four ways:

1. Reducing subexpressions to constants
2. Flattening C<if> statements if the condition is known
3. Dropping unused expressions
4. Crash forcing: remove any branch that would cause a crash

Most languages would make (3) difficult, but it's straightforward in phi because
we simulate the interpreter itself as a value as we step through it. This means
we can effectively trace the side effect space of a function and emit a new list
based on that.

(4) is fun and is ultimately why phi provides no way to catch errors. If we
assume that the program completes without crashing (which is generally a safe
assumption), then we can produce a result which also will not crash. This means
that we can sometimes fold _unknown_ conditional values; for example:

  [ unknown_int dup [crash] [<some code>] if ]

If C<unknown_int> is nonzero, the program will crash; so we now know it must be
zero: the possibility of a crash has forced the value. As a result, the above
program will be reduced to this:

  [ 0 <some code> ]

The same logic applies anytime we invoke an instruction illegally, e.g.
C<uncons> or C<restack> on a non-cons value. Those branches will be removed and
their corresponding unknowns forced.


=head2 Basic strategy: trace an interpreter
A phi program can be defined by two things: the state of the interpreter it
returns, and the journal of side-effects involved in getting there. The side
effect journal can itself be a concatenative list, so an interpreter trace can
have a standard form:

  [ <side effect replay...> [d' c' r'] i< ]

We can often simplify things a bit further since most programs don't modify C<c>
or C<r>, but the above form is a good starting point.

Let's get into some examples, starting with simple immutable expressions.

=head3 Example: sum a mapped list
The input program is C< [x y z] [1 +] map sum >, for which C<x>, C<y>, and
C<z> are unknown ints. C<map> and C<sum> are defined like this:

  map = [swap dup nil?                  # see list_map definition in philist.pm
         [top]
         [uncons [0 2] restack .
          [3 2 1 0] restack map swap cons]
         if]

  sum = [dup nil?                       # xs nil?
         [drop 0]                       # 0
         [uncons swap                   # x xs'
          sum +]                        # x+sum(xs')
         if]

This example is fairly easy because all of the list structure is constant, so
C<map> and C<sum> can be fully erased. Here's what that process looks like:

  [[]                      [[[x y z] [1 +] map sum]] []]
  [[[x y z]]               [[[1 +] map sum]]         []]
  [[[1 +] [x y z]]         [[[swap dup...] sum]]     []]
  [[[x y z] [1 +]]         [[[dup nil?...] sum]]     []]
  [[[x y z] [x y z] [1 +]] [[[nil? ...] sum]]        []]
  [[0 [x y z] [1 +]]       [[[...if] sum]]           []]

At this point we can constant-fold the first C<if> branch since its condition is
known. I'm going to start skipping some steps for brevity.

  [[[x y z] [1 +]]       [[uncons [0 2]...] [sum]]      []]
  [[x [y z] [1 +]]       [[[0 2] restack...] [sum]]     []]
  [[[1 +] x [y z] [1 +]] [[. ...] [sum]]                []]
  [[x [y z] [1 +]]       [[1 +] [...] [sum]]            []]
  [[1 x [y z] [1 +]]     [[+] [...] [sum]]              []]
  [[x+1 [y z] [1 +]]     [[[3 2 1 0] restack...] [sum]] []]

  [[[1 +] [y z] x+1]     [[map ...] [sum]]              []]
  # recursion!
  [[[y+1 z+1] x+1]       [[swap cons] [sum]]            []]
  [[x+1 [y+1 z+1]]       [[cons] [sum]]                 []]
  [[[x+1 y+1 z+1]]       [[sum]]                        []]

Now let's walk through C<sum>:

  [[[x+1 y+1 z+1]]       [[dup nil? ...]]               []]
  [[0 [x+1 y+1 z+1]]     [[...if]]                      []]
  [[[x+1 y+1 z+1]]       [[uncons swap...]]             []]
  [[[y+1 z+1] x+1]       [[sum +]]                      []]
  # recursion!
  [[y+1+z+1+0 x+1]       [[+]]                          []]
  [[x+1+y+1+z+1+0]       []                             []]

...and that's where it ends. We now have a final interpreter state that can be
reconstructed with far fewer instructions, since all of the C<map> and C<sum>
logic can be reduced to a series of constants and C<+>.

=head3 C<map/sum> is simplistic
I picked that example because it illustrates the upside of partial evaluation,
but it differs from real-world code in a few ways:

1. Some conditions can't be flattened
2. Some functions like C<sha256> can't be reverse-engineered easily
3. Real-world code involves mutable values like strings
4. There will be cases where we care about aliasing

Let's go through these in detail.

=head4 Unflattenable conditions
The list passed into C<map> had a fully-specified structure in our example, but
let's suppose that it instead had involved an unknown:

  [ [1]::xs map sum ]                   # xs is an unknown value

We can crash-force C<xs> into a cons or nil, but beyond that what do we do? If
we freely speculate on both branches of C<map>'s C<if>, we'll inline the
recursive call and run forever. We need some way to measure the marginal utility
of further speculation.

This is a pretty involved issue, so I discuss the bulk of it in its own section
below.

=head4 C<sha256>
Example implementation:
L<https://github.com/B-Con/crypto-algorithms/blob/cfbde48414baacf51fc7c74f275190881f037d32/sha256.c>

C<sha256> is unusual in that it contains no unknown conditional expressions but
is impossible to reverse-engineer; that is, its output entropy is always the
same as its input entropy in practical terms. (A thought experiment: suppose we
defined a new function, C<sha256-onebit>, that simply returned the
least-significant bit of the hash; our luck is no better even though the entropy
is fully collapsible.)

I should be more specific about what exactly goes wrong here. Let's suppose we
have a phi implementation of C<sha256> that takes a list of ints (one per byte,
for instance) and calculates the hash. Everything is fine if the list is fully
constant, but what happens if we introduce an unknown?

  [ [ 1 2 3 x 5 6 7 8 ... ] sha256 ]

As with any constant function, the result here will be an expression tree with a
dependency on C<x>; that much is no surprise. The problem is that our memory use
becomes proportional to CPU runtime: our output expressions will end up
unrolling every loop and getting no (or negligible) advantage in the process. We
could easily end up generating code that is inferior to what we started with
simply due to its size.

Put differently, there are heuristics that govern when it makes sense to trace
expressions. C<sha256> shouldn't be traced because its trace entropy exceeds its
output entropy, and because the trace is effectively incompressible.

All of this changes if we model the unknown C<x> as an enumerable union rather
than an unknown. In that case we can specialize it to each possible constant,
evaluate C<sha256> on each, and end up with a union of the results. At that
point we will have effectively reverse-engineered C<sha256> for the use case at
hand: we could start with an output value and reconstruct the C<x> that produced
it.

=head4 Mutability
TODO

=cut


1;
