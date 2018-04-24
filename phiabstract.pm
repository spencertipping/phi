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

our @EXPORT;
our @EXPORT_OK;
BEGIN { *EXPORT_OK = \@EXPORT }


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

=head4 Mutability, interpreter forking, and unknowns
C<mut> cells are their own issue, but let's just talk about strings for a
moment. There are two reasons we can't use native strings to model abstract
ones:

1. Branches of an unknown condition may commit different modifications
2. The arguments to C<str> and C<sset> may be unknown

(1) alone isn't insurmountable: we could theoretically deep-clone the
interpreter and any string values it depends on. But (2) complicates the world
much further; now we need a way to track unknown-ness of various parts of a
string.

Because mutable values have identity, we also can't do the obvious thing of
naively replacing them with immutable journals. The problem arises when you do
something like this:

  [ "foo" dup 0 0 sset ]

C<dup> preserves the identity of its argument, so we'll end up with two
references to the same modified copy of C<"foo">.

This means we need a level of indirection: C<"foo"> creates an entry in the
interpreter's "mutable things" list and returns a mutable reference that refers
to it. This, of course, leads to a new problem: how does the interpreter quickly
determine which mutable things are being used? We need a garbage collector.


=head2 Entropy and speculation
A common theme above is that we want to limit the amount of entropy we
accumulate in the trace. What exactly does this mean, and what heuristics govern
"acceptable" vs "unacceptable" entropy? Let's kick this off by going on a wild
philosophical tangent.

The CPU executes a stream of instructions to achieve some result. Instructions
are somewhat redundant; the two instructions C<add $4, %rax; add $4, %rax> can
be replaced by a single C<add $8, %rax> with no loss of resulting information.
From the processor's point of view, we've done the same thing with less
information, which means we've compressed the instruction stream.

When we trace an interpreter, we're doing so with the expectation that we'll
identify opportunities to compress the concatenative instruction stream phi will
evaluate. C<sha256> is a case where this bet is likely to fail; C<map/sum> over
a fixed list structure is the opposite. Most programs fall somewhere in between
those two extremes.

Given this constraint alone, the correct strategy is to inline every function
call; that way we turn C<[f...] .> into C<f...>, saving two instructions. This
is clearly a terrible idea, though; we need some way to limit the amount of
memory we use to trace things (it's far more acceptable to have a program that
runs forever in finite space than to have one that runs forever in infinite
space). Before I get into how we do this, let's get philosophical again for a
moment.

=head3 Speculation and the journal
Optimization results in a journal: a list of things that we know have to happen
in order to reconstruct the desired output. If we wanted to do the bare minimum
with 15 pieces of flair, we could write an "optimizer" that simply journaled
every single intepreter instruction verbatim. This optimizer has some desirable
properties, most notably that it uses no memory at all because it's the identity
function. Its speculative entropy (and therefore state space) is zero.

Speculation, then, is the difference between the amount of stuff we've traced
and the amount of stuff we've committed to the journal.

=head3 The speculation bet
Tracing the interpreter involves stepping through each instruction and either
committing or speculating on it. Anything we commit will slow down the output,
and anything we speculate will increase the trace entropy. This tradeoff creates
an exchange rate between the two; unfortunately, it's a probabilistic one:
there's no guarantee that the next instruction will help us compress the
journal. And this means we're ultimately designing a betting strategy.

As is generally true in resource allocation problems, trace entropy cost isn't
necessarily linear: maybe we don't care about the first 2GB of memory usage but
every byte over that costs a minor fortune (or is just off-limits). It also
isn't sufficient to write an opaque function that returns the marginal cost of
the next bit of entropy because (1) we may not know real-world memory usage, and
(2) some deeper optimizations are nonlocal enough that we need some lookahead.
(For instance, what is the relative cost of committing to memory usage now given
that we might later want to speculate?)

Clearly this problem is pretty intractable, which should be no real surprise
because humans aren't great at solving it either. We've developed a mixture of
heuristics and conditioned opinions that get us decently close, and I think if
phi manages anything like this I'll call it a huge win.


=head2 Object/interpreter state modeling
As described earlier, objects and interpreters can't easily be separated;
mutable objects exist within the context of a heap, and heaps are owned by
interpreters. We also need to store the current journal and the list of
coercions we've produced so far; altogether we have the following state:

1. The heap
2. The journal
3. The set of crash coercions
4. The data stack
5. The continuation stack
6. The resolver

C<d>, C<c>, and C<r> are themselves abstract values which may refer into the
heap. The heap, journal, and crash coercion set are all concrete phi objects.

We need to think about performance here, so let's talk about how some of this
stuff works.

=head3 Mutable value journaling
C<mut> values are easy to model: they can either be C<[]> (unset) or C<[val]>
(set). Strings, on the other hand, aren't straightforward at all, especially if
we want them to be fast. There are a couple of obvious strategies for strings:

1. Use underlying strings that use multibyte to refer to arrayed abstracts
2. Use a bisection list of abstract objects

Of these, I think I prefer (2) simply because it creates less work for the
userspace garbage collector. It also gives us free string cloning.
=cut


1;
