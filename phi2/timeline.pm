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
Code doesn't directly map to an evaluation schedule; there are too many degrees
of freedom to commit to a single ordering, and not all programs that are parsed
are intended to be evaluated at all. Instead, the parse output is something more
generally informative: a set of functional dependencies that describe the
calculations required to execute the code. The degree of sequencing is encoded
by the dialect and the abstracts, and collectively the calculation set is a
graph called a timeline.

Functionally speaking, timelines are phi's intermediate representation and form
a variation of CPS that specifies which expressions are temporally independent.
More broadly:

1. Timelines can be trivially converted to bytecode
2. Bytecode produced by a timeline links back to the timeline object
3. Backend-specific JIT compilers use timelines as input
4. Bytecode can be trivially converted to a GC-unsafe timeline
5. Timelines preserve/store CTTI
6. Timelines preserve/store all method calls, including monomorphic ones
7. Timelines can generate and splice more timelines as first-class values

(5) and (6) are important for algebraic analysis. Basically, the idea is that we
want timelines to be a lossless semantic encoding of the source they came from.
This means no type or intent erasure: if we have a situation where an abstract
defines two methods that compile to the same underlying function, we should
still be able to know which one was called.

(7) is how control flow works, but more generally it specifies that code isn't
confined to a static compilation model.


=head3 Timeline nodes
Timelines are made of these types of nodes:

1. C<< const(X, ctti)               -> value, ctti >>
2. C<< arg(name, ctti)              -> value, ctti >>
3. C<< method(ctti, m)              -> timeline(...) >>
4. C<< code(X, [inputs], [outputs]) -> timeline([inputs], [outputs]) >>
5. C<< call(timeline, args...)      -> v1, v2, ..., vN >>
6. C<< return(vals...) >>
7. C<< goto(timeline, args...) >>

NB: C<arg> and C<return> seem like hacks, but they aren't. C<call> and C<goto>
relink those nodes when we want to make an inline function call, and any
timeline that gets compiled into an IR node will use C<arg> and C<return> to
construct the C<ir_fn> node.

Node linkages always point towards dependencies:

  method-----------+       call-------------+
  |                |       |                |
  |ctti            |       |            ret1|<--- ...
  |        timeline|<------|timeline    ret2|<--- ...
  |method          |       |             ...|<--- ...
  |                | +-----|arg1            |
  +----------------+ | +---|arg2            |
                     | |   +----------------+
     const(5)-+      | |
     |   value|<-----+ |
     +--------+        |
                       |
     const(0)-+        |
     |   value|<-------+
     +--------+


=head3 Optimization mechanics
First, timeline rewrites aren't lossy. Rewritten timelines store C<source>
pointers that refer to the input of the rewrite in question; this makes it much
easier to debug things. Timeline nodes aren't entirely immutable, but their
canonical representation is. I'll explain this in more detail below.

Optimization parsers often look for multilayer structures, most of which won't
match due to some property of a child node. For example, many CTTIs define
parsers that will match C<call(method(...), args...)> and replace that with an
inlined sub-timeline. Ideally we can reject mismatching C<call> nodes without
inspecting C<method> directly.

We have 64 bits to work with and we want some ability to mask stuff,
particularly specific arguments for function call matching. In particular:

1. C<goto> and C<return> are never linked to, so they have no hashes
2. C<const>, C<call>, and C<method> are things we want to identify by type
3. C<method> is identified by CTTI and by name
4. C<call> always matches arity, and arguments can match type and structure
5. C<arg> and C<code> should cause many parsers to fail

Let's start with three bits for the type:

  000 = const
  001 = method
  010 = call arity == 1
  011 = call arity == 2
  100 = call arity == 3
  101 = call arity == 4
  110 = call arity >= 4
  111 = arg|code

What happens next depends on which type of node we have. C<method> has a couple
of indicator bits to specify whether the CTTI and method name are C<const>
nodes, followed by a hash that varies accordingly (here, C<hashN(X)> refers to
the most-significant C<N> bits of C<hash(X)>):

  method(const(type), const(mname)) -> 001 11 (hash59(type) ^ hash59(mname))
  method(type, const(mname))        -> 001 01 hash59(mname)
  method(const(type), mname)        -> 001 10 hash59(type)
  method(type, mname)               -> 001 00 hash27(type) hash32(mname)

C<call>'s hashing structure depends on its arity, and we have an indicator bit
for the const-ness of the function.

  call(const(f), ...)                          -> XXX 1 ...
  call(method(const(type), const(mname)), ...) -> XXX 1 ...
  call(non-const, ...)                         -> XXX 0 ...

  call(X, a)                -> 010 Xc? hash28(X) hash32(a)
  call(X, a, b)             -> 011 Xc? hash20(X) hash20(a) hash20(b)
  call(X, a, b, c)          -> 100 Xc? h15(X) h15(a) h15(b) h15(c)
  call(X, a, b, c, d)       -> 101 Xc? h12(X) h12(a) h12(b) h12(c) h12(d)
  call(X, arity<=19, xs...) -> 110 Xc? (arity-4) h12(X) (h[arity//44](x) for xs)

Note that this hashing strategy is just to get things off the ground. I think we
can derive a more efficient one based on the set of patterns we're looking for,
but that's a phi3 thing.


=head3 Sequence arguments and side-effect domains
Like Haskell, phi relies on functional dependencies for scheduling and sequence
points. Every side-effecting function takes a "sequence argument" and returns
another one; in practice, these sequence arguments represent side-effect
domains. These domains sometimes overlap; for example, if you have two open
files you may or may not care about synchronization between them. You can
indicate this by either serializing or forking the sequence argument.

Structurally, sequence arguments are objects that provide methods to implement
their side effects. Each method returns a new, fictitiously-modified object that
depends on the first side effect being complete, thereby establishing an
evaluation ordering.

Sequence arguments aren't the only objects that behave this way; any mutable
data structure needs to provide some synchronization mechanism. This mechanism
doesn't need to involve the receiver, and sometimes it won't by design. Instead,
it should reflect a degree of serialization we require from the program -- and
that's a question of semantics.

Because all modifications to a sequence argument can be fictitious, merging two
sequence arguments is also fictitious. This makes it possible to define full
fork/join structures.


=head3 Fictitious modification
phi's job is to see through various sorts of duplicity to unify values that are
in fact the same. Any decent optimization algebra has the potential to detect a
fictitious dependency and eliminate it, which would break things like sequence
arguments and cause all sorts of problems.

To work around this, phi provides a timeline variant for backend code and a
guarantee surrounding it: backend code isn't subject to timeline-level
optimizations. Sequence arguments are both inputs and outputs of these backend
code implementations, which breaks abstract dependency chains and prevents alias
analysis from modifying the ordering.

Note that "backend code" here refers not only to native x86 or other JIT output,
but also to phi bytecode. The contract is that timelines are an isolated
optimization domain; we can't reduce stuff to bytecode and use that knowledge
against timeline quantities.


=head3 Managing side effect domains
Dialects need a way to make sequence arguments implicit; we don't want users to
have to write stuff like C<obj = obj.method(foo)> for mutators.
C<obj.method(foo)> needs to do that for us. Getting this isn't quite as simple
as it sounds.

Let's make this concrete: C<array(n, type)> is a mutable class with C<n>
different sequence arguments since each element is a distinct side-effect
domain. C<array.set(i, x)> and C<array.get(i)> both use the C<array.t(i)>
timeline, which doesn't depend on C<root>.

If C<n> is unknown at compile-time, then the array can be unified to a single
timeline. Then all side effects against it are serialized.

Back to array modification though: C<array.set(i, x)> involves some timeline
owned by the array. How is this encoded? We have no functional dependency on
C<root>, but we _do_ have a functional dependency on the original array
timeline. Any dialect managing an array's timeline needs to know about this.

I think we can get away with a lot:

  int xs[3];
  xs[0] = 10;                           # xs.t(0)
  xs[1] = 20;                           # xs.t(1)
  xs[2] = 30;                           # xs.t(2)
  int total = 0;
  for i : xs.indexes
  {
    total += xs[i];                     # total ^ xs.t(i)
  }

Here's the timeline we want, topsorted by dependencies. I'm drawing the arrows
pointing towards outputs just because it's more intuitive. Note that I'm
assuming C<i> is a fictitious variable, C<xs.t(i)> is 1:1 with C<i>, and the
loop is fully unrolled.

  [heap] { heap, xs <- array.new(heap, 3, int),
           total0   <- 0 }
  [xs] { xst0 <- xs.t(0),
         xst1 <- xs.t(1),
         xst2 <- xs.t(2) }
  [xs, xst0, xst1, xst2] { xst0 = xs.set(xst0, 0, 10),
                           xst1 = xs.set(xst1, 1, 20),
                           xst2 = xs.set(xst2, 2, 30) }
  [xs, xst0, total0] { total1 <- total0.+(xs.get(xst0, 0)) }
  [xs, xst1, total1] { total2 <- total1.+(xs.get(xst1, 1)) }
  [xs, xst2, total2] { total  <- total2.+(xs.get(xst2, 2)) }

The only initial dependency is on some heap, but we don't care whether it's
compile-time or runtime. C<xst0> etc are sequence arguments to track
modifications to individual elements of C<xs>.

We do the normal SSA/CPS thing with numbered revisions of C<total>. In practice
these wouldn't have names, but C<int> is an immutable type so we don't need to
track modifications; we can just refer to functional transformations, in this
case driven by C<int::+>.


=head3 Sequence argument constructors
In the example above, C<xs.t(i)> returns a sequence argument to track
modifications of C<xs[i]>. In practice this is a non-value; it just needs to be
present for sequencing. Let's get into this a bit.

First, C<xs.t(i)> is a self-contained domain: it's unrelated to C<root>,
C<heap>, and everything else except for C<xs>. This means we can sequence it
anywhere we want to. The fact that C<xs.t(i)> takes no external arguments means
that arrays have a confined algebra of some kind. More specifically, it means we
can simulate their behavior without committing to things.

Second, C<xs.set(t, i, x)> never interacts with C<root> because arrays are
process-subjective; they aren't real to anyone outside the current process. This
means we can eliminate array assignments to elements we don't subsequently
access. Proving this may be difficult and involves alias analysis, but it's an
option.

At this point it's natural to ask how C<xst0> is any different from a local
variable that's acting as a less-C<volatile> cache of C<xs[0]>. Practically
speaking, that's exactly what it is; but we get to use memory that's managed by
a data structure. The real difference is that C<xs.t> can either use or ignore
its index argument -- and this happens monomorphically, which in turn influences
the way the dialect will create timelines.

C<xs> has a timeline of its own, and we can synchronize that to individual
elements (commit the elements) by merging it:

  xst = xs.merge(xst, xst0);            # xs[0] is now up to date
  xst = xs.merge(xst, xst1);            # xs[1] is now up to date
  xst = xs.merge(xst, xst2);            # xs[2] is now up to date

Note that like C<xs.t()>, this operation is completely fictitious: C<xs.merge>
generates no code. Moreover, C<xst0> and other sequence arguments also don't
exist; their CTTI is specified as occupying no space. All of this machinery is a
purely compile-time abstraction we use to order side effects correctly.


=head3 Dialect/sequence negotiation
How do dialects figure out which sequence arguments are required to make a given
method call? This is some sort of CTTI negotiation, but I'm not sure how it
works yet. There's also the question of later joins. Do imperative languages
take on the burden of managing this, along with something like a sequence-arg
calling convention?

Let's take arrays, for example. An array should indicate that C<xs.t(0)> is not
the same thing as C<xs.t(1)>, and that both relate to C<xs.t()> (if we're
unfolding elements). How does C<array> present this to someone using it? It's
clearly happening at the timeline level: C<timeline(const(0))> is different from
C<timeline(non_const)>.

Simplifying it a bit, though, let's just say we've got a couple of mutable
objects C<foo> and C<bar>, and our function reads from one and writes to the
other:

  f = fn (point foo, point bar) foo.x=(bar.x());

Nothing to it from a logical point of view. Timeline parameters look like this:

  f = fn (sequence t, point foo, point bar)
      {
        (foo.sequence ft', _ result) = foo.x=(foo.t(ft), bar.x(bar.t(bt)));
        sequence t' = ft'.merge(t);
        return (t', result);
      }

...so the general contract is that, for strict languages, side effects are
committed at each sequence point.

Q: does this even make any sense? It seems like we're not fully committing to
strict evaluation if we have any moment with unmerged timelines.

=cut


1;
