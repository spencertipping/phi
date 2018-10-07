=head1 phi2 compilation layer
I've been beating the dead horse of intermediate representations for a while
now; time to take stock of things and decide how to proceed.


=head2 The big issue: side effects
Basically, expressions have two completely distinct facets. One is the value
being returned, and the other is the set of side effects produced in the
process. C/C++ has a surprisingly subtle model of how these things work,
ultimately involving optimization-driven orderings bounded by sequence points.
It's tempting to try to incorporate this into phi and generalize it into an API
of some sort, hence abstractions like C<timeline>.

I've come up with a few IR designs that vary in how explicit they are in their
treatment of side effects. None are as powerful as C from an optimization point
of view, although timelines would be if I were more deliberate about separating
sequence points from root commits. At that point it would basically be an
optimizer-guided free for all between each pair of sequence points, limited only
by functional dependencies -- exactly how gcc works.

Sequencing as a synchronization mechanism is a little less clear when you're
managing multiple timelines, which I'd argue is an important thing to be able to
do. Ideally you'd be able to construct a timeline, send it someplace, and
specify how its side effects are spliced into your runtime. The use case for
this is subtle but great, so I'll try to explain the vision here.


=head2 Timeline algebra
Let's suppose we're grepping over a bunch of log files, and for whatever reason
they're on different IO devices. We get only one CPU/thread, but multiple
incoming file descriptors each of which is individually (and independently)
IO-bound. Basically a coroutine problem.

Single-threaded logic would look like this:

  for each file
  {
    open it;
    while (read data)
    {
      if (we found stuff)
        print the stuff;
    }
    close it;
  }

From a timeline point of view, C<for each file> concatenates each iteration's
side effects; it does exactly what you'd expect serializing loop to do. This
isn't optimal, though; ideally we could grab each of the iterations as a stream
of side effects and merge them with some constraints:

  list<timeline> ts = for each file { ... };
  ts.fold(union)
    .interleave(max_open_files = 8, ...)
    .run();

Timelines aren't just for running things, though. Maybe you want to
reverse-engineer something, in which case you would do things like intersecting,
differencing, or searching. Timelines are data.


=head2 Timeline representations from the compiler's point of view
This is what I've had trouble with, so let's go through it.

First, phi is a parser that consumes syntax and produces timelines. There's some
circularity to that relationship: timelines can modify/extend the parser as it's
running. This means we need robust parse-time constant folding.

Second, languages differ in their treatment of timelines. Most languages are
strict and straightforward, but some use out-of-order semantics (R, Scala) and
others are fully lazy (Haskell). All of these differences can be taken up in the
syntactic layer, which is owned by dialects. So we have:

  source code -> dialect -> timelines -> abstracts -> timelines -> runtime
                       ^                 |
                       +-----------------+

Dialects and abstracts are independently polymorphic. Timelines are polymorphic
in a different way: each type of timeline describes operations performed within
a given semantic model. By default we use phi bytecode, but you can translate a
timeline to some backend, e.g. C or machine code. These are variants because
they have different merge and sequencing semantics. Realistically, though, you'd
only use a single timeline variant per runtime -- compared to dialects and
abstracts, timelines are monomorphic.

The big question for timelines is how we represent side-effects and operation
sequencing. The maximally-efficient, most reductive strategy is to say timelines
are bytecode; then the order of operations is fully specified. The least
reductive strategy is to represent every intermediate value as a graph node, and
to have side effects modeled as functional transformations of an IO value. Then
compilation involves sorting the timeline by C<root> interactions, folding up
constant nodes, and scheduling individual instructions using whatever heuristics
are appropriate for the backend.

Not every timeline that impacts C<root> has a static linkage to it. Put
differently, timelines can be computed quantities: there's a node type that lets
you splice a timeline-as-a-value into the execution schedule. I think this is
also how alias management works. If you want a conservative approach, you can
synchronize all memory/IO/etc access to C<root>; otherwise you can splice in a
specialized timeline that's more specific about the resources it's accessing.


=head2 Control flow and splicing
I think all control flow happens using a timeline splice operation, which
ultimately compiles down to a C<goto>. We're relying on timelines'
constant-folding here, and I think we also allow inline specialization in cases
where we want to turn RTTI into CTTI.


=head2 Local side effects
It's erroneously conservative to entangle frame variables with C<root>. We do
better when we can reschedule local variable accesses and heap allocations with
respect to side effects.
