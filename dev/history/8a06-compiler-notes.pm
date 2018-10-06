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

Normally the logic would look like this:

  while (not everyone is EOF)
  {
    select(readers);
    for (selected)
    {
      read stuff;
      grep it;
      send results to synchronized outchannel;
    }
  }


