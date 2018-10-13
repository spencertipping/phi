=head1 Why timelines are horrifically complicated
...because they form an open-ended algebra. C<int> has associative + commutative
addition; C<real> has addition that may or may not have these properties (due to
numerical stability); file descriptors may be semantically connected or
independent. There's an entirely distinct semantic algebra that describes the
set of transformations we could make against a linearized event list. Timelines
would amount to translating those degrees of freedom into a graph structure,
which is challenging in part because the graph structure can be predicated on
runtime quantites (alias analysis, loop unrolling).

Timelines are possible, but non-conservative implementations aren't remotely
straightforward. It makes sense to implement them as libraries.
