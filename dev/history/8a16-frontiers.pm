=head1 Frontiers (a stream of consciousness firehose)
Let's abandon all pretense of normal language function for a second.

What I care about is precisely defining the division between dialects and
abstracts. Dialects are more significant than just flavors-of-language; they're
also flavors-of-interconnections -- really, they pick up the residual semantics
from abstracts. Abstracts are the values being driven by dialects.

We can look at dialects in a few ways:

1. Dialects are abstract fabric
2. Dialects are search space fabric
3. Dialects are a network for abstracts

I mean, these are all basically the same thing -- but none imply the usual
linkages that frame-oriented languages tend to have. And that's what interests
me: we have stuff like search spaces. If a dialect drives a solution search,
then the active frame is the frontier.


=head2 Parse time vs runtime
This is a metaclass-vs-class distinction. Metaclasses share an inheritance
relationship with classes.

Frontier metaclasses are a dialect's interface with the rest of the world and
with each other. I think it's that simple. I think we can also use these
metaclasses as possibly-mutable parse states.


=head2 Conclusion?
Dialects are strategies we use to search for a concrete solution to code, which
is a problem spec. At parse time, they're metaclasses that adhere to varying
protocols the details of which I don't care about right now. At runtime they
need to be GC-able and that's about it.

Abstracts specify methods that adhere to some standard calling convention.
There's some communication with the dialect, particularly about things like
return types -- details TBD. All abstract calls are one layer deep from an
evaluation perspective: all method arguments are evaluated by the dialect up
front. (This is a lie, but it simplifies my life so I'm running with it for
now.)

Frontiers/frames/whatever can have a token-ring topology if we want them to.
This lets us implement soft threads, deadlines, etc, with very specific critical
section boundaries.


=head2 Factoring
Two layers of dialect/frontier/things:

1. Syntax -> high-level semantics
2. High-level semantics -> low-level semantics

(1) would be, e.g. "python syntax"; (2) would be "strict evaluation with GC and
locals". Things sharing (2) can talk to each other.

...so we _do_ get code reuse.
