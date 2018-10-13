=head1 Timelines and operation sequencing
B<Update:> I'm canceling timelines. The evaluator/compiler we ship in phi2
should be as linear as possible; no need for anything out-of-order at this
stage. Anything else can be provided by libraries later on.

This is going on my epitaph, you just wait. "Well, at least he tried."

Let's start with what we know:

1. Dialects have different sequencing disciplines
2. Sequence points order side effects but not general evaluation
3. Side effects have domains; disjoint effects can be reordered
4. Each local variable is an independent side effect domain
5. Optimizations depend on escape analysis and aliasing
6. Optimizations can modify the frame
7. Abstract values can quantify their side effect domains
8. Functions can be inlined by optimizers, which should involve frame sharing

I think (8) compromises or breaks my IR layer, but that should be fine. We will
need some sort of cycle detection or other mechanism to avoid inlining recursive
functions indefinitely. This is a Hard Problem (TM) but a solvable one; it
involves looking at the structure of optimization parsers and lazily expanding
inlines.

(3), (4), and (7) really complicate things, at least the way I'm thinking about
it now. If an operation involves multiple domains, we need to return a set; it's
just awkward and potentially slow.


=head2 Ground truth for sequencing
All sequencing models can be generalized to side effect domain graphs. The
degree of nuance we get from this representation isn't something SSA or CPS can
encode; each is a linearized traversal.

Side effect graphs are compile-time proofs that quantities are disjoint.

Put in a completely different way, deriving optimal side effect domains amounts
to factoring parts of a program. Doing this at parse time implies that the
factor space doesn't involve coalesced terms... which I think is reasonable
enough. Doing this at all implies that we can identify constant dimensions and
potentially specialize code depending on whether values are aliased. It isn't
remotely clear that phi should do this automatically.


=head2 Inferring ambivalence conservatively
Let's back away from the aggressive form of factoring-for-ambivalence I
described above. We don't need to catch everything to be useful; we just need to
get easy stuff. The user can always hint the program to get more optimization,
either in source or in the abstract domain.

Ideally we just identify cases like loops with no global side effect
entanglement and provide a simple way to automatically parallelize them (for
instance, modifying the interpreter's parallelism disposition). With some help
from abstracts, maybe we conclude that every iteration of a C<for>-loop operates
on a separate timeline -- meaning nothing from the write set of one iteration is
referred to from any future iteration.

B<Important question:> given that dialects manage their own sequence points, is
it remotely reasonable to expect phi to do this kind of analysis? It seems like
the dialect supplies constraints, then ambivalence operates within that. So phi
does indeed have some license to do this stuff.

Inference really isn't the issue; it's just representation. If we have the data
layer right, inference can be extensible.


=head2 Synchronization domains for RPCs
We care about ambivalence and independence because synchronization can be
expensive. If we can desynchronize two RPC events, we potentially halve the
latency. Doing this involves breaking a timeline.


=head2 Insane ideas
=head3 Evaluator is a queue
I have a terrible feeling about this, but what if we treat the evaluator like a
queue and have each CPU/machine be an executor? I think this ends in absolute
disaster, but if we get residency right then maybe it doesn't.
