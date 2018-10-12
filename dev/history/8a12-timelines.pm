=head1 Timelines and operation sequencing
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
