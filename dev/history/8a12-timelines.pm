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
