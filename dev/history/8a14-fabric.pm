=head1 Dialects as connection fabric
Let's run with this for a minute and see where we get. I'm optimistic about this
particular factoring.

First, "fabric" here means "the thing that hooks abstracts to each other" -- and
that's subtle because abstracts themselves maintain links to arguments in some
cases. For instance, we could have some abstract that lazily applied a
transformation like C<< list.lazy_map<f> >>. Instances would store a
dialect-independent link to both the function and to the list being transformed.
Abstract links are subject to optimization algebras.

Second, abstracts can modify the dialect, which is where things get interesting.
I had originally concluded that abstracts did this by specifying arbitrary parse
continuations, but I think it makes more sense to have them provide an
alternative sub-dialect instead. This way we have the usual inter-dialect
communication protocols for addressing variables by name, etc.

Once we're past the parse stage we just have linked abstracts; this is our
semantic IR. Abstract constructors can apply case-specific semantic
optimizations inline, so we end up with a generic form awaiting evaluator-level
optimization.

It's a little unclear to me what happens next. Do we have different evaluators
and have those specialize the code, or do we apply a monomorphic process to get
to bytecode?

Q: should we split abstracts into "container types" and "value types"? It isn't
a robust distinction, but it partially resolves the problem we have now where
dialects need to dictate things like storage paradigms which presumably entails
some delegation to abstracts.

Aha, it's a fictitious distinction that exists only within a single frame of
reference. A dialect is itself an abstract if you look at it from the host's
point of view: dialects are libraries. But dialects are also compilers, which
means they produce libraries of their own. This means we have a root dialect
with a minimal set of abstracts; sub-dialects are written in that. Now we also
have a solution to the problem of scope-limited subdialects.
