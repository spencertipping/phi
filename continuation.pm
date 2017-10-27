=head1 Continuation
1. [done] Think about parser representation, but don't worry about it too much.
2. [done] Get a trivial test case for flatmapping parsers.

3. Prototype Perl JIT.
4. Prototype C JIT.

5. Write a very simple shell.
6. Start writing some DSLs for common things like data processing or CAD.
7. Refactor parsers to not store start locations, and to support insert/delete
deltas.
=cut


=head1 Compiled OOP
Most structs are monomorphic, but we have "protocol" structs which delegate to
a vtable. The two types of structs are essentially disjoint, although
internally a protocol-struct works like a mono-struct that includes its own
delegation logic.

Structs themselves are polymorphic objects subject to various protocols
including compilation, serialization, GC, etc (details TBD).

The compilation protocol describes the way a struct converts operations against
it into backend language constructs. Structs are live objects in the compiling
environment.

I think struct compilation is a fairly minimal process. We should be able to
write up a simple one for Perl and then port parsers into it. IOW, let's create
a minimalistic language and write phi in phi -- it's all parsers + compiled
objects anyway.

From where we are:

1. Add struct constructors to the parse grammar
2. Convert the evaluation strategy to a generate-compiled-stuff, then evaluate
externally to the parse (Q: does this break parser flatmaps? not if we include
an "eval at parse time" backdoor for expressions that forces all dependent
expressions)

Realistically, I think it's time to focus on writing this live-environment
compiler -- first with a Perl API, then with a phi API.
=cut
