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


=head1 Abstract evaluation
Structs are language-independent, and phi code should also be. phi code is
parsed and built into an AST, which supports abstract evaluation; ideally, the
output of abstract evalutation is directly compilable.
=cut
