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

TODO: the below is most likely completely wrong; I'm not sure we formally
declare protocols/classes like this.

On the parse side, phi structs use (meta-)protocols that dictate how their
instances react with the surrounding syntactic environment. For example:

  # NB: in meta-terms, "class" refers to a value that overloads the parse
  meta::parse = class T
    T "next" :: parser<string, syntax>

We then have protocols that work with backends to create abstracts:

  eval::abstract = |out| class T out
    op :: T syntax -> out

  # TODO: how do we create circular references like this?
  eval::c99_abstract = meta::parse + eval::abstract eval::c99_abstract

  meta::c99_abstract = class T
    instantiate :: T struct -> eval::c99_abstract
=cut
