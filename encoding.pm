=head1 Value encoding
phi values all take a standard form, which internally is a named binary
operator. Punctuation is all translated into methods:

  foo.bar(1, 2, 3).bif      # .bif == .bif()

  { method => 'bif',
    lhs =>
      { method => 'bar',
        lhs => { unknown => 'foo' },
        rhs =>
          { method => 'comma',
            lhs => { int => 1 },
            rhs =>
              { method => 'comma',
                lhs => { int => 2 },
                rhs => { int => 3 } } } },
    rhs => { unit => 0 } }

=head2 Operator precedence parsing
This should be handled either by the scope, or by a default operator-precedence
continuation. The problem with using a continuation is that we then have to
ignore the continuation of each value we encounter; we're probably best off if
the scope applies some amount of syntactic structure based on the definitions
available.

  "+".#associates_left;
  "+".#precedence.sorts_before("*".#precedence);
  "<<".#precedence.sorts_before("+".#precedence);

=head2 Fast structural parsing?
We can optimize this by calculating structural indicators for each type of thing
we have. For example, C<method 'comma'> might encode to C<murmurhash('comma')>;
then we have entropy we can use to rapidly reject some values from some
patterns. I think the encoding should be recursively split:

  hash0 = hash(method) : 64
  hash1 = (hash(lhs) : 16) + (hash(rhs) : 16) + (hash0 : 32)
  ...

This is TBD though, and it isn't crucial to the design.

(Also, at the very least, let's have parsers be JIT-compilable; using objects in
the line of fire is insane.)
