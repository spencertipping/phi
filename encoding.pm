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

The semicolon and comma are also binary operators, I think. Semicolons might be
something slightly different just because it's unnatural to omit them from final
statements (e.g. C<x; y; z;> is something people write). It should be fine for
the scope to parse them as special-ish cases.

Also, while it's tempting to have C<,> parse into a proper cons list, this
obviously fails for single-element lists: phi has no way to know that C<(5)> is
a list, vs C<5> is an atom -- at least, most likely. We certainly don't want to
go and make a list out of every parenthesized thing. So fully general list
parsers will need something like this:

  list()              = nil_case;
  list(x:any)         = one_case;
  list(x:any, xs:any) = many_case;

We might be able to fix this up by having C<list> define a parse continuation.

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
