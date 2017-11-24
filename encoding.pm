=head1 Value encoding
phi methods and calls are two separate types of values:

  foo.bar(1, 2, 3).bif      # .bif != .bif()

  (method .bif
    (call
      (method .bar (unknown foo))
      (call
        (method .comma (int 1))
        (call
          (method .comma (int 2))
          (int 3)))))

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

Q: C<xs:any> is a very loose type assertion. Do we infer that C<list(...)> only
happens for C<()> and cons cells in the C<xs> position? Otherwise how do we
avoid type checking there? (Put differently, can phi operate in a closed world?)

We might be able to fix this up by having C<list> define a parse continuation.
Actually, something like this might suffice:

  list(x:any) = list_(x, ())

Then we have C<()> ending the list each time.

=head2 Data constructors
Data constructors must be methods. The problem with unknown functions is that we
can't match against them because it's ambiguous whether we want to bind the
unknown or treat it as a literal. Do we have a convention about data
constructors being method calls against nil?

  ().list(a, b, c)

If methods are our symbolic containers, then this does make a certain amount of
sense; and matching against nil should be fast as well since it's a constant. It
looks a little strange, but I think this is a good way to do it.

...so to define the list() constructor:

  list(x:any) = ().list(x);
  ().list.#parse_continuation = ...;

Oh, this isn't good. The problem is that we need a method-specific parse
continuation, but the grammar has too much atomicity to it. Methods and calls do
in fact need to be two separate types of operation. We want to write this:

  list = ().list;
  list.#parse_continuation = ...;

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

For runtime stuff, we should implement scope partitioning -- particularly for
method calls. This can form the basis for a type system.
