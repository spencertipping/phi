=head1 Applicative grammar for phi
Writing concatenative code is miserable. I'd much rather use something with
ocaml or python syntax than have to keep track of the stack state all the time.
So I'm going to write a self-hosting grammar for phi that compiles applicative
to concatenative code.

=head2 Subexpression mapping + parser symbols
This approach is convenient because it easily reduces to a series of C<[...] 0>
restack calls, each of which involves a single operator and produces a new stack
entry. Then the stack state is reduced to just a number that tracks the ID of
the next value we allocate.

We still need a list of symbol -> number mappings; that would be in the tail of
the list. So altogether the stack layout would be something like this:

  [next-id [s1 i1] [s2 i2] ...]

The most important invariant is that each expression nets exactly one stack
entry. Then we're guaranteed that, if each binary operator nets exactly -1,
we'll end up with exactly one return value. The final C<restack> keeps C<[0]>
and pops C<next-id> entries.

We can build up the C<restack> fetch lists very easily: to fetch C<i1> and
C<i2>, we would take the cells C<next-id - i1 - 1> and C<next-id - i2 - 1>.
These would be consed into a single C<restack> list, we'd "apply" the operator
(cons it onto the result list), and increment C<next-id>. This C<next-id> is the
parser's return value.

=head2 Example parse of the list C<map> function
Written in applicative notation:

  fn map (f, xs) = xs.type == 'nil
    ? xs
    : cons(f(xs.head), map(f, xs.tail))

Starting inside the body of the function:

  |xs.type == 'nil            [3 [xs 2] [f 1] [map 0]]

Right off the bat, we've already destructured to create the argument bindings.
We've also created a mutable object to refer to the function itself, which makes
it possible to implement recursion.

  xs|.type == 'nil            [4 [xs 2] [f 1] [map 0]]    xs = 3

C<xs> is compiled as C<[0] 0 restack>.

  xs.type| == 'nil            [5 [xs 2] [f 1] [map 0]]    xs.type = 4

C<.type> was parsed as part of the continuation of C<x>, which is a generic
value. Technically, C<.type> is a postfix unary operator that binds with very
high precedence. The generic-value parse continuation dictates its compilation,
which in this case is C<[0] 0 restack i_type>. (Even though C<xs> is used in a
linear way here, we don't delete that stack entry because we can't prove its
linearity.)

  xs.type ==| 'nil            same

Nothing happens yet; C<==> now owns the parse.

  xs.type == 'nil|            [7 [xs 2] [f 1] [map 0]]

I'm jumping the gun a little here: C<==> would continue parsing the RHS until it
finds a lower-precedence operator, which C<?> is.

C<==> is a polymorphic operator that doesn't necessarily know its argument types
up front. In this case, though, C<.type> owns the parse continuation for C<==>,
which means the operator is now specialized to symbols and will compile to
C<[x y] 0 restack i_symeq>. This may die explosively if the RHS isn't a symbol,
which seems like a bug.

Above is TODO, but low priority because it only impacts erroneous code.

=head2 Operator precedence
If we're parsing C<x + y>, C<y> needs to be aware that it's being parsed within
the RHS of C<+> so its own continuation can reject the right set of operators.
This means that parse continuations need to be parameterized by precedence,
which means that values don't themselves kick off their parse continuations.
Instead, the I<containing> parser invokes C<.parse_continuation()> and specifies
its precedence.

TODO: work out the details. This sounds feasible but might need some work.
=cut
