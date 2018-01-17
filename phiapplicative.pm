=head1 Applicative grammar for phi (stage 1)
Writing concatenative code is miserable. I'd much rather use something with
ocaml or python syntax than have to keep track of the stack state all the time.
So I'm going to write a self-hosting grammar for phi that compiles applicative
to concatenative code.

I'm doing this in two stages for my own sanity. This first stage looks like
Lisp, which isn't bad at all and makes it much easier to create the second
stage, which supports infix + operator precedence + parse continuations.

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

  [fn map [f xs]
    [if [sym= [type xs] 'nil]
      xs
      [cons [f [head xs]] [map f [tail xs]]]]]
=cut

package phiapplicative1;
use strict;
use warnings;

use phiboot;
use phibootmacros;



