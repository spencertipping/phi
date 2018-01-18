=head1 Applicative grammar for phi (stage 1)
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
=cut

package phiapplicative;
use strict;
use warnings;

use Exporter qw/import/;
use phiboot;
use phibootmacros;
use philocal;
use phiparse;

our @EXPORT =
our @EXPORT_OK = qw/ line_comment any_whitespace ignore /;


=head2 Base syntax definitions
Stuff like whitespace, comments, etc. It's worth having these ready.
=cut

use constant line_comment => l
  l(l(pstr "#", phiparse::str, i_eval),
    l(l(pstr "\n", lit 0, phiparse::oneof, i_eval), phiparse::rep, i_eval),
    l(pstr "\n", phiparse::str, i_eval)),
  phiparse::seq, i_eval;

use constant any_whitespace => l
  l(pstr " \n\r\t", lit 1, phiparse::oneof, i_eval), phiparse::rep, i_eval;

use constant ignore => l
  l(line_comment, any_whitespace), phiparse::alt, i_eval;


=head2 Example parse of the list C<map> function
Now that we've got the easy stuff working, it's time to get straight into the
gnarly bits. Let's go through a simple function written in applicative notation:

  fn:map f xs ->
    xs.type.sym= 'cons
      ? cons (f xs.head) (map f xs.tail)
      : xs

The function signature of C<f xs> means we're consuming two stack entries and
binding them to locals. We could alternatively destructure by writing
C<(f, xs)>, which would refer to a tuple.

C<fn:map> is two things: C<fn> is a symbol that is resolved to the C<fn> value,
and its parse continuation specifies an optional C<:name> prefix that lets you
construct an anonymous but self-referential function. If you use this,
self-references will be implemented with mutable forwards (just like we do in
the test harnesses in Perl).

After that, the parse continuation specifies a lambda rule, which means we get
destructuring. Unlike a lot of languages, phi implements destructuring in a
first-class way; C<f> and C<xs> are unbound symbols, which are themselves values
(?) and specify their own parse continuations. (TODO: think this through)
=cut
