=head1 Local variable support
We want to bind stack entries to names so we can easily refer to them and place
them without using C<restack>. To do that, we need a C<let> construct that
stacks a custom resolver that contains local variables. (Although in this case
it's called C<fn> to emphasize that we're capturing args.)

This is the first example of a concatenative macro: we transform the
continuation stack to restore the original resolver after the local scope ends.
In this case that happens after the current list. For example:

  [fn [x y] x y +]

Here, C<fn> resolves to a value that immediately fetches the continuation and
pops C<[x y]> from that list. It then saves the current resolver, pops C<x y +>,
and adds it back after pushing a "resolver reset" list.

Manipulating continuation lists is delicate: if you grab the current
continuation and then put it back, you'll get an infinite loop. So we need to
pop the rest of the currently-executing continuation before replacing the
cstack.
=cut

package philocal;
use strict;
use warnings;

use Exporter qw/import/;
use phiboot;
use phibootmacros;

our @EXPORT =
our @EXPORT_OK = qw/ quote fn1 /;


=head2 C<quote> implementation
We need a way to force phi to quote stuff that might not be self-quoting. For
example, if we want to bind C<x> to C<5>, we can't just put C<[x 5]> into the
resolver because phi would run C<5> as an instruction (cons).

So instead, we do what C<lit> does and put the value into a list. Here's the
equation:

  quote(v) = [[v] head]

Concatenative:

  v     [] swons [head] swons     = [[v] head]

=cut

use constant quote => l pnil, swons, l(head), swons;


=head2 C<fn1> implementation
This is a fairly involved definition compared to stuff like parsers.

  fn1 = let c = get-clist in             # self continuation = c.head
        let resolver = get-rlist in      # capture current resolver
        let caller = c.tail.head in      # this contains the bindings + stuff
        let resume = caller.tail in      # after binding list
        let callernext = c.tail.tail in  # this is where we prepend restore

        # Now we need to collect the symbol and the value, bind them into a
        # resolver entry, and build the new resolver.
        let binding = caller.head in
        let capture = pop quote nil swons binding cons in
        let newresolver = resolver capture cons in

        let restore_resolver = resolver quote [rset] swons in
        resolver rset;
        callernext restore_resolver cons
                   resume           cons cset

Concatenative derivation:

  x                         iquote tail uncons    = x r [s c cs...]
  x r [s c cs...]           tail uncons uncons    = x r [cs...] [c...] sym
  x r [cs...] [c...] sym    [4 0 3 1 2] 5 restack = [cs...] [c...] r sym x
  [cs...] [c...] r sym x    quote swons           = [cs...] [c...] r [sym 'x]

  ... [rs rc...] [sym 'x]   swap dup rot3> uncons = ... r [sym 'x] [rc...] rs
  ... r [sym 'x] [rc...] rs rot3< cons cons       = ... r r'
  ... r r'                  rset                  = [cs...] [c...] r

  [cs...] [c...] r          [rset] swons rot3<    = [c...] [r rset] [cs...]
  [c...] [r rset] [cs...]   swons swons cset      = [[c...] [r rset] cs...] cset

=cut

use constant fn1 => l
  i_quote, tail, i_uncons, tail, i_uncons, i_uncons, stack(5, 4, 0, 3, 1, 2),
  quote, i_eval, swons, swap, dup, rot3r, i_uncons, rot3l, i_cons, i_cons,
  i_rset, l(i_rset), swons, rot3l, swons, swons, i_cset;
