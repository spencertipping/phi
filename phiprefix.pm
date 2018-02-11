=head1 phi prefix syntax
80% of the benefit of applicative syntax happens even if we don't have full
infix operators; just getting a Lisp going would be incredibly useful at this
point. C<phiapplicative> has enough to do that, so let's define a prefix
grammar with special forms that correspond to phi ops.

NB: despite the syntax, we don't use conses under the hood. I'm not sure how
egregious this is.

=head2 What this looks like
For instance:

  (let x 10
    (let y 20
      (fn z (+ x (+ y z)))))
=cut

package phiprefix;
use strict;
use warnings;

use phiboot;
use phibootmacros;
use philocal;
use phiparse;
use phiapplicative;


=head2 Literal values
We have a few types of literals, all of which produce quoted values:

1. Integers
2. Strings
3. Quoted symbols, e.g. C<'foo>
4. Lists, which compile to cons forms; e.g. C<['foo 3]>

=head3 Literal wrapper
Parsing a literal should update the stack depth and produce an
appropriately-formatted value. We can do this pretty easily using
C<philocal::quote> and generating unpacking instructions. Here's what this looks
like:

  x state make-literal = match state with
    [] -> x state
    s  -> let q = x quote in
          let [c l depth] = s in
          [depth nil drop . q .] [c l depth+1]

=cut

use constant drop_list => l drop;
use constant make_literal => l
  dup, nilp,                            # x state <1|0>
    pnil,
    l(swap, philocal::quote, i_eval,    # state q
      swap, unswons, unswons, i_uncons, # q c l [] depth
      dup, lit 1, i_plus, l(i_eval),    # q c l [] depth depth+1 [.]
      stack(0, 6), i_cons, lit i_eval, i_cons, drop_list, i_cons,
        pnil, i_cons, rot3l, i_cons,    # q c l [] depth+1 [depth nil ...]
      stack(5, 1, 2, 3, 4, 0),          # [depth nil ...] c l [] depth+1
      i_cons, swons, swons),            # [depth nil ...] [c l depth+1]
    if_;


=head3 Integer parsing
The usual radix conversion:

  cs list-int = 0 cs list-int'
  n cs list-int' = match cs with
    []    -> n
    c:cs' -> (n*10 + (c-48)) cs' list-int'
=cut

use constant list_int_mut => pmut;
use constant list_int => l
  ;

use constant literal_int_parser => l
  l(list_int, i_eval, philocal::quote, i_eval),
  l(l(pstr join('', 0..9),
      lit 1,
      phiparse::oneof, i_eval),
    phiparse::rep, i_eval),
  phiparse::pmap, i_eval;


=head2 C<let> form
Exactly what you'd expect, except with no support for destructuring binds.
=cut




=head2 C<fn> form


=cut
