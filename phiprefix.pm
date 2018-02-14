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
          let [str offset [c l depth] s'...] = s in
          [depth nil drop . q .] [str offset [c l depth+1] s'...]

=cut

use constant drop_list => l drop;
use constant make_literal => l
  dup, nilp,                            # x state <1|0>
    pnil,
    l(swap, philocal::quote, i_eval,    # state q
      swap, unswons, unswons, i_uncons, # q str offset s' [c l depth]
      unswons, unswons, i_uncons,       # q str offset s' c l [] depth
      dup, lit 1, i_plus, l(i_eval),    # q str offset s' c l [] d d+1 [.]
      stack(0, 9), i_cons, lit i_eval,  # q str offset s' c l [] d d+1 [q .] '.
      i_cons, drop_list, i_cons,        # q str offset s' c l [] d d+1 [drop . q .]
      pnil, i_cons, rot3l, i_cons,      # q str offset s' c l [] d+1 [d nil drop . q .]
      stack(9, 1..8, 0),                # [d nil ...] q str offset s' c l [] d+1
      i_cons, swons, swons, i_cons,     # [d nil ...] q str offset [[c l d+1] s'...]
      swons, swons,                     # [d nil ...] q [str offset [c l d+1] s'...]
      swap, drop),
    if_;


=head3 Integer parsing
The usual radix conversion:

  cs list-int = 0 cs list-int'
  n cs list-int' = match cs with
    []    -> n
    c:cs' -> (n*10 + (c-48)) cs' list-int'

=cut

use constant list_int1_mut => pmut;
use constant list_int1 => l
  dup, nilp,
    l(drop),
    l(i_uncons,                         # n cs' c
      lit 48, i_neg, i_plus, rot3l,     # cs' c-48 n
      lit 10, i_times, i_plus, swap,    # (n*10+(c-48)) cs'
      list_int1_mut, i_eval),
    if_;

list_int1_mut->set(list_int1);

use constant list_int => l lit 0, swap, list_int1, i_eval;


use constant literal_int_parser => l
  list_int,
  l(l(pstr join('', 0..9), lit 1, phiparse::oneof, i_eval),
    phiparse::rep, i_eval),
  phiparse::pmap, i_eval,
  make_literal, i_eval;


=head3 String parsing
For now, strings hard-quote everything except backslash-escapes, of which the
usual suspects exist: \n, \r, \t, \", and \\.
=cut

sub string_escape($$) { l l(drop, lit ord $_[1]),
                          l(pstr "\\$_[0]", phiparse::str, i_eval),
                          phiparse::pmap, i_eval }

use constant str_char => l
  l(l(pstr "\"\\", lit 0, phiparse::oneof, i_eval),
    string_escape("\"" => "\""),
    string_escape("\\" => "\\"),
    string_escape(  n  => "\n"),
    string_escape(  t  => "\t"),
    string_escape(  r  => "\r")),
  phiparse::alt, i_eval;

use constant str_quote => l pstr "\"", lit 1, phiparse::oneof, i_eval;

use constant literal_str_parser => l
  l(tail, head),
  l(l(str_quote,
      l(phiapplicative::list_string,
        l(str_char, phiparse::rep, i_eval),
        phiparse::pmap, i_eval),
      str_quote),
    phiparse::seq, i_eval),
  phiparse::pmap, i_eval,
  make_literal, i_eval;


=head3 Symbol parsing
These are prefixed with ' to indicate quoting.
=cut

use constant literal_sym_parser => l
  l(tail, head, phiapplicative::list_string, i_eval, i_strsym),
  l(l(l(pstr "'", lit 1, phiparse::oneof, i_eval),
      l(l(pstr join("", 0..9, "a".."z", "_-'"), lit 1, phiparse::oneof, i_eval),
        phiparse::rep, i_eval)),
    phiparse::seq, i_eval),
  phiparse::pmap, i_eval,
  make_literal, i_eval;


=head2 C<let> form
Exactly what you'd expect, except with no support for destructuring binds.
=cut




=head2 C<fn> form


=cut


1;
