=head1 phi builtin type/abstract definitions
Picking up where we left off in L<philang>...
=cut

package phitypes;
use strict;
use warnings;

use phiboot;
use phibootmacros;
use phiparse;
use phiobj;
use philang;


=head2 Example type: integers
=head3 Integer parsing
The usual radix conversion:

  cs list-int = 0 cs list-int'
  n cs list-int' = match cs with
    []    -> n
    c:cs' -> (n*10 + (c-48)) cs' list-int'

=cut

use phi list_int1_mut => pmut;
use phi list_int1 => l
  dup, nilp,
    l(drop),
    l(i_uncons,                         # n cs' c
      lit 48, i_neg, i_plus, rot3l,     # cs' c-48 n
      lit 10, i_times, i_plus, swap,    # (n*10+(c-48)) cs'
      list_int1_mut, i_eval),
    if_;

list_int1_mut->set(list_int1);

use phi list_int => l lit 0, swap, list_int1, i_eval;


use phitype int_type =>
  bind(val      => isget 0),
  bind(with_val => isset 0),

  bind(with_continuation =>             # v self
    swap, dup, nilp,                    # self v vnil?
      l(drop),                          # self
      l(tail, head, mcall"val",         # self n
        swap, dup, mcall"val",          # n self self-n
        rot3l, i_plus,                  # self n'
        swap, mcall"with_val"),         # self'
    if_),

  bind(parse_continuation => drop, drop,
    pcons(l(pcons(l(pcons(pstr "+", phiparse::str),
                    l(pstr "+", philang::expr, i_eval, i_eval)),
              phiparse::seq),
            phiparse::none),
          phiparse::alt));

use phi int_literal => l
  l(list_int, i_eval, pnil, swons, int_type, swons),
  l(l(pstr join('', 0..9), lit 1, phiparse::oneof, i_eval),
    phiparse::rep, i_eval),
  phiparse::pmap, i_eval;


1;
