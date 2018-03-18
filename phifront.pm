package phifront;
use strict;
use warnings;

use phiboot;
use phibootmacros;
use phiparse;
use phiobj;
use phiabstract;
use philang;
use phiops;


=head2 Generic wrapper type
Let's keep it simple and define a single type that doesn't use any
value-determined parse continuation logic. It supports all of the builtin phi
operators plus some custom ones to build functions. Most of these are
implemented as owned operators to keep it easy.

Here's the precedence list:

  .method () []         10
  unary- unary~ unary!  20
  *                     30
  +                     40
  << >>                 50
  < >                   60
  ==                    80
  &                     90
  ^                     100
  ::                    110 right
  ->                    120 right
  = ? :                 130 right
  ;                     1000

=cut

use phi paren_local =>
  local_
    str_(pstr"("),
    pcons l(str_(pstr")"),
            le(lit phiops::opener, philang::expr, i_eval),
            phiparse::fail),
          phiops::grouping_type;

sub binop
{
  my ($precedence, $associativity, $opname, @fn) = @_;
  pcons l(pcons(l($precedence, $associativity), phiops::op_precedence_type),
          str_(pstr$opname),
          philang::expr,
          l(@fn)),
        phiops::owned_op_type;
}

use phi generic_val_mut => pmut;

use phi times_op => binop 30, 0, "*",
  mcall"val", swap, mcall"val", swap,
  pnil, swons, swons, phiabstract::op_itimes, i_eval, generic_val_mut, i_eval;

use phi plus_op => binop 40, 0, "+",
  mcall"val", swap, mcall"val", swap,
  pnil, swons, swons, phiabstract::op_iplus, i_eval, generic_val_mut, i_eval;

use phi minus_op => binop 40, 0, "-",
  mcall"val", swap, mcall"val", swap,
  pnil, swons, phiabstract::op_ineg, i_eval,
  pnil, swons, swons,
  phiabstract::op_iplus, i_eval, generic_val_mut, i_eval;


use phitype generic_val =>
  bind(val => isget 0),

  # Reject all postfix modifications; vals aren't operators
  bind(postfix_modify => stack(3), phiops::fail),

  bind(parse_continuation =>            # op vself self
    drop,
    pnil,                               # op self []

    # none case (must be last in the list, so first consed)
    stack(0, 1),                        # op self [cases] self
    identity_null_continuation, i_eval, # op self [cases] p
    i_cons,                             # op self [cases']

    # unowned op case
    stack(0, 1, 2),                     # op self [cases] op self
    phiops::unowned_as_postfix, i_eval, # op self [cases] p
    i_cons,                             # op self [cases']

    # Now build up the list of other possibilities, then filter it down by
    # applicable precedence.
    stack(3, 2, 1, 0),                  # [cases] self op
    rot3l,                              # self op [cases]
    l(times_op,
      plus_op, minus_op),               # self op [cases] +op
    phiops::applicable_ops_from,
    i_eval,                             # [cases']

    phiparse::alt, swons);

generic_val_mut->set(generic_val);


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
  l(i_uncons,                           # n cs' c
    lit 48, i_neg, i_plus, rot3l,       # cs' c-48 n
    lit 10, i_times, i_plus, swap,      # (n*10+(c-48)) cs'
    list_int1_mut, i_eval),
  if_;

list_int1_mut->set(list_int1);

use phi list_int => l lit 0, swap, list_int1, i_eval;

use phi int_literal => l
  rep_ oneof_(pstr join('', 0..9), lit 1),
  l(list_int, i_eval,
    phiabstract::const, i_eval,
    pnil, swons, generic_val, swons),
  phiparse::pmap, i_eval;


=head2 Default language scope
Time to boot this puppy up.
=cut

use phi root_scope =>
  pcons l(pnil,
          l(paren_local,
            phiops::whitespace_literal,
            phiops::line_comment_literal,
            int_literal),
          pnil,
          pnil),
        philang::scope_chain_type;


1;

__END__


use phi times_op => pcons l(pcons(l(2, 0), phiops::op_precedence_type),
                            times_fn,
                            philang::expr,
                            phiops::fail,
                            pnil),
                          phiops::unowned_op_type;


use phi timesop_literal => local_ str_(pstr "*"), times_op;



