=head1 Abstract type definitions
Parameterizable objects that become abstract types. At a high level, abstract
types support a few different things:

1. Partial evaluation and evaluation state
2. Postfix modification, if supported
3. Parse continuations, split into postfix and prefix

Abstract values, via (1), form an interpreter -- and that's a bit nontrivial, so
let's talk about what it entails.


=head2 Using abstracts to interpret code


=cut


package phiabstract;
use strict;
use warnings;

use Exporter qw/import/;
use phiboot;
use phibootmacros;
use phiparse;
use phiobj;

our @EXPORT =
our @EXPORT_OK = qw/ abstract_fail abstract_fail_type /;


=head2 Meta-abstracts
Basically, things that aren't actual values. These are used to force a parse
failure in situations where an abstract, rather than a parser, is being
returned. Because the parse continuation is required to match and invariably
fails, C<abstract_fail> is unparsable and will cause some amount of
backtracking.
=cut

use phitype abstract_fail_type =>
  bind(parse_continuation => stack(3), phiparse::fail);

use phi abstract_fail => pcons pnil, abstract_fail_type;


__END__

use phitype int_type =>
  bind(val      => isget 0),
  bind(with_val => isset 0),

  # Reject all postfix modifications; ints aren't operators
  bind(postfix_modify => stack(3), abstract_fail),

  bind(parse_continuation =>            # op vself self
    drop,
    pnil,                               # op self []

    # none case (must be last in the list, so first consed)
    stack(0, 1),                        # op self [cases] self
    identity_null_continuation, i_eval, # op self [cases] p
    i_cons,                             # op self [cases']

    # unowned op case
    stack(0, 1, 2),                     # op self [cases] op self
    unowned_as_postfix, i_eval,         # op self [cases] p
    i_cons,                             # op self [cases']

    # Now build up the list of other possibilities, then filter it down by
    # applicable precedence.
    stack(3, 2, 1, 0),                  # [cases] self op
    rot3l,                              # self op [cases]
    l(plus_op, xor_op),                 # self op [cases] +op
    applicable_ops_from, i_eval,        # [cases']

    phiparse::alt, swons);


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


=head3 Ops for testing: C<+>, C<*>, and C<^>
These operate live as opposed to building an expression tree.
=cut

use phi int_type_mut => pmut;

use phi times_fn => l                   # v1 v2
  mcall"val", swap, mcall"val",         # n2 n1
  i_times, pnil, swons,                 # [n2*n1]
  int_type_mut, swons;                  # [n2*n1]::int_type

use phi plus_fn => l                    # rhs lhs
  mcall"val", swap, mcall"val",         # nl nr
  i_plus, pnil, swons,                  # [nl+nr]
  int_type_mut, swons;                  # [nl+nr]::int_type

use phi xor_fn => l                     # rhs lhs
  mcall"val", swap, mcall"val",         # nl nr
  i_xor, pnil, swons,                   # [nl^nr]
  int_type_mut, swons;                  # [nl^nr]::int_type

use phi plus_op_mut  => pmut;
use phi times_op_mut => pmut;
use phi xor_op_mut   => pmut;

use phi plus_op => pcons l(pcons(l(3, 0), op_precedence_type),
                           str_(pstr"+"),
                           l(plus_op_mut, philang::expr, i_eval, i_eval),
                           plus_fn),
                         owned_op_type;

use phi times_op => pcons l(pcons(l(2, 0), op_precedence_type),
                            times_fn,
                            l(times_op_mut, philang::expr, i_eval, i_eval),
                            abstract_fail,
                            pnil),
                          unowned_op_type;

use phi xor_op => pcons l(pcons(l(1, 0), op_precedence_type),
                          str_(pstr"^"),
                          l(xor_op_mut, philang::expr, i_eval, i_eval),
                          xor_fn),
                        owned_op_type;

plus_op_mut->set(plus_op);
times_op_mut->set(times_op);
xor_op_mut->set(xor_op);

use phi timesop_literal => local_ str_(pstr "*"), times_op;


use phitype int_type =>
  bind(val      => isget 0),
  bind(with_val => isset 0),

  # Reject all postfix modifications; ints aren't operators
  bind(postfix_modify => stack(3), abstract_fail),

  bind(parse_continuation =>            # op vself self
    drop,
    pnil,                               # op self []

    # none case (must be last in the list, so first consed)
    stack(0, 1),                        # op self [cases] self
    identity_null_continuation, i_eval, # op self [cases] p
    i_cons,                             # op self [cases']

    # unowned op case
    stack(0, 1, 2),                     # op self [cases] op self
    unowned_as_postfix, i_eval,         # op self [cases] p
    i_cons,                             # op self [cases']

    # Now build up the list of other possibilities, then filter it down by
    # applicable precedence.
    stack(3, 2, 1, 0),                  # [cases] self op
    rot3l,                              # self op [cases]
    l(plus_op, xor_op),                 # self op [cases] +op
    applicable_ops_from, i_eval,        # [cases']

    phiparse::alt, swons);

int_type_mut->set(int_type);

use phi int_literal => l
  rep_ oneof_(pstr join('', 0..9), lit 1),
  l(list_int, i_eval, pnil, swons, int_type, swons),
  phiparse::pmap, i_eval;


1;
