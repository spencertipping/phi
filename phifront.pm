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

use phi generic_val_type_mut => pmut;
use phi generic_val => l                # x
  pnil, swons,                          # [x]
  generic_val_type_mut, swons;          # [x]::gv_type


use phi times_op => binop 30, 0, "*",
  mcall"abstract", swap, mcall"abstract", swap,
  pnil, swons, swons, phiabstract::op_itimes, i_eval, generic_val, i_eval;

use phi plus_op => binop 40, 0, "+",
  mcall"abstract", swap, mcall"abstract", swap,
  pnil, swons, swons, phiabstract::op_iplus, i_eval, generic_val, i_eval;

use phi minus_op => binop 40, 0, "-",
  swap, mcall"abstract", pnil, swons, phiabstract::op_ineg, i_eval, swap,
  mcall"abstract",
  pnil, swons, swons,
  phiabstract::op_iplus, i_eval, generic_val, i_eval;


use phitype generic_val_type =>
  bind(abstract => isget 0),

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

generic_val_type_mut->set(generic_val_type);


=head2 Integer literals
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
  l(list_int, i_eval, phiabstract::const, i_eval, generic_val, i_eval),
  phiparse::pmap, i_eval;


=head2 Symbol literals
Not quoted -- these are used for variables and function arguments.
=cut

use phi list_str1_mut => pmut;
use phi list_str1 => l                  # dest i cs
  dup, nilp,
  l(drop),
  l(i_uncons,                           # dest i cs' c
    stack(4, 3, 2, 0, 1, 2),            # i cs' c i dest
    i_sset,                             # i cs' dest
    rot3l, lit 1, i_plus,               # cs' dest i+1
    rot3l, list_str1_mut, i_eval),
  if_;

list_str1_mut->set(list_str1);

use phi list_str => l                   # xs
  dup, philang::list_length, i_eval,    # xs n
  i_str, swap, lit 0, swap,             # str 0 xs
  list_str1, i_eval;                    # str'

use phi list_sym => l                   # xs
  list_str, i_eval, i_strsym;           # sym


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


=head2 REPL
Mostly for use with native code.
=cut

use phi repl_mut => pmut;
use phi repl => l                       # scope
  pstr"phi> ", 0x100,                   # scope
  0x102,                                # scope line?
  dup, nilp,
  pnil,
  l(stack(0, 1), pnil, swons,           # scope line [scope]
    lit 0, i_cons, swons,               # scope [line 0 scope]
    lit phiops::opener,
    philang::expr, i_eval, i_eval,      # scope v state'|[]
    dup, nilp,
    l(drop,                             # scope v
      pstr"failed to parse: ", 0x100,   # scope v
      0x101,                            # scope
      pstr"\n", 0x100,                  # scope
      repl_mut, i_eval),                # scope repl

    l(tail, tail, head, swap,           # scope scope' v
      mcall"abstract", mcall"val",      # scope scpoe' vv
      pstr"= ", 0x100, 0x101,           # scope scope'
      pstr"\n", 0x100,                  # scope scope'
      swap, drop, repl_mut, i_eval),    # scope' repl

    if_),
  if_;

repl_mut->set(repl);


1;

__END__


use phi times_op => pcons l(pcons(l(2, 0), phiops::op_precedence_type),
                            times_fn,
                            philang::expr,
                            phiops::fail,
                            pnil),
                          phiops::unowned_op_type;


use phi timesop_literal => local_ str_(pstr "*"), times_op;



