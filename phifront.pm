package phifront;
use strict;
use warnings;

use phiboot;
use phibootmacros;
use phiparse;
use phiobj;
use phieval;
use philang;
use phiops;


=head2 phi dialect
This is pretty simple: everything except syntax nodes becomes a C<generic_val>
when inflected.
=cut

use phi generic_val_type_mut => pmut;
use phi generic_dialect =>
  pcons l(generic_val_type_mut), philang::class_wrapper_dialect_type;


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
          l(swap, drop, philang::expr, i_eval),
          l(@fn)),
        phiops::owned_op_type;
}

use phi generic_val => l                # x
  pnil, swons,                          # [x]
  generic_val_type_mut, swons;          # [x]::gv_type


use phi times_op => binop 30, 0, "*", phieval::op_itimes, i_eval;
use phi plus_op  => binop 40, 0, "+", phieval::op_iplus, i_eval;
use phi minus_op => binop 40, 0, "-", phieval::op_ineg, i_eval,
                                      phieval::op_iplus, i_eval;


use phitype assign_parser_type =>
  bind(name       => isget 0),
  bind(rhs_parser => isget 1),

  bind(parse =>                             # state self
    dup, mcall"rhs_parser",                 # state self p
    rot3l, swap, mcall"parse",              # self state'
    dup, mcall"is_error",
    l(stack(2, 0)),                         # state'
    l(                                      # self state'
      swap, mcall"name", i_symstr,          # state' namestr
      pnil, swons,                          # state' [namestr]
      phiparse::str_type, swons,            # state' p
      swap, dup, mcall"value",              # p state' v
      swap, mcall"bind_local"),             # state''
    if_);


use phi assign_op =>
  pcons
    l(pcons(l(130, 1), phiops::op_precedence_type),
      str_(pstr"="),
      l(                                    # lhs op
        # We need to extend the scope to include the binding at the end of this
        # parse continuation. So parse a value at this precedence, then bind a
        # new local in the parse state.
        swap, mcall"native",                # op sym
        swap, philang::expr, i_eval,        # sym p

        pnil, swons, swons,                 # [sym p]
        assign_parser_type, swons),         # assign_parser(sym p)

      l(                                    # lhs rhs
        stack(2, 0))),
    phiops::owned_op_type;


use phitype function_parser_type =>
  bind(argname    => isget 0),
  bind(rhs_parser => isget 1),

  bind(parse =>                             # state self
    dup, mcall"rhs_parser",                 # state self p
    rot3l, mcall"enter_child_scope",        # self p state'
    stack(0, 2), mcall"argname", i_symstr,  # self p state' argstr
    pnil, swons, phiparse::str_type, swons, # self p state' argp
    phieval::arg, rot3l,                    # self p argp arg state'
    mcall"bind_local",                      # self p state''
    swap, mcall"parse",                     # self state'''
    dup, mcall"is_error",                   # self state 1|0
    l(stack(2, 0)),                         # state
    l(                                      # self state
      mcall"exit_child_scope",              # self child state
      dup, mcall"value",                    # self child state body
      rot3l, mcall"capture",                # self state body capture-obj
      mcall"capture_list",                  # self state body capture
      swap, phieval::fn, i_eval,            # self state fn
      swap, mcall"with_value",              # self state'
      stack(2, 0)),                         # state'
    if_);


use phi function_op =>
  pcons
    l(pcons(l(120, 1), phiops::op_precedence_type),
      str_(pstr"->"),
      l(                                    # lhs op
        # Parse the body within a linked child scope, then collapse and produce
        # a function with the resulting expression.
        swap, mcall"native",                # op sym
        swap, philang::expr, i_eval,        # sym p

        pnil, swons, swons,
        function_parser_type, swons),

      l(                                    # lhs rhs
        stack(2, 0))),
    phiops::owned_op_type;

use phitype generic_val_type =>
  bind(abstract => isget 0),

  # Reject all postfix modifications; vals aren't operators
  bind(postfix_modify => stack(3), phiops::fail_node),

  bind(parse_continuation =>            # op self
    mcall"abstract",                    # op abstract
    pnil,                               # op abstract []

    # none case (must be last in the list, so first consed)
    stack(0, 1),                        # op abstract [cases] abstract
    identity_null_continuation, i_eval, # op abstract [cases] p
    i_cons,                             # op abstract [cases']

    # unowned op case
    stack(0, 1, 2),                     # op abstract [cases] op abstract
    phiops::unowned_as_postfix, i_eval, # op abstract [cases] p
    i_cons,                             # op abstract [cases']

    # Now build up the list of other possibilities, then filter it down by
    # applicable precedence.
    stack(3, 2, 1, 0),                  # [cases] abstract op
    rot3l,                              # abstract op [cases]
    l(times_op,
      plus_op, minus_op,
      function_op,
      assign_op),                       # abstract op [cases] +op
    phiops::applicable_ops_from,
    i_eval,                             # [cases']

    pnil, swons,                        # [[cases']]
    phiparse::alt_type, swons);

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

use phi int_literal => map_
  rep_ oneof_(pstr join('', 0..9), 1),
  l(list_int, i_eval, phieval::native_const, i_eval);


=head2 Symbol literals
Not quoted -- these are used for variables and function arguments.
=cut

use phi list_str1_mut => pmut;
use phi list_str1 => l                  # dest i cs
  dup, nilp,
  l(stack(2)),
  l(i_uncons,                           # dest i cs' c
    stack(4, 0, 2, 3, 1, 2),            # i cs' dest i c
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

use phi sym_literal => map_
  rep_ oneof_(pstr join('', "a".."z", 0..9, "'_"), 1),
  l(list_sym, i_eval, phieval::native_const, i_eval);


=head2 Default language scope
Time to boot this puppy up.
=cut

use phi root_scope =>
  pcons l(pnil,
          l(paren_local,
            phiops::whitespace_literal,
            phiops::line_comment_literal,
            int_literal,
            sym_literal),
          pnil,
          generic_dialect),
        philang::scope_type;


=head2 REPL
Mostly for use with native code.
=cut

use phi initial_eval_state =>
  pcons l(pnil,                         # value
          pnil,                         # node
          pnil,                         # arg
          pnil,                         # capture
          pnil),                        # timelines
        phieval::eval_state_type;

use phi fuzzify => l                    # node
  initial_eval_state, mcall"with_node", # state
  phieval::thefuzz,                     # state fuzz
  mcall"parse";                         # state'


use phi repl_mut => pmut;
use phi repl => l                       # scope
  #pstr"phi> ", 0x100,                   # scope
  0x102,                                # scope line?
  dup, nilp,                            # scope line? 1|0
  l(drop),                              # scope
  l(                                    # scope line
    stack(0, 1), pnil, swons,           # scope line [scope]
    swons, lit 0, i_cons,               # scope [0 line scope]
    pnil, i_cons,                       # scope [nil 0 line scope]
    philang::scoped_state_type, swons,  # scope state
    lit phiops::opener,
    philang::expr, i_eval, mcall"parse",# scope state'
    dup, mcall"is_error",               # scope state' error?
    l(mcall"value",                     # scope e
      pstr"failed to parse: ", 0x100,   # scope e
      0x101,                            # scope
      pstr"\n", 0x100,                  # scope
      repl_mut, i_eval),                # scope repl

    l(dup, mcall"value",                # scope state' v'
      fuzzify, i_eval,                  # scope state' vstate
      dup, mcall"is_error",             # scope state' vstate e?

      l(mcall"value",                   # scope state' e
        pstr"failed the fuzz: ", 0x100, # scope state'
        0x101,                          # scope
        pstr"\n", 0x100,                # scope
        repl_mut, i_eval),              # scope
      l(mcall"value",                   # scope state' v
        #pstr"= ", 0x100,
        0x101,                          # scope state'
        pstr"\n", 0x100,                # scope state'
        stack(2, 0), mcall"scope",      # scope'
        repl_mut, i_eval),              # scope' repl
      if_),
    if_),
  if_;

repl_mut->set(repl);


1;

__END__


use phi times_op => pcons l(pcons(l(2, 0), phiops::op_precedence_type),
                            times_fn,
                            philang::expr,
                            phiops::fail_node,
                            pnil),
                          phiops::unowned_op_type;


use phi timesop_literal => local_ str_(pstr "*"), times_op;



