=head1 License
    phi programming language
    Copyright (C) 2018  Spencer Tipping

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.


=head1 phi infix frontend
The idea here is to build an extensible grammar we can use to generate
C<phieval> nodes so we aren't coding things with the Perl API ... in other
words, like any other programming language. If you start by assuming the parser
works exactly like OCaml (pre-typechecking), most of this will make sense.

There are two layers to the infix language. The lower-level layer, implemented
here, contains just enough features to represent a usable subset of the final
product, implemented using this language in C<philang.phi>. C<philang.phi> is a
fixed point.


=head2 Low-level layer: bootstrapping the infix syntax
This lower-level layer implements the following features:

=head3 Syntax
1. C<#> line comments

=head3 Literals
1. Integer literals
2. Quoted-symbol literals, e.g. C<'foo>
3. String literals, e.g. C<"foo">
4. Owned operators that correspond to primitives

=head3 Control flow/bindings
1. Non-destructuring C<let..in> bindings
2. Non-destructuring, unary C<fn> lambdas (using C<\> as the lambda marker)
3. Sequential evaluation using C<;>
4. Function calls by juxtaposition: C<f x> rather than C<f(x)>
5. Ternary operator for conditions: C<?:> (implemented as a group)

Recursive functions work by self-reference, enabled with muts. This happens
inside assignment-bindings, for instance C<let>, and looks like this:

  let f = \x -> x ? f (x - 1) : 1 in f 5
                    ^                ^
                    |                normal reference
                    |
                    self-reference via mut

=head3 Conses
1. Unowned operator for conses: C<::>; owned operators for C<.h> and C<.t>
2. C<[]> literal for nil
3. C<++> list append operator (this is later generalized for non-lists)
=cut

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


=head2 Operator definitions
It's worth wrapping this a bit for the common case. This macro takes care of
most of the machinery involved in creating an owned op:
=cut

sub binop
{
  my ($precedence, $associativity, $opname, @fn) = @_;
  pcons l(pcons(l($precedence, $associativity), phiops::op_precedence_type),
          str_(pstr$opname),
          l(swap, drop, philang::expr, i_eval),
          l(@fn)),
        phiops::owned_op_type;
}


=head2 Generic primitive type
We don't yet have the machinery to do type-dependent parsing. By which I mean,
we do but it would suck right now. For example, some cases that would be
awkward:

  let xs = 1 :: 2 :: 3 :: [] in xs.t.t.h          # this won't work
  \x -> x + 1                                     # this won't work

The main problem is that we don't yet have a type-inferring parse time
evaluator; all we have is the fuzz, which is fully strict. And while I could
write another evaluator for this infix layer, it's easier to make values generic
for now and then write the next evaluator from inside the infix syntax.

Using generic values means that we have one suboptimality: both integers and
symbols can be meaningfully compared for equality.
=cut

use phi itimes_op => binop 30, 0, "*", phieval::op_itimes, i_eval;
use phi iplus_op  => binop 40, 0, "+", phieval::op_iplus, i_eval;
use phi iminus_op => binop 40, 0, "-", phieval::op_ineg, i_eval,
                                       phieval::op_iplus, i_eval;

use phi ilsh_op   => binop 50, 0, "<<", phieval::op_ilsh, i_eval;
use phi irsh_op   => binop 50, 0, ">>", phieval::op_irsh, i_eval;
use phi ilt_op    => binop 60, 0, "<",  phieval::op_ilt, i_eval;
use phi igt_op    => binop 60, 0, ">",  phieval::op_igt, i_eval;

use phi ieq_op    => binop 80, 0, "==", phieval::op_ixor, i_eval,
                                        phieval::op_inot, i_eval;

use phi iand_op   => binop 90,  0, "&", phieval::op_iand, i_eval;
use phi ior_op    => binop 100, 0, "|", phieval::op_ior,  i_eval;
use phi ixor_op   => binop 100, 0, "^", phieval::op_ixor, i_eval;


use phitype generic_abstract_type =>
  bind(abstract => isget 0),

  # No postfix modifications (TODO: fix this for function application)
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
    l(itimes_op, iplus_op, iminus_op,
      ilsh_op, irsh_op, ilt_op, igt_op,
      ieq_op, iand_op, ior_op, ixor_op,
    ),                                  # abstract op [cases] +op
    phiops::applicable_ops_from,
    i_eval,                             # [cases']

    pnil, swons,                        # [[cases']]
    phiparse::alt_type, swons);


use phi infix_dialect =>
  pcons l(generic_abstract_type), philang::class_wrapper_dialect_type;


use phi paren_local =>
  local_
    str_(pstr"("),
    le pcons(l(str_(pstr")"),
               le(lit phiops::opener, philang::expr, i_eval),
               phiparse::fail),
             phiops::grouping_type),
       phieval::syntax, i_eval;


=head2 Unowned operators
There are two unowned operators:

1. C<::> for consing things
2. C<;> for building expression sequences
=cut

use phi cons_op => pcons l(pcons(l(70, 1), phiops::op_precedence_type),
                           phieval::op_cons,
                           philang::expr,
                           phiops::fail_node,
                           pnil),
                         phiops::unowned_op_type;

use phi seqr_op => pcons l(pcons(l(1000, 0), phiops::op_precedence_type),
                           phieval::op_seqr,
                           philang::expr,
                           phiops::fail_node,
                           pnil),
                         phiops::unowned_op_type;

use phi cons_op_local => local_ str_(pstr "::"), cons_op;
use phi seqr_op_local => local_ str_(pstr ";"),  seqr_op;

use phi nil_local => local_ str_(pstr"[]"), phieval::c_nil;

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
        swap, phieval::alias_deref_proxy, i_eval,
        mcall"native",                      # op sym
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
        swap, phieval::alias_deref_proxy, i_eval,
        mcall"native",                      # op sym
        swap, philang::expr, i_eval,        # sym p

        pnil, swons, swons,                 # [sym p]
        function_parser_type, swons),

      l(                                    # lhs rhs
        stack(2, 0))),                      # rhs
    phiops::owned_op_type;


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
            nil_local,
            phiops::whitespace_literal,
            phiops::line_comment_literal,
            int_literal,
            sym_literal),
          philang::empty_capture_list,
          infix_dialect),
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
        dup, mcall"parent", nilp,       # scope' parent-nil?
        pnil,
        l(lit parent_scope_not_nil => i_crash),
        if_,
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



