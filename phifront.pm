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
C<phioptree> nodes so we aren't coding things with the Perl API ... in other
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
1. Non-destructuring C<let..in> bindings (NB: not in the base layer)
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

I explain the mechanics of this near the definition of C<assign_op>.

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
use philist;
use phiparse;
use phiobj;
use phioptree;
use phifuzz;
use philang;
use phiops;


=head2 Operator definitions
It's worth wrapping this a bit for the common case. This macro takes care of
most of the machinery involved in creating an owned op:
=cut

sub make_binop
{
  my ($precedence, $associativity, $opname, @fn) = @_;
  pcons l(psym$opname,
          pcons(l($precedence, $precedence, $associativity),
                phiops::op_precedence_type),
          str_(pstr$opname),
          l(swap, drop, philang::expr, i_eval),
          l(@fn)),
        phiops::owned_op_type;
}


=head2 Symbol literals
Not quoted -- these are used for variables and function arguments. All named
bindings are routed through this parser because we want to deliberately take a
miss for longer names that begin with these strings (e.g. binding C<x> shouldn't
eat into C<xs>, for obvious reasons).
=cut

use phi list_sym_mut => pmut;           # defined later

use phi symbol => map_
  rep_ oneof_(pstr join('', "a".."z", 0..9, "'_"), 1),
  list_sym_mut;

use phi symbol_matcher => l             # sym
  pnil, swons,                          # [sym]
  l(                                    # s1 [s2]
    head, i_symeq),                     # [sym] unbound-f
  swons,                                # f

  symbol, swap,                         # p f
  pnil, swons, swons,                   # [p f]
  phiparse::filter_type, swons;         # p'

sub sym_($) { le lit(shift), symbol_matcher, i_eval }


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

use phi call_op   => make_binop 10, 0, "", call, i_eval;

use phi itimes_op => make_binop 30, 0, "*", op_itimes, i_eval;
use phi iplus_op  => make_binop 40, 0, "+", op_iplus, i_eval;
use phi iminus_op => make_binop 40, 0, "-", op_ineg, i_eval,
                                            op_iplus, i_eval;

use phi ilsh_op   => make_binop 50, 0, "<<", op_ilsh, i_eval;
use phi irsh_op   => make_binop 50, 0, ">>", op_irsh, i_eval;
use phi ilt_op    => make_binop 60, 0, "<",  op_ilt, i_eval;
use phi igt_op    => make_binop 60, 0, ">",  op_igt, i_eval;

use phi ieq_op    => make_binop 80, 0, "==", op_ixor, i_eval,
                                             op_inot, i_eval;

use phi iand_op   => make_binop 90,  0, "&", op_iand, i_eval;
use phi ior_op    => make_binop 100, 0, "|", op_ior,  i_eval;
use phi ixor_op   => make_binop 100, 0, "^", op_ixor, i_eval;


use phitype generic_abstract_type =>
  bind(abstract => isget 0),

  # No postfix modification: function calls are implemented as empty operators.
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
    l(call_op,
      itimes_op, iplus_op, iminus_op,
      ilsh_op, irsh_op, ilt_op, igt_op,
      ieq_op, iand_op, ior_op, ixor_op,
    ),                                  # abstract op [cases] oplist
    stack(0, 2),                        # abstract op [cases] oplist op
    mcall"applicable_owned_ops",        # abstract op [cases] oplist'
    l(                                  # op lhs opgate
      rot3l,                            # lhs opgate op
      mcall"parser"                     # op.parser(lhs, opgate)
    ),                                  # abstract op [cases] oplist' [...]

    stack(1, 0, 3, 4),                  # abstract op [cases] oplist' abstract op [...]
    swons, swons,                       # abstract op [cases] oplist' f
    list_map, i_eval,                   # abstract op [cases] op_parsers
    swap, list_append, i_eval,          # abstract op [cases']

    stack(3, 0),                        # [cases']

    pnil, swons,                        # [[cases']]
    phiparse::alt_type, swons);


use phi infix_dialect =>
  pcons l(generic_abstract_type), philang::class_wrapper_dialect_type;


use phi inside_parens => le lit phiops::root_opgate, philang::expr, i_eval;

use phi paren => pcons l(str_(pstr")"), inside_parens, inside_parens),
                       phiops::grouping_type;

use phi paren_local => local_ str_(pstr"("), le paren, syntax, i_eval;


=head2 Unowned operators
There are two unowned operators:

1. C<::> for consing things
2. C<;> for building expression sequences
=cut

use phi cons_op => pcons l(psym"::",
                           pcons(l(70, 70, 1), phiops::op_precedence_type),
                           op_cons,
                           philang::expr,
                           phiops::fail,
                           pnil),
                         phiops::unowned_op_type;

use phi seqr_op => pcons l(psym";",
                           pcons(l(130, 130, 0), phiops::op_precedence_type),
                           op_seqr,
                           philang::expr,
                           phiops::fail,
                           pnil),
                         phiops::unowned_op_type;

use phi cons_op_local => local_ str_(pstr"::"), le cons_op, syntax, i_eval;
use phi seqr_op_local => local_ str_(pstr";"),  le seqr_op, syntax, i_eval;

use phi nil_local => local_ str_(pstr"[]"), c_nil;


=head2 Lambdas
Lambdas are a very simple form of what they look like in Haskell:

  \x -> x + 1

Structurally, this begins with the C<\> local, which parses an unbound symbol
that owns the -> operator. (I explain this more below.) The next layer, written
in this one, will replace C<\> with a better lambda operator that is aware of
things like destructuring.

(FIXME: the above is a lie)
=cut

use phitype lambda_parser_type =>
  bind(argname    => isget 0),
  bind(rhs_parser => isget 1),

  bind(parse =>                             # state self
    dup, mcall"rhs_parser",                 # state self p
    rot3l, mcall"enter_child_scope",        # self p state'
    stack(0, 2), mcall"argname",            # self p state' argsym
    symbol_matcher, i_eval,                 # self p state' argp
    arg, rot3l,                             # self p argp arg state'
    mcall"bind_local",                      # self p state''
    swap, mcall"parse",                     # self state'''
    dup, mcall"is_error",                   # self state 1|0
    l(stack(2, 0)),                         # state
    l(                                      # self state
      mcall"exit_child_scope",              # self child state
      dup, mcall"value",                    # self child state body
      rot3l, mcall"capture",                # self state body capture-obj
      mcall"capture_list",                  # self state body capture
      swap, fn, i_eval,                     # self state fn
      swap, mcall"with_value",              # self state'
      stack(2, 0)),                         # state'
    if_);


use phi lambda_arrow_op =>
  pcons
    l(psym"->",
      pcons(l(110, 110, 1), phiops::op_precedence_type),
      str_(pstr"->"),
      l(                                # lhs op
        # The LHS here is the unbound symbol opnode, which should be a syntax
        # instance. This means we can get the symbol value itself by
        # dereferencing the syntax and calling .sym().
        swap,                           # op lhs
        mcall"syntax", mcall"sym",      # op sym
        swap, philang::expr, i_eval,    # sym rhs-parser
        pnil, swons, swons,             # [sym rhs-parser]
        lambda_parser_type, swons),     # parser
      l(                                # lhs rhs
        stack(2, 0))),                  # rhs
    phiops::owned_op_type;


=head2 Unbound symbols
These are a bit strange in that they're not part of the root scope, at least not
directly. Instead, other parsers like lambdas and let-bindings delegate to
unbound symbols. That means unbound symbols end up doing most of the work in
parsing these constructs. It also means that C<let> and C<\> are identical from
a syntactic point of view: either will enter a parsing context where you can
write a symbol that will be used as a free value.
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

list_sym_mut->set(list_sym);


=head3 Unbound symbol syntax frontend
You can define unowned ops that apply to unbound symbols just like you could for
any other value. For owned ops, unbound symbols provide:

  sym -> value                          # return a lambda

The next layer defines something that works similarly to this, but generalizes
from symbols to destructuring value parsers.
=cut

use phitype unbound_symbol_abstract_type =>
  bind(sym      => isget 0),
  bind(abstract => syntax, i_eval),

  # No postfix modifications
  bind(postfix_modify => stack(3), phiops::fail_node),

  # TODO: refactor the obviously common logic here
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
    l(lambda_arrow_op),                 # abstract op [cases] oplist

    stack(0, 2),                        # abstract op [cases] oplist op
    mcall"applicable_owned_ops",        # abstract op [cases] oplist'
    l(swap, mcall"parser"),             # abstract op [cases] oplist' [swap .parser]
    stack(0, 4), i_cons,                # abstract op [cases] oplist' f
    list_map, i_eval,                   # abstract op [cases] op_parsers
    swap, list_append, i_eval,          # abstract op [cases']

    stack(3, 0),                        # [cases']

    pnil, swons,                        # [[cases']]
    phiparse::alt_type, swons);

use phi unbound_sym_literal => map_
  rep_ oneof_(pstr join('', "a".."z", 0..9, "'_"), 1),
  l(list_sym, i_eval,                   # parsed-sym
    pnil, swons,                        # [parsed-sym]
    unbound_symbol_abstract_type, swons,# syntax-v
    syntax, i_eval);                    # opnode


=head3 Unbound symbol constructor: C<\>
C<\> parses its continuation as an unbound symbol, even if that symbol is in
fact bound. This is important because otherwise we'd be unable to shadow
variables.
=cut

use phitype unbound_symbol_quoting_type =>
  bind(postfix_modify => stack(3), phiops::fail_node),
  bind(parse_continuation =>            # op self
    drop, unbound_sym_literal,          # op literal_parser
    swap, philang::expr_parser_for, i_eval);

use phi unbound_symbol_quoter =>
  le pcons(pnil, unbound_symbol_quoting_type), syntax, i_eval;

use phi lambda_local => local_ str_(pstr"\\"), unbound_symbol_quoter;


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
  l(list_int, i_eval, native_const, i_eval);


=head2 Default language scope
Time to boot this puppy up.
=cut

use phi root_scope =>
  pcons l(pnil,
          l(paren_local,
            nil_local,
            lambda_local,
            cons_op_local,
            seqr_op_local,
            phiops::whitespace_literal,
            phiops::hash_line_comment_literal,
            int_literal),
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
          pnil),                        # capture
        phifuzz::eval_state_type;

use phi fuzzify => l                    # node
  initial_eval_state, mcall"with_node", # state
  phifuzz::thefuzz,                     # state fuzz
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
    phiops::root_opgate,
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
        pstr"failed the fuzz: ", 0x100, # scope state' e
        0x101,                          # scope state'
        pstr"\n", 0x100,                # scope state'
        pstr"value = ", 0x100,          # scope state'
        mcall"value", 0x101,            # scope
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
