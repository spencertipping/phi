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


=head1 phi language frontend
A more useful syntax for concatenative phi programming. This layer uses
L<phioptree> and L<phiinterp> to convert infix syntax (and lexical scopes) to
values that can be executed concatenatively.
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
use phiinterp;
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
          l(                            # lhs opgate
            top, philang::expr, i_eval),# expr(opgate)
          l(@fn)),
        phiops::owned_op_type;
}

sub make_postop
{
  my ($precedence, $associativity, $opname, @fn) = @_;
  pcons l(psym$opname,
          pcons(l($precedence, $precedence, $associativity),
                phiops::op_precedence_type),
          str_(pstr$opname),
          l(                            # lhs opgate
            stack(2), phiparse::none),  # none
          l(drop, @fn)),
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
evaluator; all we have is L<phiinterp>, which is fully strict. And while I could
write another evaluator for this infix layer, it's easier to make values generic
for now and then write the next evaluator from inside the infix syntax.

Using generic values means that we have one suboptimality: both integers and
symbols can be meaningfully compared for equality.

We don't need a full operator set here. We just need enough to JIT the ones we
don't have yet.
=cut

use phi head_op   => make_postop 10, 0, ".h", op_head, i_eval;
use phi tail_op   => make_postop 10, 0, ".t", op_tail, i_eval;

use phi call_op   => make_binop 20, 0, "", call, i_eval;

use phi itimes_op => make_binop 30, 0, "*", op_itimes, i_eval;
use phi iplus_op  => make_binop 40, 0, "+", op_iplus, i_eval;
use phi iminus_op => make_binop 40, 0, "-", op_ineg, i_eval,
                                            op_iplus, i_eval;

use phi ilsh_op   => make_binop 50, 0, "<<", op_ilsh, i_eval;
use phi irsh_op   => make_binop 50, 0, ">>", op_irsh, i_eval;
use phi ilt_op    => make_binop 60, 0, "<",  op_ilt, i_eval;
use phi igt_op    => make_binop 60, 0, ">",  op_igt, i_eval;

# NB: only for ints
use phi ieq_op    => make_binop 80, 0, "==", op_ixor, i_eval,
                                             op_inot, i_eval;

use phi iand_op   => make_binop 90,  0, "&", op_iand, i_eval;
use phi ior_op    => make_binop 100, 0, "|", op_ior,  i_eval;
use phi ixor_op   => make_binop 100, 0, "^", op_ixor, i_eval;


=head3 Conditional (ternary) operator
This turns out to be pretty straightforward since we have operator shadowing.
C<:> isn't normally defined, but let's go ahead and shadow it anyway as a matter
of good practice.

We parse the inside of C<?:> as a group. C<?:> is right-associative.
=cut

use phi if_op => pcons
  l("?",
    pcons(l(110, 110, 1), phiops::op_precedence_type),
    str_(pstr"?"),
    l(                                  # lhs opgate
      phiops::root_opgate,              # lhs opgate root
      lit":", swap, mcall"shadow",      # lhs opgate root'
      philang::expr, i_eval,            # lhs opgate then-expr
      swap, philang::expr, i_eval,      # lhs then-expr else-expr
      str_(pstr":"), swap,              # lhs then-expr ":" else-expr
      pnil, swons, swons, swons,        # lhs [then ":" else]
      pnil, swons,                      # lhs [[then ":" else]]
      phiparse::seq_type, swons,        # lhs parser
      top),
    l                                   # lhs [then ":" else]
      unswons, tail, head,              # lhs then else
      rot3l, phioptree::if, i_eval),    # if-node

  phiops::owned_op_type;


use phitype generic_abstract_type =>
  bind(abstract => isget 0),

  # No postfix modification: function calls are implemented as empty operators.
  bind(postfix_modify => stack(3), phiops::fail_node),

  bind(parse_continuation =>            # opgate self
    mcall"abstract",                    # opgate lhs
    l(call_op, head_op, tail_op,
      itimes_op, iplus_op, iminus_op,
      ilsh_op, irsh_op, ilt_op, igt_op,
      ieq_op, iand_op, ior_op, ixor_op, if_op,
    ),                                  # opgate lhs oplist
    rot3l,                              # lhs oplist opgate
    lit 1, swap,                        # lhs oplist 1 opgate
    mcall"parse_continuation_for");     # p


use phi infix_dialect =>
  pcons l(generic_abstract_type), philang::class_wrapper_dialect_type;


use phi inside_parens => le lit phiops::root_opgate, philang::expr, i_eval;

use phi paren => pcons l(str_(pstr")"), inside_parens, inside_parens),
                       phiops::grouping_type;

use phi paren_local => local_ str_(pstr"("), le paren, syntax, i_eval;


=head3 List literals
C<[a, b, ...]> is a shorthand for C<(a :: b :: ... :: nil)>.
=cut

use phi cons_list_mut => pmut;
use phi cons_list => l                  # exprs tail
  nip, nilp,                            # exprs tail nil?
  l(top),                               # tail
  l(                                    # exprs tail
    swap, i_uncons,                     # tail exprs' expr
    rot3l, op_cons, i_eval,             # exprs' tail'
    cons_list_mut, i_eval),             # tail''
  if_;

cons_list_mut->set(cons_list);


use phi inside_list => le
  lit phiops::root_opgate,              # g
  lit ",", swap, mcall"shadow",         # g'
  philang::expr, i_eval,                # expr(g')
  maybe_(str_ pstr","),                 # ep ","?
  pnil, swons, swons,                   # [ep ","?]
  pnil, swons, phiparse::seq_type, swons, # one

  l(head),                              # one f
  pnil, swons, swons,                   # [one f]
  phiparse::map_type, swons,            # one'

  pnil, swons,                          # [one']
  phiparse::rep_type, swons,            # one'+

  pnil, swons,
  phiparse::maybe_type, swons,          # one'*

  l(                                    # exprs
    rev, i_eval,                        # rev(exprs)
    c_nil, cons_list, i_eval),          # one'* f
  pnil, swons, swons,                   # [one'* f]
  phiparse::map_type, swons;            # parse_map(one*, f)

use phi bracket => pcons l(str_(pstr"]"), inside_list, inside_list),
                         phiops::grouping_type;

use phi bracket_local => local_ str_(pstr"["), le bracket, syntax, i_eval;


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


=head2 Lambdas
Lambdas are a very simple form of what they look like in Haskell:

  \x -> x + 1

Structurally, this begins with the C<\> local, which parses an unbound symbol
that owns the -> operator. (I explain this more below.) The next layer, written
in this one, will replace C<\> with a better lambda operator that is aware of
things like destructuring.
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
    l(top),                                 # state
    l(                                      # self state
      mcall"exit_child_scope",              # self child state
      dup, mcall"value",                    # self child state body
      rot3l, mcall"capture",                # self state body capture-obj
      mcall"capture_list",                  # self state body capture
      swap, fn, i_eval,                     # self state fn
      swap, mcall"with_value",              # self state'
      top),                                 # state'
    if_);


use phi lambda_arrow_op =>
  pcons
    l(psym"->",
      pcons(l(120, 120, 1), phiops::op_precedence_type),
      str_(pstr"->"),
      l(                                # lhs opgate
        # The LHS here is the unbound symbol opnode, which should be a syntax
        # instance. This means we can get the symbol value itself by
        # dereferencing the syntax and calling .sym().
        swap,                           # opgate lhs
        mcall"syntax", mcall"sym",      # opgate sym
        swap, philang::expr, i_eval,    # sym rhs-parser
        pnil, swons, swons,             # [sym rhs-parser]
        lambda_parser_type, swons),     # parser
      l(                                # lhs rhs
        top)),                          # rhs
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

  bind(parse_continuation =>            # opgate self
    mcall"abstract",                    # opgate lhs
    l(lambda_arrow_op),                 # opgate lhs oplist
    rot3l,                              # lhs oplist opgate
    lit 1, swap,                        # lhs oplist 1 opgate
    mcall"parse_continuation_for");     # p

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


=head3 Symbol literal parser: C<'foo>
This isn't the same thing as an unbound symbol; instead, it's a normal value
that contains a symbol.
=cut

use phi symbol_literal => map_
  seq_(str_ pstr"'", symbol),
  l                                     # ["'" sym]
    tail, head,                         # sym
    native_const, i_eval;               # const(sym)


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
            bracket_local,
            lambda_local,
            cons_op_local,
            seqr_op_local,

            local_(sym_"make_type", c phiobj::make_type),

            phiops::whitespace_literal,
            phiops::hash_line_comment_literal,
            symbol_literal,
            int_literal),
          philang::empty_capture_list,
          infix_dialect),
        philang::scope_type;


=head2 REPL
Mostly for use with native code.
=cut

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
      pnil, pnil, interp, i_eval,       # scope state' v''

      #pstr"= ", 0x100,
      0x101,                            # scope state'
      pstr"\n", 0x100,                  # scope state'
      top, mcall"scope",                # scope'
      dup, mcall"parent", nilp,         # scope' parent-nil?
      pnil,
      l(lit parent_scope_not_nil => i_crash),
      if_,
      repl_mut, i_eval),                # scope' repl
    if_),
  if_;

repl_mut->set(repl);


1;
