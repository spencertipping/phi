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

=head1 The Fuzz
An eager interpreter for phi op trees.
=cut

package phifuzz;
use strict;
use warnings;

use phiboot;
use phibootmacros;
use philist;
use phiobj;
use phiparse;
use phioptree;


=head2 Structural parsing
Parsing expression grammars are normally applied to strings, but strings aren't
the only things you might want to parse. For example, let's suppose we have a
list of integers -- ASCII codes perhaps -- and we want to use parser combinators
against it. Then the parse state would point to the list itself, and
continuations would be states which pointed to cons cells later down the list.
You could easily port string combinators to operate on lists of char codes
because they're fundamentally the same data structure.

Things get a little more interesting if you want to parse nonlinear structures
-- although this is also something that happens in the string case. Let's start
with something simple like an arithmetic expression tree, for instance:

  binop(+, const(3), binop(*, const(4), const(5)))

The parser to evaluate a structure like this is similar to the one we'd use for
strings:

  evaluated  ::= plus_case | times_case | const_case
  plus_case  ::= binop('+', evaluated, evaluated) -> v[0] + v[1]
  times_case ::= binop('*', evaluated, evaluated) -> v[0] * v[1]
  const_case ::= const(evaluated)

Ok, so what about the parse state? In this case it could be the node being
evaluated. Continuations would be children of that node.

If this seems a lot like destructuring, that's because it really is, just like
parsers are destructuring binds over strings. Parsers as a concept give you much
more flexibility than typical implementations of destructuring binds,
particularly when you can compute grammars, so phi prefers them -- but really,
we're just pattern matching.

=head3 Evaluator parse state
Let's start here:

  state.is_error      -> 1|0
  state.value         -> v
  state.with_value(v) -> state'

We're parsing over objects, so we additionally have a pointer to the object
we're focused on:

  state.node          -> node
  state.with_node(n)  -> state'

And we need to carry some information around; in particular, we need to know the
current C<arg> and C<capture> for any function we're evaluating. Before I list
out a couple more properties, let's talk about how parse states interact with
the evaluation process.

=head4 Subexpression continuations
Suppose we have a parse rule that calls a function on a value (which we will).
Then it sets the parse state's C<arg> and C<capture>, then parses the body with
the overall evaluation grammar to get a resulting logical value. So far so good.

Now let's suppose the body of the function contains a fork with two more
function calls, e.g. C<binop(+, f(3), f(4))>. How is the parse state threaded
between C<f(3)> and C<f(4)>, if at all? If we're parsing them in sequence, then
they need to share some state.

...and that's where things start to get interesting. In sequential terms,
C<f(4)> happens strictly after C<f(3)> -- it has to, because timelines need a
coherent ordering. (This example might be more obvious with a C<seqr> node, but
it's the same ordering for binops.) So we need to take the state coming out of
C<f(3)> and then continue it for C<f(4)>.

We could apply modifications to the return from C<f(3)>, but that doesn't really
make sense when we can just make a new state. In object terms, then, we have a
few more methods:

  state.arg               -> arg-node
  state.capture           -> capture-node
  state.with_arg(a)       -> state'
  state.with_capture(c)   -> state'

We inherit fail states from C<phiparse>.
=cut

use phitype eval_state_type =>
  bind(is_error => drop, lit 0),

  bind(value   => isget 0),
  bind(node    => isget 1),
  bind(arg     => isget 2),
  bind(capture => isget 3),

  bind(with_value   => isset 0),
  bind(with_node    => isset 1),
  bind(with_arg     => isset 2),
  bind(with_capture => isset 3);


=head2 Evaluation parsers
The most important of these is C<const-able>, which sounds like "constable," so
I'm calling it C<thefuzz> for brevity. The idea here is to take a
possibly-complicated expression and match if we can reduce it to a constant. If
so, we return that constant.

The fuzz is a complete parser: that is, it traverses the expression tree all the
way down to leaf nodes. This forces a full evaluation of the thing you apply it
to.

Operator implementations are provided by three objects you give to the fuzz:
C<nullop>, C<unop>, and C<binop>. Each of these objects binds the _symbol_ of
the op as a method over the two parse states. For example, if C<+> is a binary
op, then the fuzz would expect this method to exist:

  binop.+(lhs_state, rhs_state) -> state'

TODO: can we just work with values, rather than entire parse states?

=head3 Building type-specific parsers
Here's what the const parser for the fuzz looks like:

  use phitype thefuzz_const_parser =>
    bind(parse =>                         # state self
      drop, dup, mcall"node", dup,        # state node node
      node_type_is(t_native_const),       # state node const?
      l(                                  # state node
        mcall"native", swap,              # native state
        mcall"with_value"),               # state'
      l(stack(2), failure),               # fail
      if_);

It gets repetitive type-filtering all of our nodes, though, so let's automate
it. Here's what we want:

  TYPE [FN] type_filtered_parser ->
    [(parse ::                            # state self
       drop, dup, mcall"node", dup,       # state node node
       node_type_is(TYPE),                # state node type?
       [FN],                              # state node -> state'
       [stack(2), failure]                # state node -> fail
       if_)]
    make_type

The success function should have this signature:

  state node -> state'

It can, of course, return a failing state.
=cut


# Sub-parsers can recursively refer to the fuzz to evaluate their arguments as
# necessary.
use phi thefuzz_mut => pmut;

use phi type_filtered_parser_typegen => l # type fn -> parser_type
  l(l(stack(2), phiparse::failure),
    if_),                               # type fn tail
  swons,                                # type tail
  swap, pnil, swons, swap,              # [type] tail

  # Now build up the type detector function, which has this signature:
  #
  #   node [type] -> match?
  #
  # We can prepend this to the tail.

  l(                                    # node [type]
    head, swap, mcall"flags",           # type flags
    lit f_typemask, i_and,              # type type
    i_xor, i_not),                      # match?
                                        # [type] tail f
  swap, list_append, i_eval,            # [type] f++tail
  swons,                                # [type]::(f++tail)

  # Now we have a function that takes a node and does the rest. All we need to
  # do is adapt the initial call stack correctly. Specifically, we need this
  # conversion:
  #
  #   state self -> state node node

  l(                                    # state self
    drop, dup, mcall"node", dup),       # state node node
                                        # tail f
  swap, list_append, i_eval,            # f++tail

  lit psym"parse", i_cons,              # bind(parse => ...)
  pnil, swons,                          # [bind(parse => ...)]
  make_type, i_eval;                    # the type


use phi type_filtered_parser => l       # type fn -> parser
  type_filtered_parser_typegen, i_eval, # ptype
  pnil, i_cons;                         # []::ptype


=head3 Simple instantiations
These don't have any delegation like op tables; they're simple values.
=cut

use phi thefuzz_const_parser =>
  le lit t_native_const,
     l(                                 # state node
       mcall"native", swap,             # v state
       mcall"with_value"),              # state'
     type_filtered_parser, i_eval;

use phi thefuzz_arg_parser =>
  le lit t_arg,
     l(                                 # state node
       drop, dup, mcall"arg",           # state argval
       swap, mcall"with_value"),        # state'
     type_filtered_parser, i_eval;

use phi thefuzz_capture_parser =>
  le lit t_capture,
     l(                                 # state node
       drop, dup, mcall"capture",       # state cval
       swap, mcall"with_value"),        # state'
     type_filtered_parser, i_eval;


=head3 Function parser
The function parser returns a version of the function with a fully-evaluated
capture value, or fails. It's parameterized on the generalized expression
evaluator, which would usually be the fuzz itself.
=cut

use phitype thefuzz_fn_parser_type =>
  bind(parser => isget 0),
  bind(parse  =>                        # state self
    nip, mcall"node", dup, mcall"flags",# state self node flags
    lit f_typemask, i_and,              # state self node type
    lit t_fn, i_xor,                    # state self node not-match?

    l(stack(3), phiparse::failure),
    l(                                  # state self node
      dup, mcall"capture",              # state self node c
      stack(0, 3), mcall"with_node",    # state self node state'
      stack(0, 2), mcall"parser",       # state self node state' p
      mcall"parse",                     # state self node state''
      dup, mcall"is_error",             # state self node state'' e?
      l(stack(4, 0)),                   # state''
      l(                                # state self node state''
        mcall"value",                   # state self node cv'
        native_const, i_eval,           # state self node v
        swap, mcall"with_capture",      # state self node'
        stack(0, 2), mcall"with_value", # state self state'
        stack(3, 0)),                   # state'
      if_),
    if_);

use phi thefuzz_fn_parser => pcons l(thefuzz_mut), thefuzz_fn_parser_type;


=head3 Indexed capture access
See L<philang> for the reasoning behind this strategy. Basically, capture lists
have this format:

  [N cN ... c3 c2 c1 c0]

So C<capture_nth(0)> would refer to C<c0>, which is the nth element in the list.
=cut

use phitype thefuzz_capture_nth_parser_type =>
  bind(parse =>                         # state self
    nip, mcall"node", dup, mcall"flags",# state self node flags
    lit f_typemask, i_and,              # state self node type
    lit t_capture_nth, i_xor,           # state self node not-match?

    l(stack(3), phiparse::failure),
    l(                                  # state self node
      mcall"n",                         # state self n
      stack(0, 2), mcall"capture",      # state self n c
      mcall"native",                    # state self n cn
      i_uncons,                         # state self n cs cl
      rot3l, i_neg, i_plus,             # state self cs cl-n
      lget, i_eval,                     # state self cs[cl-n]
      native_const, i_eval,             # state self const(cs[cl-n])
      stack(3, 2, 0),                   # const(...) state
      mcall"with_value"),               # state'
    if_);

use phi thefuzz_capture_nth_parser =>
  pcons pnil, thefuzz_capture_nth_parser_type;


=head3 Nullary operators
These, unary, and binary operators follow a similar implementation strategy.
Basically, the parser objects are parameterized by two things:

1. The op -> fn dispatch table (an object)
2. The subordinate parser/parsers, if they exist

This makes the pieces mostly-reusable.

The op->fn dispatch table works like this, for successful parses:

  self.operator.<op>(state)             # for nullary
  self.operator.<op>(state, s1)         # for unary
  self.operator.<op>(state, s1, s2)     # for binary

=cut

use phitype thefuzz_nullary_operator_type =>
  bind(say_hi =>                        # state self
    drop, lit hi => i_print,            # state
    lit"hi", swap, mcall"with_value");  # state'

use phitype thefuzz_nullary_parser_type =>
  bind(operator => isget 0),
  bind(parse =>                         # state self
    nip, mcall"node", dup, mcall"flags",# state self node flags
    lit f_typemask, i_and,              # state self node type
    lit t_strict_nullary, i_xor,        # state self node not-match?

    l(stack(3), phiparse::failure),
    l(                                  # state self node
      mcall"op",                        # state self op
      swap, mcall"operator", i_eval),   # self.operator.<op>(state)
    if_);

use phi thefuzz_nullary_operator => pcons pnil, thefuzz_nullary_operator_type;
use phi thefuzz_nullary_parser =>
  pcons l(thefuzz_nullary_operator), thefuzz_nullary_parser_type;


=head3 Unary operators
Same as above, just with more fuzz.
=cut

sub bind_unop
{
  bind(shift,                           # state s1 self
    stack(3, 1, 1), mcall"value",       # s1 v
    @_, swap, mcall"with_value");       # s1.with_value(v @_)
}

use phitype thefuzz_unary_operator_type =>
  bind_unop(head => head),
  bind_unop(tail => tail),
  bind_unop('i-' => i_neg),
  bind_unop('i~' => i_inv),
  bind_unop('i!' => i_not);


use phitype thefuzz_unary_parser_type =>
  bind(parser   => isget 0),
  bind(operator => isget 1),
  bind(parse    =>                      # state self
    nip, mcall"node", dup, mcall"flags",# state self node flags
    lit f_typemask, i_and,              # state self node type
    lit t_strict_unary, i_xor,          # state self node not-match?

    l(stack(3), phiparse::failure),
    l(                                  # state self node
      dup, mcall"lhs",                  # state self node node'
      stack(0, 3), mcall"with_node",    # state self node state'
      stack(0, 2), mcall"parser",       # state self node state' p
      mcall"parse",                     # state self node state''
      dup, mcall"is_error",             # state self node state'' e?
      l(stack(4, 0)),                   # failstate
      l(                                # state self node state''
        stack(4, 1, 2, 0, 3),           # state state'' self node
        mcall"op", swap,                # state state'' op self
        mcall"operator", i_eval),       # opr.<op>(state, state'')
      if_),
    if_);

use phi thefuzz_unary_operator => pcons pnil, thefuzz_unary_operator_type;
use phi thefuzz_unary_parser =>
  pcons l(thefuzz_mut, thefuzz_unary_operator), thefuzz_unary_parser_type;


=head3 Binary operators
Ditto - the only new thing here is that we sequentially evaluate things.
=cut

sub bind_binop
{
  bind(shift,                           # state s1 s2 self
    stack(4, 2, 1), mcall"value",       # s2 v1
    nip, mcall"value",                  # s2 v1 v2
    @_, swap, mcall"with_value");       # s2.with_value(v1 v2 @_)
}

use phitype thefuzz_binary_operator_type =>
  bind(seql =>                          # state s1 s2 self
    stack(4, 2, 1), mcall"value",       # s2 v1
    swap, mcall"with_value"),           # s2'

  bind(seqr =>                          # state s1 s2 self
    stack(4, 1)),                       # s2

  bind_binop(cons  => swons),

  bind_binop('i+'  => i_plus),
  bind_binop('i*'  => i_times),
  bind_binop('i<<' => i_lsh),
  bind_binop('i>>' => i_rsh),
  bind_binop('i<'  => i_lt),
  bind_binop('i>'  => swap, i_lt),

  bind_binop('i&'  => i_and),
  bind_binop('i|'  => ior),
  bind_binop('i^'  => i_xor);

use phitype thefuzz_binary_parser_type =>
  bind(parser   => isget 0),
  bind(operator => isget 1),
  bind(parse    =>                      # state self
    nip, mcall"node", dup, mcall"flags",# state self node flags
    lit f_typemask, i_and,              # state self node type
    lit t_strict_binary, i_xor,         # state self node not-match?

    l(stack(3), phiparse::failure),

    # First, parse the LHS and handle failures.
    l(                                  # state self node
      dup, mcall"lhs",                  # state self node node'
      stack(0, 3), mcall"with_node",    # state self node state'
      stack(0, 2), mcall"parser",       # state self node state' p
      mcall"parse",                     # state self node state''
      dup, mcall"is_error",             # state self node state'' e?
      l(stack(4, 0)),                   # failstate
      l(                                # state self node state''
        # Now parse the RHS.
        nip, mcall"rhs",                # state self node rstate rhs
        nip, mcall"with_node",          # state self node rstate rstate'
        stack(0, 3), mcall"parser",     # state self node rstate rstate' p
        mcall"parse",                   # state self node rstate rstate''
        dup, mcall"is_error",           # state self node rstate rstate'' e?
        l(stack(5, 0)),                 # failstate
        l(                              # state self node s1 s2
          stack(4, 2, 3, 0, 1),         # state s1 s2 self node
          mcall"op", swap,              # state s1 s2 op self
          mcall"operator", i_eval),     # opr.<op>(state, s1, s2)
        if_),
      if_),
    if_);

use phi thefuzz_binary_operator => pcons pnil, thefuzz_binary_operator_type;
use phi thefuzz_binary_parser =>
  pcons l(thefuzz_mut, thefuzz_binary_operator), thefuzz_binary_parser_type;


=head3 Conditionals
These are just computed sequences: first the condition, then either branch.
Branches are strict once we know which one to take.
=cut

use phitype thefuzz_if_parser_type =>
  bind(cond_parser => isget 0),
  bind(body_parser => isget 1),
  bind(parse       =>                   # state self
    nip, mcall"node", dup, mcall"flags",# state self node flags
    lit f_typemask, i_and,              # state self node type
    lit t_if, i_xor,                    # state self node not-match?

    l(stack(3), phiparse::failure),
    l(                                  # state self node
      dup, mcall"cond",                 # state self node cond
      stack(0, 3), mcall"with_node",    # state self node state'
      stack(0, 2), mcall"cond_parser",  # state self node state' cp
      mcall"parse",                     # state self node cstate
      dup, mcall"is_error",             # state self node cstate e?

      l(stack(4, 0)),                   # fail-state
      l(                                # state self node cstate
        dup, rot3r, mcall"value",       # state self cstate node bool
        l(mcall"then"),
        l(mcall"else"),
        if_,                            # state self cstate branch
        swap, mcall"with_node",         # state self state'
        stack(3, 1, 0), mcall"parser",  # state' parser
        mcall"parse"),
      if_),
    if_);

use phi thefuzz_if_parser =>
  pcons l(thefuzz_mut, thefuzz_mut), thefuzz_if_parser_type;


=head3 Function calls
These are unusual in that they delegate specifically to a C<fn> parser to
extract the capture value; then we parse the function body with a revised parse
state.

Evaluation ordering is always:

1. Function capture value
2. Function argument value
3. Function body
=cut

use phitype thefuzz_call_parser_type =>
  bind(fn_parser   => isget 0),
  bind(arg_parser  => isget 1),
  bind(body_parser => isget 2),

  bind(parse =>                         # state self
    nip, mcall"node", dup, mcall"flags",# state self node flags
    lit f_typemask, i_and,              # state self node type
    lit t_call, i_xor,                  # state self node not-match?

    l(stack(3), phiparse::failure),
    l(                                  # state self node
      dup, mcall"fn",                   # state self node fnode
      stack(0, 2), mcall"fn_parser",    # state self node fnode fp
      swap, stack(0, 4),                # state self node fp fnode state
      mcall"with_node", swap,           # state self node state' fp
      mcall"parse",                     # state self node state''
      dup, mcall"is_error",             # state self node state'' e?

      l(stack(4, 0)),                   # fail-state
      l(                                # state self node state''
        mcall"value",                   # state self node fnode
        nip, mcall"arg",                # state self node fnode argnode
        stack(0, 4), mcall"with_node",  # state self node fnode state'
        stack(0, 3), mcall"arg_parser", # state self node fnode state' ap
        mcall"parse",
        dup, mcall"is_error",           # state self node fnode astate e?

        l(stack(5, 0)),                 # fail-state
        l(                              # state self node fnode astate
          dup, mcall"value", swap,      # state self node fnode av astate
          mcall"with_arg",              # state self node fnode astate'
          swap, dup, mcall"capture",    # state self node astate' fnode fc
          mcall"native",                # state self node astate' fnode fn
          rot3l, mcall"with_capture",   # state self node fnode astate''
          swap, mcall"body",            # state self node astate'' fbody
          swap, mcall"with_node",       # state self node astate'''
          stack(4, 2, 0, 3),            # state astate''' self
          mcall"body_parser",           # state astate''' bparser
          mcall"parse",                 # state bstate

          # Restore the original values for arg and capture
          nip,  mcall"arg",             # state bstate arg
          swap, mcall"with_arg",        # state bstate'
          nip,  mcall"capture",         # state bstate capture
          swap, mcall"with_capture",    # state bstate''
          top),                         # bstate''

        if_),
      if_),
    if_);

use phi thefuzz_call_parser =>
  pcons l(thefuzz_mut, thefuzz_mut, thefuzz_mut),
        thefuzz_call_parser_type;


=head3 Syntax nodes
These evaluate to an unresolved mut. This forces you to put syntax nodes in some
position in which their value will not be used.
=cut

use phitype thefuzz_syntax_parser_type =>
  bind(parse =>                         # state self
    nip, mcall"node", dup, mcall"flags",# state self node flags
    lit f_typemask, i_and,              # state self node type
    lit t_syntax, i_xor,                # state self node not-match?

    l(stack(3), phiparse::failure),
    l(                                  # state self node
      stack(2),                         # state
      i_mut, dup, i_mset,               # state mutbomb
      swap, mcall"with_value"),         # state'
    if_);

use phi thefuzz_syntax_parser => pcons pnil, thefuzz_syntax_parser_type;


=head3 Alias nodes
These are simple: we just pull the C<node> field and continue.
=cut

use phitype thefuzz_alias_parser_type =>
  bind(parser => isget 0),
  bind(parse =>                         # state self
    nip, mcall"node", dup, mcall"flags",# state self node flags
    lit f_typemask, i_and,              # state self node type
    lit t_alias, i_xor,                 # state self node not-match?

    l(stack(3), phiparse::failure),
    l(                                  # state self node
      dup, mcall"can_be_real",          # state self node real?
      l(mcall"real_node",               # state self realnode
        rot3l, mcall"with_node",        # self state'
        swap, mcall"parser",            # state' p
        mcall"parse"),
      l(                                # state self node
        # Return a self-referential mut bomb
        stack(2),                       # state
        i_mut, dup, i_mset,             # state mutbomb
        swap, mcall"with_value"),       # state'
      if_),
    if_);

use phi thefuzz_alias_parser =>
  pcons l(thefuzz_mut), thefuzz_alias_parser_type;


=head3 The Fuzz
The final moment: we just C<alt> everything together.
=cut

use phi thefuzz =>
  pcons l(l(thefuzz_const_parser,
            thefuzz_arg_parser,
            thefuzz_capture_parser,
            thefuzz_capture_nth_parser,
            thefuzz_fn_parser,
            thefuzz_nullary_parser,
            thefuzz_unary_parser,
            thefuzz_binary_parser,
            thefuzz_if_parser,
            thefuzz_call_parser,
            thefuzz_syntax_parser,
            thefuzz_alias_parser)),
        phiparse::alt_type;

thefuzz_mut->set(thefuzz);


use phi fuzz_initial_state =>
  pcons l(pnil,                         # value
          pnil,                         # node
          pnil,                         # arg
          pnil),                        # capture
        eval_state_type;

use phi fuzzify => l                    # node
  fuzz_initial_state, mcall"with_node", # state
  thefuzz, mcall"parse",                # state'
  dup, mcall"is_error",                 # state' error?
  l(lit failed_the_fuzz => i_crash),    # <crash>
  l(mcall"value"),                      # state'.value
  if_;


1;
