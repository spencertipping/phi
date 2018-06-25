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


=head1 phi prefix/infix/postfix operator support
Alright, let's define some actual syntax now that C<philang> works. Parse
continuations are technically managed by values rather than types, but in
practice most abstract values will delegate to a type to keep it simple. (Rarely
do specifics of a value impact the set of operations you can perform on it.)
Those types will in turn use a library like this to manage the operators they
provide.


=head2 Owned operators
Before I get into the details, let's talk about some high-level stuff. First,
most values support operator precedence by looking at the surrounding operator
and removing lower-precedence stuff from the continuation:

  int.parse_continuation("+") = [[* ...], [/ ...], [** ...] ...]

=head3 Repeated left application
Each value has only one parse continuation, which is normally fine but causes
problems for cascades of lowering-precedence or left-associative operators. For
example:

  3 + 4 + 5

C<3>'s parse continuation accepts C<+>, which in turn accepts C<4> and stops. If
we want to parse C<+ 5>, we'll need another parse continuation from the value
C<3 + 4> -- and that second continuation needs to fit inside the first.

This turns out to be quite simple; all we need to do is C<expr>-ify the output
of an owned operator.


=head2 Unowned (universal) operators
Many languages like Haskell and OCaml support operators-as-constructors, e.g.
C<::> for cons. Given that values-via-types are the sole drivers for parse
continuations, type-independent operators like C<::> appear to be off the table
by design. (And we can't do something independent of the
operator/parse-continuation mechanism simply because then we'll break
precedence.) Fortunately we do have some options:

1. Programmatically add operators to existing types by rewriting the types
2. Use lexical scopes to maintain operator-constructors as metadata
3. Have types talk to some lexically-scoped value to ask about extra ops
4. Implement a type hierarchy

(1) is inconvenient and requires symbolic refs between abstract values and
types. This means we have a nametable somewhere, which we might want for other
reasons but I'm not sure I want to force it for this feature. (For example, how
bad is it really if someone just hard-links the type? It shouldn't break things
like this.)

(2) is just egregious. This involves a whole new metadata channel inside the
scoping object, which is likely to become an open-ended namespace of fail that
you could drive a truck through. Let's make the rule that lexical scopes can't
store arbitrary data just to avoid this outcome.

(3) and (4) differ only in how they're scoped: (3) allows you to add operators
on a lexically-scoped basis, whereas (4) is dynamically scoped. I prefer (3) in
this case simply because it greatly reduces the chances of breaking stuff
through monkey-patching. There's also nothing wrong with having both mechanisms
available.

Getting into the details a bit, (3) requires some kind of symbolic linkage, but
it's not terrible simply because operators themselves are symbolic quantities.
So there's nothing wrong with us binding the name C<::> lexically and having
that become an operator. The only question is how a type would understand that
C<::> is an operator, vs something like C<x> which is a value (and the intended
precedence of C<::>, although maybe it's up to the type to slot it in wherever
it stacks up). We also need to prevent C<::> from parsing as a name.

I think we have enough machinery to do all of this. First, if we want something
not to parse as a value, all we have to do is specify C<fail> as its parse
continuation; then it will parse only as an atom, never as an expression. It's
fine to have parse continuations be forcing elements this way.

Next we need to have C<::> indicate that it can be parsed as an operator. This
is a great opportunity for abstraction: perhaps we want regular values to be
able to behave as operators in some cases, for instance deltas, units,
encodings, or timezones: C<30cm> has C<cm>, a value, behaving as a postfix
operator and modifying another value. Any value could potentially take this
role.

Let's look at C<30cm> from C<30>'s point of view. Its parse continuation
contains an alternation of type-specific operators and atoms, where the atoms
are modified by asking whether they want to postfix-modify C<30>. We have
something like this:

  # NB: this is subtly broken; see below
  30.parse_continuation(op) = alt("+" expr,
                                  "-" expr,
                                  ...,
                                  atom >>= postfix_modify
                                       >>= parse_continuation(op))

If an atom doesn't intend to function as an operator then its C<postfix_modify>
can accept a value and then return C<fail> as its parse continuation,
effectively rejecting the parse. This will cause the whole value to be reparsed
and other alternatives to be considered, which I think is correct.

If the atom does intend to modify a value, then C<postfix_modify> will return a
modified abstract that can dictate its parse continuation normally.

Finally, of course, there's no reason to limit postfix things to just atoms --
and that's the last important piece. Really what we want is to say this:

  30.parse_continuation(op) =
    alt("+" expr("+"),
        "-" expr("-"),
        ...,
        expr("postfix") >>= postfix_modify(op) >>= parse_continuation(op))

...and that exemplifies the last piece of the picture, opportunistic parse
rejection. Before I talk about that, though, let's discuss the details of the
postfix case.

=head3 Postfix operators and expression flatmapping
This is the most subtle thing going on. The idea is that we have something like
C<3 :: nil>, where C<::> is a value. C<::> is part of C<3>'s parse continuation,
so we have to flatmap to keep the parse going (since C<3> only gets one parse
continuation). From C<3>'s point of view, C<::...> is a single parse element.

From C<::>'s point of view, C<3> passes control first via a closer-precedence
expr; then it flatmaps into the postfix-modify/parse-continuation parser.

So equationally:

  postfix_case(v) = let ep     = expr(closer) in
                    let e next = e.postfix_modify(op, v)
                                  .parse_continuation(op) in
                    [ep (v c -> c) next flatmap.]

Because C<3> has only one parse continuation, C<::> needs to parse the RHS at
C<::> precedence _before_ kicking over to the surrounding op continuation. So
C<expr(closer)> parses C<:: expr("::")>.


=head2 Multi-channel precedence rejection
C<3.parse_continuation(op)> implicitly rejects some of its alternatives based on
operator precedence, but of course it doesn't necessarily understand the
precedence of every ad-hoc postfix operator. Instead, those postfix operators
look at the surrounding precedence and make a call inside C<postfix_modify>,
selectively emitting fail values.

This mechanism arises more often than you might think: it's the only reason the
C<;> operator works at all, for example. It's also why C<;> is postfix rather
than infix, which makes it possible to parse C-style grammars.


=head2 Whitespace and comments
Yep, you guessed it: whitespace elements are just regular values. Space, tab,
CR, and LF are bound to identity transformers that can function as passthrough
prefix/postfix operators, and the line comment marker C<#> is a value whose
parse continuation eats things until the next newline.

This, of course, means you can do some interesting things (NB: the example below
is nonsensical and won't work, but has the tabloid appeal I'm going for here so
I'll keep it):

  let '// = '# in
  // this is now a line comment


=head2 Grouping
It isn't immediately obvious how parens should work given that precedence gets
passed into the C<expr> parser -- but it's simpler than you might think. C<(> is
a value whose continuation is C<expr(opener)> followed by C<)>; then the parse
continuation of all of that comes from the expr:

  "(".parse_continuation(op)
    = (x=expr(opener) ++ ")") >>= x.parse_continuation(op)

List brackets and other things work the same way. Delimited things are at
liberty to create subscopes that include operator bindings for commas, etc, if
they want special treatment for these things -- or more conventionally, the
parse continuation could simply look for those delimiters.


=head2 Shadowing
Operators and delimiters aren't distinct. For example, Python defines C<in> as
an infix operator, whereas ML/Haskell define C<in> as a grouping closer for
C<let> bindings. Similarly, C-style languages use C<:> for the ternary operator
while many functional languages appropriate it for either consing or type
specification. C<|> is another example of a symbol that can take multiple roles.

If we parse things in the obvious way, we'll run into unresolvable problems when
values take ownership of delimiting symbols:

  foo match [x] -> 5                    # 5 owns | as bitwise-or
          | []  -> 6                    # ...so this will fail miserably

The solution is to allow constructs like C<match> and C<let> to create a
continuation context in which some owned operators are marked as playing a
syntactic role. This will remove them from owned-operator parse continuations.

Shadowing and precedence are both reset by grouping constructs, and are managed
by the same "operator gate" object.

=head3 Operator gating and shadow linkage
Operator applicativity is gated on two conditions:

1. Its precedence is high enough
2. It isn't being applied in a context in which it's been shadowed

So an operator gate would need to store the current precedence+associativity and
the set of shadowed operators. But there's a bit more to it; for example:

  foo match [x] -> (x | 10)             # | is shadowed outside parens
          | []  -> 0;                   # ; lower precedence than |
  y = 10 | 20                           # this | isn't shadowed

We have a couple of things going on here. First, C<()> groups locally erase
shadowing, which is appropriate because the shadowed operator can't reasonably
cut a group. Second, C<|> is shadowed only within a given precedence; there's no
sense in having it remain shadowed after we've exited the region.

Let's walk through the parse above from an operator gating point of view. I'll
mark the parse position with C<_>.

  foo match _                           # precedence from "match",
                                        # shadow = surrounding union "|"

Notice that we're unioning the shadow with whatever was shadowed in the
surrounding scope. This is really important: precedence and shadow sets aren't
stored in the same way, nor can they be because unlike precedence, shadowing is
only partially ordered. So we have something like this:

  gate.refine(gate') = { precedence: gate'.precedence,
                         shadow:     gate.shadow union gate'.shadow }

Rather than unioning at every step, it's more efficient to link the inner gate
to the one it's refining; then the shadow check can traverse upwards.

  foo match [x] -> (_ ...)              # A: precedence = opener, no shadow

  foo match [x] -> (x | 10)_            # B: precedence = closer,
                                        # shadow = surrounding union "|"
    # -> fail until precedence <= "|", at which point "|" is unshadowed

  foo match [x] -> (x | 10)             # C: precedence = "|",
          | _                           # shadow = surrounding union "|"

Notice the recovery between steps A and B: the paren group is responsible for
restoring the outer shadow set after the closer. Steps B and C don't involve
restoration the same way; instead, the precedence drops until C<|> can be
parsed, at which point the structural parser's C<|> owned-op sets its
continuation precedence and shadows C<|>.

=head3 Shadow/precedence interoperation
Precedence-tracked shadows force some amount of consistency in the way operator
precedence is interpreted. A good example of this is OCaml's interpretation of
ambiguous nested C<match> constructs (and bear in mind that OCaml is
whitespace-insensitive):

  match x with
    | foo -> ...                          # matching x
    | bar -> match y with                 # matching x
      | bif -> ...                        # matching y
      | baz -> ...                        # matching y

Perl has something similar:

  1 + reverse 1, 2, 3                     # 1 + reverse(1, 2, 3)

OCaml doesn't model C<match> as a regular operator; instead, it behaves more
like a grouping construct, binding everything rightwards. Perl's interpretation
of C<reverse> has similar behavior using a different mechanism: it's implemented
using asymmetric operator precedence.

phi can emulate both of these whether C<match> is modeled as a group or as an
infix operator, in part because groups and operators work the same way.

=head3 Asymmetric precedence
Oh yes, phi has this. Asymmetric operators usually present a higher precedence
leftwards than rightwards, as in Perl. I can't imagine a real-world situation
where you'd want it to go the other way, but I suspect one exists.

Since precedence is just a number, the C<op_precedence> object just stores the
left and right levels separately and does the comparison in
C<binds_rightwards_of>.
=cut


package phiops;
use strict;
use warnings;

use Exporter qw/import/;
use phiboot;
use phibootmacros;
use philist;
use phiparse;
use phiobj;
use phioptree;
use philang;


=head2 Meta-values
Basically, things that aren't actual values. These are used to force a parse
failure in situations where an abstract, rather than a parser, is being
returned. Because the parse continuation is required to match and invariably
fails, C<fail> is unparsable and will cause some amount of backtracking.
=cut

use phitype fail_type => bind(parse_continuation => stack(2), phiparse::fail);
use phi     fail      => pcons pnil, fail_type;
use phi     fail_node => le fail, syntax, i_eval;


=head2 Precedence objects
These manage precedence and associativity.

NB: precedence values are priorities: low values = high binding order. So zero
is the maximum precedence.

NB: zero is special in the precedence order: it's used to indicate a postfix
binding. All user-defined operators should have positive precedence.

Operators at the same precedence level should share associativity, but this
isn't strictly required (your grammar may just be a little weird if they don't).
For example, suppose C<+> is left-associative and C<-> is right-associative:

  2 + 3 + 4 - 5 - 6     # parsed as ((2 + 3) + 4) - (5 - 6)

More specifically, the surrounding (left-side) operator determines
associativity.
=cut

use phitype op_precedence_type =>
  bind(left_precedence       => isget 0),
  bind(right_precedence      => isget 1),
  bind(binds_rightwards      => isget 2),

  bind(with_left_precedence  => isset 0),
  bind(with_right_precedence => isset 1),
  bind(with_binds_rightwards => isset 2),

  bind(is_postfix => mcall"left_precedence", i_not),

  bind(binds_rightwards_of =>           # left-precedence self
    mcall"left_precedence", swap,       # rp l
    dup, mcall"right_precedence", rot3l,# l lp rp
    stack(0, 0, 1),                     # l lp rp lp rp
    i_lt,                               # l lp rp rp<lp?
    l(                                  # l lp rp
      stack(3), lit 1),                 # 1
    l(                                  # l lp rp
      swap, i_lt,                       # l lp<rp?
      l(                                # l
        drop, lit 0),                   # 0
      l(                                # l
        mcall"binds_rightwards"),       # lr
      if_),
    if_);


=head2 Identity null parse continuation
You can (and probably should) specify this as the last element in an C<alt> list
of parse continuations. This enables a value to be parsed without a suffix if no
other continuation accepts the parse.

Although you can manually construct parse continuations for values, it's often
easier to delegate to op gates, which will build them for you from structured op
lists.
=cut

use phi identity_null_continuation => l # lhs
  l(top), swons,                        # [lhs top]
  phiparse::none, swap,                 # p f
  pnil, swons, swons,                   # [p f]
  phiparse::map_type, swons;            # map(p f)


=head2 Operator gate
An object that stores the current operator precedence, any shadowed operators,
and, if applicable, a parent linkage. This information collectively determines
which operators apply at any given moment.

NB: shadow lists are of symbols, not strings and not operator objects.
=cut

use phi unowned_as_postfix_mut => pmut;

use phitype op_gate_type =>
  bind(precedence        => isget 0),
  bind(shadowed_ops      => isget 1),
  bind(parent            => isget 2),

  bind(with_precedence   => isset 0),
  bind(with_shadowed_ops => isset 1),
  bind(with_parent       => isset 2),

  bind(child_for_op =>                  # op self
    dup, tail,                          # op self selftype
    rot3l, mcall"precedence",           # self selftype opprec
    rot3l, pnil, swap,                  # selftype opprec [] self
    pnil, swons, swons, swons,          # selftype [opprec [] self]
    i_cons),                            # [opprec [] self]::selftype

  bind(shadow =>                        # opname self
    dup, mcall"shadowed_ops",           # opname self shadowed
    rot3l, i_cons,                      # self opname::shadowed
    swap, mcall"with_shadowed_ops"),    # self'

  bind(is_shadowed =>                   # opname self
    dup, mcall"parent",                 # opname self parent
    swap, mcall"shadowed_ops",          # opname parent shadowlist
    stack(3, 2, 0, 2, 1),               # parent opname shadowlist opname
    list_contains_sym, i_eval,          # parent opname shadowed?
    l(stack(2), lit 1),                 # 1
    l(swap, dup, nilp,                  # opname parent parent-nil?
      l(stack(2), lit 0),               # 0
      l(mcall"is_shadowed"),            # parent.is_shadowed(opname)
      if_),
    if_),

  bind(is_applicable =>                 # op self
    # Two checks here. The first is for precedence and the second is for
    # shadowing (precedence happens first because it's more likely to fail).
    dup, mcall"precedence",             # op self self-pred
    stack(0, 2), mcall"precedence",     # op self self-pred op-pred
    mcall"binds_rightwards_of",         # op self bind?

    l(swap, mcall"name",                # self opname
      swap, mcall"is_shadowed",         # self.is_shadowed(opname)
      i_not),                           # !self.is_shadowed(opname)

    l(stack(2), lit 0),                 # 0
    if_),

  bind(applicable_owned_ops =>          # oplist self
    l(mcall"is_applicable"),            # oplist self [.is_applicable()]
    swons,                              # oplist [self.is_applicable()]
    list_filter, i_eval),               # oplist'

  bind(parse_continuation_for =>        # lhs oplist allow-none? self
    swap,                               # lhs oplist self allow-none?
    l(                                  # lhs oplist self
      stack(0, 2),                      # lhs oplist self lhs
      identity_null_continuation, i_eval, # lhs oplist self p
      pnil, swons),                     # lhs oplist self [p]
    l(pnil),                            # lhs oplist self []
    if_,                                # lhs oplist self cases

    stack(0, 3, 1),                     # lhs oplist self cases self lhs
    unowned_as_postfix_mut, i_eval,     # lhs oplist self cases p
    i_cons,                             # lhs oplist self cases'

    stack(4, 1, 2, 1, 3, 0),            # cases' lhs self oplist self
    mcall"applicable_owned_ops",        # cases' lhs self oplist'
    rot3r,                              # cases' oplist' lhs self
    l(                                  # op lhs opgate
      stack(3, 2, 0, 1, 0),             # opgate lhs opgate op
      mcall"parser",                    # opgate op.parser(lhs, opgate)
      swap,                             # p opgate
      philang::expr_parser_for, i_eval  # expr(p, opgate)
    ),
                                        # cases' oplist' lhs self [...]
    swons, swons,                       # cases' oplist' f
    list_map, i_eval,                   # cases' opparsers
    swap, list_append, i_eval,          # cases''

    pnil, swons,                        # [cases'']
    phiparse::alt_type, swons);         # alt(cases'')


=head2 Special operators
These "operators" are used in two special cases. C<(> means that there is no
surrounding precedence, so all parse continuations should be considered; and
C<)> means that we're parsing a postfix value and no/few parse continuations
should be considered.
=cut

use phitype special_operator_type =>
  bind(name       => isget 0),
  bind(precedence => isget 1);

use phi opener_precedence => pcons l(pint 0x7fffffff, pint 0x7fffffff, pint 0),
                                   op_precedence_type;

use phi closer_precedence => pcons l(pint 0, pint 0, pint 0),
                                   op_precedence_type;

use phi opener => pcons l(psym"(", opener_precedence), special_operator_type;
use phi closer => pcons l(psym")", closer_precedence), special_operator_type;

use phi root_opgate    => pcons l(opener_precedence, pnil, pnil), op_gate_type;
use phi postfix_opgate => pcons l(closer_precedence, pnil, pnil), op_gate_type;


=head2 Grouping
These functions give you a way to create values that will group things. All you
need to do is make sure that the closing element, e.g. a closing paren, doesn't
parse as a value; otherwise phi will do that instead of closing the group. You
can work around this in a ham-fisted way by creating a new binding that maps the
closing element to C<fail_node>, although this carries some overhead because
you'll then have to unbind it and fix up your scope.

=head3 Grouping elements as postfix operators
Grouping elements can function as postfix operators, which is useful for
defining things like function calls or array slicing:

  [1, 2, 3]         # standalone list
  x[1, 2, 3]        # some kind of slicing operation

  (1 + 2)           # standalone expr
  f(1 + 2)          # function call

C<(1, 2)> might parse as a tuple, whereas C<f(1, 2)> might parse as a binary
function call -- and those grammars may be different. We differentiate between
them by having two different inner grammars: C<inner> for the standalone case
and C<postfix_inner> for the postfix case. Groupings themselves don't support
postfix modification; you should have the grouping parse into a different value
that supports this if you're using a group as a postfix value (e.g, the group in
C<f(1, 2)> should return something like a "function call postfix value" that can
modify C<f>).
=cut

use phitype grouping_type =>
  bind(closer             => isget 0),
  bind(inner              => isget 1),
  bind(postfix_inner      => isget 2),

  bind(with_closer        => isset 0),
  bind(with_inner         => isset 1),
  bind(with_postfix_inner => isset 2),

  bind(postfix_modify => lit groupings_are_not_postfix => i_crash),

  bind(parse_continuation =>            # op self
    stack(0, 1, 0),                     # op self self op
    mcall"precedence",
    mcall"is_postfix",                  # op self self postfix?
    l(mcall"postfix_inner"),
    l(mcall"inner"),
    if_,                                # op self inner-parser
    swap, mcall"closer",                # op inner-parser closer-parser
    pnil, swons, swons,                 # op [inner closer]
    pnil, swons,                        # op [[inner closer]]
    phiparse::seq_type, swons,          # op seqp
    l(head),                            # op seqp [head]
    pnil, swons, swons,                 # op [seqp [head]]
    phiparse::map_type, swons,          # op vparser
    swap,                               # vparser op
    philang::expr_parser_for, i_eval);  # expr-parser


# This paren value will reject a parse as a postfix operator because that's a
# higher-level concern than just getting grouping to work.
use phi paren_value => pcons l(str_(pstr")"),
                               le(root_opgate, philang::expr, i_eval),
                               phiparse::fail),
                             grouping_type;

use phi paren_literal => local_ str_(pstr"("),
                                le(paren_value, syntax, i_eval);


=head2 Comments and whitespace
We might as well jump right in on this, starting with whitespace. Whitespace can
take two roles, prefix and postfix. If it's prefix, it will be asked for a parse
continuation; if postfix, it will be asked to modify a value.
=cut

use phitype whitespace_comment_type =>
  bind(abstract    => syntax, i_eval),
  bind(parser      => isget 0),
  bind(with_parser => isset 0),

  bind(postfix_modify => stack(3, 1)),  # op v self -> v
  bind(parse_continuation =>            # op self
    dup, rot3l,                         # self self op
    dup, mcall"precedence",
         mcall"is_postfix",             # self self op postfix?

    # postfix case: delegate to the parser to consume input (if appropriate,
    # e.g. for line comments); then return self
    l(                                  # self self op
      drop, mcall"abstract",            # self abstract
      l(top), swons,                    # self [abstract top]
      swap, mcall"parser", swap,        # p f
      pnil, swons, swons,               # [p f]
      phiparse::map_type, swons),       # map(p, f)

    # prefix case: parse the rest of this construct, then throw it away and
    # parse a toplevel expr
    l(                                  # self self op
      stack(3, 2, 0),                   # op self
      mcall"parser",                    # op p
      swap, philang::expr, i_eval,      # p expr(op)
      pnil, swons, swons,               # [p expr(op)]
      pnil, swons,                      # [[p expr(op)]]
      phiparse::seq_type, swons,        # seq([p expr])
      l(tail, head),                    # p f
      pnil, swons, swons,               # [p f]
      phiparse::map_type, swons),       # map(p f)

    if_);


use phi whitespace_value    => pcons l(phiparse::none), whitespace_comment_type;
use phi line_comment_parser => maybe_ seq_ str_ pstr" ", rep_ oneof_ pstr"\r\n", 0;
use phi line_comment_value  => pcons l(line_comment_parser), whitespace_comment_type;

use phi whitespace_literal => local_
  rep_ oneof_(pstr" \n\r\t", 1),
  le whitespace_value, mcall"abstract";

use phi hash_line_comment_literal => local_
  str_ pstr"#",
  le line_comment_value, mcall"abstract";


=head2 Unowned operators
These temporarily store the RHS and then delegate to a combiner function whose
signature is C<< lhs rhs -> v' >>. Unowned ops can be used as prefix values;
that behavior is fully delegated.

An unowned op function is at liberty to return C<fail> to indicate that the
operator doesn't apply for some reason. In that case the parse will be rejected
and another operator may be used.
=cut

use phitype unowned_op_type =>
  bind(name               => isget 0),
  bind(precedence         => isget 1),
  bind(fn                 => isget 2),
  bind(rhs_parser_fn      => isget 3),  # opgate -> parser
  bind(prefix_value       => isget 4),  # NB: must be a syntax value
  bind(rhs                => isget 5),  # NB: transient state

  bind(with_name          => isset 0),
  bind(with_precedence    => isset 1),
  bind(with_fn            => isset 2),
  bind(with_rhs_parser_fn => isset 3),
  bind(with_prefix_value  => isset 4),
  bind(with_rhs           => isset 5),

  bind(rhs_parser =>                    # opgate self
    dup, mcall"rhs_parser_fn",          # opgate self f
    stack(3, 2, 1, 0),                  # f self opgate
    mcall"child_for_op",                # f opgate'
    swap, al(0)),                       # f(opgate')

  bind(postfix_modify =>                # opgate v self -> opnode
    # Verify that we're allowed to bind within this opgate.
    stack(0, 2, 0),                     # opgate v self self opgate
    mcall"is_applicable",               # opgate v self applicable?
    l(                                  # opgate v self
      rot3l, drop,                      # v self
      dup, mcall"rhs",                  # v self self.rhs
      swap, mcall"fn",                  # v self.rhs f
      al(-1)),                          # v'
    l(stack(3), fail_node),             # fail
    if_),

  bind(parse_continuation =>            # opgate self
    swap,                               # self opgate
    dup, mcall"precedence",
         mcall"is_postfix",             # are we being used as a postfix op?

    # If we're a postfix op, then parse an expression at our precedence and
    # store the RHS. We can complete the operation in postfix_modify.
    l(                                  # self opgate
      nip, mcall"rhs_parser",           # self self.rhs_parser(opgate)

      # NB: it's appropriate to use a syntax node here, rather than something
      # with a real value. The reason is that we don't presume RHS-bound unowned
      # ops have a "real value" -- in the expression "3 + 4", for instance, does
      # "+ 4" correspond to something you could bind to a variable? Probably
      # not, and that's probably ok.
      swap, l(mcall"with_rhs",
              syntax,
              i_eval), swons,           # p [v -> syntax(self.with_rhs(v))]
      pnil, swons, swons,               # [p f]
      phiparse::map_type, swons),       # map(p f)

    # ...otherwise, pretend we're the prefix value and hand the parse over.
    l(                                  # self op
      swap, mcall"prefix_value",        # op syntax-v
      mcall"parse_continuation"),       # p

    if_);


use phi unowned_suffix     => le postfix_opgate, philang::expr, i_eval;
use phi unowned_as_postfix => l         # opgate lhs -> parser
  l(                                    # state e v opgate -> continuation
    stack(3, 3, 2, 1, 0, 0),            # state opg opg v e state
    mcall"scope",                       # state opg opg v e scope
    mcall"dialect", mcall"inflect",     # state opg opg v e'
    mcall"postfix_modify",              # state opg e'
    stack(0, 2), mcall"scope",          # state opg e' scope
    mcall"dialect", mcall"inflect",     # state opg se
    mcall"parse_continuation"           # state parser
  ),                                    # opg lhs next-unbound
  rot3l,                                # lhs next-unbound opg
  i_cons, swons,                        # f
  unowned_suffix, swap,                 # p f
  l(top), swap,                         # p c f
  pnil, swons, swons, swons,            # [p c f]
  phiparse::flatmap_type, swons;        # flatmap(p c f)

unowned_as_postfix_mut->set(unowned_as_postfix);


=head2 Owned operators
These integrate with values, typically as part of a precedence list. They work
differently from unowned operators in that they already have access to the LHS,
so they can operate eagerly. Also, owned operators aren't bound as locals so
they don't have to be fully general; that is, they don't have to handle the
postfix case.

Using an owned operator from an abstract just involves passing the LHS into the
C<parser> method and adding the result to an C<alt>.
=cut

use phitype owned_op_type =>
  bind(name               => isget 0),
  bind(precedence         => isget 1),
  bind(op_parser          => isget 2),
  bind(rhs_parser_fn      => isget 3),  # lhs opgate -> parser
  bind(fn                 => isget 4),  # lhs rhs    -> abstract

  bind(with_name          => isset 0),
  bind(with_precedence    => isset 1),
  bind(with_op_parser     => isset 2),
  bind(with_rhs_parser_fn => isset 3),
  bind(with_fn            => isset 4),

  # TODO(minor): the op parser returns a value that we ignore. It should go into
  # the RHS parser function so you can write parameterized ops.
  bind(rhs_parser =>                    # lhs opgate self
    mcall"rhs_parser_fn", al(-1)),      # parser

  bind(parser =>                        # lhs opgate self
    # OK, here's what's going on here.
    # We need to build a parser that first consumes the operator itself
    # (op_parser), then consumes the operand (rhs_parser). This involves a
    # couple of things.
    #
    # First, we need to pass the left operand and the surrounding opgate into
    # the rhs_parser_fn (via .rhs_parser()). This will give us the real RHS
    # parser, which we can splice into the grammar using a parser flatmap. We're
    # flatmapping instead of using seq() because we want to delay the call to
    # .rhs_parser until we know we have the correct operator -- our computed
    # grammar should be lazy.
    #
    # Second, we call into the "fn", which is a constructor that takes the LHS
    # and RHS and returns a new abstract. This is a simple parser-map operation
    # that closes over the LHS.

    stack(3, 1, 0, 0, 2),               # lhs self self opgate
    mcall"child_for_op",                # lhs self opgate'

    nip, mcall"op_parser",              # lhs self opgate' p
    l(top),                             # lhs self opgate' p c
    l(                                  # op-out lhs opgate self
      mcall"rhs_parser",                # op-out rhsp
      top,                              # rhsp
    ),                                  # lhs self opgate' p c [.rhs_parser() top]

    stack(1, 0, 4, 3, 5),               # lhs self opgate' p c lhs opgate' self [...]
    swons, swons, swons,                # lhs self opgate' p c f
    pnil, swons, swons, swons,          # lhs self opgate' [p c f]
    phiparse::flatmap_type, swons,      # lhs self opgate' flatmap

    stack(3, 2, 0),                     # lhs flatmap self
    mcall"fn",                          # lhs flatmap opfn
    l(swap), swap,                      # lhs flatmap [swap] opfn
    list_append, i_eval,                # lhs flatmap swap++opfn
    rot3l, i_cons,                      # flatmap lhs::(swap++opfn)

    pnil, swons, swons,                 # [flatmap f]
    phiparse::map_type, swons);         # map


1;