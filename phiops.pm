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

TODO: describe the mechanics
=cut


package phiops;
use strict;
use warnings;

use Exporter qw/import/;
use phiboot;
use phibootmacros;
use phiparse;
use phiobj;
use phioptree;
use philang;

our @EXPORT =
our @EXPORT_OK = qw/ identity_null_continuation /;


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
  bind(precedence            => isget 0),
  bind(binds_rightwards      => isget 1),
  bind(with_precedence       => isset 0),
  bind(with_binds_rightwards => isset 1),

  bind(is_postfix => mcall"precedence", i_not),

  bind(binds_rightwards_of =>           # left-precedence self
    mcall"precedence", swap,            # rp l
    dup, mcall"precedence", rot3l,      # l lp rp
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


use phi applicable_ops_from_mut => pmut;
use phi applicable_ops_from => l        # lhs lop r ops
  dup, nilp,                            # lhs lop r ops nil?
  l(stack(4, 1)),                       # r
  l(i_uncons,                           # lhs lop r ops' op
    stack(0, 3, 0),                     # lhs lop r ops' op op lop
    mcall"precedence",                  # lhs lop r ops' op op lp
    swap, mcall"precedence",            # lhs lop r ops' op lp rp
    mcall"binds_rightwards_of",         # lhs lop r ops' op bind?
    l(                                  # lhs lop r ops' op
      stack(1, 0, 4),                   # lhs lop r ops' lhs op
      mcall"parser",                    # lhs lop r ops' opp
      stack(1, 3, 0),                   # lhs lop r ops' opp lop
      philang::expr_parser_for, i_eval, # lhs lop r ops' p
      rot3l, swons, swap),              # lhs lop p::r ops'
    l(drop),                            # lhs lop r ops'
    if_, applicable_ops_from_mut, i_eval),
  if_;

applicable_ops_from_mut->set(applicable_ops_from);


=head2 Special operators
These "operators" are used in two special cases. C<(> means that there is no
surrounding precedence, so all parse continuations should be considered; and
C<)> means that we're parsing a postfix value and no/few parse continuations
should be considered.
=cut

use phitype special_operator_type =>
  bind(precedence => isget 0);

use phi opener => pcons l(pcons(l(pint 0x7fff_ffff, pint 0),
                                op_precedence_type)),
                          special_operator_type;

use phi closer => pcons l(pcons(l(pint 0, pint 0), op_precedence_type)),
                        special_operator_type;


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
                               le(lit opener, philang::expr, i_eval),
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
      l(stack(2, 0)), swons,            # self [abstract swap drop]
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
use phi line_comment_parser => maybe_ seq_ str_(pstr" "), rep_ oneof_(pstr"\r\n", lit 0);
use phi line_comment_value  => pcons l(line_comment_parser), whitespace_comment_type;

use phi whitespace_literal => local_
  rep_(oneof_(pstr" \n\r\t", 1)),
  le whitespace_value, mcall"abstract";

use phi hash_line_comment_literal => local_
  str_(pstr"#"),
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
  bind(precedence         => isget 0),
  bind(fn                 => isget 1),
  bind(rhs_parser_fn      => isget 2),
  bind(prefix_value       => isget 3),  # NB: must be a syntax value
  bind(rhs                => isget 4),  # NB: transient state

  bind(with_precedence    => isset 0),
  bind(with_fn            => isset 1),
  bind(with_rhs_parser_fn => isset 2),
  bind(with_prefix_value  => isset 3),
  bind(with_rhs           => isset 4),

  bind(rhs_parser =>                    # self
    dup, mcall"rhs_parser_fn", i_eval), # parser

  bind(postfix_modify =>                # op v self -> opnode
    # Verify that we're allowed to bind at this precedence level.
    stack(0, 2, 0),                     # op v self self op
    mcall"precedence", swap,            # op v self lp self
    mcall"precedence",                  # op v self lp rp
    mcall"binds_rightwards_of",         # op v self bind?
    l(
      rot3l, drop,                      # v self
      dup, mcall"rhs",                  # v self self.rhs
      swap, mcall"fn",                  # v self.rhs f
      i_eval),                          # v'
    l(stack(3), fail_node),             # fail
    if_),

  bind(parse_continuation =>            # op self
    swap,                               # self op
    dup, mcall"precedence",
         mcall"is_postfix",             # are we being used as a postfix op?

    # If we're a postfix op, then parse an expression at our precedence and
    # store the RHS. We can complete the operation in postfix_modify.
    l(                                  # self op
      drop, dup, mcall"rhs_parser",     # self p

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


use phi unowned_suffix     => le lit closer, philang::expr, i_eval;
use phi unowned_as_postfix => l         # op lhs -> parser
  l(                                    # state e v op -> continuation
    stack(3, 3, 2, 1, 0, 0),            # state op op v e state
    mcall"scope",                       # state op op v e scope
    mcall"dialect", mcall"inflect",     # state op op v e'
    mcall"postfix_modify",              # state op e'
    stack(0, 2), mcall"scope",          # state op e' scope
    mcall"dialect", mcall"inflect",     # state op se
    mcall"parse_continuation"           # state parser
  ),                                    # op lhs next-unbound
  rot3l,                                # lhs next-unbound op
  i_cons, swons,                        # f
  unowned_suffix, swap,                 # p f
  philang::continuation_combiner, swap, # p c f
  pnil, swons, swons, swons,            # [p c f]
  phiparse::flatmap_type, swons;        # flatmap(p c f)


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
  bind(precedence         => isget 0),
  bind(op_parser          => isget 1),
  bind(rhs_parser_fn      => isget 2),
  bind(fn                 => isget 3),
  bind(with_precedence    => isset 0),
  bind(with_op_parser     => isset 1),
  bind(with_rhs_parser_fn => isset 2),
  bind(with_fn            => isset 3),

  bind(rhs_parser =>                    # lhs self
    dup, mcall"rhs_parser_fn", i_eval), # parser

  bind(parser =>                        # lhs self
    dup, mcall"op_parser",              # lhs self p
    philang::continuation_combiner,     # lhs self p c
    l(                                  # op-out lhs self
      mcall"rhs_parser",                # op-out rhs
      stack(2, 0)),                     # lhs self p c uf
    stack(5, 0, 3, 4, 1, 2, 3, 4),      # lhs self p c lhs self uf
    swons, swons,                       # lhs self p c f
    pnil, swons, swons, swons,          # lhs self [p c f]
    phiparse::flatmap_type, swons,      # lhs self flatmap
    swap, mcall"fn",                    # lhs flatmap opfn
    l(swap), swap,                      # lhs flatmap [swap] opfn
    philist::list_append, i_eval,       # lhs flatmap swap++opfn
    rot3l, i_cons,                      # flatmap lhs::(swap++opfn)
    pnil, swons, swons,                 # [flatmap f]
    phiparse::map_type, swons);         # map


=head2 Identity null continuation
You can (and probably should) specify this as the last element in an C<alt> list
of parse continuations. This enables a value to be parsed without a suffix if no
other continuation accepts the parse.
=cut

use phi identity_null_continuation => l # lhs
  l(stack(2, 0)), swons,                # [lhs swap drop]
  phiparse::none, swap,                 # p f
  pnil, swons, swons,                   # [p f]
  phiparse::map_type, swons;            # map(p f)


1;
