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

  int.parse_continuation(self, "+") = [[* ...], [/ ...], [** ...] ...]


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
  30.parse_continuation(self, op) = alt("+" expr,
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

  30.parse_continuation(self, op) =
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
                    let e next = op v op v e
                                 .postfix_modify()
                                 .parse_continuation() in
                    [ep (v c -> c) next flatmap.]

Because C<3> has only one parse continuation, C<::> needs to parse the RHS at
C<::> precedence _before_ kicking over to the surrounding op continuation. So
C<expr(closer)> parses C<:: expr("::")>.


=head2 Values, syntax frontends, and operator precedence
The goal is to end up with a language that can accept existing grammars as
syntax, so we need a way to fully overload things like operator precedence,
parse continuations, and unowned operators. (Grouping and whitespace operators
are trivial and covered below.)

This overloading is lexically scoped and is implemented by creating a new scope
link that wraps every atom with a syntax frontend that customizes the operators
and precedence appropriately. The scope linkage also binds any unowned operators
required to parse the language.

Frontends go beyond just specifying syntax: they also provide an important
semantic barrier between different pieces of code. For example, if you write a
function with Python-style syntax, you're probably making Python-style
assumptions about how those values will behave. This could mean things like
reference-counted GC, for instance. You need to have delimiters that indicate
when different ambient semantics are required for things like this; you can't
meaningfully mix values from different semantic namespaces.

=head3 Modeling existing languages
Let's suppose we want to parse Python. We only need a single type for all Python
values because Python's values all occupy the same role in the language grammar.
Most of the work goes into special constructs like C<if>, C<class>, etc.

In other words, phi's types often end up corresponding to other languages'
grammar rules. This should make it possible to simulate macro systems and other
higher-order grammar behavior.


=head2 Multi-channel precedence rejection
C<3.parse_continuation(self, op)> implicitly rejects some of its alternatives
based on operator precedence, but of course it doesn't necessarily understand
the precedence of every ad-hoc postfix operator. Instead, those postfix
operators look at the surrounding precedence and make a call inside
C<postfix_modify>, selectively emitting fail values.

This mechanism arises more often than you might think: it's the only reason the
C<;> operator works at all, for example. It's also why C<;> is postfix rather
than infix, which makes it possible to parse C-style grammars.


=head2 Whitespace and comments
Yep, you guessed it: whitespace elements are just regular values. Space, tab,
CR, and LF are bound to identity transformers that can function as passthrough
prefix/postfix operators, and the line comment marker C<#> is a value whose
parse continuation eats things until the next newline.

This, of course, means you can do some interesting things:

  let '// = '# in
  // this is now a line comment


=head2 Grouping
It isn't immediately obvious how parens should work given that precedence gets
passed into the C<expr> parser -- but it's simpler than you might think. C<(> is
a value whose continuation is C<expr(nil)> followed by C<)>; then the parse
continuation of all of that comes from the expr:

  "(".parse_continuation(self, op)
    = (x=expr(nil) ++ ")") >>= x.parse_continuation(op)

List brackets and other things work the same way. Delimited things are at
liberty to create subscopes that include operator bindings for commas, etc, if
they want special treatment for these things -- or more conventionally, the
parse continuation could simply look for those delimiters.
=cut


package phiops;
use strict;
use warnings;

use phiboot;
use phibootmacros;
use phiparse;
use phiobj;
use philang;


=head2 Special operators
These "operators" are used in two special cases. C<(> means that there is no
surrounding precedence, so all parse continuations should be considered; and
C<)> means that we're parsing a postfix value and no/few parse continuations
should be considered.
=cut

use phi opener => psym"(";
use phi closer => psym")";


=head2 Meta-abstracts
Basically, things that aren't actual values. These are used to modify the parse
in situations where we can't directly fail.
=cut

use phitype abstract_fail_type =>
  bind(parse_continuation => drop, drop, drop, phiparse::fail);

use phi abstract_fail => pcons pnil, abstract_fail_type;


=head2 Grouping
These functions give you a way to create values that will group things. All you
need to do is make sure that the closing element, e.g. a closing paren, doesn't
parse as a value; otherwise phi will do that instead of closing the group. You
can work around this in a ham-fisted way by creating a new binding that maps the
closing element to C<abstract_fail>, although this carries some overhead because
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

  bind(parse_continuation =>            # op vself self
    swap, drop, dup,                    # op self self
    stack(0, 2), lit closer, i_symeq,   # op self self postfix?
    l(mcall"postfix_inner"),
    l(mcall"inner"),
    if_,                                # op self inner-parser
    swap, mcall"closer",                # op inner-parser closer-parser
    pnil, swons, swons,                 # op [inner closer]
    phiparse::seq, swons,               # op [[inner closer] seq.]
    l(head),                            # op [[inner closer] seq.] [head]
    phiparse::pmap, swons, swons,       # op vparser
    swap,                               # vparser op
    philang::expr_parser_for, i_eval);  # expr-parser


# This paren value will reject a parse as a postfix operator because that's a
# higher-level concern than just getting grouping to work.
use phi paren_value => pcons l(str_(pstr")"),
                               le(lit opener, philang::expr, i_eval),
                               phiparse::fail),
                             grouping_type;

use phi paren_literal => local_ str_(pstr"("), paren_value;


=head2 Comments and whitespace
We might as well jump right in on this, starting with whitespace. Whitespace can
take two roles, prefix and postfix. If it's prefix, it will be asked for a parse
continuation; if postfix, it will be asked to modify a value.
=cut

use phitype whitespace_comment_type =>
  bind(parser      => isget 0),
  bind(with_parser => isset 0),

  bind(postfix_modify     => drop, swap, drop),             # op v self -> v
  bind(parse_continuation =>            # op vself self
    rot3r, swap,                        # self vself op
    dup, lit closer, i_symeq,           # self vself op postfix?

    # postfix case: delegate to the parser to consume input (if appropriate,
    # e.g. for line comments); then return vself
    l(                                  # self vself op
      drop, l(swap, drop), swons,       # self [vself swap drop]
      swap, mcall"parser", swap,        # p f
      phiparse::pmap, swons, swons),    # [p f map.]

    # prefix case: parse an expression and return it
    l(                                  # self vself op
      stack(3, 0),                      # op
      philang::expr, i_eval),           # expr(op)

    if_);

use phi line_comment_parser  => rep_ oneof_(pstr"\n", lit 0);

use phi whitespace_value     => pcons l(phiparse::none),      whitespace_comment_type;
use phi line_comment_value   => pcons l(line_comment_parser), whitespace_comment_type;

use phi whitespace_literal   => local_ rep_(oneof_(pstr " \n\r\t", lit 1)), whitespace_value;
use phi line_comment_literal => local_ str_(pstr "#"), line_comment_value;


=head2 Precedence objects
These manage precedence and associativity.

NB: precedence values are priorities: low values = high binding order. So zero
is the maximum precedence.

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


use phitype op_list_type =>
  bind(ops      => isget 0),
  bind(with_ops => isset 0),

  bind(applicable_ops =>                # left-precedence self
    # TODO: list filter function
    );


=head2 Unowned operators
These temporarily store the RHS and then delegate to a combiner function whose
signature is C<< lhs rhs -> v' >>. Unowned ops can be used as prefix values;
that behavior is fully delegated.
=cut

use phitype unowned_op_type =>
  bind(fn                => isget 0),
  bind(rhs_parser        => isget 1),
  bind(prefix_value      => isget 2),
  bind(rhs               => isget 3),
  bind(with_fn           => isset 0),
  bind(with_rhs_parser   => isset 1),
  bind(with_prefix_value => isset 2),
  bind(with_rhs          => isset 3),

  bind(postfix_modify =>                # op v self
    rot3l, drop,                        # v self
    dup, mcall"rhs",                    # v self self.rhs
    swap, mcall"fn",                    # v self.rhs f
    i_eval),                            # v'

  bind(parse_continuation =>            # op vself self
    rot3r, drop,                        # self op
    dup, lit closer, i_symeq,           # are we being used as a postfix op?

    # If we're a postfix op, then parse an expression at our precedence and
    # store the RHS. We can complete the operation in postfix_modify.
    l(                                  # self op
      drop, dup, mcall"rhs_parser",     # self p
      swap, l(mcall"with_rhs"), swons,  # p [self mcall"with_rhs"]
      phiparse::pmap, swons, swons),    # [p [self mcall"with_rhs"] map.]

    # ...otherwise, pretend we're the prefix value and hand the parse over.
    l(                                  # self op
      swap, mcall"prefix_value", dup,   # op v v
      mcall"parse_continuation"),       # p

    if_);


use phi unowned_suffix     => le lit closer, philang::expr, i_eval;
use phi unowned_as_postfix => l         # op lhs -> parser
  l(                                    # e v 'op -> continuation
    i_eval,                             # e v op
    stack(3, 2, 1, 0, 0),               # op op v e
    mcall"postfix_modify", dup,         # op e' e'
    mcall"parse_continuation"           # k
  ),                                    # op lhs next-unbound
  rot3l, philang::quote, i_eval,        # lhs next-unbound 'op
  i_cons, swons,                        # f
  unowned_suffix, swap,                 # p f
  philang::continuation_combiner, swap, # p c f
  phiparse::flatmap, swons, swons,
                            swons;      # [p c f flatmap.]


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
  bind(op_sym          => isget 0),
  bind(op_parser       => isget 1),
  bind(rhs_parser      => isget 2),
  bind(fn              => isget 3),
  bind(with_op_sym     => isset 0),
  bind(with_op_parser  => isset 1),
  bind(with_rhs_parser => isset 2),
  bind(with_fn         => isset 3),

  bind(parser =>                        # lhs self
    dup,       mcall"op_parser",        # lhs self opp
    swap, dup, mcall"rhs_parser",       # lhs opp self rhs
    swap,      mcall"fn",               # lhs opp rhs fn
    stack(4, 3, 0, 1, 2),               # opp rhs fn lhs
    i_cons,                             # opp rhs [lhs fn...]
    lit i_eval, i_cons,                 # opp rhs [. lhs fn...]
    l(tail, head), i_cons,              # opp rhs [[tail head] . lhs fn...]
    rot3r, pnil, swons, swons,          # f [opp rhs]
    phiparse::seq, swons, swap,         # [[opp rhs] seq.] f
    phiparse::pmap, swons, swons);      # [[[opp rhs] seq.] f map.]


=head2 Identity null continuation
You can (and probably should) specify this as the last element in an C<alt> list
of parse continuations. This enables a value to be parsed without a suffix if no
other continuation accepts the parse.
=cut

use phi identity_null_continuation => l # lhs
  l(swap, drop), swons,                 # [lhs swap drop]
  phiparse::none, swap,                 # p f
  phiparse::pmap, swons, swons;         # [p f map.]


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


=head3 Ops for testing: C<+> and C<*>
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

use phi plus_op => pcons l(psym"+",
                           str_(pstr"+"),
                           le(lit psym"+", philang::expr, i_eval),
                           plus_fn),
                         owned_op_type;

use phi times_op => pcons l(times_fn,
                            le(lit psym"*", philang::expr, i_eval),
                            phiparse::fail,
                            pnil),
                          unowned_op_type;

use phi timesop_literal => local_ str_(pstr "*"), times_op;


use phitype int_type =>
  bind(val      => isget 0),
  bind(with_val => isset 0),

  # Reject all postfix modifications; ints aren't operators
  bind(postfix_modify => drop, drop, drop, abstract_fail),

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

    # plus case
    swap, dup, rot3r,                   # op self [cases] self
    plus_op, mcall"parser",             # op self [cases] p
    i_cons,                             # op self [cases']

    phiparse::alt, swons,               # op self [[cases] alt.]
    rot3r, drop, drop);                 # [[cases] alt.]

int_type_mut->set(int_type);

use phi int_literal => l
  rep_ oneof_(pstr join('', 0..9), lit 1),
  l(list_int, i_eval, pnil, swons, int_type, swons),
  phiparse::pmap, i_eval;


1;
