=head1 phi prefix/infix/postfix operator support
Alright, let's define some actual syntax now that C<philang> works. Parse
continuations are technically managed by values rather than types, but in
practice most abstract values will delegate to a type to keep it simple. (Rarely
do specifics of a value impact the set of operations you can perform on it.)
Those types will in turn use a library like this to manage the operators they
provide.

=head2 Operator precedence
Before I get into the details, let's talk about some high-level stuff. First,
most values support operator precedence by looking at the surrounding operator
and removing lower-precedence stuff from the continuation:

  int.parse_continuation(self, "+") = [[* ...], [/ ...], [** ...] ...]

=head2 Unowned (universal) operators
Second, many languages like Haskell and OCaml support operators-as-constructors,
e.g. C<::> for cons. Given that values-via-types are the sole drivers for parse
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
so we have to flatmap to keep the parse going. From C<3>'s point of view,
C<::...> is a single parse element.

From C<::>'s point of view, C<3> passes control first via a closer-precedence
expr; then it flatmaps into the postfix-modify/parse-continuation parser.

So equationally:

  postfix_case(v) = let ep     = expr(closer) in
                    let e next = op v op v e
                                 .postfix_modify()
                                 .parse_continuation() in
                    [ep (v c -> c) next flatmap.]

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
parse continuation is prepended with a rule that eats things until the next
newline.

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

Grouping elements can function as postfix operators, which is useful for
defining things like function calls or array slicing:

  [1, 2, 3]         # standalone list
  x[1, 2, 3]        # some kind of slicing operation

  (1 + 2)           # standalone expr
  f(1 + 2)          # function call

If you want a grouping construct to work this way, you can bind a C<postfix_fn>
with the same signature as C<postfix_modify>. The idea is to treat the entire
grouped quantity as a postfix modifier, which is exactly how it would work.

Put differently, the grouping construct has the ability to postfix-operate
independently of the way its computed value would. (TODO)
=cut

use phitype grouping_type =>
  bind(closer          => isget 0),
  bind(with_closer     => isset 0),
  bind(inner           => isget 1),
  bind(with_inner      => isset 1),
  bind(postfix_fn      => isget 2),
  bind(with_postfix_fn => isset 2),

  bind(postfix_modify =>                # op v self
    ),

  bind(parse_continuation =>            # op vself self
    swap, drop,                         # op self
    dup, mcall"inner",                  # op self inner-parser
    swap, mcall"closer",                # op inner-parser closer-parser
    pnil, swons, swons,                 # op [inner closer]
    phiparse::seq, swons,               # op [[inner closer] seq.]
    l(head), swap,                      # op [head] [[inner closer] seq.]
    phiparse::pmap, swons, swons,       # op vparser
    swap,                               # vparser op
    philang::expr_parser_for, i_eval);  # expr-parser

use phitype paren_type =>
  bind(parse_continuation =>            # op vself self
    drop, drop,
    lit opener, philang::expr, i_eval,  # op inner
    pstr")", phiparse::str, swons,      # op inner closer
    pnil, swons, swons,                 # op [inner closer]
    phiparse::seq, swons,               # op [[inner closer] seq.]
    l(head), swap,                      # op [head] [[inner closer] seq.]
    phiparse::pmap, swons, swons,       # op vparser
    swap,                               # vparser op
    philang::expr_parser_for, i_eval);  # expr-parser

use phi paren_value => pcons l(pnil), paren_type;

use phi paren_literal => l
  l(drop, paren_value),                 # [f]
  pcons(pstr "(", phiparse::str),       # [p]
  phiparse::pmap, i_eval;               # [[f] [p] map.]


=head2 Comments and whitespace
We might as well jump right in on this, starting with whitespace. Whitespace can
take two roles, prefix and postfix. If it's prefix, it will be asked for a parse
continuation; if postfix, it will be asked to modify a value.
=cut

use phitype whitespace_type =>
  bind(postfix_modify     => drop, swap, drop),             # op v self -> v
  bind(with_continuation  => drop),                         # v self -> v

  bind(parse_continuation =>            # op vself self
    drop, swap,                         # vself op
    dup, lit closer, i_symeq,           # vself op postfix?

    # postfix case: parse nothing since we're carrying the value to our left
    l(                                  # vself op
      drop,                             # vself
      l(swap, drop),                    # vself [swap drop]
      swons,                            # [vself swap drop]
      phiparse::none,                   # f none
      phiparse::pmap, swons, swons),    # [f none map.]

    # prefix case: parse an expression and return it
    l(                                  # vself op
      swap, drop,                       # op
      philang::expr, i_eval),           # expr(op)

    if_);

use phi whitespace_value => pcons pnil, whitespace_type;

use phi whitespace_literal => l
  l(drop, whitespace_value),
  l(l(pstr " \n\r\t", lit pint 1, phiparse::oneof, i_eval),
    phiparse::rep, i_eval),
  phiparse::pmap, i_eval;


=head2 Example unowned op: C<*>
Let's go ahead and test this model before building out the full generalized
operator precedence stuff.
=cut

use phi timesop_suffix => le pstr"*", philang::expr, i_eval;

use phitype timesop_type =>
  bind(val                => isget 0, mcall"val"),
  bind(with_val           => isset 0),
  bind(postfix_modify     => mcall"with_val", swap, drop),

  bind(parse_continuation =>            # op vself self
    drop, swap,                         # vself op
    dup, lit closer, i_symeq,           # are we being used as a postfix op?

    # If we're a postfix op, then parse nothing successfully
    l(                                  # vself op
      drop,                             # vself
      l(swap, drop), swons,             # [vself swap drop]
      phiparse::none,
      phiparse::pmap, swons, swons),    # [[vself swap drop] none map.]

    # ...otherwise, parse a normal expression with the surrounding op
    # precedence; then operate once we have it
    l(                                  # vself op
      philang::expr, i_eval,            # vself ep
      swap,                             # ep vself
      l(                                # v vself
        mcall"val", swap, dup,          # vself.val v v
        mcall"val", rot3l, i_times,     # v v.val*vself.val
        swap, mcall"with_val"),         # ep vself unbound-f
      swons,                            # ep f
      swap, phiparse::pmap,             # f ep map
      swons, swons),                    # [f ep map.]

    if_);

use phi timesop_value => pcons l(pnil), timesop_type;

use phi timesop_literal => l
  l(drop, timesop_value),
  l(pstr"*", phiparse::str, i_eval),
  phiparse::pmap, i_eval;


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


use phi plus_suffix => le pstr"+", philang::expr, i_eval;

use phitype int_type =>
  bind(val      => isget 0),
  bind(with_val => isset 0),

  # Reject all postfix modifications; ints aren't operators
  bind(postfix_modify => drop, drop, abstract_fail),

  bind(parse_continuation =>            # op vself self
    drop,
    pnil,                               # op self []

    # none case (must be last in the list, so first consed)
    swap, dup, rot3r,                   # op self [cases] self
    l(                                  # v self -> self
      swap, drop),                      # op self [cases] self [...]
    swons,                              # op self [cases] f
    phiparse::none,
    phiparse::pmap, swons, swons,       # op self [cases] none-case

    i_cons,                             # op self [cases']

    # unowned op case
    swap, dup, rot3r,                   # op self [cases] self
    l(                                  # e v 'op -> continuation
      i_eval,                           # e v op
      stack(3, 2, 1, 0, 1, 0),          # op v op v e
      mcall"postfix_modify",            # op v e'
      mcall"parse_continuation"         # k
    ),                                  # op self [cases] self next-unbound
    stack(0, 4), philang::quote, i_eval,# op self [cases] self next-unbound 'op
    i_cons, swons,                      # op self [cases] next
    lit closer, philang::expr, i_eval,  # op self [cases] next ep
    swap,                               # op self [cases] ep next
    l(                                  # v c
      swap, drop),                      # op self [cases] ep next (v c -> c)
    swap,                               # op self [cases] ep (v c -> c) next
    phiparse::flatmap, swons, swons,
                              swons,    # op self [cases] p

    i_cons,                             # op self [cases']

    # plus case
    swap, dup, rot3r,                   # op self [cases] self
    l(                                  # v self -> v'
      mcall"val",                       # v self.val
      swap, dup, mcall"val",            # self.val v v.val
      rot3l, i_plus, swap,              # self.val+v.val v
      mcall"with_val"),                 # op self [cases] self [+...]
    swons,                              # op self [cases] [self +...]
    l(tail, head),
    pcons(l(pcons(pstr"+", phiparse::str),
            plus_suffix),
          phiparse::seq),
    phiparse::pmap, swons, swons,       # op self [cases] f p
    phiparse::pmap, swons, swons,       # op self [cases] +-case

    i_cons,                             # op self [cases']

    phiparse::alt, swons,               # op self [[cases] alt.]
    rot3r, drop, drop);                 # [[cases] alt.]

use phi int_literal => l
  l(list_int, i_eval, pnil, swons, int_type, swons),
  l(l(pstr join('', 0..9), lit 1, phiparse::oneof, i_eval),
    phiparse::rep, i_eval),
  phiparse::pmap, i_eval;


1;
