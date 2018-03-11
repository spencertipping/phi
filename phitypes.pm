=head1 phi builtin type/abstract definitions
Alright, let's define some actual types now that C<philang> works. Parse
continuations are technically managed by values rather than types, but in
practice most abstract values will delegate to a type to keep it simple. (Rarely
do specifics of a value impact the set of operations you can perform on it.)

=head2 Operator precedence
Before I get into the details, let's talk about some high-level stuff. First,
most values support operator precedence by looking at the surrounding operator
and removing lower-precedence stuff from the continuation:

  int.parse_continuation(self, "+") = [[* ...], [/ ...], [** ...] ...]

It's worth having some functions to handle this for us so we can specify the
operator lists declaratively.

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
TODO

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

package phitypes;
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


=head2 Parentheses
TODO: convert this to a more general API
=cut

use phitype paren_type =>
  bind(with_continuation =>             # v self
    drop),                              # v

  bind(parse_continuation =>            # op vself self
    drop, drop,
    pnil, philang::expr, i_eval,        # op inner
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
  bind(postfix_modify     =>            # op v self
    rot3l, drop,                        # v self
    mcall"val",                         # v self.val
    swap, dup, mcall"val",              # self.val v v.val
    rot3l, i_times, swap,               # self.val*v.val v
    mcall"with_val"),                   # v'

  bind(parse_continuation =>            # op vself self
    drop, swap,                         # vself op
    dup, lit closer, i_symeq,           # are we being used as a postfix op?
    l(                                  # vself op
      swap,                             # op vself
      l(                                # v vself
        mcall"with_val"),               # op vself [...]
      swons,                            # op f
      timesop_suffix,                   # op f p
      phiparse::pmap, swons, swons,     # op [f p map.]
      swap, philang::expr_parser_for,
      i_eval),                          # expr-parser
    l(drop, drop, abstract_fail),
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
    # This is the most subtle thing going on. The idea is that we have something
    # like "3 :: nil", where "::" is a value. TODO: explain the rest
    swap, dup, rot3r,                   # op self [cases] self
    l(                                  # postfixval self 'op -> continuation
      i_eval,                           # postfixval self op
      #stack(3, 2, 1, 0, 0),             # op op self postfixval
      stack(3, 2, 1, 0),                # op self postfixval
      mcall"postfix_modify"             # v.postfix_modify(self, op)
      #mcall"postfix_modify",            # op v.postfix_modify(self, op)
      #swap, dup, rot3l,                 # op op vp
      #dup,                              # op op vp vp
      #mcall"parse_continuation", swap,  # k op
      #philang::expr_parser_for, i_eval  # expr
    ),                                  # op self [cases] self [...]
    stack(0, 4), philang::quote, i_eval,# op self [cases] self [...] 'op
    i_cons, swons,                      # op self [cases] p
    lit closer, philang::expr, i_eval,  # expr-parser
    phiparse::pmap, swons, swons,       # op self [cases] [f p map.]

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
