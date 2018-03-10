=head1 phi builtin type/abstract definitions
Alright, let's define some actual types now that C<philang> works. Parse
continuations are technically managed by values rather than types, but in
practice most abstract values will delegate to a type to keep it simple. (Rarely
do specifics of a value impact the set of operations you can perform on it.)

Before I get into the details, let's talk about some high-level stuff. First,
most values support operator precedence by looking at the surrounding operator
and removing lower-precedence stuff from the continuation:

  int.parse_continuation("+") = [[* ...], [/ ...], [** ...] ...]

It's worth having some functions to handle this for us so we can specify the
operator lists declaratively.

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
scoping object, which is likely to become an open-ended namespace of awful that
you could drive a truck through. Let's make the rule that lexical scopes can't
store arbitrary data just to avoid this outcome.

(3) and (4) differ only in how they're scoped: (3) allows you to add operators
on a lexically-scoped basis, whereas (4) is dynamically scoped. I prefer (3) in
this case simply because it greatly reduces the chances of breaking stuff
through monkey-patching. There's also nothing wrong with having both mechanisms
available.

=cut

package phitypes;
use strict;
use warnings;

use phiboot;
use phibootmacros;
use phiparse;
use phiobj;
use philang;


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
    l(i_uncons,                         # n cs' c
      lit 48, i_neg, i_plus, rot3l,     # cs' c-48 n
      lit 10, i_times, i_plus, swap,    # (n*10+(c-48)) cs'
      list_int1_mut, i_eval),
    if_;

list_int1_mut->set(list_int1);

use phi list_int => l lit 0, swap, list_int1, i_eval;


use phitype int_type =>
  bind(val      => isget 0),
  bind(with_val => isset 0),

  bind(with_continuation =>             # v self
    swap, dup, nilp,                    # self v vnil?
      l(drop),                          # self
      l(tail, head, mcall"val",         # self n
        swap, dup, mcall"val",          # n self self-n
        rot3l, i_plus,                  # self n'
        swap, mcall"with_val"),         # self'
    if_),

  bind(parse_continuation =>            # op self
    drop, drop,
    pcons(l(pcons(l(pcons(pstr "+", phiparse::str),
                    l(pstr "+", philang::expr, i_eval, i_eval)),
              phiparse::seq),
            phiparse::none),
          phiparse::alt));

use phi int_literal => l
  l(list_int, i_eval, pnil, swons, int_type, swons),
  l(l(pstr join('', 0..9), lit 1, phiparse::oneof, i_eval),
    phiparse::rep, i_eval),
  phiparse::pmap, i_eval;


1;
