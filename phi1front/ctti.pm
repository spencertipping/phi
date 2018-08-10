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
=cut

package phi;

use strict;
use warnings;

no warnings 'void';


=head2 CTTI for phi2
Ultimately our goal is to build up to a full set of self-hosting CTTI instances
for phi2. We don't need to define that full set here, though; we just need to
get enough stuff off the ground that we can use the frontend syntax to get the
rest. Before I get into the details, though, let's discuss bootstrap strategy.

A few things are true at this point:

1. Anything we write in phi1 will get rewritten in the phi2 boot code
2. ...therefore, we should minimize the phi1->phi2 support
3. Any nontrivial bytecode we write by hand is GC-unsafe
4. Any composite data structures we write manually will get rewritten

...so with that in mind, let's design the set of CTTI instances required to get
phi2 off the ground.


=head3 Dialect-free CTTIs
Let's suppose we don't define dialects as objects themselves. Can we have a
CTTI-only world? There are reasons we'd want to do this, not least that it
greatly simplifies, well, everything.

First, we can get C<in python:>-style dialect markers to do the right thing by
binding to a value that modifies the scope of its parse continuation. This scope
would adapt any globals/locals/whatever to wrapped CTTIs that implemented the
right dialectal stuff; in all likelihood we'd still have some channel
negotiation like we do now.

Can parens and other syntax elements be encoded as CTTIs? In theory they're
fully erased "values" of sorts, classes with no members that can't be
runtime-instantiated.

  "(".parse_continuation(incoming_opgate)
    = v:expr(opener_opgate)
        ++ ")"
        ++ v.parse_continuation(incoming_opgate)

NB: it's a good opportunity to think about whether we really want "opgate" to be
the operative thing we're passing around between parsers. Is there any language
that demands more context than this? For example, do we have enough firepower to
parse multilayer heredocs in perl? It's possible that "opgate" makes more sense
as a dialect-specific opaque value -- and if CTTIs are literally parsers, then
we could just fold it into the parse state.


=head4 Dialect-free channel negotiation
A couple of ways we might do this. A channel negotiation protocol isn't the
worst idea and could be quite useful, but another strategy is just to have
parsers themselves define type-specific alternatives wrt inputs. Reasons to
prefer this:

1. We'll better accommodate non-text source, if we go that route
2. Dialects can then be (structural)parser->(text)parser transforms
3. Editors can interact with CTTIs the same way compilers do

My only reservation right now is that it involves some amount of type
negotiation between parsers and inputs; but that's not a bad thing to figure
out, nor is it complicated.


=head4 Parsers as interaction protocols
Whether we think about them as parsers or as generalized functions, I think
combinatory parsers do provide the right structure for specifying all
interactions. In particular, parsers provide a side channel to nondestructively
reply to failed requests (as parse failures).


=head3 Let's simplify more: scopes and opgates
There's no reason we need to have any particular information embedding within
the parse state. The lexical scope chain can be dialect-specific, as can the
opgate (established above). This eliminates capture logic from expression
parsers, and conveniently makes expression parsers themselves a dialect-specific
issue.

If we go this route we can represent a dialect as a CTTI that addresses the
target's compile-time state, also via a structural parser.

TODO: think this through; there's a lot of potential here, but I'm not yet
convinced that it's a good idea overall.

=cut


1;
