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

=cut


1;
