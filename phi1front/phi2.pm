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


=head2 phi2 language frontend
Here's where things start to get real. We use the machinery defined in phi1 to
parse phi2, which provides a proper infix language with parse-time CTTI
propagation, expression type inference, lexical closure, etc. phi2 is a simple
language that provides enough grammar extensibility to host anything else.

Let's talk a little about how the parsers work internally.


=head3 CTTI interop and parsing
CTTIs themselves don't need to provide parsers in order to participate in a
grammar. Most languages don't have computed grammars in the first place, so
there isn't really a precedent for classes to be syntax-aware; but beyond that,
we can get a lot of mileage from a CTTI's set of offered methods. If some of
those methods look like operators, then those can be integrated into the syntax
as such (and at the appropriate precedence for the dialect in question).

Put differently, CTTIs are at liberty to rely solely on the method/virtual
tables to convey functionality and they can be confident that any frontend will
present those in a sane way.


=head3 Assembling expressions
Most frontends (including phi2) convert expressions to something close to ANF to
flow-assemble them. So we'd have something like this:

  # if we compile this:
  x + y.bar(bif + baz, bok.bork())

  # ...we'd compile it like this:
  let tmp1 = bif + baz in
  let tmp2 = bok.bork() in
  let tmp3 = y.bar(tmp1, tmp2) in
  let tmp4 = x + tmp3 in
  tmp4

C<let>-bindings don't dynamically extend the scope; rather, phi2 finds them up
front and builds a frame class for the full set of referenced variables. That
makes it important for C<tmpN> variables to be unique gensyms in their own
namespace.

Because scopes aren't always defined in parse order, we use parsers to stage
flow assembler links. That is, parsers return objects that do two things:

1. Return a map of C<< name -> CTTI >> entries to describe their definition set
2. Return a set of C<name> entries to describe their reference set
3. Add new links to an existing flow assembler

NB: these links are right-inclusive, just like the tail of a cons cell. There is
no tail-tail or "rest of the list" otherwise.
=cut

use constant phi2_link_protocol => phi::protocol->new('phi2_link',
  qw/ defset
      refset
      link_onto /);


=head3 Parse state
Frontend parse states are slightly nontrivial because they have a bit of
negotiation to do. In particular, not all frontends support:

1. Lexical scoping and lambda-capture
2. Dynamic scoping
3. Prefix-free variable addressing
4. Variable addressing at all

We care about this stuff because CTTIs can operate as parsers to provide
computed grammars, so they need a standard way to interoperate with the
frontend's parse state.

TODO: design the negotiation protocol
=cut


1;
