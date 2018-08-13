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
tables to convey functionality, and be confident that any frontend will adapt
those in a sane way.


=cut


1;
