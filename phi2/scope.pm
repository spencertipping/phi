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


=head2 Generic multichannel scope objects
Most languages bind variables in some way or another, so it's worth defining a
reusable scope setup. I'm going to keep this simple for now since all of this
gets rewritten once we have phi2.

Luckily there isn't much to scopes to begin with. A scope is just a mapping from
logical name to a pair of C<ctti, anf_symbol>. Scopes have layers to them
corresponding to block structure, but this layering tracks syntax closely; we
don't have a lot of machinery surrounding lexical capture because that's handled
by dialects if they support it.

Scope objects are manipulated by parsers, so they're immutable.

=cut


1;
