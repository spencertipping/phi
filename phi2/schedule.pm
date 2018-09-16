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


=head2 Expression-level scheduling
Not all control flow is managed at the side-effect level: short-circuit and
ternary operators are examples that modify the basic-block chain in IR terms.
Put differently, not all expressions can be represented using a single C<ir_bb>.
We need to manage linked structures of C<ir_bb>s as a single unit so we can
return that structure from the dialect's C<expression> parser.

TODO: go through an example that involves branching
=cut


1;
