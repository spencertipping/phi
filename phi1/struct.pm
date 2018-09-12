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

# TODO: remove support for array fields
# TODO: back structs into indirect arrays of field objects (no linkage)

package phi;

use strict;
use warnings;


=head2 Structs
phi structs are basically exactly the same thing as C structs, except that they
support things like herepointers. They're backed by ordered indirect i64 maps
and provide named lookups using hashing.
=cut


1;
