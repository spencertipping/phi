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
use bytes;


=head1 OOP and method calls
In phi1, all OOP happens at runtime using polymorphic dispatch. The calling
convention is simple enough; each object begins with a hereptr to a function
that resolves method hashes to implementations. That is:

  (g64 object) :: hereptr (int -> hereptr (args... offset base -> ...))

Function addresses are always at least 4-byte aligned, and method hashes always
have their least-significant bit set. This means that, as quadwords, the two
value spaces are disjoint, which makes it possible to use C<mfnd> on a packed
array to fetch the function for a given method hash.

C<ptr> and C<hereptr> differ slightly in how we invoke methods:

  (ptr).method(...)     -> [ <ptr> l8(0) swap l64(hash) sget(1) call call ]
  (hereptr).method(...) -> [ <ptr> unh4       l64(hash) sget(1) call call ]

=cut


1;
