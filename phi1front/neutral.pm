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


=head2 Neutral dialect
This is deliberately minimal. In this context, "neutral" means "passthrough" as
much as possible: methods and operators are basically as-offered from the
underlying CTTIs.

Userspace interpolation happens using a parse-delegating CTTI that compiles its
class and forwards dialect negotiation to it.

Most values are untyped generics, but you can bind specific names to CTTIs.

How do method signatures work? Dialects specify method-invocation syntax, so
the CTTI has to bridge the gap between dialect-provided syntax and semantics.
This raises some questions:

1. How does the CTTI even receive the method args?
2. How does it indicate the CTTI being returned?
=cut


1;
