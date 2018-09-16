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


=head2 phi2 core CTTIs
CTTI (compile-time type information) is everything we know about a value at
compile time. phi has this abstraction for a few reasons:

1. phi supports value types that behave like objects
2. In general, the language grammar is parameterized by a value's type
3. You might not want just one way to encode RTTI (runtime type information)

In other words, the "virtual-ness" of methods and other such things is a
first-class idea in phi. phi1 uses a default convention, but you're free to
define your own to coexist with or replace it.


=head3 CTTIs, functionally speaking
CTTIs are the bridge between phi's frontend syntax and backend IR. For example,
if we write something like C<int x = 10;>, the parse is entirely owned by CTTIs:

1. C<int> (whose CTTI is an instance of "type") parses C<x> and binds it
2. C<x> (whose CTTI is "int") parses C<= 10> and emits an C<ir_val>
3. The RHS (right-hand side) of C<=> parses C<10> and allocates a val slot
4. C<x = ...> returns a CTTI (int), which parses C<; ...>

If we wanted to commit to a C-style syntax forever then that would be the whole
story: CTTIs own the grammar. In practice, though, there's some cooperation
between CTTIs and the parse state to provide support for dialects: regions of
code in which certain syntactic rules are used. CTTIs themselves don't change,
but the grammar does. This is managed by the parse state.


=head3 Dialects
TODO


=cut


1;
