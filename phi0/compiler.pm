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


=head2 Classes, protocols, and compilation
Stuff I'm currently assuming:

1. Protocols describe interfaces to classes
2. Classes own vtable generation and contain method sets and protocol lists
3. Classes are implementations, so they manage structs
4. Some classes are fixed-size, others aren't (do we want allocation protos?)
5. Class objects generate compilers that write into macro assemblers
6. vtables are prepended by C<polymorphic> ... but this is wrong
7. C<polymorphic> converts protocols to classes ... probably wrong

Here's what's broken about this picture:

1. You can't prepend stuff to a class without telling the class about it
2. C<polymorphic> won't know how much memory to reserve
3. We aren't left in a good position to have functional classes
4. It's unclear how to inline stuff, e.g. for int primitive ops

Some things that should be true:

1. C<int.+> should emit C<iplus> by itself
2. C<poly_baseptr.method> should emit C<dup m64get mcall...>
3. C<mono_baseptr.method> should emit C<lit64(vtable) mcall...>
4. It should be possible to have a pointer/vtable pair as a single logical value
5. C<poly_hereptr.method> should emit C<dup -2 + m16get - + dup m64get mcall...>
6. We need to know at parse time whether something is mono/poly
7. Meta-protocols are first-class objects that dictate interface strategy
8. Monomorphic class instances should have no stored vtable
9. Small polymorphic instances should have small/externally stored vtables

=cut


1;
