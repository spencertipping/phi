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

no warnings 'portable';


=head1 Classes
First a few definitions:

1. An object is something you hold a pointer to.
2. A class is an object that owns the functionality of other objects.
3. A metaclass is a class whose instances are classes.

phi doesn't support object inheritance natively. This is all managed by
metaclasses, which are responsible for assembling the method lookup functions
used by classes. This is an important point because all objects in phi support a
handful of methods like C<gc_mark> and C<class>, and these methods are managed
by the C<phi_gc_class> metaclass rather than inherited from a base as they would
be in most languages.
=cut


1;
