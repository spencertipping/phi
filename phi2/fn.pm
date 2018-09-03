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


=head2 C<fn> non-lambda functions
These kick off a new root ANF node, parse the body, and drop a reference to the
compiled bytecode. Technically, functions are objects with a C<call> method that
specifies their arity and argument types. Arguments types must match in order
for the function to operate correctly (for GC reasons).
=cut

use phi::genconst fn_ctti => bin q{
  ctti "fn"_ .defname

  # TODO
  };


1;
