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


=head2 phi compiler
L<phi0/classes.pm> defines the underlying machinery we use to generate bytecode
and other compiled outputs; now we need to put that stuff to work. Let's kick
this off with a simple class like a cons cell:

  protocol cons_proto<int>
  {
    int        head(self*);
    cons<int> *tail(self*);
  }

  class poly_cons = vtable_polymorphic<cons_proto<int>>;

  class cons<int> : cons_proto<int>
  {
    struct
    {
      int                head;          # offset = 0
      baseptr<poly_cons> tail;          # offset = 8
    };

    int head(self*)
    [                                   # self cc
      sget01 m64get sset01 goto
    ]

    baseptr<...> tail(self*)
    [                                   # self cc
      sget01 const8 iplus m64get sset01 goto
    ]
  }

C<poly_cons> is equivalent to C<cons>, but prepends a vtable here-pointer...?

FIXME: I don't like this. Why are we externally prepending a vtable pointer when
we could have C<polymorphic> be a class->class transform instead?
C<vtable_polymorphic> seems like it should be a protocol and not a class.
=cut


1;
