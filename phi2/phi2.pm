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


=head2 phi2 language definition
We need two things at a minimum to define phi2: a dialect and a value CTTI. phi2
(like most languages) also defines other CTTIs that play auxiliary grammar
roles, but phi2 keeps them to a minimum for simplicity.


=head3 Language syntax
phi2 looks sort of like C++, but is parsed more like OCaml:

  let f = int (int a, int b)            # "int" owns the parse, binds a fn
  (                                     # NB: parens instead of braces
    int     x = 10;                     # int == int64_t
    int.ptr y = x.addr;                 # int.ptr == int*
    let     z = y + 18;                 # let == auto

    x += 1;                             # vars are mutable

    # NB: semicolons are infix operators: do left then right, return right
    z > 10
      ? return 5
      : ( while x                       # while binds to a custom CTTI
            ( x -= 1; x.to_string.pnl );
          return y.deref ? 6 : 7 )
  );

You can define new CTTIs in phi2, which behave as you'd expect:

  let vec2 = class();                   # class() returns a new reftype CTTI
  real vec2::x;                         # define instance fields (the
  real vec2::y;                         #   "hereptr class" prefix is implied)

  vec2::+ = vec2 (vec2 self, vec2 rhs) vec2(self.x + rhs.x, self.y + rhs.y);

TODO: name mangling for type overloads

Q: how are CTTIs aware of the C<class::member> syntax, and how do they address
those classes? This needs to be standardized.
=cut


1;
