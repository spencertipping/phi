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

You can define new CTTIs in phi2, typically using a metaclass like C<class>:

  let vec2 = class.new;                 # a new reftype CTTI
  real vec2::x;                         # define instance fields (the
  real vec2::y;                         #   "hereptr class" prefix is implied)

C<real vec2::x> is syntax that's recognized by the phi2 type meta-CTTI, and is
equivalent to C<vec2.field("x", real)>. A similar mechanism is used to define
methods:

  vec2 vec2::+(vec2 rhs)                # define a virtual method
    vec2.new(self.x + rhs.x, self.y + rhs.y);

This is a little more involved because C<+> will be joined with the argument
types to support type-based overloads:

  vec2.method("+", vec2(vec2 self, vec2 rhs)
                     vec2.new(self.x + rhs.x, self.y + rhs.y));


=head3 Meta-CTTI internals
Clearly most of the interesting stuff here is happening inside type CTTIs;
that's where we get rewriting and class construction. Let's talk a bit about how
this all works.

First, C<class> is a meta-CTTI: it is both an instance of and produces instances
of CTTIs. The CTTIs it returns are reference types, meaning that they are
pointers to values that have a C<hereptr class> prefix field. This makes it
possible to define virtual methods.

C<class.new> is a regular CTTI: it's an instance of CTTI, but instances of it
aren't themselves CTTIs (unless you basically reimplement CTTI's fields and
methods). Because C<class> is defined as a reference type constructor, the CTTI
you get from it is configured in a few ways:

1. It contains C<hereptr class> as mentioned above
2. It automatically defines accessors when you create fields
3. It automatically implements copying GC
4. Its dispatch function is indirect, so you can modify the class at runtime

(4) means that you can define new virtual methods and they'll immediately become
available on existing instances. This class modification also makes it possible
to profile the method call distribution and reorder the lookup table to place
frequently-called methods first.
=cut


1;
