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
2. You might want to extend the language syntax in arbitrary ways
3. You might not want just one way to encode RTTI (runtime type information)
4. You might want to specify your own garbage collection semantics
5. You might want to specify your own compile-time optimizations

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
but the grammar does. This is managed by the parse state (see
L<phi2/dialect.pm>).


=head3 Language syntax
phi2 looks sort of like C++, but is parsed more like OCaml:

  let f = int(int a, int b)             # "int" owns the parse, binds a fn
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


=head3 Type-CTTI syntax
phi2's CTTI-wrapping CTTI (which is used for anything that functions as
typename, e.g. C<class>, C<vec2>, C<int>) defines some common syntactic behavior
that enables local variable, function, class field, and class method
definitions. The way this all works is a bit subtle.

The core problem is that C<int x> by itself is ambiguous: are we defining a
local or a function argument? Here's the difference:

  int x = 10;                           # local
  let f = int(int x) x + 1;             # fn arg

This is a challenge because C<int> interpolates the language grammar to consume
C<x>, but it doesn't know the context in which it's being used. We can't
selectively disable C<int>'s parse continuation to override the way arglists are
parsed: phi doesn't have any support for this aside from rebinding all CTTIs
within a child scope.

If we want this all to work, then, we need to conscript the dialect, which
maintains syntactic state. In particular, CTTIs talk to the dialect in order to
extend the current local scope; what we need is for some of those "local scopes"
to actually specify function arguments.


=head3 CTTI const-ness
C<int> is a parse-time constant, which means two things:

1. Constant operations against C<int> shouldn't result in runtime code
2. Constant operations against C<int> must be applied at parse-time

(2) is more important than (1), and it means that the parse results need to be
aware of their const-ness. Schedules are eagerly allocated, which means
returning them violates (1) to some degree. Schedules are also value-opaque, so
they rule out (2). We can't have every parser return a schedule.

Q: can we split the CTTI concept into "semantics" and "representation"? Then we
can externally detect repr-constants and have the semantics applied live. (Or
even better, write the interpreter and trace the bytecodes to compile stuff.)

There's some subtlety here. First, we still need to manage continuations and
scheduling -- so the evaluation function would need to be very careful to run
only relevant code. That should be fine.
=cut


1;
