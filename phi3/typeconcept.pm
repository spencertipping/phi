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


=head1 phi3 CTTI-driven type system
I suspect a lot of this reasoning will end up having been redundant, but that's
not obvious to me yet -- so let's work through it.

First, types (by which I mean CTTI) drive parsing. So to the extent that we lack
forward-propagated type values, we also lack forward-propagated syntax. That
still leaves some room for variation, of course. Here's a trivial example:

  let f = fn(x) array(x.type, 1) << x;

This is a template function because no CTTI has been specified. We have to treat
it that way; if we don't, we'll get incorrect GC semantics for the frame
structure. It's syntactically clear that C<f> has a template parameter simply
because the type of C<x> was left completely unspecified.


=head2 Backward constraint propagation?
I don't think we want this feature because it involves closing the set of
available CTTIs -- which means new CTTIs modify type inference simply by
existing. That's not a good side effect to have.

If we did want this feature, though, it would look like a signature-driven
intersection:

  let f = fn(x:ptr(_)) ...;
  let g = fn(x) f(x) + 1;               # x is coerced to ptr(_)

The big question here is, once we invoke C<f> on C<x> in C<let g>, does C<x>
acquire pointer semantics and parse continuations? It depends on whether we
support implicit coercion. If C<int> provides an implicit cast to C<ptr(_)>,
then it's legal to set C<g>'s C<x> to C<int> and have the coercion happen for
C<f>. Put differently, implicit coercion provides decoupling between value
types, which renders us unable to propagate information backwards.

Coercion is useful. Without it we need distinct literals for integers and reals,
among other things.
=cut


1;
