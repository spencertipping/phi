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


=head2 Abstracts
An abstract is the stuff we know about a runtime value at compile-time. This
always includes the value's CTTI, but can be more descriptive -- up to
specifying the value concretely if we know enough to do that. Abstracts are the
focal point of phi, making all of the interesting decisions about parsing,
compilation, and compile-time evaluation.


=head3 Polymorphism and higher-order structure
The set of abstracts is open-ended: any library can define a new one that
changes phi's compile-time structure for its data types. This is how floating
point numbers are defined, for example. We need to not only interoperate with
existing abstracts, but also agree about when and how handoff between abstract
values happens.

This higher-order interaction protocol has a few parameters:

1. A dialect that provides specific syntactic elements (more below)
2. An interpreter that specifies aspects of the compilation target
3. A shared set of named bindings stored in a canonical format

(2) is rarely used by libraries; it's more relevant for binding to existing code
that's running natively (as opposed to phi bytecode). Over time the goal is to
reduce this set to zero.


=head3 Dialect interaction
A dialect is a set of syntactic invariants that apply lexically. Abstracts
themselves aren't typically dialect-polymorphic; for instance, Perl and Python
could both use the same dictionary abstract with different type parameters. The
difference between C<{}> (perl) and C<{}> (python) is the mapping between the
C<{}> literal syntax and abstracts.

Abstracts delegate to dialect for certain types of parsing if they want to use
idiomatic syntax. For example, an abstract integer could request to parse any of
C<+ - * / ... & | ^> as binary operators, and the dialect would take care of
parsing the RHS at the appropriate level of precedence. (The dialect can also
apply some artistic license about which operators are which; C<&> can become
C<land> in OCaml, for instance.)

Maybe a better way to explain all of this is that dialects convert syntactic
insertion points to semantic ones. Dialects don't operate alone in this
equation, though; the other part of the picture involves bindings.


=head3 Bindings
Why are bindings their own concept as opposed to being a variable part of a
dialect? Because I'm willing to die on this hill and am a glutton for punishment
I suppose. But more than that, dialects owning bindings creates some major
problems from a semantic point of view: we'd be unable to emulate Java, for
instance, which specifies that local scopes pin values into the GC live set,
even if those values aren't subsequently used. C++ uses scopes to schedule
destructors.

Another issue is that we don't want dialects to function as data structures; we
want things like bound variables to be reduced to some common form so we can
change dialects and maintain access to things like local variables and instance
methods. I should be able to switch to Ruby inside a C++ class and use C<@x> to
refer to C<double x>, for example. Ruby's C<attr_reader> should be able to
address Java fields.

=cut


1;
