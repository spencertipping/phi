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


=head2 Timelines
Code doesn't directly map to an evaluation schedule; there are too many degrees
of freedom to commit to a single ordering, and not all programs that are parsed
are intended to be evaluated at all. Instead, the parse output is something more
generally informative: a set of functional dependencies that describe the
calculations required to execute the code. The degree of sequencing is encoded
by the dialect and the abstracts, and collectively the calculation set is a
graph called a timeline.

Functionally speaking, timelines are phi's intermediate representation and form
a variation of CPS that specifies which expressions are temporally independent.
More broadly:

1. Timelines can be trivially converted to bytecode
2. Bytecode produced by a timeline links back to the timeline object
3. Backend-specific JIT compilers use timelines as input
4. Bytecode can be trivially converted to a GC-unsafe timeline
5. Timelines preserve/store CTTI
6. Timelines preserve/store all method calls, including monomorphic ones
7. Timelines can generate and splice more timelines as first-class values

(5) and (6) are important for algebraic analysis. Basically, the idea is that we
want timelines to be a lossless semantic encoding of the source they came from.
This means no type or intent erasure: if we have a situation where an abstract
defines two methods that compile to the same underlying function, we should
still be able to know which one was called.

(7) is how control flow works, but more generally it specifies that code isn't
confined to a static compilation model.


=head3 Sequence arguments and side-effect domains
Like Haskell, phi relies on functional dependencies for scheduling and sequence
points. Every side-effecting function takes a "sequence argument" and returns
another one; in practice, these sequence arguments represent side-effect
domains. These domains sometimes overlap; for example, if you have two open
files you may or may not care about synchronization between them. You can
indicate this by either serializing or forking the sequence argument.

Structurally, sequence arguments are objects that provide methods to implement
their side effects. Each method returns a new, fictitiously-modified object that
depends on the first side effect being complete, thereby establishing an
evaluation ordering.

Sequence arguments aren't the only objects that behave this way; any mutable
data structure needs to provide some synchronization mechanism. This mechanism
doesn't need to involve the receiver, and sometimes it won't by design. Instead,
it should reflect a degree of serialization we require from the program -- and
that's a question of semantics.


=head3 Fictitious modification
phi's job is to see through various sorts of duplicity to unify values that are
in fact the same. Any decent optimization algebra has the potential to detect a
fictitious dependency and eliminate it, which would break things like sequence
arguments and cause all sorts of problems.

To work around this, phi provides a timeline variant for native code and a
guarantee surrounding it: native code isn't subjected to timeline-level
optimizations. Sequence arguments are both inputs and outputs of these native
code implementations, which breaks abstract dependency chains and prevents alias
analysis from modifying the ordering.

=cut


1;
