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
The general idea behind functional programming is that processes are pure and
deterministic: if I have a value, I can repeat an operation against it and end
up in the same place. Sequence arguments are a problem in this sense because in
almost every case they simply return C<self> -- but we can't constant-fold it:
we need to preserve a dependency that in reality doesn't exist.

We do this using a fictitious modification node, which semantically is an
identity function that algebras agree to treat as an opaque transformation of
some kind. Internally, a fictitious modification requires any algebra to break
an abstract-simulation chain: the contract is that we have no way to determine
the return value at compile time.

Q: do we need this, or can we delegate to native code and agree to treat that as
opaque?

TODO: C<mXget> and C<mXset> seem like they should be implemented using some sort
of sequence argument, and it also seems like we should delegate to native code
for these things. This results in a better match between offered and available
backend semantics.

=cut


1;
