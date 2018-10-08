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
7. Timelines can be parameterized on unspecified quantities

(5) and (6) are important for algebraic analysis. Basically, the idea is that we
want timelines to be a lossless semantic encoding of the source they came from.
This means no type or intent erasure: if we have a situation where an abstract
defines two methods that compile to the same underlying function, we should
still be able to know which one was called.

(7) is questionable...


=head3 Sequence arguments


=cut


1;
