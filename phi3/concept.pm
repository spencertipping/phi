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


=head1 phi3 compiler
phi0/1/2 have been a haphazard bootstrapping strategy to build up to phi3, which
is more aligned with the concept I've had in mind for this project. phi3 is the
first layer that supports garbage collection and JIT compilation, and it's also
the first point where code we write will work forever (as opposed to being
thrown away in subsequent bootstrapping).

Structurally, phi3 intersects the semantics of phi2 and itself. It's sort of
like using a non-standards-compliant C compiler to write a standards-compliant
one: you want to use compliant code to do it, so you're living in the
intersection between standards compliance and whatever your bootstrap compiler
supports.

phi2 is the last layer of stuff we support in the phi0-based bootstrap compiler;
there's no C<use phi3::val>. We use phi2 to compile our phi3 source and emit a
new ELF that complies with the phi3 language standard.

With that in mind, let's talk about how this language works.


=head2 Abstract bytecode optimization
phi1 is minimalistic because that simplifies bytecode analysis. There are ways
you can still make life difficult -- in particular, doing things like addressing
the stack as memory -- but for the most part the bytecode is easy to parse and
interpret. This means it's also easy to optimize: we can step through with an
abstract interpreter and emit condensed versions of things.

The condensed output isn't necessarily minimal in bytecode terms. Instead, it's
written to facilitate compilation via a register-based backend.


=head2 CTTI method mapping
phi2 uses string templates to map methods into type-dispatch space: C<contains?>
becomes C<contains?:int> for integer collections, for example. This creates a
lot of problems, including:

1. CTTIs must have names
2. Automatic upcasting is impossible
3. Parameterized types are difficult to generate
4. It's inefficient
5. There's no way to have an individually-templated method

Basically, it's awful; we need a proper type inference mechanism that doesn't
just lean on the IR to do the work for us. I think we should be able to erase
semantic type information when targeting the IR -- although we'll obviously want
to keep enough for GC and optimization purposes.

Q: what's an ideal solution here? There are two questions: one, how do we do
forward propagation; and two, do we support fixed-point definition for, e.g., HM
type inference?


=head2 Intermediate representation
As of this writing, the phi2 ANF layer is utter crap. It's horrifically slow, it
has inconsistencies around how it handles frame-class struct linking, and its
model of continuations is just plain wrong. phi3 does this completely
differently, which means phi2 and phi3 syntax elements are incompatible (even
though they present compatible frontends).

An ideal IR should have:

1. Numbered slots rather than names: full name erasure and no gensyms
2. Nullary continuations for imperative branching (C<goto> instead of C<call>)
3. Arbitrarily-sized frame slots addressed by pointer, not by value
4. C<defset> and C<refset> as bitsets, not linked maps
5. SSA, possibly? I'm not sure we do anything that requires it yet.

NB: we can get C<alloca()> support from the unused part of the heap, rather than
trying to merge it with the stack. I'm not yet sure how those values should be
GC'd.

...really, the IR should be register-based and address frame memory as local
space. It can back into concatenative code as micro operations, but
concatenative arguments are addressed as fixed offsets into the frame and
there's a definite load/process/store cycle:

  get_frameptr =36 iplus m32get         # load operand 1
  get_frameptr =32 iplus m32get         # load operand 2
    iplus                               # do the thing
  get_frameptr =36 m32set               # store result

I think we want to always use concatenative as the backend, rather than
optimizing the IR. This should give us something like SSA for free since each
stack value is potentially unique -- although modeling the memory may be a
little tricky. I'm not sure yet. (The upside to concatenative-only is that we
have a really small set of operations to think about, so JIT backends would be
very simple.)

=cut


1;
