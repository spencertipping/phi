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


=head1 Booting phi
phi is the most self-referential project I've ever worked on, so it's not
remotely obvious to me where it bottoms out into a bootstrap script. Let's
figure this out.

First, the main goal of phi is to provide metaclass-aware OOP for a wide variety
of different runtimes with maximal code/data portability. So the language has a
few constraints:

1. Almost everything is written in a semantically consistent bytecode
2. Data layout is done with pre-idiom "logical structs", which are then compiled
3. Structs and classes are portable object instances
4. The interpreter implementation is replaceable and can be GC'd

(GC isn't independently a big deal; objects trace and collect themselves on
backends that don't provide memory management.)

(3) and (4) conspire to make our lives interesting. Jointly, they imply that
every aspect of our bytecode implementations and base objects/structs be
described in terms of objects. Combined with the other constraints, then, we
have three fixed point elements:

1. Classes are and implement objects
2. Bytecode functions are and implement classes
3. Interpreters are instances of and implement classes

phi is designed to implement a minimal solution to these fixed points while
providing a runtime you'd want to use for real problems.


=head1 Boot image
The ELF file we generate contains a heap image with a header that we'll later
GC, so we need to model its allocation process. There are a few challenges:

1. We don't always know the exact size of an object up-front
2. We also don't know the exact value of an object
3. Objects can have circular references

I want something that detects some failure modes, like I've modified an object
beyond its allocation boundaries or forgotten to initialize some memory. This
means we'll end up with two objects, a heap and an allocation. They work like
this:

  $heap << $an_allocation;              # inline it
  $heap << pack(Q => 5);                # allocate a constant

Allocations exist in one of two states, uninitialized and initialized. This
distinction is used because due to (3) we'll end up with a delay between
allocation and memory setup.


=head1 Interpreter mechanics and machine code
These implement bytecodes for the interpreter, which means we need a register
convention at this point. Let's go ahead and define that.

  %rsp = data and return stack
  %rbp = frame pointer
  %rdi = current interpreter hereptr (to instruction vector)
  %rsi = next bytecode instruction address


=head1 Boot OOP
OK, let's forget the pretense of having metaclasses for a minute and just talk
about what we need to encode stuff. We have a few types of objects for sure:

1. Bytecode functions (binary strings of bytecode)
2. Native functions (binary strings of machine code)
3. Interpreters (and heaps?)
4. Generic call frames
5. Base pointers
6. Here-pointers

Interestingly, we don't need primitive value types for bootup. Primitive values
reduce to stack-addressed bytecode operations (e.g. C<int.+> is effectively the
same thing as the C<int+> instruction) and don't use vtables for any type of
polymorphism. In other words, primitive types play no role in RTTI for the base
image, so they end up being fully erased.
=cut

use phi0::image;                # perl -> phi memory allocations
use phi0::interpreter;
use phi0::oop;                  # perl -> phi classes
use phi0::test;                 # provides "use phi::testfn"


=head1 Boot protocols/classes
There's no technical reason to define protocols before classes -- phi1 uses
symbolic method resolution -- but we'll get better warnings if we specify them
up front.
=cut

use phi1::protocols;


=head1 phi2 image generator
C<phi1> (which we're producing here) is responsible for generating C<phi2> using
phi-hosted compilation libraries. The first step is to define the compiler
backend.
=cut

use phi1::bytestring;
use phi1::interpreter;
use phi1::list;
use phi1::map;
use phi1::stringbuffer;

use phi1::test;
use phi1::profile;

use phi1::parsers;                # required for phi1::oop

use phi1::bytecode;
use phi1::asm;
use phi1::struct;                 # phi1 -> phi1 data layout
use phi1::oop;                    # phi1 -> phi1 classes
use phi1::ctti;


=head1 phi2 language
We need to define enough syntax for phi2 that we can use a subset of the
language to build up the rest of it.
=cut

use phi2::ctti;
use phi2::anf;
use phi2::scope;
use phi2::dialect;
use phi2::parsers;
use phi2::methods;
use phi2::phi1;
use phi2::let;
use phi2::fn;
use phi2::phi2;
use phi2::repl;


=head1 Reflective exports
This makes all phi1 classes self-aware, which is useful because phi2 will need
to link to them for certain cases. phi3 doesn't have this dependency, which is
how we eventually throw phi1 away.
=cut

use phi1::reflection;           # NB: this always comes last


=head1 phi3 language
L<phi2::phi2> defines the C<use phi2::val> construct, which enables us to write
code in phi2 that interoperates with phi1. At this point the rest of the boot
code is written in phi2.
=cut

use phi3::str;


=head1 Image entry point
Not much involved here. We just need to set C<%rdi> and C<%rsi>, then invoke the
advancement macro to kick off evaluation.

GC atomicity requires the initial bytecode to create a null frame and set
C<%rbp> to point to it; otherwise the GC will segfault. In practice, this means
we get a bootup heap "runway" to allocate objects and compile GC-safe code
(since writing it by hand is tedious).
=cut

use phi0::genelf;


1;
