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


=head1 Boot interpreter
The goal here is to be able to write objects and phi1 bytecode in Perl, and
generate an image that can then independently execute this stuff. To get there
we'll need a heap simulator (C<phi0::image>), a bytecode interpreter, and a way
to generate classes that implement polymorphic dispatch.
=cut

use phi0::image;                # perl -> phi/ELF memory allocations
use phi0::interpreter;          # phi1 bytecode -> x86-64 machine code
use phi0::oop;                  # perl -> phi1 classes
use phi0::test;                 # "use phi::testfn"


=head1 phi1 bytecode/object compiler
Now we need to take our bytecode layer and build up to a real C-style syntax,
phi2. The first step to get there is to define a standard library that
replicates the functionality we have in phi0.
=cut

use phi1::protocols;              # forward method references

use phi1::array;                  # i8i, i64i, i1d, i8d, i64d
use phi1::interpreter;            # "i", .heap_allocate, .pnl, etc
use phi1::cons;                   # ::, .head, .tail
use phi1::stringbuffer;           # strbuf

use phi1::test;                   # unit test definitions
use phi1::profile;                # method/general profiling

use phi1::parsers;                # low-level string parsing
use phi1::asm;                    # phi1 -> phi1 bytecode
use phi1::oop;                    # phi1 -> phi1 classes


=head1 phi2 compiler and language frontend
At this point we effectively have phi0 available in phi1, so we can start to
define the syntax and compiler logic that implements the phi2 frontend.
Internally it will go through phi1 abstractions like C<asm> to generate
bytecode that can interoperate with everything that exists already.

NB: nothing defined at this point is GC-atomic, so garbage collection is
impossible. phi2 will need to emit GC-safe code, at which point we'll rewrite
everything to get the first world we can collect. This means that for now we
need to be careful about not allocating tons of memory; we're on a finite
runway.
=cut

use phi2::parsers;                # library of common syntax elements
use phi2::ir;                     # pre-bytecode intermediate representation
use phi2::dialect;                # frontend parse states
use phi2::schedule;               # parsed expressions
use phi2::ctti;                   # frontend syntax <-> backend IR bridge
use phi2::phi2;                   # phi2 language definition

#use phi2::repl;


=head1 ELF generator
This module collects all of the heap allocations and provides C<phi::genelf>,
which returns the ELF binary as a string that you can write into a file.
=cut

use phi0::genelf;


1;
