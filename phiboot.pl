#!/usr/bin/env perl

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


=head1 phi boot image generator
This script emits a Linux/AMD64 machine code image. We aren't linked to any
libraries (including libc), so everything bottoms out in terms of system calls
and we aren't at all portable to other POSIX systems. This is an OK place to
start the world; later on we can specify how to build a C+JIT system that
interfaces to system functions using the standard C calling convention. The
image can then port itself to this backend.

Not all backends are low-level; we just start there because it's conveniently
minimalistic. phi can also recompile itself to languages like Javascript,
Python, Ruby, Perl, OCaml, Java, etc, each of which provides some form of GC
and/or OOP. phi is set up to delegate to hosting facilities when they're
available. (Optimizing effectively for each backend is another story that I'll
address within the phi codegen libraries.)
=cut

package phi;

use v5.14;          # required for pack() endian modifiers, // operator
use strict;
use warnings;

no warnings 'void';

use Carp;

BEGIN
{
  $Carp::Verbose = 1;
  $SIG{__DIE__} = sub { Carp::confess @_ };
  push @INC, $0 =~ s/\/[^\/]+$//r;
}


=head2 General build/debug settings
We want to be able to inspect various aspects of phi's behavior quickly, so I've
built in a bunch of conditional build settings that compile debug tracers into
the image if we need them.
=cut

use constant DEBUG_TRACE_INSNS     => $ENV{PHI_DEBUG_TRACE_INSNS}     // 0;
use constant DEBUG_ILLEGAL_INSNS   => $ENV{PHI_DEBUG_ILLEGAL_INSNS}   // 1;
use constant DEBUG_MISSING_METHODS => $ENV{PHI_DEBUG_MISSING_METHODS} // 1;
use constant DEBUG_SYMBOLS         => $ENV{PHI_DEBUG_SYMBOLS};

use constant ILLEGAL_SEGFAULT_OK   => $ENV{PHI_ILLEGAL_SEGFAULT_OK} // 0;


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
=cut

use phiboot::concept;


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
=cut

use phiboot::image;


=head1 Boot OOP
OK, let's forget the pretense of having metaclasses for a minute and just talk
about what we need to encode stuff. We have a few types of objects for sure:

1. Bytecode functions (binary strings of bytecode)
2. Native functions (binary strings of machine code)
3. vtable objects
4. Interpreters (and heaps?)
5. Generic call frames
6. Base pointers
7. Here-pointers

That gets us to a self-encoding state. All of the class/protocol stuff can be
used to _generate_ vtables, but we could also use some other mechanism if we
cared to. Practically speaking, all we need are functions (bytecode things) that
allocate values.

Interestingly, we don't need primitive value types for bootup. Primitive values
reduce to stack-addressed bytecode operations (e.g. C<int.+> is effectively the
same thing as the C<int+> instruction) and don't use vtables for any type of
polymorphism. In other words, primitive types play no role in RTTI for the base
image, so they end up being fully erased.

Technically we could emit single-protocol vtables and be done for bootstrapping
purposes. That would run correctly, but that information alone isn't enough for
the resulting image to recompile itself; it would be forced to interpret those
vtables verbatim, which would make it impossible to modify any method
definitions or even resolve symbols to methods (due to protocol erasure). So we
need to emit some structural representation of the protocols we care about along
with the vtables they produce.

...and that means we'll need a couple more things for our image:

8. Class objects
9. Protocol objects
=cut

use phiboot::oop;


=head1 Boot protocols/classes
It's worth defining protocols before classes because protocols register method
indexes, which makes it possible to invoke those methods from inside C<bin>
snippets. (Having a protocol is sort of like having a C++ header file for a
class.)
=cut

use phiboot::protocols;
use phiboot::classes;


=head1 Interpreter mechanics and machine code
These implement bytecodes for the interpreter, which means we need a register
convention at this point. Let's go ahead and define that.

  %rsp = data and return stack
  %rbp = frame pointer
  %rdi = current interpreter hereptr (to instruction vector)
  %rsi = next bytecode instruction address

Like Jonesforth, each machine code primitive ends with an advancement snippet to
load the next instruction. If we're executing the first bytecode instruction
within a bytecode string, here's what our registers are pointing to:

    %rdi -----+
              |
              V
  here_marker insn0* insn1* insn2* ... insn255*
              |8bytes|8bytes|...


  %rsi -----+
            |
            V
  bytecode0 bytecode1 ... bytecodeN
  |1 byte   |

  %rax = bytecode0

Therefore, if we assume the high 56 bits of C<%rax> are zero, our advancement
looks like this:

  lodsb
  jmp *(%rdi + 8*%rax)

Instructions are individually responsible for clearing the high bits of C<%rax>.
=cut

use constant mc_next => bin"
  ac                                    # lodsb
  ffo044o307                            # jmp *(%rdi + 8*%rax)";

BEGIN { bin_macros->{N} = mc_next }


use phiboot::interpreter;


=head1 Image entry point
Not much involved here. We just need to set C<%rdi> and C<%rsi>, then invoke the
advancement macro to kick off evaluation.

GC atomicity requires the initial bytecode to create a null frame and set
C<%rbp> to point to it; otherwise the GC will segfault. In practice, this means
we get a bootup heap "runway" to allocate objects and compile GC-safe code
(since writing it by hand is tedious).
=cut

heap->mark("start_address")
  << bin("48bf") << [rdi_init => 8]     # interpreter dispatch
  << bin("48be") << [rsi_init => 8]     # initial bytecode instruction
  << bin("31o300 N");                   # zero %rax and go


our $foo_string  = (str("foo\n") >> heap)->address;
our $bar_string  = (str("bar ")  >> heap)->address;
our $bif_string  = (str("bif\n") >> heap)->address;
our $bif2_string = (str("bif\n") >> heap)->address;

heap << phi::allocation->constant(bin qq{
  # Use interpreter methods to print stuff.
  [                                     # cc
  lit64 >pack "Q>" => $foo_string       # cc foo
  dup const0 swap .[]                   # cc foo 'f
  get_interpptr .print_char             # cc foo
  swap goto                             # foo
  ]
  call                                  # foo [print "f"]

  lit64 >pack "Q>" => $bif_string       # foo bif
  lit64 >pack "Q>" => $bif2_string      # foo bif bif2
  .==                                   # foo 1

  lit64 >pack "Q>" => $foo_string       # foo 1 foo
  lit64 >pack "Q>" => $bif_string       # foo 1 foo bif
  .==                                   # foo 1 0
  [07] [goto] if call                   # foo 1

  [                                     # foo cc
  swap                                  # cc foo
  dup const1 swap .[]                   # cc foo 'o
  get_interpptr .print_char             # cc foo

  dup const2 swap .[]                   # cc foo 'o
  get_interpptr .print_char             # cc foo

  dup lit8 03 swap .[]                  # cc foo '\\n
  get_interpptr .print_char             # cc foo

  drop                                  # cc
  goto                                  #
  ]                                     # 1 then

  # NB: 07 is an invalid insn that will crash the interpreter (and 7 is
  # arbitrary, but easily visible in the error output)
  [07]                                  # 1 then else
  if                                    # then
  call                                  # [print "oo\\n"]

  # Map the initial heap
  lit32 00100000                        # 1MB
  get_interpptr .map_heap               #

  # Print "bif bar\\n" using string object methods.
  lit64 >pack "Q>" => $bar_string       # bar
  lit64 >pack "Q>" => $bif_string       # bar bif
  swap .+                               # bar++bif
  get_interpptr .print_string           #

  # Cons up a list and run some tests
  lit64 >pack"Q>", nil_instance         # nil
  const2 ::                             # 2::nil
  const1 ::                             # 1::(2::nil)

  dup .length                           # 1::(2::nil) 2
  const2 ieq                            # 1::(2::nil) 1
  [goto] [07] if call                   # 1::(2::nil)

  dup .+                                # xs=1::2::1::2::nil
  dup .length                           # xs 4
  const4 ieq                            # xs 1
  [goto] [07] if call                   # xs

  dup lit8 +3 swap .[]                  # xs xs[3]
  const2 ieq                            # xs 1
  [goto] [07] if call                   # xs

  # Exit with status 42.
  get_interpptr
  lit8 +42 swap                         # 42 interp
  .exit                                 # never returns })

  ->named('initial_bytecode');


heap->initialize(
  rdi_init        => pack(Q => heap->addressof("interpreter_dispatch_table")),
  rsi_init        => pack(Q => heap->addressof("initial_bytecode")),
  elf_start_addr  => pack(Q => heap->addressof("start_address")),
  elf_memory_size => pack(Q => heap->size),
  elf_file_size   => pack(Q => heap->size));


if (defined DEBUG_SYMBOLS and length DEBUG_SYMBOLS)
{
  my $symbols = DEBUG_SYMBOLS . ".symbols";
  my $methods = DEBUG_SYMBOLS . ".methods";
  my $macros  = DEBUG_SYMBOLS . ".macros";

  open my $fh, "> $symbols" or die "failed to open $symbols: $!";
  printf $fh "%d\t%d\t%x\t%s\n",
             $_->address,
             $_->size,
             $_->address,
             $_->name
    for sort { $a->address <=> $b->address } heap->objects;

  open $fh, "> $methods" or die "failed to open $methods: $!";
  printf $fh "%d\t.%s\n", method_lookup->{$_}, $_
    for sort keys %{+method_lookup};

  open $fh, "> $macros" or die "failed to open $macros: $!";
  printf $fh "%d\t%s\t%s\n", length bin_macros->{$_},
                             $_,
                             unpack "H*" => bin_macros->{$_}
    for sort keys %{+bin_macros};
}


print heap->compile unless caller;


1;
