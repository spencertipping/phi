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


=head1 Boot protocols/classes
There's no technical reason to define protocols before classes -- phi1 uses
symbolic method resolution -- but we'll get better warnings if we specify them
up front.
=cut

use phi1back::protocols;
use phi1front::protocols;


=head1 phi2 image generator
C<phi1> (which we're producing here) is responsible for generating C<phi2> using
phi-hosted compilation libraries. The first step is to define the compiler
backend.
=cut

use phi1back::bytestring;
use phi1back::interpreter;
use phi1back::list;
use phi1back::map;
use phi1back::stringbuffer;

use phi1back::bytecode;
use phi1back::asm;
use phi1back::struct;             # phi1 -> phi1 data layout
use phi1back::oop;                # phi1 -> phi1 classes
use phi1back::flow;               # phi2 -> phi1 frame-aware semantics


=head1 phi2 language parser
We need to define enough syntax for phi2 that we can use a subset of the
language to build up the rest of it.
=cut

use phi1front::parsers;
use phi1front::lang;
use phi1front::ctti;
use phi1front::neutral;


=head1 Reflective exports
This makes all phi1 classes self-aware, which is useful because phi2 will need
to link to them for certain cases. phi3 doesn't have this dependency, which is
how we eventually throw phi1 away.
=cut

use phi1back::reflection;       # NB: this always comes last


=head1 Image entry point
Not much involved here. We just need to set C<%rdi> and C<%rsi>, then invoke the
advancement macro to kick off evaluation.

GC atomicity requires the initial bytecode to create a null frame and set
C<%rbp> to point to it; otherwise the GC will segfault. In practice, this means
we get a bootup heap "runway" to allocate objects and compile GC-safe code
(since writing it by hand is tedious).
=cut

heap << interpreter_class->fn
     << phi::allocation->constant(
          pack QQQQQS => interpreter_class,
                         0,             # heap_base
                         0,             # heap_allocator
                         0,             # heap_limit
                         0,             # globals
                         42)            # here_marker
          ->named("interpreter_object_header")
     << phi::allocation->constant(
          pack "Q*" => @{+bytecode_allocations})
          ->named("interpreter_dispatch_table");

heap->mark("start_address")
  << bin("48bf") << [rdi_init => 8]     # interpreter dispatch
  << bin("48be") << [rsi_init => 8]     # initial bytecode instruction
  << bin("31o300 N");                   # zero %rax and go


heap << phi::allocation->constant(bin q{
  # Map the initial heap and set up the globals k/v map
  rdtsc
  lit32 00100000 i.map_heap             # 1MB heap
  rdtsc swap ineg iplus
  strmap i.globals=

  "" i.pnl_err
  "heap mapped + globals allocated" i.pnl_err

  "heap_mapped_time"    i.def
  "bytecode_start_time" i.def

  # Initialize some global bindings
  $bytecode_native_list "bytecode_natives" i.def
  $protocol_map         "protocol_map"     i.def
  $class_map            "class_map"        i.def
  $methods_by_hash      "methods_by_hash"  i.def

  $setup_struct_link_globals_fn call

  # Generate struct definitions
  $generate_structs_fn call "vtable_to_struct" i.def

  rdtsc "test_start_time" i.def

  "tests starting" i.pnl_err

  $reflection_test_fn      call  "reflection tests ok"      i.pnl_err

  $byte_string_test_fn     call  "bytestring tests ok"      i.pnl_err
  $linked_list_test_fn     call  "linked list tests ok"     i.pnl_err
  $linked_map_test_fn      call  "linked map tests ok"      i.pnl_err
  $string_buffer_test_fn   call  "string buffer tests ok"   i.pnl_err
  $macro_assembler_test_fn call  "macro assembler tests ok" i.pnl_err
  $struct_link_test_fn     call  "struct link tests ok"     i.pnl_err
  $parser_test_fn          call  "parser tests ok"          i.pnl_err

  $phi1_oop_linkage_test_fn     call "phi1 OOP linkage tests ok"     i.pnl_err
  $phi1_runtime_linkage_test_fn call "phi1 runtime linkage tests ok" i.pnl_err
  $phi1_compile_linkage_test_fn call "phi1 compile linkage tests ok" i.pnl_err
  $accessor_test_fn             call "accessor tests ok"             i.pnl_err

  rdtsc "test_end_time" i.def

  # Print some profiling data to stderr
  strbuf lit8 0a swap .append_int8                    # buf

  "phi1 rdtsc latency: " swap .append_string
    rdtsc rdtsc swap ineg iplus swap
    rdtsc rdtsc swap ineg iplus swap
    rdtsc rdtsc swap ineg iplus swap
    rdtsc rdtsc swap ineg iplus swap                  # t1 t2 t3 t4 buf

    .append_dec " " swap .append_string
    .append_dec " " swap .append_string
    .append_dec " " swap .append_string
    .append_dec
    lit8 0a         swap .append_int8                 # buf

  "phi1 mmap clocks:   " swap .append_string
    %heap_mapped_time swap .append_dec
    lit8 0a           swap .append_int8

  "phi1 test clocks:   " swap .append_string
    %test_end_time
    %test_start_time ineg iplus
                    swap .append_dec
    lit8 0a         swap .append_int8

  "phi1 compile heap:  " swap .append_string
    $heap->size     swap .append_dec
    lit8 0a         swap .append_int8

  "phi1 runtime heap:  " swap .append_string
    i.heap_usage    swap .append_dec
    lit8 0a         swap .append_int8

  .to_string i.pnl_err

  =0 i.exit })

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
  printf $fh ".%s\t%x\n", $_, defined_methods->{$_}
    for sort keys %{+defined_methods};

  open $fh, "> $macros" or die "failed to open $macros: $!";
  printf $fh "%d\t%s\t%s\n", length bin_macros->{$_},
                             $_,
                             unpack "H*" => bin_macros->{$_}
    for sort keys %{+bin_macros};
}


print heap->compile unless caller;


1;
