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
  $SIG{__WARN__} = $SIG{__DIE__} = sub { Carp::confess @_ };
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


=head2 Generate phi1
L<phi0/theworks.pm> imports all of the phi0+phi1 components required to generate
the image. From there, we allocate the initial objects into our simulated heap
and generate an ELF image using our boot bytecode.
=cut

use phi0::theworks;

allocate_interpreter(heap);
allocate_machine_bootcode(heap);

use constant initial_bytecode => q{
  # Map the initial heap and set up the globals k/v map
  [ lit32 00800000 i.map_heap goto ] "mmap heap" test
  [ strmap i.globals=         goto ] "allocate globals map" test

  # Generate genconsts
  [ >genconst_generator_code
    goto ] "genconst code" test

  # Initialize some global bindings
  [ $bytecode_native_list "bytecode_natives" i.def
    $protocol_map         "protocol_map"     i.def
    $class_map            "class_map"        i.def
    $methods_by_hash      "methods_by_hash"  i.def goto ]
  "setup global bindings" test

  >test_runner_code

  # Print some profiling data to stderr
  strbuf =10_ .append_int8

  "phi1 compile heap: "_ .append_string
    $heap->size _ .append_dec
    lit8 0a     _ .append_int8

  "phi1 runtime heap: "_ .append_string
    i.heap_usage _ .append_dec
    " / "_         .append_string
    i.heap_size  _ .append_dec
    " ["         _ .append_string
    i.heap_usage =100 itimes
      i.heap_size idivmod drop
                 _ .append_dec
    "%]"         _ .append_string
    lit8 0a      _ .append_int8

  .to_string =2 i.print_string_fd
  =0 i.exit };

print genelf initial_bytecode unless caller;


=head2 Debugging outputs
Metadata about the phi1 image. This is useful for taking hex addresses and
figuring out which function they came from, or knowing which method was missing
from an object.
=cut

if (defined DEBUG_SYMBOLS)
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
  printf $fh ".%s\t%016x\n", $_, defined_methods->{$_}
    for sort keys %{+defined_methods};

  open $fh, "> $macros" or die "failed to open $macros: $!";
  printf $fh "%d\t%s\t%s\n", length bin_macros->{$_},
                             $_,
                             unpack "H*" => bin_macros->{$_}
    for sort keys %{+bin_macros};
}


1;
