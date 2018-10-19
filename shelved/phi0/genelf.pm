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

no warnings 'void';


=head2 Image generator
We already emitted the ELF headers in L<phi0/image.pm>, so really we're just
linking stuff from here. This involves a few steps:

1. Allocate the interpreter object
2. Write some machine code to initialize C<%rsi> and C<%rdi>
3. Fill in addresses for global objects
=cut


sub allocate_interpreter($)
{
  shift() << interpreter_class->fn
          << phi::allocation->constant(
               pack QQQQQS => interpreter_class,
                              0,        # heap_base
                              0,        # heap_allocator
                              0,        # heap_limit
                              0,        # globals
                              42)       # here_marker
               ->named("interpreter_object_header")
          << phi::allocation->constant(
               pack "Q*" => @{+bytecode_allocations})
               ->named("interpreter_dispatch_table");
}


sub allocate_machine_bootcode($)
{
  shift->mark("start_address")
    << bin("48bf") << [rdi_init => 8]   # interpreter dispatch
    << bin("48be") << [rsi_init => 8]   # initial bytecode instruction
    << bin("31o300 N");                 # clear %rax and jump into bytecode
}


sub genelf($)
{
  my $boot_bytecode = shift;
  heap << phi::allocation->constant(bin $boot_bytecode)
                         ->named("initial_bytecode");

  heap->initialize(
    rdi_init        => pack(Q => heap->addressof("interpreter_dispatch_table")),
    rsi_init        => pack(Q => heap->addressof("initial_bytecode")),
    elf_start_addr  => pack(Q => heap->addressof("start_address")),
    elf_memory_size => pack(Q => heap->size),
    elf_file_size   => pack(Q => heap->size));

  heap->compile;
}


1;
