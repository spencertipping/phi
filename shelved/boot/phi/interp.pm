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


=head2 Bytecode interpreter
A simple direct-threaded bytecode interpreter based on C<lods> for advancement.
Bytecode implementations are stored as offsets from the runtime object referred
to by C<%rdi>, which is a here-pointer to the dispatch table to save space.

Many bytecodes refer to addressed operands, which are typically encoded in
subsequent bytes and decoded on the fly.

At a high level, we have these operations:

  00 - 0f: invalid instructions
  10 - 1f: initialize to constant
  20 - 2f: interpreter operations
  30 - 3f: memory operations, if memory model = flat
  40 - 4f: integer operations
  50 - 5f: float operations
  60 - ff: reserved


=head3 Instruction addressing, generally speaking


=head3 Invalid/reserved instructions
Instructions 0x00-0x0f are all invalid, which is useful for debugging. Each one
will print a message to STDERR and exit nonzero. 0x60-0xff are the same, but I
may allocate them for something in the future.
=cut

use phi::use 'phi::insn_reserved' => sub
{
  eval sprintf q{use phi::amd64native invalid_0x%02x => 0x%x => asm
                   ->debug_print("reserved (illegal) instruction 0x%02x\n", 2)
                   ->exit_constant(1);
                 1}, $_, $_, $_ or die $@
       for @_;
  ();
};

use phi::insn_reserved 0x00 .. 0x0f;
use phi::insn_reserved 0x60 .. 0xff;


=head3 Initialize-to-constant instructions

=cut



1;
