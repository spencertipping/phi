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


=head3 Invalid instructions
Instructions 0x00-0x0f are all invalid, which is useful for debugging. Each one
will print a message to STDERR and exit nonzero.
=cut

BEGIN
{
  eval sprintf "use phi::amd64native invalid_%x => 0x%x => asm
                  ->debug_print('invalid instruction 0x%x\n', 2)
                  ->exit_constant(1);
                1", $_, $_, $_ or die $@
       for 0x00 .. 0x0f;
}


1;
