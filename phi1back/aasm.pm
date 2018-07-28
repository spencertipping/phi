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


=head2 Abstract assembler
We need a way to assemble code from a parser, which means we need the assembler
object itself to be immutable. While we're at it, let's have the assembler
incrementally parse bytecodes and track the stack and frame states in terms of
CTTI.

The API is a lot like the underlying macro-assembler object we already have, but
with some important changes:

1. All bytecode-adding methods return a new assembler; there's no mutability
2. This assembler tracks CTTI instances (abstract values, if you want)
3. This assembler accepts symbolic methods and forwards those to the top CTTI
4. This assembler adds an insertion point for bytecode->bytecode parsers
5. Operations preserve structure, e.g. symbolic methods and child references

(4) is interesting because there's no requirement for you to emit phi bytecode;
you could just as easily JIT to low-level machine code, optimizing in the
process.

=cut


1;
