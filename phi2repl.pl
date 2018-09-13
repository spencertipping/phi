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

use v5.14;          # required for pack() endian modifiers, // operator
use strict;
use warnings;

no warnings 'void';

BEGIN
{
  push @INC, $0 =~ s/\/[^\/]+$//r;
}

use constant DEBUG_TRACE_ALL_METHODS => $ENV{PHI_DEBUG_TRACE_ALL_METHODS} // 0;
use constant DEBUG_TRACE_INSNS       => $ENV{PHI_DEBUG_TRACE_INSNS}       // 0;
use constant DEBUG_ILLEGAL_INSNS     => $ENV{PHI_DEBUG_ILLEGAL_INSNS}     // 1;
use constant DEBUG_MISSING_METHODS   => $ENV{PHI_DEBUG_MISSING_METHODS}   // 1;
use constant DEBUG_SYMBOLS           => $ENV{PHI_DEBUG_SYMBOLS};

use constant PROFILE_RECEIVERS       => $ENV{PHI_PROFILE_RECEIVERS} // 0;
use constant PROFILE_METHODS         => $ENV{PHI_PROFILE_METHODS}   // 0;
use constant PROFILE_FNS             => $ENV{PHI_PROFILE_FNS}       // 0;
use constant PROFILE_INSNS           => $ENV{PHI_PROFILE_INSNS}     // 0;
use constant PROFILE_MACROS          => $ENV{PHI_PROFILE_MACROS}    // 0;
use constant PROFILE_MLOOKUP         => $ENV{PHI_PROFILE_MLOOKUP}   // 0;

use phi0::theworks;

allocate_interpreter(heap);
allocate_machine_bootcode(heap);

use constant repl_bytecode => q{
  lit32 01000000 i.map_heap
  >genconst_generator_code
  # phi2_repl .loop };

print genelf repl_bytecode;
