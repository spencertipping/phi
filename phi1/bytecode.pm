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


=head2 Bytecode debugging
It's useful to be able to print the source for a bytecode as a string. This
involves parsing stack instructions, but that isn't too difficult. We just need
to know how many literal bytes follow each one.
=cut

use constant phi_insn_follow_bytes => int_kvmap
  insn_index('lit8')  => 1,
  insn_index('lit16') => 2,
  insn_index('lit32') => 4,
  insn_index('lit64') => 8,

  insn_index('sget') => 1,
  insn_index('sset') => 1,

  map +(insn_index $_ => 0),
      qw/ call
          call_native
          if
          syscall

          get_frameptr
          set_frameptr
          get_interpptr
          set_interpptr
          get_stackptr
          set_stackptr
          get_insnptr
          goto

          drop
          swap

          m8get m8set
          m16get m16set
          m32get m32set
          m64get m64set
          memset memcpy

          iplus
          itimes
          idivmod
          ishl
          isar
          ishr
          iand
          ior
          ixor
          ilt
          ieq
          iinv
          ineg
          bswap16
          bswap32
          bswap64 /;

use constant phi_insn_names => int_kvmap
  map +(insn_index($_) => str($_)), keys %{+insns};


use phi::fn bytecode_to_string => bin q{# bytecode cc
  strbuf                                # b cc s
  sget02 .size                          # b cc s n
  sget03 .data                          # b cc s n d
  [                                     # b cc s n d i loop
    sget03 sget02 ilt                   # b cc s n d i loop i<n?
    [ # Prepend the absolute hex address of each instruction. This is just d+i
      # as a number.
      sget02 sget02 iplus               # b cc s n d i loop &insn
      dup sget06 =8_ .append_hex        # b cc s n d i loop &insn s
      ": "_ .append_string              # b cc s n d i loop &insn s
      drop m8get                        # b cc s n d i loop insn

      # Retrieve the instruction's name and append that to the buffer.
      dup $phi_insn_names .{}           # b cc s n d i loop insn iname
      sget06 .append_string             # b cc s n d i loop insn s
      " "_ .append_string               # b cc s n d i loop insn s

      sget03 =1 iplus sset03            # b cc s n d i+1 loop insn s

      # Now add following bytes if we have them. We can cheat a little here and
      # read 64 bits little-endian, then print only as many as we care about.
      # This is totally awful and prints in the wrong endianness, but it works.
      swap $phi_insn_follow_bytes .{}   # b cc s n d i+1 loop s fbs
      sget04 sget04 iplus m64get        # b cc s n d i+1 loop s fbs data
      sget01 sget03 .append_hex drop    # b cc s n d i+1 loop s fbs

      sget03 iplus sset02               # b cc s n d i' loop s
      $nl_string _ .append_string drop  # b cc s n d i' loop
      dup goto ]                        # ->loop(i')
    [ drop drop drop drop               # b cc s
      .to_string sset01 goto ]          # s.to_string
    if goto ]                           # b cc s n d loop
  =0_ dup goto                          # ->loop(0) };


1;
