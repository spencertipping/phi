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


=head2 Compiled bytecode
We need two classes to encode this stuff:

  struct ref                            # 16 bytes
  {
    hereptr vtable;
    uint32  offset;
    uint32  pointer_type;
  };

  struct bytecode                       # size = 18 + 16*nrefs + codesize
  {
    hereptr        vtable;              # offset = 0
    uint32         nrefs;               # offset = 8
    uint32         codesize;            # offset = 12
    ref[nrefs]     refs;                # offset = 16
    here_marker;                        # offset = 16 + 16*nrefs
    byte[codesize] data;                # offset = 18 + 16*nrefs
  };

Refs always refer to full 64-bit quantities in code. They manage endian
conversion on get/set, which for now is hard-coded to assume little-endian
native encoding.
=cut


use constant ref_class => phi::class->new('ref',
  ref_protocol)

  ->def(
    offset       => bin"swap =8      iplus m32get swap goto",
    pointer_type => bin"swap lit8+12 iplus m32get swap goto",

    get => bin"                         # buf self cc
      sget 01 .offset                   # buf self cc offset
      sget 03 .data iplus               # buf self cc &ref
      m64get bswap64                    # buf self cc rval
      sset 02 sset 00 goto              # rval",

    set => bin"                         # x buf self cc
      sget 03 bswap64                   # x buf self cc rval
      sget 02 .offset                   # x buf self cc rval offset
      sget 04 .data iplus               # x buf self cc rval &ref
      m64set sset 02 drop drop goto     #");


# NB: as a list, bytecode is a series of refs; as a string-ish thing it's a
# bunch of code.
use constant bytecode_class => phi::class->new('bytecode',
  list_protocol,
  byte_string_protocol,
  fn_protocol,
  here_protocol,
  bytecode_protocol)

  ->def(
    here => bin"swap .data swap goto",
    size => bin"swap lit8+12 iplus m32get swap goto",
    data => bin"                        # self cc
      sget 01 .length =4     ishl       # self cc n<<4
      sget 02 iplus lit8+18 iplus       # self cc &data
      sset 01 goto                      # &data",

    # Living dangerously
    call        => bin"swap .here goto",
    goto        => bin"drop .here goto",
    call_native => bin"swap .here call_native",

    length => bin"swap =8 iplus m32get swap goto",
    "[]"   => bin"                      # i self cc
      sget 02 =4     ishl               # i self cc i<<4
      sget 02 =16     iplus iplus       # i self cc &refs[i]
      sset 02 sset 00 goto              # &refs[i]",

    reduce => bin q{                    # x0 f self cc
      swap dup .length =0               # x0 f cc self l i
      [ sget02 sget02 ilt               # x0 f cc self l i loop i<l?
        dup sget04 .[]                  # x0 f cc self l i loop self[i]
        sget07 sget07 call              # x0 f cc self l i loop x0' exit?
        [ sset06 drop drop drop drop    # x0' f cc
          sset00 swap goto ]            # x0'
        [ sset06 swap =1 iplus swap     # x0' f cc self l i+1 loop
          dup goto ]                    # ->loop
        if goto ]                       # x0 f cc self l i loop
      dup goto                          # ->loop });


sub refless_bytecode($)
{
  phi::allocation->constant(
    pack 'QLLSa*' => bytecode_class,
                     0,
                     length $_[0],
                     18,
                     $_[0]) >> heap;
}


1;
