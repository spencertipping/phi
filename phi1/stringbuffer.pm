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

# TODO: abbreviate append_X method names

package phi;

use strict;
use warnings;


=head2 String buffer
This is derived from an indirect int8 array; we just add more methods.
=cut

use phi::class string_buffer =>
  i8_indirect_array_class->protocols,
  string_buffer_protocol,

  i8_indirect_array_class->methods,

  clear => bin q{                     # self cc
    =0 sget02 .rewind_to drop goto    # self },

  headroom => bin"                    # self cc
    sget 01 .capacity                 # self cc c
    sget 02 .size ineg iplus          # self cc c-s
    sset 01 goto                      # c-s",

  append_int8 => bin q{               # x self cc
    =1     sget03 sget03              # x self cc 1 x self
    .append_int drop sset01 swap goto # self },

  append_int16 => bin q{              # x self cc
    =2     sget03 sget03              # x self cc 2 x self
    .append_int drop sset01 swap goto # self },

  append_int32 => bin q{              # x self cc
    =4     sget03 sget03              # x self cc 4 x self
    .append_int drop sset01 swap goto # self },

  append_int64 => bin q{              # x self cc
    =8     sget03 sget03              # x self cc 8 x self
    .append_int drop sset01 swap goto # self },

  append_int => bin q{                # size_bytes x self cc
    # Chop the quad into two halves. We're appending it in native-endianness,
    # so we want 0x8877665544332211 to be appended as 11 22 33 44 55 66 77 88.
    #
    # Truncate the integer's length to the desired number of bytes.

    sget02 =32 ishr                   # size q self cc q>>32
    sget03 =32 ishl                   # size q self cc q>>32 q<<32
    sget05 ior                        # size q self cc q2 q1
    $byte_string_class                # size q self cc q2 q1 vt
    get_stackptr                      # size q self cc q2 q1 vt &s
    sget05 .append_string             # size q self cc q2 q1 vt self
    drop drop drop drop sset01        # size cc self
    sset01 goto                       # self },

  append_hex => bin q{                # n pairs self cc
    =0                                # n pairs self cc i
    [                                 # n pairs self cc i loop
      sget04 sget02 ineg iplus dup    # n pairs self cc i loop pairs-i pairs-i?
      [ =1 ineg iplus                 # n pairs self cc i loop byteindex
        =3 ishl                       # n pairs self cc i loop bitindex
        sget06_ ishr lit8 ff iand     # n pairs self cc i loop byte

        dup =4 ishr                   # n pairs self cc i loop byte byte>>4
        "0123456789abcdef" .[]        # n pairs self cc i loop byte digit
        sget05 .append_int8 _         # n pairs self cc i loop self byte

        =15 iand                      # n pairs self cc i loop self byte&15
        "0123456789abcdef" .[] _      # n pairs self cc i loop digit self
        .append_int8 drop             # n pairs self cc i loop

        _=1 iplus_ dup goto ]         # ->loop(i+1)
      [ drop drop drop                # n pairs self cc
        sset01 sset01 goto ]          # self
      if goto ]                       # n pairs self cc i loop
    dup goto                          # ->loop(i=0) },

  append_dec => bin q{                # n self cc
    # Emit a minus sign if the number is negative
    =0 sget03 ilt                     # n self cc n<0?
    [ lit8'- sget03 .append_int8 drop # n self cc cc'
      sget03 ineg sset03              # |n| self cc cc'
      goto ]                          # |n| self cc
    [ goto ]                          # |n| self cc
    if call                           # |n| self cc

    # If the number is zero, emit "0" and return directly
    sget02                            # n self cc n!=0?
    [ goto ]                          # n self cc
    [ lit8'0 sget03 .append_int8 drop # n self cc cc'
      drop sset01 swap goto ]         # self
    if call                           # n self cc

    # Search upwards to find the leading digit, then start subtracting powers
    # of ten.
    =1                                # n self cc p
    [ sget05 sget03 swap ilt inot     # n self cc p loop cc' p<=n?
      [ sget02 lit8+10 itimes sset02  # n self cc p*10 loop cc'
        sget01 goto ]                 # ->loop
      [ sset00 goto ]                 # n self cc p
      if goto ]                       # n self cc p loop
    dup call                          # n self cc p'

    lit8+10 idivmod drop              # n self cc p'/10

    # Now we have a power of ten that is at least as large as the quantity.
    # Perform successive integer division until the base becomes 0, appending
    # a character each time.
    sget03                            # n self cc p' n'
    [                                 # n self cc p' n' loop
      sget02                          # n self cc p' n' loop p'?

      [ sget01 sget03 idivmod         # n self cc p' n' loop digit rem
        sset02                        # n self cc p' rem loop digit
        lit8'0 iplus                  # n self cc p' rem loop ascii
        sget05 .append_int8 drop      # n self cc p' rem loop

        # Divide p' by 10 to select the next digit
        sget02 lit8+10 idivmod drop   # n self cc p' rem loop p'/10
        sset02                        # n self cc p'/10 rem loop

        dup goto ]                    # ->loop

      [ drop drop drop                # n self cc
        sset01 swap goto ]            # self

      if goto ]                       # n self cc p' n' loop
    dup goto                          # self },

  append_string => bin q{             # x self cc
    sget02 sget02 .+= sset02          # self self cc
    sset00 goto                       # self },

  to_string => bin q{                 # self cc
    sget 01 .size =12 iplus           # self cc ssize
    i.heap_allocate                   # self cc &s
    $byte_string_class sget 01 m64set      # self cc &s [.vt=]
    sget 02 .size sget 01 =8  iplus m32set # [.size=]
    sget 02 .data sget 01 =12 iplus        # self cc &s from to
    sget 04 .size memcpy                        # self cc &s [copy]
    sset 01 goto                      # &s };


use phi::fn strbuf => bin q{            # cc
  i8i                                   # cc buf
  $string_buffer_class sget01 m64set    # cc buf
  _ goto                                # buf };


use phi::testfn string_buffer => bin q{# cc
  strbuf                              # cc buf
  dup .to_string "" .== "empty tostring"   i.assert
  dup .size =0      ieq "size(0)"          i.assert
  dup .capacity =64     ieq "capacity(64)" i.assert

  "foo" swap .append_string           # cc buf
  dup .size lit8+3 ieq "size(3)"           i.assert
  dup .to_string "foo" .== "tostring(foo)" i.assert

  "bar" swap .append_string           # cc buf
  dup .size lit8+6 ieq "size(6)"                 i.assert
  dup .to_string "foobar" .== "tostring(foobar)" i.assert

  "foobar" swap .append_string                  # len=12
  "0123456789012345678" swap .append_string     # len=31
  "9" swap .append_string                       # len=32

  dup .size     =32     ieq "size(32)"     i.assert
  dup .capacity =64     ieq "capacity(64)" i.assert
  dup .to_string "foobarfoobar01234567890123456789" .== "tos(32)" i.assert

  lit8 'x swap .append_int8           # cc buf

  dup .size     lit8 +33 ieq "size(33)"     i.assert
  dup .capacity lit8 +64 ieq "capacity(64)" i.assert

  dup .to_string "foobarfoobar01234567890123456789x" .== "tos(33)" i.assert

  lit64 'abcdefgh swap .append_int64  # cc buf
  dup .size     lit8 +41 ieq "size(41)"     i.assert
  dup .capacity lit8 +64 ieq "capacity(64)" i.assert

  dup .to_string "foobarfoobar01234567890123456789xhgfedcba" .==
    "tos(41)" i.assert

  drop                                # cc

  # Decimal conversion
  lit8+137       strbuf .append_dec .to_string "137"  .== "dec137"  i.assert
  lit8+0         strbuf .append_dec .to_string "0"    .== "dec0"    i.assert
  lit8+10        strbuf .append_dec .to_string "10"   .== "dec10"   i.assert
  lit8+1         strbuf .append_dec .to_string "1"    .== "dec1"    i.assert
  lit8+137 ineg  strbuf .append_dec .to_string "-137" .== "dec-137" i.assert

  lit32 00100000 strbuf .append_dec .to_string "1048576" .==
    "dec1048576" i.assert

  # Hex digit stuff
  =15 =1 strbuf .append_hex .to_string "0f"               .== "hex15"   i.assert
  =16 =1 strbuf .append_hex .to_string "10"               .== "hex16"   i.assert
  =16 =2 strbuf .append_hex .to_string "0010"             .== "hex2:16" i.assert
  =16 =4 strbuf .append_hex .to_string "00000010"         .== "hex4:16" i.assert
  =16 =8 strbuf .append_hex .to_string "0000000000000010" .== "hex8:16" i.assert

  goto                                # };


1;
