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


=head2 String buffer
This is derived from an indirect int8 array; we just add a few more
string-oriented methods.
=cut

use phi::protocol string_buffer =>
  qw/ <<hex
      <<dec
      clear
      to_string /;

use phi::class string_buffer =>
  i8_indirect_array_class->protocols,
  string_buffer_protocol,

  i8_indirect_array_class->methods,

  clear => bin q{                     # self cc
    =0 sget02 .rewind_to drop goto    # self },

  to_string => bin q{_ .to_direct _ goto},

  "<<hex" => bin q{                   # n pairs self cc
    =0                                # n pairs self cc i
    [                                 # n pairs self cc i loop
      sget04 sget02 ineg iplus dup    # n pairs self cc i loop pairs-i pairs-i?
      [ =1 ineg iplus                 # n pairs self cc i loop byteindex
        =3 ishl                       # n pairs self cc i loop bitindex
        sget06_ ishr lit8 ff iand     # n pairs self cc i loop byte

        dup =4 ishr                   # n pairs self cc i loop byte byte>>4
        "0123456789abcdef" .[]        # n pairs self cc i loop byte digit
        sget05 .<< _                  # n pairs self cc i loop self byte

        =15 iand                      # n pairs self cc i loop self byte&15
        "0123456789abcdef" .[] _      # n pairs self cc i loop digit self
        .<< drop                      # n pairs self cc i loop

        _=1 iplus_ dup goto ]         # ->loop(i+1)
      [ drop drop drop                # n pairs self cc
        sset01 sset01 goto ]          # self
      if goto ]                       # n pairs self cc i loop
    dup goto                          # ->loop(i=0) },

  "<<dec" => bin q{                   # n self cc
    # Emit a minus sign if the number is negative
    =0 sget03 ilt                     # n self cc n<0?
    [ lit8'- sget03 .<< drop          # n self cc cc'
      sget03 ineg sset03              # |n| self cc cc'
      goto ]                          # |n| self cc
    [ goto ]                          # |n| self cc
    if call                           # |n| self cc

    # If the number is zero, emit "0" and return directly
    sget02                            # n self cc n!=0?
    [ goto ]                          # n self cc
    [ lit8'0 sget03 .<< drop          # n self cc cc'
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
        sget05 .<< drop               # n self cc p' rem loop

        # Divide p' by 10 to select the next digit
        sget02 lit8+10 idivmod drop   # n self cc p' rem loop p'/10
        sset02                        # n self cc p'/10 rem loop

        dup goto ]                    # ->loop

      [ drop drop drop                # n self cc
        sset01 swap goto ]            # self

      if goto ]                       # n self cc p' n' loop
    dup goto                          # self };


use phi::fn strbuf => bin q{            # cc
  i8i                                   # cc buf
  $string_buffer_class sget01 m64set    # cc buf
  _ goto                                # buf };


use phi::testfn string_buffer => bin q{# cc
  strbuf                              # cc buf
  dup .to_direct "" .== "empty tostring"   i.assert
  dup .size =0      ieq "size(0)"          i.assert
  dup .capacity =64     ieq "capacity(64)" i.assert

  "foo" swap .+=           # cc buf
  dup .size lit8+3 ieq "size(3)"           i.assert
  dup .to_string "foo" .== "tostring(foo)" i.assert

  "bar" swap .+=           # cc buf
  dup .size lit8+6 ieq "size(6)"                 i.assert
  dup .to_string "foobar" .== "tostring(foobar)" i.assert

  "foobar" swap .+=                  # len=12
  "0123456789012345678" swap .+=     # len=31
  "9" swap .+=                       # len=32

  dup .size     =32     ieq "size(32)"     i.assert
  dup .capacity =64     ieq "capacity(64)" i.assert
  dup .to_string "foobarfoobar01234567890123456789" .== "tos(32)" i.assert

  lit8 'x _ .<<           # cc buf

  dup .size     lit8 +33 ieq "size(33)"     i.assert
  dup .capacity lit8 +64 ieq "capacity(64)" i.assert

  dup .to_string "foobarfoobar01234567890123456789x" .== "tos(33)" i.assert

  drop                                # cc

  # Decimal conversion
  lit8+137       strbuf .<<dec .to_string "137"  .== "dec137"  i.assert
  lit8+0         strbuf .<<dec .to_string "0"    .== "dec0"    i.assert
  lit8+10        strbuf .<<dec .to_string "10"   .== "dec10"   i.assert
  lit8+1         strbuf .<<dec .to_string "1"    .== "dec1"    i.assert
  lit8+137 ineg  strbuf .<<dec .to_string "-137" .== "dec-137" i.assert

  lit32 00100000 strbuf .<<dec .to_string "1048576" .==
    "dec1048576" i.assert

  # Hex digit stuff
  =15 =1 strbuf .<<hex .to_string "0f"               .== "hex15"   i.assert
  =16 =1 strbuf .<<hex .to_string "10"               .== "hex16"   i.assert
  =16 =2 strbuf .<<hex .to_string "0010"             .== "hex2:16" i.assert
  =16 =4 strbuf .<<hex .to_string "00000010"         .== "hex4:16" i.assert
  =16 =8 strbuf .<<hex .to_string "0000000000000010" .== "hex8:16" i.assert

  goto                                # };


1;
