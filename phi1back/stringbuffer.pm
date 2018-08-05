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


=head2 String buffer
The idea here is to maintain a buffer that has some headroom to minimize
reallocation. Each overflow doubles the buffer size. Here's what the struct
looks like:

  struct string_buffer
  {
    hereptr vtable;
    cell    size;
    cell    capacity;
    byte*   data;
  };

=cut


use constant string_buffer_class => phi::class->new('string_buffer',
  clone_protocol,
  byte_string_protocol,
  string_buffer_protocol)

  ->def(
    clone => bin q{                     # self cc
      =32     i.heap_allocate           # self cc &b
      sget02 sget01 =24     memcpy      # self cc &b [.vt=,.size=,.cap=]
      sget02 .capacity                  # self cc &b n
      i.heap_allocate                   # self cc &b &data

      dup
      sget03 .data                      # self cc &b &data &data &from
      swap sget04 .size                 # self cc &b &data &from &to n
      memcpy                            # self cc &b &data [copy]

      sget01 =24     iplus m64set       # self cc &b [.data=]
      sset01 goto                       # &b },

    size     => bin"swap =8      iplus m64get swap goto",
    capacity => bin"swap =16     iplus m64get swap goto",
    data     => bin"swap =24     iplus m64get swap goto",

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

      sget02 =32     ishr               # size q self cc q>>32
      sget03 =32     ishl               # size q self cc q>>32 q<<32
      sget05 ior                        # size q self cc q2 q1
      $byte_string_class                # size q self cc q2 q1 vt
      get_stackptr                      # size q self cc q2 q1 vt &s
      sget05 .append_string             # size q self cc q2 q1 vt self
      drop drop drop drop sset01        # size cc self
      sset01 goto                       # self },

    append_dec => bin q{                # n self cc
      # Emit a minus sign if the number is negative
      =0     sget03 ilt                 # n self cc n<0?
      [ lit8'- sget03 .append_int8 drop # n self cc cc'
        sget03 ineg sset03              # |n| self cc cc'
        goto ]                          # |n| self cc
      [ goto ]                          # |n| self cc
      if call                           # |n| self cc

      # If the number is zero, emit "0" and return directly
      sget02
      [ goto ]                          # n self cc
      [ lit8'0 sget03 .append_int8 drop # n self cc cc'
        drop sset01 swap goto ]         # self
      if call                           # n self cc

      # Search upwards to find the leading digit, then start subtracting powers
      # of ten.
      =1                                # n self cc p
      [ sget05 sget03 ilt               # n self cc p loop cc' p<n?
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

    append_string => bin"               # x self cc
      sget 01 .headroom                 # x self cc h
      sget 03 .size                     # x self cc h s
      swap ilt inot                     # x self cc s<=h?
      [
        sget 02 .data                   # x self cc from-ptr
        sget 02 .size                   # x self cc from selfsize
        sget 03 .data iplus             # x self cc from to
        sget 04 .size                   # x self cc from to bytes
        memcpy                          # x self cc [.data+=]
        sget 02 .size sget 02 .size iplus # x self cc size'
        sget 02 =8     iplus m64set     # x self cc [.size=]
        sset 01 swap goto               # self
      ]
      [                                 # x self cc
        sget 01 .capacity               # x self cc c
        =1     ishl                     # x self cc c*2
        sget 02 .reallocate             # x self cc self
        sget 03 swap .append_string     # x self cc self
        sset 02 sset 00 goto            # self
      ]
      if goto                           # self",

    reallocate => bin"                  # size self cc
      sget 01 .data                     # size self cc from
      sget 03 i.heap_allocate           # size self cc from to
      dup sget 04 =24     iplus m64set  # size self cc from to [.data=]
      sget 03 .size memcpy              # size self cc [copy]
      sget 02 sget 02 =16     iplus m64set  # size self cc [.capacity=]
      sset 01 swap goto                 # self",

    rewind => bin q{                    # n self cc
      # Deallocate some bytes. This never causes the buffer's allocation to
      # change.
      sget01 .size sget03 ineg iplus    # n self cc size'
      sget02 =8     iplus m64set        # n self cc
      sset01 swap goto                  # self },

    to_string => bin q{                 # self cc
      sget 01 .size lit8 +12 iplus      # self cc ssize
      i.heap_allocate                   # self cc &s
      $byte_string_class sget 01 m64set           # self cc &s [.vt=]
      sget 02 .size sget 01 =8     iplus m32set   # [.size=]
      sget 02 .data sget 01 lit8 +12 iplus        # self cc &s from to
      sget 04 .size memcpy                        # self cc &s [copy]
      sset 01 goto                      # &s });


use constant string_buffer_fn => phi::allocation
  ->constant(bin q{                     # cc
    =32     i.heap_allocate             # cc &buf
    =32     i.heap_allocate             # cc &buf &data
    sget 01 =24     iplus m64set        # cc &buf [.data=]
    $string_buffer_class                # cc &buf vt
    sget 01 m64set                      # cc &buf [.vt=]
    =0     sget 01 =8     iplus m64set  # cc &buf [.size=]
    =32     sget 01 =16     iplus m64set# cc &buf [.capacity=]
    swap goto                           # &buf })
  ->named('string_buffer_fn') >> heap;


BEGIN
{
  bin_macros->{strbuf} = bin '$string_buffer_fn call';
}


use constant string_buffer_test_fn => phi::allocation
  ->constant(bin q{                     # cc
    strbuf                              # cc buf
    dup .to_string "" .== "empty tostring"   i.assert
    dup .size =0      ieq "size(0)"          i.assert
    dup .capacity =32     ieq "capacity(32)" i.assert

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
    dup .capacity =32     ieq "capacity(32)" i.assert
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
    lit8+137       strbuf .append_dec .to_string "137"     .== "dec1" i.assert
    lit8+0         strbuf .append_dec .to_string "0"       .== "dec2" i.assert
    lit8+137 ineg  strbuf .append_dec .to_string "-137"    .== "dec3" i.assert

    lit32 00100000 strbuf .append_dec .to_string "1048576" .== "dec4" i.assert

    goto                                # })

  ->named('string buffer test fn') >> heap;


1;
