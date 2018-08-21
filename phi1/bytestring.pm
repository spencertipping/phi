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


=head2 Byte strings
The simplest possible class: a byte string is just a length-prefixed series of
bytes. Specifically:

  struct byte_string
  {
    here_pointer vtable;
    uint32       length;
    byte         data[length];
  }

Byte strings behave as mutable bitsets if you address them with the set
protocol.
=cut

use phi::class byte_string =>
  byte_string_protocol,
  clone_protocol,
  eq_protocol,
  joinable_protocol,
  set_protocol,
  mutable_set_protocol,
  invertible_protocol,
  list_protocol,

  clone => bin q{                     # self cc
    sget01 .length lit8+12 iplus      # self cc size
    dup i.heap_allocate               # self cc size &s
    sget03 sget01 sget03 memcpy       # self cc size &s [copy]
    sset02 drop goto                  # &s },

  "contains?" => bin q{               # bit self cc
    sget02 lit8+3 ishr                # bit self cc bytei
    sget02 .[]                        # bit self cc byte
    =1 sget04 lit8+7 iand ishl        # bit self cc byte mask
    iand                              # bit self cc b
    sset02 sset00 goto                # b },

  "<<" => bin q{                      # bit self cc
    sget02 lit8+3 ishr                # bit self cc i
    sget02 .data iplus                # bit self cc &c
    =1 sget04 lit8+7 iand ishl        # bit self cc &c b
    sget01 m8get ior swap m8set       # bit self cc [c|=b]
    sset01 swap goto                  # self },

  "~" => bin q{                       # self cc
    sget01 .length lit8+12 iplus      # self cc size
    dup i.heap_allocate               # self cc size &s

    # Copy ourselves into the new object verbatim
    sget03 sget01 sget03 memcpy       # self cc size &s [copy]

    # Now invert each character in the new string
    sset02                            # s cc size
    drop sget01 .data                 # s cc &d
    sget02 .length                    # s cc &d l
    =0                                # s cc &d l i
    [                                 # s cc &d l i loop
      sget02 sget02 ilt               # s cc &d l i loop i<l?
      [ sget03 sget02 iplus           # s cc &d l i loop &d[i]
        dup m8get iinv swap m8set     # s cc &d l i loop [d[i]=~d[i]]
        swap =1 iplus swap            # s cc &d l i+1 loop
        dup goto ]                    # ->loop
      [ drop drop drop drop goto ]    # s
      if goto ]                       # s cc &d l i loop
    dup goto ]                        # ->loop },

  "+" => bin"                         # rhs self cc
    sget 01 .size                     # rhs self cc n1
    sget 03 .size                     # rhs self cc n1 n2
    sget 01 sget 01 iplus             # rhs self cc n1 n2 n
    dup lit8 +12 iplus                # rhs self cc n1 n2 n size

    i.heap_allocate                   # rhs self cc n1 n2 n r

    # Copy in the vtable from the LHS. We can't rely on the RHS here because
    # there's no reason the RHS needs to be a string (as long as it implements
    # .size and .data).
    sget 05 m64get sget 01 m64set     # rhs self cc n1 n2 n r [r.vtable]
    sget 01 sget 01                   # rhs self cc n1 n2 n r n r
    =8     iplus m32set               # rhs self cc n1 n2 n r [r.size]

    # Copy the LHS string data.
    dup .data                         # rhs self cc n1 n2 n r &data
    sget 06 .data                     # rhs self cc n1 n2 n r &data &s1d
    swap sget 05                      # rhs self cc n1 n2 n r &s1d &data n1
    memcpy                            # rhs self cc n1 n2 n r [s1d]

    # Copy RHS data.
    dup .data sget 04 iplus           # rhs self cc n1 n2 n r &rdata
    sget 07 .data                     # rhs self cc n1 n2 n r &rdata &s2d
    swap sget 04                      # rhs self cc n1 n2 n r &rdata &s2d n2
    memcpy                            # rhs self cc n1 n2 n r [s2d]

    # Now we're done: return the new string.
    sset 05                           # r self cc n1 n2 n
    drop drop drop swap drop goto     # r",

  "[]" => bin"                        # i self cc
    sget 01 .data                     # i self cc &data
    sget 03 iplus                     # i self cc &data[i]
    m8get                             # i self cc data[i]
    sset 02 swap drop goto            # i",

  reduce => bin q{                    # x0 f self cc
    =0                                # x0 f self cc i
    sget02 .length                    # x0 f self cc i l
    sget03 .data                      # x0 f self cc i l d
    [                                 # x0 f self cc i l d loop
      sget02 sget04 ilt               # x0 f self cc i l d loop i<l?
      [
                                      # 8  7 6    5  4 3 2 1    0
        sget01 sget04 iplus m8get     # x0 f self cc i l d loop d[i]
        sget08 sget08                 # x0 f self cc i l d loop d[i] x0 f
        call                          # x0 f self cc i l d loop x0' exit?
        [
          # Early exit
          sset07 drop drop drop drop  # x0' f self cc
          sset01 drop goto            # x0'
        ]
        [
          # No early exit; continue normally by replacing x0 and i
          sset07                      # x0' f self cc i l d loop
          sget03 =1 iplus sset03      # x0' f self cc i+1 l d loop
          dup goto                    # ->loop
        ]
        if goto
      ]
      [                               # x0 f self cc i l d loop
        # No more list items: return x0
        drop drop drop drop           # x0 f self cc
        sset 01 drop goto             # x0
      ]
      if goto
    ]                                 # x0 f self cc i l d loop
    dup goto                          # ->loop },

  "==" => bin"                        # rhs self cc
    # Optimization: if the strings' base pointers are equal, then the contents
    # must also be.
    sget 02 sget 02 ieq               # rhs self cc identical?
    [ drop sset 01 drop =1 swap goto ]
    [ goto ]
    if call

    sget 01 .size                     # rhs self cc n1
    dup sget 04 .size ieq             # rhs self cc n1 size=?

    [                                 # rhs self cc size
      # Strings are equal length, so loop over each byte:
      [                               # rhs self cc loop size i
        sget 01 sget 01 ilt           # rhs self cc loop size i i<size?
        [                             # rhs self cc loop size i
          dup sget 06 .[]             # rhs self cc loop size i rhs[i]
          sget 01 sget 06 .[]         # rhs self cc loop size i rhs[i] self[i]
          ieq                         # rhs self cc loop size i eq?
          [                           # rhs self cc loop size i
            =1 iplus                  # rhs self cc loop size i+1
            sget 02 goto              # tail call into loop
          ]
          [                           # rhs self cc loop size i
            drop drop drop            # rhs self cc
            swap drop swap drop       # cc
            =0 swap goto              # 0
          ]
          if goto
        ]
        [                             # rhs self cc loop size i
          # i >= size: we're done, return 1
          drop drop drop              # rhs self cc
          swap drop swap drop         # cc
          =1 swap goto                # 1
        ]
        if goto
      ]                               # rhs self cc size loop
      swap                            # rhs self cc loop size
      =0                              # rhs self cc loop size 0
      sget 02 goto                    # tail call into loop
    ]
    [                                 # rhs self cc _
      drop                            # rhs self cc
      swap drop swap drop             # cc
      =0     swap goto                # 0
    ]
    if goto                           # 0|1",

  data => bin"                        # self cc
    swap lit8 +12 iplus swap goto     # &data",

  length => bin"                      # self cc
    swap .size swap goto              # self.size",

  size => bin"                        # self cc
    swap =8     iplus                 # cc self+8
    m32get swap goto                  # size";


sub str($)
{
  phi::allocation->constant(pack "QL/a" => byte_string_class, $_[0])
                 ->named("string constant \"" . ($_[0] =~ s/[[:cntrl:]]/./gr)
                                              . "\""
                                              . ++($phi::str_index //= 0))
    >> heap;
}


use phi::fn memset => bin q{            # c &m size cc
  =0                                    # c &m size cc i
  [                                     # c &m size cc i loop
    sget03 sget02 ilt                   # c &m size cc i loop i<size?
    [ sget05 sget05 sget03 iplus        # c &m size cc i loop c &m[i]
      m8set                             # c &m size cc i loop [m[i]=c]
      swap =1     iplus swap            # c &m size cc i+1 loop
      dup goto ]                        # ->loop
    [ drop drop sset02 drop drop        # cc
      goto ]                            #
    if goto ]
  dup goto                              # };


use phi::fn bitset => bin q{            # capacity cc
  sget01 lit8+7 iplus lit8+3 ishr       # capacity cc bytes
  dup lit8+12 iplus i.heap_allocate     # capacity cc bytes &s

  $byte_string_class sget01 m64set      # [.vt=]
  sget01 sget01 =8 iplus m32set         # [.length=]

  =0 sget01 .data sget03                # cap cc bytes &s 0 &data bytes
  memset                                # cap cc bytes &s
  sset02 drop goto                      # &s };


use phi::fn murmur2a => bin q{          # s seed cc
  sget02 .size sget02 ixor              # s seed cc h
  [                                     # s seed cc h loop &d n i
    sget01 sget01 lit8+7 iplus ilt      # s seed cc h loop &d n i i+7<n?
    [                                   # s seed cc h loop &d n i
      sget02 sget01 iplus m64get        # s seed cc h loop &d n i k
      lit64 c6a4a793 5bd1e995 itimes    # s seed cc h loop &d n i k'
      dup lit8+47 isar ixor             # s seed cc h loop &d n i k''
      lit64 c6a4a793 5bd1e995 itimes    # s seed cc h loop &d n i k'''

      sget05 ixor                       # s seed cc h loop &d n i h'
      lit64 c6a4a793 5bd1e995 itimes    # s seed cc h loop &d n i h''
      sset04                            # s seed cc h'' loop &d n i
      =8     iplus                      # s seed cc h'' loop &d n i+8

      sget03 goto ]                     # ->loop

    [ # Fewer than 8 bytes left: mix in a partial little-endian qword. We
      # need to build this up byte by byte; otherwise we risk running beyond
      # the string, and ultimately beyond a page boundary, which could cause a
      # segfault.
      #
      # Do we have anything left at all? If not, then we're done.

      sget01 sget01 ilt                 # s seed cc h _ &d n i i<n?
      [ =0                              # s seed cc h _ &d n i k
        [ sget02 sget02 ilt             # s seed cc h _ &d n i k i<n?
          [ sget03 sget02 iplus m8get   # s seed cc h loop' &d n i  k d[i]
            sget02 lit8+7 iand          # s seed cc h loop' &d n i  k d[i] bi
            lit8+3 ishl ishl ior        # s seed cc h loop' &d n i  k'
            swap =1     iplus swap      # s seed cc h loop' &d n i' k'
            sget04 goto ]               # ->loop'

          [ lit64 c6a4a793 5bd1e995 itimes  # s seed cc h _ &d n i k'
            dup lit8+47 isar ixor           # s seed cc h _ &d n i k''
            lit64 c6a4a793 5bd1e995 itimes  # s seed cc h _ &d n i k'''
            sget05 ixor                     # s seed cc h _ &d n i h'
            lit64 c6a4a793 5bd1e995 itimes  # s seed cc h _ &d n i h''

            sset07 drop drop drop drop drop
            sset00 goto ]               # h''
          if goto ]                     # s seed cc h _ &d n i k loop'
        dup sset05 goto ]               # ->loop'

      [                                 # s seed cc h _ &d n i
        drop drop drop drop             # s seed cc h
        sset02 sset00 goto ]            # h
      if goto ]
    if goto ]                           # s seed cc h loop

  sget04 dup .data swap .size =0        # s seed cc h loop &d n i
  sget03 goto                           # ->loop };

BEGIN
{
  bin_macros->{method_hash} = bin q{=0 murmur2a};
}


sub mhash_test($)
{
  bin qq{
    lit64 >pack("Q>", method_hash "$_[0]")
    "$_[0]" method_hash ieq "mhash[$_[0]]" i.assert };
}

sub all_mhash_tests()
{
  join"", map mhash_test($_), sort keys %{+defined_methods};
}

use phi::fn byte_string_test => bin q{  # cc
  "foo" "bar" .+
  "barfoo" .== "barfoo" i.assert

  "foo" "foo=" .== inot "not=" i.assert

  =0
  [                                   # total c cc
    sget02 sget02 iplus sset02        # total' c cc
    =0     sset01 goto ]              # total' 0
  "01" .reduce
  lit8+97 ieq "total97" i.assert

  "foo" .~ .~ "foo" .== "inv2" i.assert

  lit8+13 bitset                      # cc b
    =0 sget01 .contains? =0 ieq "bcontains0" i.assert
    =1 sget01 .contains? =0 ieq "bcontains1" i.assert
    =2 sget01 .contains? =0 ieq "bcontains2" i.assert
    =4 sget01 .contains? =0 ieq "bcontains4" i.assert
    =8 sget01 .contains? =0 ieq "bcontains8" i.assert

    .~
    =0 sget01 .contains? "~bcontains0" i.assert
    =1 sget01 .contains? "~bcontains1" i.assert
    =2 sget01 .contains? "~bcontains2" i.assert
    =4 sget01 .contains? "~bcontains4" i.assert
    =8 sget01 .contains? "~bcontains8" i.assert
    .~

                                  # cc b
    =0 sget01 .<<                 # cc b b
    =0 swap .contains? "bcontains0" i.assert

    =1 sget01 .contains? =0 ieq "bcontains1" i.assert
    =2 sget01 .contains? =0 ieq "bcontains2" i.assert
    =4 sget01 .contains? =0 ieq "bcontains4" i.assert
    =8 sget01 .contains? =0 ieq "bcontains8" i.assert

    =2 sget01 .<< =2 swap .contains? "bcontains2" i.assert
    =1 sget01 .contains? =0 ieq "bcontains1" i.assert
    =4 sget01 .contains? =0 ieq "bcontains4" i.assert
    =8 sget01 .contains? =0 ieq "bcontains8" i.assert

    =8 sget01 .<< =8 swap .contains? "bcontains8" i.assert
    =1 sget01 .contains? =0 ieq "bcontains1" i.assert
    =4 sget01 .contains? =0 ieq "bcontains4" i.assert
  drop

  # Important: we need the same hashed value from both perl and from phi
  # (otherwise phi won't be able to compile compatible method calls)
  >mhash_test "a"
  >mhash_test "ab"
  >mhash_test "abc"
  >mhash_test "abcd"
  >mhash_test "abcde"
  >mhash_test "abcdef"
  >mhash_test "abcdefg"
  >mhash_test "abcdefgh"
  >mhash_test "abcdefghabcdefgh"
  >mhash_test "foobarbifbazbok"
  >mhash_test "foobarbifbazbokzzz"
  >all_mhash_tests

  goto                                # };


1;
