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


=head2 Parsing expression grammars
I want to get beyond perl-hosted notation quickly, so it's worth writing a
parser library that makes this possible. Like most PEG libraries we have a few
basic elements:

  "foo" pstr
  p1 p2 palt
  p1 p2 pseq
  p     pmaybe


=head3 Parse state representation
Parsers are trivial to write, but we need to decide on a way to encode parse
states. The most important thing is to minimize memory usage, which we can do by
having each parser return multiple values:

  <input> <position> parser .parse      # value position'

C<-1> is a reserved position value that indicates a failed parse. C<value> is
set to null on parse failure, an arbitrary value on success.

C<position> has no intrinsic meaning; that is, it isn't a memory address. It's
up to individual parsers to use it in conjunction with the input, whose type is
also arbitrary.


=head3 String parser
The simplest parser: match a literal string at the specified position.

  struct string_parser
  {
    hereptr  vtable;
    string  *text;
  }
=cut

use constant string_parser_class => phi::class->new('string_parser',
  string_parser_protocol,
  parser_protocol)

  ->def(
    text  => bin q{swap const8 iplus m64get swap goto},
    parse => bin q{                     # input pos self cc
      # First make sure the input is large enough to contain the text.
      sget02 sget02 .text .length iplus # input pos self cc end
      sget04 .length ilt                # input pos self cc end>len?

      [ sset00                          # input pos cc
        const0 sset02                   # 0 pos cc
        const1 ineg sset01              # 0 -1 cc
        goto ]                          # 0 -1

      [ # Now run the loop to compare individual characters.
                                        # input pos self cc
        swap .text dup .length          # input pos cc text len
        const0                          # input pos cc text len i
        [                               # input pos cc text len i loop
          sget02 sget02 ilt             # input pos cc text len i loop i<len?

          [ sget01 sget06 iplus         # in pos cc t len i loop pos+i
            sget07 .[]                  # in pos cc t len i loop in[pos+i]
            sget02 sget05 .[] ieq       # in pos cc t len i loop inc==tc?
            [ swap const1 iplus swap    # in pos cc t len i+1 loop
              dup goto ]                # ->loop
            [ drop drop drop drop       # input pos cc
              const0 sset02             # 0 pos cc
              const1 ineg sset01 goto ] # 0 -1
            if goto ]

          [ # We're at the end of our text, so return success.
            drop drop                   # in pos cc t len
            sget03 iplus                # in pos cc t pos'
            sset02 sset02               # t pos' cc
            goto ]                      # t pos'

          if goto ]                     # input pos cc text len i loop

        dup goto ]

      if goto                           # v pos' });


use constant str_fn => phi::allocation
  ->constant(bin q{                     # str cc
    const16 i.heap_allocate             # str cc &p
    $string_parser_class sget01 m64set  # str cc &p [.vt=]
    sget02  sget01 const8 iplus m64set  # str cc &p [.text=]
    sset01 goto                         # &p })
  ->named('str_fn') >> heap;

BEGIN
{
  bin_macros->{pstr} = bin q{$str_fn call};
}


use constant str_parser_test_fn => phi::allocation
  ->constant(bin q{                     # cc
    "foobar" const2                     # cc in pos
    "ob" pstr .parse                    # cc "ob" 4

    const4 ieq i.assert
    "ob"   .== i.assert                 # cc

    "foobar" const2                     # cc in pos
    "ba" pstr .parse                    # cc 0 -1

    const1 ineg ieq i.assert
    const0      ieq i.assert            # cc

    "foobar" const1                     # cc in pos
    "oobar" pstr .parse                 # cc "oobar" 6
    lit8 +6 ieq i.assert
    "oobar" .== i.assert                # cc

    goto })
  ->named('str_parser_test_fn') >> heap;


=head3 Character classes
These are backed by a bitset and parse either one or many characters depending
on which class you use.

Here are the structs:

  struct one_char_parser
  {
    hereptr      vtable;
    byte_string *chars;         // a bitset
  }

  struct many_char_parser
  {
    hereptr      vtable;
    int32        mincount;
    byte_string *chars;         // a bitset
  }

C<one_char_parser> returns a byte, whereas C<many_char_parser> returns a string.
=cut

use constant char_one_parser_class => phi::class->new('char_one_parser',
  char_parser_protocol,
  parser_protocol)

  ->def(
    chars => bin q{swap const8 iplus m64get swap goto},
    parse => bin q{                     # in pos self cc
      # Check for EOF
      sget03 .length sget03 ilt         # in pos self cc pos<len?

      [ swap .chars                     # in pos cc cs
        sget02 sget04 .[]               # in pos cc cs ci
        dup sget02 .contains?           # in pos cc cs ci contains?

        [ sset03 drop                   # ci pos cc
          sget01 const1 iplus sset01    # ci pos+1 cc
          goto ]                        # ci pos+1

        [ drop drop                     # in pos cc
          const0 sset02                 # 0 pos cc
          const1 ineg sset01 goto ]     # 0 -1

        if goto ]                       # v pos'

      [ sset00                          # in pos cc
        const0 sset02                   # 0 pos cc
        const1 ineg sset01 goto ]       # 0 -1

      if goto                           # v pos' });


use constant char_many_parser_class => phi::class->new('char_many_parser',
  char_parser_protocol,
  repeat_parser_protocol,
  parser_protocol)

  ->def(
    mincount => bin q{swap const8  iplus m32get swap goto},
    chars    => bin q{swap lit8+12 iplus m64get swap goto},

    parse => bin q{                     # in pos self cc
      # Do we have mincount chars in the buffer? If not, fail immediately.
      sget03 .length sget03             # in pos self cc l pos
      sget03 .mincount iplus swap ilt   # in pos self cc (pos+min)>l?

      [ sset00
        const0 sset02
        const1 ineg sset01 goto ]       # 0 -1

      [ # Now see how many characters we can match. Then we'll allocate a byte
        # string and copy the region.
        sget03 .data                    # in pos self cc &d
        sget02 .chars sget04            # in pos self cc &d cs pos
        sget06 .length swap             # in pos self cc &d cs l pos

        [                               # &d cs l pos loop cc
          sget03 sget03 ilt             # &d cs l pos loop cc pos<l?

          [ sget05 sget03 iplus m8get   # &d cs l pos loop cc d[pos]
            sget05 .contains?           # &d cs l pos loop cc c?
            [ sget02 const1 iplus       # &d cs l pos loop cc pos+1
              sset02                    # &d cs l pos+1 loop cc
              sget01 goto ]             # ->loop
            [ sset02 drop sset01 goto ] # &d pos'
            if goto ]
          [ sset02 drop sset01 goto ]   # &d pos'
          if goto ]

        dup call                        # in pos self cc &d pos'

        sget04 ineg iplus               # in pos self cc &d n
        sget03 .mincount sget01 ilt     # in pos self cc &d n n<min?

        [ drop drop                     # in pos self cc
          sset00 const0 sset02          # 0 pos cc
          const1 ineg sset01 goto ]     # 0 -1

        [ # We have enough bytes, so allocate a string and copy the data in.
          dup lit8+12 iplus             # in pos self cc &d n n+12
          i.heap_allocate               # in pos self cc &d n &v

          $byte_string_class sget01 m64set    # [.vt=]
          sget01 sget01 const8 iplus m32set   # [.size=]

          sget05 sget03 iplus           # in pos self cc &d n &v &fd
          sget01 .data                  # in pos self cc &d n &v &fd &td
          sget03 memcpy                 # in pos self cc &d n &v

          sset05                        # &v pos self cc &d n
          sget04 iplus sset03           # &v pos+n self cc &d
          drop sset00 goto ]            # &v pos+n
        if goto ]
      if goto                           # v pos' });


use constant one_fn => phi::allocation
  ->constant(bin q{                     # str cc
    lit16 0100 bitset                   # str cc bs
    [ sget02 sget02 .<< sset02
      const0 sset01 goto ]              # str cc bs f
    sget03 .reduce                      # str cc bs

    const16 i.heap_allocate             # str cc bs &p
    $char_one_parser_class sget01 m64set# str cc bs &p [.vt=]
    sget01  sget01 const8 iplus m64set  # str cc bs &p [.chars=]
    sset02 drop goto                    # &p })
  ->named('one_fn') >> heap;

use constant atleast_fn => phi::allocation
  ->constant(bin q{                     # str n cc
    lit16 0100 bitset                   # str n cc bs
    [ sget02 sget02 .<< sset02          # c bs cc
      const0 sset01 goto ]              # str n cc bs f
    sget04 .reduce                      # str n cc bs

    lit8+20 i.heap_allocate             # str n cc bs &p
    $char_many_parser_class sget01
                            m64set      # str n cc bs &p [.vt=]
    sget03  sget01 const8  iplus m32set # str n cc bs &p [.mincount=]
    sget01  sget01 lit8+12 iplus m64set # str n cc bs &p [.chars=]
    sset03 drop sset00 goto             # &p })
  ->named('atleast_fn') >> heap;

BEGIN
{
  bin_macros->{poneof}   = bin q{$one_fn            call};
  bin_macros->{patleast} = bin q{$atleast_fn        call};
  bin_macros->{psomeof}  = bin q{const0 $atleast_fn call};
  bin_macros->{pmanyof}  = bin q{const1 $atleast_fn call};
}


use constant char_parser_test_fn => phi::allocation
  ->constant(bin q{                     # cc
    "abcabdefcFOO" const1               # cc in pos
    "abc" poneof .parse                 # cc "b" 2

    const2 ieq i.assert
    lit8'b ieq i.assert

    "abcabdefcFOO" const0               # cc in pos
    "abc" pmanyof .parse                # cc "abcab" 5

    lit8+5  ieq i.assert
    "abcab" .== i.assert

    goto                                # })
  ->named('char_parser_test_fn') >> heap;


=head3 Alternation
Simple enough: use the second parser iff the first one fails. You can extend
this for use with lists by using C<reduce>.

  struct alt_parser
  {
    hereptr  vtable;
    parser  *left;
    parser  *right;
  }

=cut

use constant alt_parser_class => phi::class->new('alt_parser',
  binary_parser_protocol,
  parser_protocol)

  ->def(
    left  => bin q{swap const8  iplus m64get swap goto},
    right => bin q{swap const16 iplus m64get swap goto},

    parse => bin q{                     # in pos self cc
      sget03 sget03 sget03 .left .parse # in pos self cc v pos'
      dup const1 ineg ieq               # in pos self cc v pos' fail?

      [ drop drop                       # in pos self cc
        sget03 sget03 sget03 .right     # in pos self cc in pos r
        .parse                          # in pos self cc v pos'
        sset03 sset03 sset00 goto ]     # v pos'

      [ sset03 sset03 sset00 goto ]     # v pos'
      if goto                           # v pos' });


use constant alt_fn => phi::allocation
  ->constant(bin q{                     # left right cc
    const24 i.heap_allocate             # left right cc &p
    $alt_parser_class sget01 m64set     # left right cc &p [.vt=]
    sget03  sget01 const8  iplus m64set # left right cc &p [.left=]
    sget02  sget01 const16 iplus m64set # left right cc &p [.right=]
    sset02 sset00 goto                  # &p })
  ->named('alt_fn') >> heap;

BEGIN
{
  bin_macros->{palt} = bin q{$alt_fn call};
}


use constant alt_parser_test_fn => phi::allocation
  ->constant(bin q{                     # cc
    "foobar" const0                     # cc in pos
    "foo" pstr
    "bar" pstr palt .parse              # cc "foo" 3

    lit8+3 ieq i.assert
    "foo"  .== i.assert                 # cc

    "foobar" const0                     # cc in pos
    "ba" pstr
    "foob" pstr palt .parse             # cc "foob" 4

    const4 ieq i.assert
    "foob" .== i.assert

    goto })
  ->named('alt_parser_test_fn') >> heap;


=head3 Sequential parsing
Sequential parsers parse one thing after another, combining the two results
using the specified function. The function allows you to reduce the results as
they are generated, which may save memory allocations.

Here's the struct:

  struct seq_parser
  {
    hereptr      vtable;
    hereptr<fn>  combiner;
    parser      *left;
    parser      *right;
  }

=cut

use constant seq_parser_class => phi::class->new('seq_parser',
  seq_parser_protocol,
  binary_parser_protocol,
  parser_protocol)

  ->def(
    combiner => bin q{swap const8  iplus m64get swap goto},
    left     => bin q{swap const16 iplus m64get swap goto},
    right    => bin q{swap const24 iplus m64get swap goto},

    combine => bin q{                   # v1 v2 self cc
      swap .combiner goto               # f(v1 v2 cc) },

    parse => bin q{                     # in pos self cc
      sget03 sget03 sget03 .left .parse # in pos self cc v pos'

      # Important: don't invoke the second parser if the first one fails.
      dup const1 ineg ieq               # in pos self cc v pos' fail?

      [ sset03 sset03 sset00 goto ]     # v pos'

      [ sget05 sget01 sget05 .right     # in pos self cc v pos' in pos' right
        .parse                          # in pos self cc v1 p1 v2 p2

        # Also important: don't invoke the combiner if the second parse fails.
        dup const1 ineg ieq             # in pos self cc v1 p1 v2 p2 fail2?

        [ sset05 sset05 drop drop       # v2 -1 self cc
          sset00 goto ]                 # v2 -1

        [                               # in pos self cc v1 p1 v2 p2
          sset05 sset00                 # in p2 self cc v1 v2
          sget03 .combine               # in p2 self cc v'
          sset03 sset00 goto ]          # v' p2

        if goto ]                       # v pos
      if goto                           # v pos });


use constant seq_fn => phi::allocation
  ->constant(bin q{                     # left right combiner cc
    const32 i.heap_allocate             # left right combiner cc &p
    $seq_parser_class sget01 m64set     # left right combiner cc &p [.vt=]
    sget02  sget01 const8  iplus m64set # left right combiner cc &p [.combiner=]
    sget04  sget01 const16 iplus m64set # left right combiner cc &p [.left=]
    sget03  sget01 const24 iplus m64set # left right combiner cc &p [.right=]
    sset03 sset01 drop goto             # &p })
  ->named('seq_fn') >> heap;

BEGIN
{
  bin_macros->{pseq} = bin q{$seq_fn call};
}


use constant seq_parser_test_fn => phi::allocation
  ->constant(bin q{                     # cc
    "foobar" const0                     # cc in pos
    "foo" pstr                          # cc in pos p1
    "bar" pstr                          # cc in pos p1 p2
    [                                   # v1 v2 cc
      "," sget03 .+                     # v1 v2 cc "v1,"
      sget02 swap .+                    # v1 v2 cc "v1,v2"
      sset02 sset00 goto ]              # cc in pos fn
    pseq .parse                         # cc "foo,bar" 6

    lit8+6 ieq i.assert
    "foo,bar" .== i.assert              # cc

    # Test failure cases
    "foobar" const1
    "foo" pstr "bar" pstr
    [ "should never be called" i.die ]
    pseq .parse

    const1 ineg ieq i.assert
    const0      ieq i.assert

    # Test failure cases
    "foobar" const1
    "oo" pstr "baR" pstr
    [ "should never be called" i.die ]
    pseq .parse

    const1 ineg ieq i.assert
    const0      ieq i.assert

    goto })
  ->named('seq_parser_test_fn') >> heap;


use constant parser_test_fn => phi::allocation
  ->constant(bin q{
    $str_parser_test_fn  call
    $char_parser_test_fn call
    $alt_parser_test_fn  call
    $seq_parser_test_fn  call
    goto })
  ->named('parser_test_fn') >> heap;


1;
