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

=cut

use phi::protocol parse_position =>
  qw/ fail?
      value
      with_value /;

use phi::protocol linear_position =>
  qw/ +
      index /;

use phi::protocol parser =>
  qw/ parse /;

use phi::protocol string_parser =>
  qw/ text /;

use phi::protocol binary_parser =>
  qw/ left
      right /;

use phi::protocol seq_parser =>
  qw/ combine
      combiner /;

use phi::protocol char_parser =>
  qw/ chars /;

use phi::protocol repeat_parser =>
  qw/ mincount /;

use phi::protocol parser_transform =>
  qw/ parser /;

use phi::protocol fn_parser =>
  qw/ fn /;


=head3 Parse state representation
Parsers are trivial to write, but we need to decide on a way to encode parse
states. The most important thing is to minimize memory usage, which we can do by
having each parser return multiple values:

  <input> <position> parser .parse      # position'

C<position> implements the parse position protocol, which provides two methods.
One indicates failure, the other contains a return value. Other protocols like
C<linear_position_protocol> specify offsets within strings.


=head3 Generic fail parse position
Parsers that fail should prefer this to constructing something, unless they need
to pass extra information through the failure.
=cut

use phi::class fail_position =>
  parse_position_protocol,
  "fail?"    => bin q{=1 sset01 goto},
  value      => bin q{"failed parse states have no value" i.die},
  with_value => bin q{"failed parse states have no value" i.die};

use phi::constQ fail_instance => fail_position_class->fn >> heap;


=head3 String parse position
All we need to do is provide two methods: C<+> and C<index>. Other parse
position classes will implement this protocol but provide additional state.

  struct string_position
  {
    hereptr vtable;
    *       value;
    int32   index;
  }

=cut

use phi::class string_position =>
  parse_position_protocol,
  linear_position_protocol,

  "fail?" => bin q{=0 sset01 goto},
  value   => bin q{swap =8  iplus m64get swap goto},
  index   => bin q{swap =16 iplus m32get swap goto},

  with_value => bin q{                # v self cc
    =0 sget03 sget03 .+               # v self cc p'
    sset02 sset00 goto                # p' },

  "+" => bin q{                       # n v self cc
    sget01 .index                     # n v self cc i
    sget04 iplus                      # n v self cc i+n
    =20 i.heap_allocate               # n v self cc i+n &r
    sget03 m64get sget01 m64set       # [.vt=]
    sget04 sget01 =8  iplus m64set    # [.value=]
    sget01 sget01 =16 iplus m32set    # [.index=]
    sset04 drop sset01 drop goto      # &r };

use phi::fn string_position => bin q{   # i v cc
  =20 i.heap_allocate                   # i v cc &s
  $string_position_class sget01 m64set  # [.vt=]
  sget02 sget01 =8  iplus m64set        # [.value=]
  sget03 sget01 =16 iplus m32set        # [.index=]
  sset02 sset00 goto                    # &s };

BEGIN
{
  bin_macros->{strpos} = bin q{=0 string_position};
}


=head3 String parser
The simplest parser: match a literal string at the specified position.

  struct string_parser
  {
    hereptr  vtable;
    string  *text;
  }

=cut

use phi::class string_parser =>
  string_parser_protocol,
  parser_protocol,

  text  => bin q{swap =8 iplus m64get swap goto},
  parse => bin q{                     # input pos self cc
    # First make sure the input is large enough to contain the text.
    sget02 .index                     # input pos self cc posi
    sget02 .text .length iplus        # input pos self cc end
    sget04 .length ilt                # input pos self cc end<len?

    [ sset01 drop                     # input cc
      $fail_instance sset01 goto ]    # fail

    [ # Now run the loop to compare individual characters.
                                      # input pos self cc
      swap .text dup .length          # input pos cc text len
      =0                              # input pos cc text len i
      [                               # input pos cc text len i loop
        sget02 sget02 ilt             # input pos cc text len i loop i<len?

        [ sget01 sget06 .index iplus  # in pos cc t len i loop pos+i
          sget07 .[]                  # in pos cc t len i loop in[pos+i]
          sget02 sget05 .[] ieq       # in pos cc t len i loop inc==tc?
          [ swap =1 iplus swap        # in pos cc t len i+1 loop
            dup goto ]                # ->loop
          [ drop drop drop drop       # input pos cc
            sset00                    # input cc
            $fail_instance sset01     # fail cc
            goto ]                    # fail
          if goto ]

        [ # We're at the end of our text, so return success.
          drop drop swap              # in pos cc len t
          sget03 .+                   # in pos cc pos'
          sset02 sset00               # pos' cc
          goto ]                      # pos'

        if goto ]                     # input pos cc text len i loop

      dup goto ]

    if goto                           # v pos' };


use phi::fn pstr => bin q{              # str cc
  =16 i.heap_allocate                   # str cc &p
  $string_parser_class sget01 m64set    # str cc &p [.vt=]
  sget02  sget01 =8 iplus m64set        # str cc &p [.text=]
  sset01 goto                           # &p };

use phi::fn str_parser_test => bin q{   # cc
  "foobar" =2 strpos                    # cc in pos
  "ob" pstr .parse                      # cc {v="ob" i=4}

  dup .index =4 ieq "strpi4" i.assert
  dup .value "ob" .== "strpvfoo" i.assert
  drop                                  # cc

  "foobar" =2 strpos                    # cc in pos
  "ba" pstr .parse                      # cc fail
  .fail? "strpfail" i.assert

  "foobar" =1 strpos                    # cc in pos
  "oobar" pstr .parse                   # cc {v="oobar" i=6}
  dup .index =6 ieq "strpi6" i.assert
  dup .value "oobar" .== "strpvoobar" i.assert
  drop

  goto                                  # };


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

use phi::class char_one_parser =>
  char_parser_protocol,
  parser_protocol,

  chars => bin q{swap =8     iplus m64get swap goto},
  parse => bin q{                     # in pos self cc
    # Check for EOF
    sget03 .length sget03 .index ilt  # in pos self cc pos<len?

    [ swap .chars                     # in pos cc cs
      sget02 .index sget04 .[]        # in pos cc cs ci
      dup sget02 .contains?           # in pos cc cs ci contains?

      [ sset03 drop                   # ci pos cc
        =1 sget03 sget03 .+           # ci pos cc pos'
        sset02 sset00 goto ]          # pos'

      [ drop drop sset00              # in cc
        $fail_instance sset01 goto ]  # fail

      if goto ]                       # pos'

    [ sset01 drop                     # in cc
      $fail_instance sset01 goto ]    # fail

    if goto                           # pos' };


use phi::class char_many_parser =>
  char_parser_protocol,
  repeat_parser_protocol,
  parser_protocol,

  mincount => bin q{swap =8      iplus m32get swap goto},
  chars    => bin q{swap lit8+12 iplus m64get swap goto},

  parse => bin q{                     # in pos self cc
    # Do we have mincount chars in the buffer? If not, fail immediately.
    sget03 .length sget03 .index      # in pos self cc l posi
    sget03 .mincount iplus swap ilt   # in pos self cc (pos+min)>l?

    [ sset01 drop                     # in cc
      $fail_instance sset01 goto ]    # fail

    [ # Now see how many characters we can match. Then we'll allocate a byte
      # string and copy the region.
      sget03 .data                    # in pos self cc &d
      sget02 .chars sget04 .index     # in pos self cc &d cs posi
      sget06 .length swap             # in pos self cc &d cs l posi

      [                               # &d cs l posi loop cc
        sget03 sget03 ilt             # &d cs l posi loop cc posi<l?

        [ sget05 sget03 iplus m8get   # &d cs l posi loop cc d[posi]
          sget05 .contains?           # &d cs l posi loop cc c?
          [ sget02 =1 iplus sset02    # &d cs l posi+1 loop cc
            sget01 goto ]             # ->loop
          [ sset02 drop sset01 goto ] # &d posi'
          if goto ]
        [ sset02 drop sset01 goto ]   # &d posi'
        if goto ]

      dup call                        # in pos self cc &d posi'

      sget04 .index ineg iplus        # in pos self cc &d n
      sget03 .mincount sget01 ilt     # in pos self cc &d n n<min?

      [ drop drop sset01 drop         # in cc
        $fail_instance sset01 goto ]  # fail

      [ # We have enough bytes, so allocate a string and copy the data in.
        dup =12 iplus                 # in pos self cc &d n n+12
        i.heap_allocate               # in pos self cc &d n &v

        $byte_string_class sget01 m64set  # [.vt=]
        sget01 sget01 =8 iplus m32set     # [.size=]

        sget05 .index sget03 iplus    # in pos self cc &d n &v &fd
        sget01 .data                  # in pos self cc &d n &v &fd &td
        sget03 memcpy                 # in pos self cc &d n &v
        sget05 .+                     # in pos self cc &d pos'
        sset04 drop sset01 drop goto ]# pos'
      if goto ]
    if goto                           # pos' };


use phi::fn poneof => bin q{            # str cc
  lit16 0100 bitset                     # str cc bs
  [ sget02 sget02 .<< sset02
    =0     sset01 goto ]                # str cc bs f
  sget03 .reduce                        # str cc bs

  =16     i.heap_allocate               # str cc bs &p
  $char_one_parser_class sget01 m64set  # str cc bs &p [.vt=]
  sget01  sget01 =8     iplus m64set    # str cc bs &p [.chars=]
  sset02 drop goto                      # &p };

use phi::fn patleast => bin q{          # str n cc
  lit16 0100 bitset                     # str n cc bs
  [ sget02 sget02 .<< sset02            # c bs cc
    =0     sset01 goto ]                # str n cc bs f
  sget04 .reduce                        # str n cc bs

  lit8+20 i.heap_allocate               # str n cc bs &p
  $char_many_parser_class sget01
                          m64set        # str n cc bs &p [.vt=]
  sget03  sget01 =8      iplus m32set   # str n cc bs &p [.mincount=]
  sget01  sget01 lit8+12 iplus m64set   # str n cc bs &p [.chars=]
  sset03 drop sset00 goto               # &p };

BEGIN
{
  bin_macros->{psomeof}  = bin q{=0 patleast};
  bin_macros->{pmanyof}  = bin q{=1 patleast};
}


use phi::fn char_parser_test => bin q{  # cc
  "abcabdefcFOO" =1     strpos        # cc in pos
  "abc" poneof .parse                 # cc {v='b i=2}
  dup .index =2     ieq "charpi2" i.assert
  dup .value lit8'b ieq "charpvb" i.assert
  drop

  "abcabdefcFOO" =0 strpos            # cc in pos
  "abc" pmanyof .parse                # cc {v="abcab" i=5}
  dup .index =5 ieq "charpi5" i.assert
  dup .value "abcab" .== "charpvabcab" i.assert
  drop

  goto                                # };


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

use phi::class alt_parser =>
  binary_parser_protocol,
  parser_protocol,

  left  => bin q{swap =8  iplus m64get swap goto},
  right => bin q{swap =16 iplus m64get swap goto},

  parse => bin q{                     # in pos self cc
    sget03 sget03 sget03 .left .parse # in pos self cc pos'
    dup .fail?                        # in pos self cc pos' fail?

    [ drop                            # in pos self cc
      sget01 .right sset01            # in pos right cc
      sget01 m64get :parse goto ]     # ->right.parse

    [ sset03 sset01 drop goto ]       # pos'
    if goto                           # pos' };


use phi::fn palt => bin q{              # left right cc
  =24 i.heap_allocate                   # left right cc &p
  $alt_parser_class sget01 m64set       # left right cc &p [.vt=]
  sget03  sget01 =8  iplus m64set       # left right cc &p [.left=]
  sget02  sget01 =16 iplus m64set       # left right cc &p [.right=]
  sset02 sset00 goto                    # &p };


use phi::fn alt_parser_test => bin q{   # cc
  "foobar" =0 strpos                    # cc in pos
  "foo" pstr
  "bar" pstr palt .parse                # cc {v="foo" i=3}

  dup .index lit8+3 ieq "altpi3" i.assert
  dup .value "foo" .== "altpvfoo" i.assert
  drop

  "foobar" =0 strpos                    # cc in pos
  "ba" pstr
  "foob" pstr palt .parse               # cc {v="foob" i=4}

  dup .index =4 ieq "altpi4" i.assert
  dup .value "foob" .== "altvfoob" i.assert
  drop

  goto                                  # };


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

use phi::class seq_parser =>
  seq_parser_protocol,
  binary_parser_protocol,
  parser_protocol,

  combiner => bin q{swap =8  iplus m64get swap goto},
  left     => bin q{swap =16 iplus m64get swap goto},
  right    => bin q{swap =24 iplus m64get swap goto},

  combine => bin q{                   # v1 v2 self cc
    swap .combiner goto               # f(v1 v2 cc) },

  parse => bin q{                     # in pos self cc
    sget03 sget03 sget03 .left .parse # in pos self cc pos'

    # Important: don't invoke the second parser if the first one fails.
    dup .fail?                        # in pos self cc pos' fail?

    [ sset03 sset01 drop goto ]       # pos'

    [ sget04 sget01 sget04 .right     # in pos self cc pos' in pos' right
      .parse                          # in pos self cc p1 p2

      # Also important: don't invoke the combiner if the second parse fails.
      dup .fail?                      # in pos self cc p1 p2 fail2?

      [ sset04 drop sset01 drop goto ]# p2
      [                               # in pos self cc p1 p2
        sget01 .value                 # in pos self cc p1 p2 v1
        sget01 .value                 # in pos self cc p1 p2 v1 v2
        sget05 .combine               # in pos self cc p1 p2 v'
        swap .with_value              # in pos self cc p1 p'
        sset04 drop sset01 drop goto ]# p'

      if goto ]                       # pos
    if goto                           # pos };


use phi::fn pseq => bin q{              # left right combiner cc
  =32     i.heap_allocate               # left right combiner cc &p
  $seq_parser_class sget01 m64set       # left right combiner cc &p [.vt=]
  sget02  sget01 =8      iplus m64set   # left right combiner cc &p [.combiner=]
  sget04  sget01 =16     iplus m64set   # left right combiner cc &p [.left=]
  sget03  sget01 =24     iplus m64set   # left right combiner cc &p [.right=]
  sset03 sset01 drop goto               # &p };

use phi::fn seq_parser_test => bin q{   # cc
  "foobar" =0     strpos              # cc in pos
  "foo" pstr                          # cc in pos p1
  "bar" pstr                          # cc in pos p1 p2
  [                                   # v1 v2 cc
    "," sget03 .+                     # v1 v2 cc "v1,"
    sget02 swap .+                    # v1 v2 cc "v1,v2"
    sset02 sset00 goto ]              # cc in pos fn
  pseq .parse                         # cc {v="foo,bar" i=6}

  dup .index =6 ieq "seqpi6" i.assert
  dup .value "foo,bar" .== "seqpvfoo,bar" i.assert
  drop

  # Test failure cases
  "foobar" =1 strpos
  "foo" pstr "bar" pstr
  [ "should never be called" i.die ]
  pseq .parse

  .fail? "seqpfail1" i.assert

  "foobar" =1 strpos
  "oo" pstr "baR" pstr
  [ "should never be called" i.die ]
  pseq .parse

  .fail? "seqpfail2" i.assert

  goto                                # };


=head3 Mapping and flatmapping
The C<map> parser transforms its output on success; nothing too complicated.

C<flatmap> invokes your function on C<in pos pos'> and expects C<pos''> as a
result. You can use this to write computed grammars.

Struct definitions:

  struct map_parser
  {
    hereptr      vtable;
    hereptr<fn>  f;
    parser      *p;
  }

  struct flatmap_parser
  {
    hereptr      vtable;
    hereptr<fn>  f;
    parser      *p;
  }

=cut

use phi::class map_parser =>
  parser_protocol,
  parser_transform_protocol,
  fn_parser_protocol,

  fn     => bin q{swap =8  iplus m64get swap goto},
  parser => bin q{swap =16 iplus m64get swap goto},

  parse => bin q{                     # in pos self cc
    sget03 sget03 sget03 .parser
    .parse dup .fail?                 # in pos self cc pos' fail?

    [ sset03 sset01 drop goto ]       # fail
    [ dup .value sget03 .fn call      # in pos self cc pos' v'
      swap .with_value                # in pos self cc pos'
      sset03 sset01 drop goto ]       # pos'
    if goto                           # pos' };


use phi::class flatmap_parser =>
  parser_protocol,
  parser_transform_protocol,
  fn_parser_protocol,

  fn     => bin q{swap =8  iplus m64get swap goto},
  parser => bin q{swap =16 iplus m64get swap goto},

  parse => bin q{                     # in pos self cc
    sget03 sget03                     # in pos self cc in pos
    sget01 sget01                     # in pos self cc in pos in pos
    sget05 .parser .parse             # in pos self cc in pos pos'
    dup .fail?                        # in pos self cc in pos pos' fail?

    [ sset05 drop drop sset01 drop goto ] # fail
    [ sget04 .fn call                 # in pos self cc pos''
      sset03 sset01 drop goto ]       # pos''
    if goto                           # pos'' };


use phi::fn pmap => bin q{              # p f cc
  lit8+24 i.heap_allocate               # p f cc &m
  $map_parser_class sget01 m64set       # [.vt=]
  sget02 sget01 =8  iplus m64set        # [.fn=]
  sget03 sget01 =16 iplus m64set        # [.parser=]
  sset02 sset00 goto                    # &m };

use phi::fn pflatmap => bin q{          # p f cc
  lit8+24 i.heap_allocate               # p f cc &m
  $flatmap_parser_class sget01 m64set   # [.vt=]
  sget02 sget01 =8  iplus m64set        # [.fn=]
  sget03 sget01 =16 iplus m64set        # [.parser=]
  sset02 sset00 goto                    # &m };


use phi::fn map_parser_test => bin q{   # cc
  "foobar" =1     strpos                # cc in pos
  "ooba" pstr
  [ swap .length swap goto ] pmap
  .parse                                # cc {v=4 i=5}

  dup .index lit8+5 ieq "mappi5" i.assert
  dup .value lit8+4 ieq "mappv4" i.assert
  drop

  "fOOBAR" =1     strpos                # cc in pos
  "ooba" pstr
  [ "shouldn't be called" i.die ] pmap
  .parse                                # cc fail

  .fail? "mappfail" i.assert

  goto                                  # };

use phi::fn flatmap_parser_test => bin q{ # cc
  "foobar" =1 strpos                    # cc in pos
  "ooba" pstr                           # cc in pos p
  [                                     # in pos pos' cc
    sget01 .index =5 ieq "flatmapp5"        i.assert
    sget02 .index =1 ieq "flatmapppos1"     i.assert
    sget03 "foobar"  .== "flatmappinfoobar" i.assert
    sget01 .value "ooba" .== "flatmappinooba" i.assert

    sget03 sget02                       # in pos pos' cc in pos'
    "r" pstr .parse                     # in pos pos' cc pos''
    sset03 sset01 drop goto ]           # cc in pos p f

  pflatmap .parse                       # cc "r" 6

  dup .index lit8+6 ieq "flatmappi6" i.assert
  dup .value "r"    .== "flatmappvr" i.assert
  drop

  "fOOBAR" =1 strpos                    # cc in pos
  "ooba" pstr
  [ "shouldn't be called" i.die ] pflatmap
  .parse                                # cc 0 -1

  .fail? "flatmappfail" i.assert

  goto                                  # };


use phi::fn parser_test => bin q{
  str_parser_test     "strp "  =2 i.print_string_fd
  char_parser_test    "charp " =2 i.print_string_fd
  alt_parser_test     "altp "  =2 i.print_string_fd
  seq_parser_test     "seqp "  =2 i.print_string_fd
  map_parser_test     "mapp "  =2 i.print_string_fd
  flatmap_parser_test "fmapp " =2 i.print_string_fd
  goto };


1;