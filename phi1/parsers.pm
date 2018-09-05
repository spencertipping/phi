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

use phi::protocol linear_position => qw/ + index /;

use phi::protocol parser           => qw/ parse /;
use phi::protocol string_parser    => qw/ text /;
use phi::protocol binary_parser    => qw/ left right /;
use phi::protocol seq_parser       => qw/ combine combiner /;
use phi::protocol char_parser      => qw/ chars /;
use phi::protocol repeat_parser    => qw/ mincount /;
use phi::protocol reducing_parser  => qw/ init_fn next_fn last_fn /;
use phi::protocol parser_transform => qw/ parser /;
use phi::protocol fn_parser        => qw/ fn /;


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

use phi::class fail_parser =>
  parser_protocol,
  parse => bin q{ fail_instance sset03 sset01 drop goto };

use phi::constQ pfail => fail_parser_class->fn >> heap;


=head3 Generic none-parser
Parses nothing, successfully.
=cut

use phi::class none_parser =>
  parser_protocol,
  parse => bin q{                       # in pos self cc
    sset02 drop swap goto               # pos };

use phi::constQ pnone => none_parser_class->fn >> heap;


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

use phi::binmacro strpos => bin q{=0 string_position};


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
    sget02 .text .size iplus          # input pos self cc end
    sget04 .size ilt                  # input pos self cc end<len?

    [ sset01 drop                     # input cc
      $fail_instance sset01 goto ]    # fail

    [ # Now run the loop to compare individual characters.
                                      # input pos self cc
      swap .text dup .size            # input pos cc text len
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

use phi::testfn str_parser => bin q{    #
  "foobar" =2 strpos                    # in pos
  "ob" pstr .parse                      # {v="ob" i=4}

  dup .index =4 ieq "strpi4" i.assert
  dup .value "ob" .== "strpvfoo" i.assert
  drop                                  #

  "foobar" =2 strpos                    # in pos
  "ba" pstr .parse                      # fail
  .fail? "strpfail" i.assert

  "foobar" =1 strpos                    # in pos
  "oobar" pstr .parse                   # {v="oobar" i=6}
  dup .index =6 ieq "strpi6" i.assert
  dup .value "oobar" .== "strpvoobar" i.assert
  drop };


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
    sget03 .size sget03 .index ilt    # in pos self cc pos<len?

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
    sget03 .size sget03 .index        # in pos self cc l posi
    sget03 .mincount iplus swap ilt   # in pos self cc (pos+min)>l?

    [ sset01 drop                     # in cc
      $fail_instance sset01 goto ]    # fail

    [ # Now see how many characters we can match. Then we'll allocate a byte
      # string and copy the region.
      sget03 .data                    # in pos self cc &d
      sget02 .chars sget04 .index     # in pos self cc &d cs posi
      sget06 .size swap               # in pos self cc &d cs l posi

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


use phi::fn poneset => bin q{           # bs cc
  =16 i.heap_allocate                   # bs cc &p
  $char_one_parser_class sget01 m64set  # bs cc &p [.vt=]
  sget02 sget01 =8 iplus m64set         # bs cc &p [.chars=]
  sset01 goto                           # &p };

use phi::fn pmanyset => bin q{          # bs n cc
  =20 i.heap_allocate                   # bs n cc &p
  $char_many_parser_class sget01 m64set # bs n cc &p [.vt=]
  sget02 sget01 =8 iplus m32set         # bs n cc &p [.mincount=]
  sget03 sget01 =12 iplus m64set        # bs n cc &p [.chars=]
  sset02 sset00 goto                    # &p };

use phi::fn poneof => bin q{            # str cc
  sget01 .byte_bitset                   # str cc bs
  poneset                               # str cc p
  sset01 goto                           # p };

use phi::fn patleast => bin q{          # str n cc
  sget02 .byte_bitset                   # str n cc bs
  sget02 pmanyset                       # str n cc p
  sset02 sset00 goto                    # p };

use phi::binmacro psomeof => bin q{=0 patleast};
use phi::binmacro pmanyof => bin q{=1 patleast};


use phi::testfn char_parser => bin q{ #
  "abcabdefcFOO" =1     strpos        # in pos
  "abc" poneof .parse                 # {v='b i=2}
  dup .index =2     ieq "charpi2" i.assert
  dup .value lit8'b ieq "charpvb" i.assert
  drop

  "abcabdefcFOO" =0 strpos            # in pos
  "abc" pmanyof .parse                # {v="abcab" i=5}
  dup .index =5 ieq "charpi5" i.assert
  dup .value "abcab" .== "charpvabcab" i.assert
  drop };


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

use phi::testfn alt_parser => bin q{    #
  "foobar" =0 strpos                    # in pos
  "foo" pstr
  "bar" pstr palt .parse                # {v="foo" i=3}

  dup .index lit8+3 ieq "altpi3" i.assert
  dup .value "foo" .== "altpvfoo" i.assert
  drop

  "foobar" =0 strpos                    # in pos
  "ba" pstr
  "foob" pstr palt .parse               # {v="foob" i=4}

  dup .index =4 ieq "altpi4" i.assert
  dup .value "foob" .== "altvfoob" i.assert
  drop };


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

use phi::binmacro pseq_ignore => bin q{ [ sset00      goto ] pseq };
use phi::binmacro pseq_return => bin q{ [ sset01 swap goto ] pseq };
use phi::binmacro pseq_cons   =>
  bin q{ [ sget02 sget02 :: sset02 sset00 goto ] pseq };

use phi::testfn seq_parser => bin q{  #
  "foobar" =0     strpos              # in pos
  "foo" pstr                          # in pos p1
  "bar" pstr                          # in pos p1 p2
  [                                   # v1 v2
    "," sget03 .+                     # v1 v2 "v1,"
    sget02 swap .+                    # v1 v2 "v1,v2"
    sset02 sset00 goto ]              # in pos fn
  pseq .parse                         # {v="foo,bar" i=6}

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

  .fail? "seqpfail2" i.assert };


=head3 Repetition
A simple way to repeatedly apply a parser. There's a one-element minimum.

  struct rep_parser
  {
    hereptr vtable;
    parser* p;                          # -> b
    fn*     init;                       # () -> a
    fn*     next;                       # (a, b) -> b
    fn*     last;                       # b -> c
  };

=cut

use phi::class rep_parser =>
  parser_protocol,
  reducing_parser_protocol,
  parser_transform_protocol,

  parser  => bin q{_=8  iplus m64get_ goto},
  init_fn => bin q{_=16 iplus m64get_ goto},
  next_fn => bin q{_=24 iplus m64get_ goto},
  last_fn => bin q{_=32 iplus m64get_ goto},

  parse  => bin q{                      # in p0 self cc
    sget01 .parser                      # in p0 self cc p

    # Try the parser once before we generate the initial value.
    sget04 sget04 sget02 .parse         # in p0 self cc p p1
    dup .fail?
    [ drop sset04 drop sset01 drop goto ]    # p1
    [ goto ]                            # in p0 self cc p p1
    if call                             # in p0 self cc p p1

    sget03 .init_fn call                # in p0 self cc p p1 x0
    sget01 .value _                     # in p0 self cc p p1 v1 x0
    sget05 .next_fn call                # in p0 self cc p p1 x1
    sget04 .next_fn                     # in p0 self cc p pi xi f

    [                                   # in p0 self cc p pi xi f loop
      sget08 sget04 sget06 .parse       # in p0 self cc p pi xi f loop pi+1
      dup .fail?

      [ drop drop drop                  # in p0 self cc p pi xi
        sget04 .last_fn call            # in p0 self cc p pi v
        _.with_value                    # in p0 self cc p pi'
        sset04 drop sset01 drop goto ]  # pi'

      [ sset03                          # in p0 self cc p pi+1 xi f loop
        sget02 sget04 .value _          # in p0 self cc p pi+1 xi f loop vi+1 xi
        sget03 call                     # in p0 self cc p pi+1 xi f loop xi+1
        sset02                          # in p0 self cc p pi+1 xi+1 f loop
        dup goto ]                      # ->loop
      if goto ]                         # in p0 self cc p pi xi f loop
    dup goto                            # ->loop };

use phi::fn prep => bin q{              # p init next last cc
  =40 i.heap_allocate                   # p i n l cc rp
  $rep_parser_class sget01 m64set       # [.vt=]
  sget05 sget01 =8  iplus m64set        # [.parser=]
  sget04 sget01 =16 iplus m64set        # [.init_fn=]
  sget03 sget01 =24 iplus m64set        # [.next_fn=]
  sget02 sget01 =32 iplus m64set        # [.last_fn=]
  sset04 sset02 drop drop goto          # rp };

use phi::binmacro prep_intlist => bin q{# p
  [ intlist _ goto ]                    # p init
  [                                     # x xs cc
    sget02 sget02 .<< sset02            # xs xs cc
    sset00 goto ]                       # p init next
  [ _ .rev _ goto ]                     # p init next last
  prep                                  # p' };

use phi::binmacro prep_bytes => bin q{  # p
  [ strbuf _ goto ]                     # p init
  [ sget02 sget02 .append_int8
    sset02 sset00 goto ]                # p init next
  [ _ .to_string _ goto ]               # p init next last
  prep                                  # p' };

use phi::binmacro prep_ignore => bin q{ # p
  [ =0_ goto ]                          # p init
  [ sset00 goto ]                       # p init next
  [ goto ]                              # p init next last
  prep                                  # p' };

use phi::testfn rep_parser => bin q{    #
  "aaab" =0 strpos                      # in pos
  "a" pstr prep_intlist .parse          # pos'

  dup .fail? inot "repfail" i.assert
  dup .index =3 ieq "repi3" i.assert
  .value                                # ["a" "a" "a"]
  dup .length =3 ieq "rep3" i.assert
  dup =0_ .[] "a" .== "repa0" i.assert
  dup =1_ .[] "a" .== "repa1" i.assert
  dup =2_ .[] "a" .== "repa2" i.assert
  drop                                  #

  "caab" =0 strpos                      # in pos
  "ac" poneof prep_intlist .parse       # pos'

  dup .fail? inot "repfail" i.assert
  dup .index =3 ieq "repi3" i.assert
  .value                                # ["c" "a" "a"]
  dup .length =3 ieq "rep3" i.assert
  dup =0_ .[] =99 ieq "repc0" i.assert
  dup =1_ .[] =97 ieq "repa1" i.assert
  dup =2_ .[] =97 ieq "repa2" i.assert
  drop                                  #

  "caab" =0 strpos
  "a" poneof
  [ "shouldn't be called: init" i.die ]
  [ "shouldn't be called: next" i.die ]
  [ "shouldn't be called: last" i.die ]
  prep .parse
  .fail? "prep_fail" i.assert           # };


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


use phi::testfn map_parser => bin q{    #
  "foobar" =1     strpos                # in pos
  "ooba" pstr
  [ swap .size swap goto ] pmap
  .parse                                # {v=4 i=5}

  dup .index lit8+5 ieq "mappi5" i.assert
  dup .value lit8+4 ieq "mappv4" i.assert
  drop

  "fOOBAR" =1     strpos                # in pos
  "ooba" pstr
  [ "shouldn't be called" i.die ] pmap
  .parse                                # fail

  .fail? "mappfail" i.assert };

use phi::testfn flatmap_parser => bin q{#
  "foobar" =1 strpos                    # in pos
  "ooba" pstr                           # in pos p
  [                                     # in pos pos'
    sget01 .index =5 ieq "flatmapp5"        i.assert
    sget02 .index =1 ieq "flatmapppos1"     i.assert
    sget03 "foobar"  .== "flatmappinfoobar" i.assert
    sget01 .value "ooba" .== "flatmappinooba" i.assert

    sget03 sget02                       # in pos pos' in pos'
    "r" pstr .parse                     # in pos pos' pos''
    sset03 sset01 drop goto ]           # in pos p f

  pflatmap .parse                       # "r" 6

  dup .index lit8+6 ieq "flatmappi6" i.assert
  dup .value "r"    .== "flatmappvr" i.assert
  drop

  "fOOBAR" =1 strpos                    # in pos
  "ooba" pstr
  [ "shouldn't be called" i.die ] pflatmap
  .parse                                # 0 -1

  .fail? "flatmappfail" i.assert };


1;
