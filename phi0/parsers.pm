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
    $str_parser_test_fn call
    $alt_parser_test_fn call
    $seq_parser_test_fn call
    goto })
  ->named('parser_test_fn') >> heap;


1;
