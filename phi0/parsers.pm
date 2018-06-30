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
  bin_macros->{str} = bin q{$str_fn call};
}


use constant parser_test_fn => phi::allocation
  ->constant(bin q{                     # cc
    "foobar" const2                     # cc in pos
    "ob" str .parse                     # cc "ob" 4

    const4 ieq i.assert
    "ob"   .== i.assert                 # cc

    "foobar" const2                     # cc in pos
    "ba" str .parse                     # cc 0 -1

    const1 ineg ieq i.assert
    const0      ieq i.assert            # cc

    "foobar" const1                     # cc in pos
    "oobar" str .parse                  # cc "oobar" 6
    lit8 +6 ieq i.assert
    "oobar" .== i.assert                # cc

    goto })
  ->named('parser_test_fn') >> heap;


1;
