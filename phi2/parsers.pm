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


=head2 Low-level syntax parsers
Basic stuff for hash line comments, integers, strings, etc. This provides a
starting point for defining a language.


=head3 Whitespace/comment parsers
These are what you'd expect, but the return values are meaningless to save
memory.
=cut


use phi::genconst whitespace => bin q{
  strbuf =32_ .append_int8              # space
         =10_ .append_int8              # lf
         =13_ .append_int8              # cr
         =9_  .append_int8              # tab
         .to_string
  pmanyof };

use phi::genconst hash_line_comment => bin q{
  "#" pstr
  $nl_string .byte_bitset .~ =0 pmanyset pseq_ignore
  $nl_string poneof pseq_ignore };

use phi::genconst hash_comment_ignore => bin q{
  whitespace hash_line_comment palt prep_ignore
  pnone palt };

use phi::testfn hash_comment_ignore => bin q{
  strbuf
    "  "_ .append_string
    =9_   .append_int8
    =10_  .append_int8
    "# foobar"_ .append_string
    =10_        .append_int8
    .to_string
  =0 strpos
  hash_comment_ignore .parse
    .index =13 ieq "ignorepos" i.assert };


=head3 Decimal integers
Inline-reduce into a single primitive integer value. This parser should use no
net memory aside from intermediate parse state objects.
=cut

use phi::genconst decimal_integer => bin q{
  "0123456789" poneof                   # digit
  [ =0_ goto ]                          # digit init
  [ _ =10 itimes                        # x cc x0*10
    sget02 =48 ineg iplus iplus sset01  # x0*10+x cc
    goto ]                              # digit init next

  [ goto ]                              # digit init next last
  prep                                  # p };

use phi::testfn decimal_integer => bin q{
  "0" =0 strpos decimal_integer .parse
  .value =0 ieq "int0" i.assert

  "65536" =0 strpos decimal_integer .parse # {v=12345}
  dup .fail? inot "int_nofail" i.assert
  dup .value lit32 00010000 ieq "int65536" i.assert
  drop                                  # };


1;
