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
  =256 i1d =32_ .<<
           =10_ .<<
           =13_ .<<
           =9_  .<<
  =1 pmanyset };

use phi::genconst hash_line_comment => bin q{
  "#" pstr
  $nl_string string_to_bitset .~ =0 pmanyset pseq_ignore
  $nl_string poneof pnone palt pseq_ignore };

use phi::genconst hash_comment_ignore => bin q{
  whitespace hash_line_comment palt prep_ignore
  pnone palt };


use phi::testfn hash_comment_ignore => bin q{
  strbuf
    "  "_       .+=
    =9_         .<<
    =10_        .<<
    "# foobar"_ .+=
    =10_        .<<
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
  "0 " =0 strpos decimal_integer .parse
    dup .index =1 ieq "intpos1"     i.assert
    dup .fail? inot   "int_nofail0" i.assert
    dup .value =0 ieq "int0"        i.assert
  drop

  "65536 " =0 strpos decimal_integer .parse # {v=65536}
    dup .fail? inot               "int_nofail" i.assert
    dup .index =5 ieq             "int_index5" i.assert
    dup .value lit32 00010000 ieq "int65536"   i.assert
  drop                                  # };


=head3 Strings
The usual stuff, using a string buffer for backing. It's safe to use a mutable
object with a C<rep> parser because C<rep> fully commits to each function call.
=cut

use phi::genconst dquote_string => bin q{ strbuf =34_ .<< .to_string };
use phi::genconst dquote_parser => bin q{ dquote_string poneof };

use phi::genconst bslash_string => bin q{ strbuf =92_ .<< .to_string };

use phi::genconst escaped_string_char_map => bin q{
  =256 i8d
    dup =10_ lit8'n _ .[]= drop
    dup =13_ lit8'r _ .[]= drop
    dup =9_  lit8't _ .[]= drop
    dup =92_ lit8'\ _ .[]= drop
    dup =34_ lit8'" _ .[]= drop
    dup =0_  lit8'0 _ .[]= drop };

use phi::genconst escaped_string_char => bin q{
  =256 i1d .~ poneset
  [ _escaped_string_char_map .[] _ goto ] pmap };

use phi::genconst string_char => bin q{
  =256 i1d =34_ .<< =92_ .<< .~ poneset # passthrough chars [^\"]
  =256 i1d =92_ .<< poneset
  escaped_string_char pseq_return       # escaped chars
  palt                                  # union };

use phi::genconst escaped_string_body => bin q{
  string_char prep_bytes
  pnone [ "" sset01 goto ] pmap
  palt };

use phi::genconst escaped_string => bin q{
  dquote_parser
  escaped_string_body pseq_return
  dquote_parser       pseq_ignore };


use phi::testfn escaped_string => bin q{
  strbuf =34_ .<<
         =34_ .<< .to_string
  =0 strpos escaped_string .parse
  dup .fail? inot "str0fail" i.assert
  dup .value "" .== "str0contents" i.assert
  drop

  strbuf =34_   .<<
         "foo"_ .+=
         =34_   .<< .to_string
  =0 strpos escaped_string .parse
  dup .fail? inot "str1fail" i.assert
  dup .value "foo" .== "str1contents" i.assert
  drop

  strbuf =34_     .<<
         "foo\n"_ .+=
         =34_     .<< .to_string
  =0 strpos escaped_string .parse
  dup .fail? inot "str2fail" i.assert
  dup .value
    strbuf "foo"_ .+=
           =10_   .<< .to_string .==
    "str2contents" i.assert
  drop

  strbuf =34_            .<<
         "foo\nbar\t\0"_ .+=
         bslash_string_  .+=
         =34_            .<<
         =34_            .<< .to_string
  =0 strpos escaped_string .parse

  dup .fail? inot "str3fail" i.assert
  dup .value
    strbuf "foo"_ .+=
           =10_   .<<
           "bar"_ .+=
           =9_    .<<
           =0_    .<<
           =34_   .<< .to_string .==
    "str3contents" i.assert
  drop };


=head3 Symbols/barewords
No scope integration here; this is just reading them as strings.
=cut

use phi::genconst ident_chars => bin q{
  strbuf "abcdefghijklmnopqrstuvwxyz"_ .+=
         "ABCDEFGHIJKLMNOPQRSTUVWXYZ"_ .+=
         "0123456789"_                 .+=
         "_"_                          .+= .to_string };

use phi::genconst ident_symbol => bin q{ ident_chars pmanyof };


use phi::testfn ident_symbol => bin q{
  "foobar bif baz" =0 strpos ident_symbol .parse
  dup .fail? inot          "isymfail" i.assert
  dup .value "foobar" .== "isymvalue" i.assert
  drop

  "bswap16" =0 strpos ident_symbol .parse
  dup .fail? inot "bssymfail" i.assert
  dup .value "bswap16" .== "bswap16symvalue" i.assert
  drop };


1;
