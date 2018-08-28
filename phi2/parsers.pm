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
  "0 " =0 strpos decimal_integer .parse
  .value =0 ieq "int0" i.assert

  "65536 " =0 strpos decimal_integer .parse # {v=65536}
  dup .fail? inot "int_nofail" i.assert
  dup .value lit32 00010000 ieq "int65536" i.assert
  drop                                  # };


=head3 Strings
The usual stuff, using a string buffer for backing. It's safe to use a mutable
object with a C<rep> parser because C<rep> fully commits to each function call.
=cut

use phi::genconst dquote_string => bin q{ strbuf =34_ .append_int8 .to_string };
use phi::genconst dquote_parser => bin q{ dquote_string poneof };

use phi::genconst bslash_string => bin q{ strbuf =92_ .append_int8 .to_string };

use phi::genconst escaped_string_char_map => bin q{
  intmap
    =10_ =110_ .{}=                     # \n
    =13_ =114_ .{}=                     # \r
    =9_  =116_ .{}=                     # \t
    =92_ =92_  .{}=                     # \\
    =34_ =34_  .{}=                     # \"
    =0_  =48_  .{}=                     # \0 };

use phi::genconst escaped_string_char => bin q{
  byteset .~ poneset
  [ _escaped_string_char_map .{} _ goto ] pmap };

use phi::genconst string_char => bin q{
  byteset =34_ .<< =92_ .<< .~ poneset  # passthrough chars [^\"]
  byteset =92_ .<< poneset
  escaped_string_char pseq_return       # escaped chars
  palt                                  # union };

use phi::genconst escaped_string_body => bin q{
  string_char
  [ strbuf_ goto ]
  [ sget02 sget02 .append_int8
    sset02 sset00 goto ]
  [ _ .to_string _ goto ]
  prep
  pnone [ "" sset01 goto ] pmap
  palt };

use phi::genconst escaped_string => bin q{
  dquote_parser
  escaped_string_body pseq_return
  dquote_parser       pseq_ignore };


use phi::testfn escaped_string => bin q{
  strbuf =34_ .append_int8
         =34_ .append_int8 .to_string
  =0 strpos escaped_string .parse
  dup .fail? inot "str0fail" i.assert
  dup .value "" .== "str0contents" i.assert
  drop

  strbuf =34_   .append_int8
         "foo"_ .append_string
         =34_   .append_int8 .to_string
  =0 strpos escaped_string .parse
  dup .fail? inot "str1fail" i.assert
  dup .value "foo" .== "str1contents" i.assert
  drop

  strbuf =34_     .append_int8
         "foo\n"_ .append_string
         =34_     .append_int8 .to_string
  =0 strpos escaped_string .parse
  dup .fail? inot "str2fail" i.assert
  dup .value
    strbuf "foo"_ .append_string
           =10_   .append_int8 .to_string .==
    "str2contents" i.assert
  drop

  strbuf =34_            .append_int8
         "foo\nbar\t\0"_ .append_string
         bslash_string_  .append_string
         =34_            .append_int8
         =34_            .append_int8 .to_string
  =0 strpos escaped_string .parse

  dup .fail? inot "str3fail" i.assert
  dup .value
    strbuf "foo"_ .append_string
           =10_   .append_int8
           "bar"_ .append_string
           =9_    .append_int8
           =0_    .append_int8
           =34_   .append_int8 .to_string .==
    "str3contents" i.assert
  drop };


=head3 Symbols/barewords
No scope integration here; this is just reading them as strings.

TODO: fix this to allow digits in non-first positions
=cut

use phi::genconst ident_chars => bin q{
  strbuf "abcdefghijklmnopqrstuvwxyz"_ .append_string
         "ABCDEFGHIJKLMNOPQRSTUVWXYZ"_ .append_string
         "_"_                          .append_string .to_string };

use phi::genconst ident_symbol => bin q{ ident_chars poneof prep_bytes };


use phi::testfn ident_symbol => bin q{
  "foobar bif baz" =0 strpos ident_symbol .parse
  dup .fail? inot          "isymfail" i.assert
  dup .value "foobar" .== "isymvalue" i.assert
  drop };


1;
