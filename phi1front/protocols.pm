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


=head2 Parsers in general
Nothing about phi's frontend demands that we parse text -- you could write a
dialect that interacted with any other format easily enough -- but phi2 is
written in text so it's worth defining a parser library for it. The usual
suspects in parsing expression grammars, plus some transforms for computed
elements (implementations in L<phi1front/parsers.pm>):
=cut

use constant parse_position_protocol => phi::protocol->new('parse_position',
  qw/ fail? /);

use constant linear_position_protocol => phi::protocol->new('linear_position',
  qw/ +
      index /);

use constant parser_protocol => phi::protocol->new('parser',
  qw/ parse /);

use constant string_parser_protocol => phi::protocol->new('string_parser',
  qw/ text /);

use constant binary_parser_protocol => phi::protocol->new('binary_parser',
  qw/ left
      right /);

use constant seq_parser_protocol => phi::protocol->new('seq_parser',
  qw/ combine
      combiner /);

use constant char_parser_protocol => phi::protocol->new('char_parser',
  qw/ chars /);

use constant repeat_parser_protocol => phi::protocol->new('repeat_parser',
  qw/ mincount /);

use constant parser_transform_protocol => phi::protocol->new('parser_transform',
  qw/ parser /);

use constant fn_parser_protocol => phi::protocol->new('fn_parser',
  qw/ fn /);


1;
