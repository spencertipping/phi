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

package phiposix;
use strict;
use warnings;

use Exporter qw/import/;
use POSIX ();
use phiboot;
use phibootmacros;

our @EXPORT =
our @EXPORT_OK = qw/ i_open i_read /;


# POSIX IO extension
$phiboot::i::extensions{posix_fileio} = 1;

use phi i_open => pint 0x110;
use phi i_read => pint 0x111;

$phiboot::i::insns[0x110] = sub
{
  my ($mode, $flags, $path) = ($_[0]->pop, $_[0]->pop, $_[0]->pop);
  my $fd = POSIX::open($path->sval, $flags->ival, $mode->ival);
  $_[0]->push(pint $fd);
};

$phiboot::i::insns[0x111] = sub
{
  my ($size, $offset, $buf, $fd) = ($_[0]->pop, $_[0]->pop,
                                    $_[0]->pop, $_[0]->pop);
  my $bytes = POSIX::read($fd->ival, my $buf2 = '', $size->ival);
  substr($buf->sval, $offset->ival, $bytes) = $buf2;
  $_[0]->push(pint $bytes);
};


1;
