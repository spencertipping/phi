#!/usr/bin/env perl

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

use v5.14;
use strict;
use warnings;
use bytes;

use File::Temp 'tempfile';

use phi0;

use phi1::asm;
use phi1::fn;
use phi1::oop;
use phi1::class;
use phi1::frame;
use phi1::sexp;

use constant ihereptr     => phi::asm->new->str($phi::bytecode_table);
use constant syscall_code => phi::asm->new->str($phi::syscall_native);

sub test($)
{
  my ($fh, $name) = tempfile;
  $fh->print(heap_image ihereptr->addr, shift->addr);
  close $fh;
  chmod 0700, $name;
  system $name and die "$name exited with $?: $!";
  unlink $name;
}

1;
