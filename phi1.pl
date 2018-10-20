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

BEGIN { push @INC, $0 =~ s/\/[^\/]+$//r }

use phi0;

use phi1::asm;
use phi1::class;
use phi1::oop;
use phi1::frame;
use phi1::sexp;

# TEST CODE
our $ihereptr     = heap_write $phi::bytecode_table;
our $syscall_code = heap_write $phi::syscall_native;
our $ok_string    = "phi1 is a thing\n";
our $ok_addr      = heap_write $ok_string;
our $ok_len       = length $ok_string;

our $code = phi::asm->new
  ->l(0)->l(0)->l(0)->l($ok_len)->l($ok_addr)->l(1)
  ->l(1)->l($syscall_code)->back
  ->drop
  ->l(0)->l(0)->l(0)->l(0)->l(0)->l(0)
  ->l(60)->l($syscall_code)->back;

print heap_image $ihereptr, $code->addr;
