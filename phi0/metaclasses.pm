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


=head2 Structs
Structs tie memory to data. Specifically, they govern things like field offsets
and object sizing.

  struct int_struct
  {
    hereptr      vtable;
    byte         intsize;               # in bytes
    byte_string *name;
  }

=cut

use constant int_getter_map =>
  int_kvmap 1 => insn_index"m8get",
            2 => insn_index"m16get",
            4 => insn_index"m32get",
            8 => insn_index"m64get";

use constant int_setter_map =>
  int_kvmap 1 => insn_index"m8set",
            2 => insn_index"m16set",
            4 => insn_index"m32set",
            8 => insn_index"m64set";


use constant int_struct_class => phi::class->new('int_struct',
  struct_protocol)

  ->def(
    fields => bin q{                    # self cc
      swap lit8+9 iplus m64get          # cc name
      strlist .<< swap goto             # [name] },

    fgetter => bin q{                   # name self cc
      swap .size $int_getter_map .{}    # name cc getter_insn
      asm .l8 .compile                  # name self cc fn[getter]
      sset 02 sset 00 goto              # fn },

    fsetter => bin q{                   # name self cc
      swap .size $int_setter_map .{}    # name cc setter_insn
      asm .l8 .compile                  # name self cc fn[setter]
      sset 02 sset 00 goto              # fn },

    "fixed_size?" => bin q{const1 sset01 goto},

    size => bin q{                      # self cc
      swap const8 m8get swap goto       # bits });


1;
