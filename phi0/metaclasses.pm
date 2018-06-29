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

The lowest-level struct is just an integer value of some sort. This is a fixed
named slot:

  struct int_struct_member
  {
    hereptr vtable;
    byte    intsize;                    # in bytes: must be 1, 2, 4, or 8
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


use constant int_struct_member_class => phi::class->new('int_struct_member',
  struct_member_protocol)

  ->def(
    getter => bin q{                    # self cc
      swap .size $int_getter_map .{}    # cc getter_insn
      asm .l8 .compile                  # self cc fn[getter]
      sset 01 goto                      # fn },

    setter => bin q{                    # self cc
      swap .size $int_setter_map .{}    # cc setter_insn
      asm .l8 .compile                  # self cc fn[setter]
      sset 01 goto                      # fn },

    "fixed_size?" => bin q{const1 sset01 goto},

    size => bin q{                      # self cc
      swap const8 iplus m8get swap goto # bytes },

    size_fn => bin q{                   # self cc
      swap .size                        # cc size
      asm lit8 drop swap .l8            # cc size asm[drop]
          lit8 lit8 swap .l8            # cc size asm[drop lit8]
                         .l8            # cc asm[drop lit8 size]
      .compile swap goto                # fn[drop lit8 size] });


=head3 Flat aggregates
A composite structure in which each member is allocated inline.

  struct flat_struct
  {
    hereptr          vtable;
    strmap<struct*> *fields;            # NB: ordered map
  }

=cut

use constant flat_struct_class => phi::class->new('flat_struct',
  struct_aggregate_protocol,
  struct_member_protocol)

  ->def(
    getter => bin q{                    # self cc
      # Structs have no getter functions as such; like C arrays, they decay to
      # self-pointers (this keeps them single-cell entries when addressed as
      # values).
      asm .compile sset 01 goto         # fn[] },

    setter => bin q{                    # self cc
      # This is more subtle. "setting" a sub-struct means we need to copy its
      # value from somewhere else. I'm going to defer on this for the moment.
      # (It isn't too difficult; we just need to emit a memcpy.)
      #
      # Note that this works only for fixed-size fields.
      "setter on flat_struct is unimplemented" i.pnl
      const1 i.exit                     # bogus },

    "fixed_size?" => bin q{             # self cc
      # We're fixed-size if every component field is.
      swap .fields .kv_pairs            # cc field_kvs
      [                                 # cc kvs loop
        sget 01 .nil?                   # cc kvs loop end?
        [ drop drop const1 swap goto ]  # 1
        [                               # cc kvs loop
          sget 01 .value .fixed_size?   # cc kvs loop fixed?
          [ sget 01 .tail sset 01       # cc kvs' loop
            dup goto ]                  # ->loop
          [ drop drop const0 swap goto ]# 0
          if goto
        ]
        if goto
      ]
      dup goto },

    size => bin q{                      # self cc
      "TODO" i.pnl const1 i.exit },

    size_fn => bin q{                   # self cc
      "TODO" i.pnl const1 i.exit },

    fields => bin q{                    # self cc
      swap const8 iplus m64get .keys    # cc fields
      swap goto                         # fields },

    "fixed_offset?" => bin q{           # name self cc
      "TODO" i.pnl const1 i.exit },

    offset_of => bin q{
      "TODO" i.pnl const1 i.exit },

    offsetfn_of => bin q{
      "TODO" i.pnl const1 i.exit });


1;
