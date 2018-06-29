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
      asm lit8 goto swap .l8            # self cc asm[goto]
      .compile sset01 goto              # fn[goto] },

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
      const1 swap                       # cc fixed? field_kvs
      [                                 # kv fixed? cc
        sget02 .value .fixed_size?      # kv fixed? cc fixed?'
        [ const0 sset01                 # kv 0 cc
          const1 sset02 goto ]          # x0'=1 exit?=0
        [ const1 sset01                 # kv 1 cc
          const0 sset02 goto ]          # x0'=0 exit?=1
        if goto ]                       # cc fixed? field_kvs f
      swap .reduce                      # cc fixed?
      swap goto                         # fixed? },

    size => bin q{                      # self cc
      # This function will fail unless we're fixed-size.
      swap .fields .kv_pairs            # cc field_kvs
      const0 swap                       # cc size field_kvs
      [                                 # kv size cc
        sget02 .value .size             # kv size cc s
        sget02 iplus                    # kv size cc size'
        sset02 const0 sset01 goto ]     # x0'=size' exit?=0
      swap .reduce                      # cc size
      swap goto                         # size },

    size_fn => bin q{                   # self cc
      # Assemble a sizing function by concatenation.
      #
      # This is a little nontrivial but not as bad as it sounds. Inefficiency is
      # fine, and all we need to do is manage the stack a little. Each size
      # function has signature &obj -> size, so our general strategy looks like
      # this (TODO)

      "TODO" i.pnl const1 i.exit },

    fields => bin q{                    # self cc
      swap const8 iplus m64get .keys    # cc fields
      swap goto                         # fields },

    "fixed_offset?" => bin q{           # name self cc
      # A field has a fixed offset if everything leading up to it has a fixed
      # size, which we can figure out using a list reduction against the set of
      # keys.
      #
      # Ordinarily we'd need to build a closure for the reducer function, but we
      # can totally cheat here by using the x0 argument to pass in the name.

      swap .kv_pairs                    # name cc kvs
      sget02 swap                       # name cc name kvs
      [                                 # name kv cc
        sget02 sget02 .key .==          # name kv cc name==?
        [ const1 sset02                 # 1 kv cc
          const1 sset01 goto ]          # x0=1    exit?=1
        [ const0 sset01 goto ]          # x0=name exit?=0
        if goto ]                       # name cc name kvs f
      swap .reduce                      # name cc fixed?
      sset01 goto                       # fixed? },

    offset_of => bin q{                 # name self cc
      # Sum up sizes until we get to the field we're looking for. This is easier
      # to do with a manual loop than it is to reduce.
      #
      # This function will die horribly if you try to address a field that
      # doesn't exist.

      swap .kv_pairs const0 swap        # name cc offset kvs
      [                                 # name cc offset kvs loop
        sget04 sget02 .key .==          # name cc offset kvs loop name=?
        [ drop drop sset01 goto ]       # offset
        [ sget01 .value .size           # name cc offset kvs loop fsize
          sget03 iplus sset02           # name cc offset' kvs loop
          swap .tail swap               # name cc offset' kvs' loop
          dup goto ]                    # ->loop
        if goto
      ]                                 # name cc offset kvs loop
      dup goto                          # ->loop },

    offsetfn_of => bin q{
      "TODO" i.pnl const1 i.exit });


1;
