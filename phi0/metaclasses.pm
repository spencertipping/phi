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

FIXME: we don't yet have enough machinery to implement things like
hereptr-prefixed var length code segments used by bytecode objects. We also
don't have any sane way to construct a closure over C<self> to ask for field
offsets, nor do we have a way to connect nonlocal fields.

...so this is kind of a disaster and it needs to be completely redesigned.

How about back-to-front linked structs with cached offset/sizes? Then any given
field can query the tail for aux sizing data.
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
      "setter on flat_struct is unimplemented"
      i.die                             # bogus },

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
      # function has signature &field -> size, so our general strategy looks
      # like this (where the initial stack layout is &struct cc):
      #
      #   const0                        # &struct cc 0
      #   sget02 dup f0_offsetfn call   # &struct cc 0 &struct f0_offset
      #          iplus f0_sizefn call   # &struct cc 0 f0_size
      #          iplus                  # &struct cc f0_size
      #   sget02 dup f1_offsetfn call   # ...

      swap .kv_pairs                    # cc kvs
      asm .const0 swap                  # cc asm[const0] kvs
      [                                 # kv asm cc
        swap                            # kv cc asm
          .sget const2 swap .l8         # kv cc asm[sget02]
          .dup                          # kv cc asm[sget02 dup]
        # TODO
      ]

      "TODO" i.die },

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
      "TODO" i.die });


=head3 New design: reverse-consed structs
Here's the idea. Structs are built like linked lists, but in reverse. When you
cons a new field onto the head of the list, you get a new struct element placed
rightwards in memory. Cells are aware of their offset/size as they are consed
up; this saves computation later on.

This model also solves the problem of prior-reference for sizing. For example,
now we can define an inline sized array:

  struct sized_array
  {
    int16    n;
    int64[n] xs;
  }

Here's the equivalent consed structure:

  var_array
  {
    n_field = "n",
    type    = int64,
    name    = "xs",
    offset  = 2,
    tail    = int_field
    {
      size   = 2,
      offset = 0,
      name   = "n",
      tail   = nil_struct_link {}
    }
  }

Each element provides data about its offset, size, and optionally functions to
compute those things. Links also provide getter/setter functions whose
signatures are identical to C<mNget> and C<mNset>:

  getter_fn : (    &struct -> val)
  setter_fn : (val &struct ->)


=head4 Fixed struct links
Here's the struct layout:

  struct fixed_struct_link
  {
    hereptr              vtable;
    baseptr<struct_link> tail;
    baseptr<string>      name;
    cell                 offset;        # -1 if computed
    cell                 size;
  }

=cut


use constant nil_struct_link_class => phi::class->new('nil_struct_link',
  maybe_nil_protocol,
  struct_link_protocol)

  ->def(
    name         => bin q{"can't call name on nil link" i.die},
    getter_fn    => bin q{"can't call getter_fn on nil link" i.die},
    setter_fn    => bin q{"can't call setter_fn on nil link" i.die},

    "nil?"       => bin q{const1 sset01 goto},

    size         => bin q{const0 sset01 goto},
    left_offset  => bin q{const0 sset01 goto},
    right_offset => bin q{const0 sset01 goto},

    size_fn         => bin q{"can't call size_fn on nil link" i.die},
    left_offset_fn  => bin q{"can't call left_offset_fn on nil link" i.die},
    right_offset_fn => bin q{"can't call right_offset_fn on nil link" i.die});


use constant nil_struct_link_instance => phi::allocation
  ->constant(pack Q => nil_struct_link_class->vtable >> heap)
  ->named('nil_struct_link_instance') >> heap;


use constant fixed_struct_link_class => phi::class->new('fixed_struct_link',
  list_protocol,
  cons_protocol,
  maybe_nil_protocol,
  struct_link_protocol)

  ->def(
    tail         => bin q{swap const8  iplus m64get swap goto},
    name         => bin q{swap const16 iplus m64get swap goto},
    left_offset  => bin q{swap const24 iplus m64get swap goto},
    size         => bin q{swap const32 iplus m64get swap goto},

    getter_fn => bin q{                 # self cc
      # Return a function whose signature is structptr -> fieldptr
      asm                               # self cc asm[]
        .sget const1 swap .l8           # self cc asm[sget01]
        sget02 .left_offset_fn          # self cc asm[sget01] lofffn
          .here swap .hereptr           # self cc asm[sget01 lit lofffn]
        .call                           # self cc asm[...call]
        .sset const1 swap .l8           # self cc asm[...call sset01]
        .goto                           # self cc asm[...sset01 goto]
      .compile                          # self cc fn[...sset01 goto]
      sset01 goto                       # fn },

    setter_fn => bin q{                 # self cc
      # Return a function that does a memcpy to set the value.
      "TODO: implement setter_fn for fixed_struct_link"
      i.die },

    right_offset => bin q{              # self cc
      # Is our offset computed? If so, return -1.
      sget01 .left_offset dup           # self cc loff loff
      const1 ineg ieq                   # self cc loff computed?
      [ drop const1 ineg sset01 goto ]  # -1
      [ sget02 .size iplus              # self cc roff
        sset01 goto ]                   # roff
      if goto                           # off|-1 },

    "nil?" => bin q{sset00 const0 swap goto},
    head   => bin q{swap .name swap goto},

    "[]"   => bin q{                    # i self cc
      sget02                            # i self cc i
      [ sget02 const1 ineg iplus        # i self cc i-1
        sset02 swap .tail swap          # i-1 tail cc
        sget01 m64get :[] goto ]        # tail.[](i-1)
      [ sset01 swap goto ]
      if goto },

    length => bin q{                    # self cc
      swap .tail .length                # cc tail.len
      const1 iplus swap goto            # tail.len+1 },

    reduce => bin q{                    # x0 f self cc
      sget01 sget04 sget04 call         # x0 f self cc x0' exit?
      [ sset03 sset01 drop goto ]       # x0
      [ sset03 swap .tail swap          # x0' f tail cc
        sget01 m64get :reduce goto ]    # tail.reduce(...)
      if goto                           # x0 },

    size_fn => bin q{                   # self cc
      # Return our size as a constant
      asm                               # self cc asm
        .lit32                          # self cc asm[lit32]
        sget02 .size                    # self cc asm[lit32] size
        bswap32 swap .l32               # self cc asm[lit32 size]
        .sset                           # self cc asm[lit32 size sset]
        const1 swap .l8                 # self cc asm[lit32 size sset01]
        .goto                           # self cc asm[lit32 size sset01 goto]
      .compile                          # self cc fn[lit32 size sset01 goto]
      sset01 goto                       # fn },

    right_offset_fn => bin q{           # self cc
      # This is pretty simple: call the left offset function and then add a
      # constant to whatever it returns. Our stack looks like this:
      #
      #   &structptr cc
      #
      # ...so we want to do this:
      #
      #   sget01                        # ptr cc ptr
      #   $left_offset_fn call          # ptr cc loff
      #   lit32 oursize iplus           # ptr cc roff
      #   sset01 goto                   # roff

      asm                               # self cc asm[]
        .sget                           # self cc asm[sget]
        const1 swap .l8                 # self cc asm[sget01]
        sget02 .left_offset_fn          # self cc asm[sget01] fn
          .here swap .hereptr           # self cc asm[sget01 'fn]
        .call                           # self cc asm[sget01 'fn call]
        .lit32                          # self cc asm[sget01 'fn call lit32]
        sget02 .size                    # self cc asm[...lit32] size
        bswap32 swap .l32               # self cc asm[...lit32 size]
        .iplus                          # self cc asm[...size iplus]
        .sset                           # self cc asm[...iplus sset]
        const1 swap .l8                 # self cc asm[...sset01]
        .goto                           # self cc asm[...goto]
      .compile                          # self cc fn[...goto]
      sset01 goto                       # fn[...] },

    left_offset_fn => bin q{            # self cc
      sget01 .tail .right_offset_fn     # self cc fn
      sset01 goto                       # fn });


1;
