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


=head3 Reverse-consed structs
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
    name    = "xs",
    n_field = "n",
    type    = int64,
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

These getter/setter functions are used by low-level structs like int/float, and
manage endian conversion when necessary. Classes may or may not rely on them
depending on how they're encoded.


=head3 Nil link
Here's the struct layout:

  struct nil_struct_link
  {
    hereptr vtable;
  }

=cut


use constant k0_fn => phi::allocation
  ->constant(bin q{ const0 sset01 goto })
  ->named('k0_fn') >> heap;


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

    size_fn         => bin q{$k0_fn sset01 goto},
    left_offset_fn  => bin q{$k0_fn sset01 goto},
    right_offset_fn => bin q{$k0_fn sset01 goto});


use constant nil_struct_link_instance => phi::allocation
  ->constant(pack Q => nil_struct_link_class->vtable >> heap)
  ->named('nil_struct_link_instance') >> heap;


=head3 Struct cons links
Struct definition:

  struct cons_struct_link               # size = 88
  {
    hereptr              vtable;        # offset = 0
    baseptr<struct_link> tail;          # offset = 8
    baseptr<string>      name;          # offset = 16
    baseptr<fn>          fget_fn;       # offset = 24
    baseptr<fn>          fset_fn;       # offset = 32

    cell                 left_offset;   # offset = 40; -1 if computed
    cell                 size;          # offset = 48; -1 if computed

    baseptr<fn>          size_fn;           # offset = 56
    baseptr<fn>          right_offset_fn;   # offset = 64

    baseptr<fn>          getter_fn;     # offset = 72
    baseptr<fn>          setter_fn;     # offset = 80
  }

C<fget_fn> and C<fset_fn> are used to form C<getter_fn> and C<setter_fn>. Their
signatures are:

  fget_fn : (&field -> val)
  fset_fn : (val &field fieldsize ->)

We need these functions to implement arrays of things or other inline
variable-sized allocations.

C<left_offset_fn> and C<right_offset_fn> are cached elements that you don't
specify. We just store them to eliminate reallocations if we ask for them
multiple times.
=cut


use constant cons_struct_link_class => phi::class->new('cons_struct_link',
  list_protocol,
  cons_protocol,
  maybe_nil_protocol,
  struct_link_protocol,
  map_protocol,
  cons_struct_link_protocol)

  ->def(
    tail        => bin q{swap const8  iplus m64get swap goto},
    name        => bin q{swap const16 iplus m64get swap goto},
    fget_fn     => bin q{swap const24 iplus m64get swap goto},
    fset_fn     => bin q{swap const32 iplus m64get swap goto},

    left_offset => bin q{swap cell8+5 iplus m64get swap goto},
    size        => bin q{swap cell8+6 iplus m64get swap goto},

    right_offset => bin q{              # self cc
      # Return -1 if our left offset or our size is computed.
      sget01 .left_offset               # self cc loff
      sget02 .size                      # self cc loff size
      sget01 const1 ineg ieq            # self cc loff size offc?
      sget01 const1 ineg ieq ior        # self cc loff size computed?

      [ drop drop const1 ineg           # self cc -1
        sset01 goto ]                   # -1
      [ iplus sset 01 ]                 # loff+size
      if goto                           # roff },

    size_fn => bin q{                   # self cc
      # If someone is asking for this function and we don't have one, generate a
      # function that returns the correct constant and save that in the field.
      sget01 cell8+7 dup m64get         # self cc &f f
      [ m64get sset01 goto ]            # f
      [                                 # self cc &f
        asm                             # self cc &f asm
          .lit32
          sget03 .size bswap32 swap .l32
          .sset .1
          .goto
        .compile                        # self cc &f fn
        sget01 m64set                   # self cc &f
        m64get sset01 goto ]            # f
      if goto                           # f },

    left_offset_fn => bin q{            # self cc
      # This function is the same as the tail's right_offset_fn, so we can
      # immediately delegate.
      swap .tail .right_offset_fn swap goto },

    right_offset_fn => bin q{           # self cc
      # If we don't have a right offset fn, generate it by doing one of two
      # things. If our right_offset is fixed, generate a constant function that
      # returns it. Otherwise call our left_offset_fn and our size_fn and sum
      # those results. Here's what that looks like:
      #
      #                                 # &struct cc (initial stack)
      #   sget01 <offset-fn> call       # &struct cc off
      #   sget02 <size-fn> call         # &struct cc off size
      #   iplus sset01 goto             # off+size

      sget01 cell8+8 dup m64get         # self cc &f f
      [ m64get sset01 goto ]            # f
      [                                 # self cc &f
        sget02 .right_offset            # self cc &f roff
        dup const1 ineg ieq             # self cc &f roff computed?

        [ sset00 asm                    # self cc &f cc' asm
            .sget .1
            sget04 .left_offset_fn
              .here swap .hereptr .call
            .sget .2
            sget04 .size_fn
              .here swap .hereptr .call
            .iplus
            .sset .1 .goto
          swap goto ]                   # self cc &f asm

        [ swap asm                      # self cc &f cc' roff asm
            .lit32
            swap bswap32 .l32
            .sset .1 .goto
          swap goto ]                   # self cc &f asm

        if call                         # self cc &f asm
        .compile sget01 m64set          # self cc &f
        m64get sset01 goto              # f
      ]
      if goto                           # f },

    getter_fn => bin q{                 # self cc
      sget01 cell8+9 dup m64get         # self cc &g g
      [ m64get sset01 goto ]            # g
      [ sget02 .generate_getter_fn      # self cc &g g
        sget01 m64set                   # self cc &g
        m64get sset01 goto ]            # g
      if goto                           # g },

    setter_fn => bin q{                 # self cc
      sget01 cell8+10 dup m64get        # self cc &s s
      [ m64get sset01 goto ]            # s
      [ sget02 .generate_setter_fn      # self cc &s s
        sget01 m64set                   # self cc &s
        m64get sset01 goto ]            # s
      if goto                           # s },

    generate_getter_fn => bin q{        # self cc
      # Compose the left field offset fn with any stored getter we may have.
      # Here's the logic we want:
      #
      #                                 # &struct cc (initial stack)
      #   swap                          # cc &struct
      #   dup <offset-fn> call          # cc &struct off
      #   iplus                         # cc &field
      #   <fget-fn> call                # cc val
      #   swap goto                     # val

      asm                               # self cc asm[]
        .swap .dup
        sget02 .left_offset_fn
          .here swap .hereptr
        .call .iplus                    # self cc asm[...iplus]

        # Do we have an fget_fn? If so, compose it in; otherwise our value is
        # the field pointer.
        sget02 .fget_fn dup             # self cc asm[...iplus] fn? fn?
        [ swap .here                    # self cc asm cc fnh
          sget02 .hereptr               # self cc asm cc asm
          .call                         # self cc asm cc asm
          drop goto ]                   # self cc asm
        [ sset00 goto ]                 # self cc asm[...iplus]
        if call                         # self cc asm[...]

        .swap .goto
      .compile                          # self cc fn[...sset01 goto]
      sset01 goto                       # fn },

    generate_setter_fn => bin q{        # self cc
      # Compose the left field offset with any stored setter we have. By default
      # we memcpy to copy data directly from the fieldptr into struct memory,
      # but we'll use fset_fn instead if that's defined. Here's how this works
      # (assuming the fset case):
      #
      #                                 # v &struct cc (initial stack)
      #   sget01 <offset-fn> call       # v &struct cc off
      #   sget02 iplus                  # v &struct cc &field
      #   sget03 swap                   # v &struct cc v &field
      #   sget03 <size-fn> call         # v &struct cc v &field fieldsize
      #   <fset-fn> call                # v &struct cc
      #   sset01 drop goto              #

      asm
        .sset .1
        sget02 .left_offset_fn
          .here swap .hereptr .call     # self cc asm[...offfn call]

        .sget .2 .iplus                 # self cc asm[...sget02 iplus]
        .sget .3 .swap
        .sget .3
        sget02 .size_fn
          .here swap .hereptr .call

        # Do we have a setter fn? If so, use that; otherwise insert a memcpy
        # instruction directly.
        sget02 .fset_fn dup             # self cc asm fn? fn?

        [                               # self cc asm fn? cc
          sget01 .here sget03 .hereptr  # self cc asm fn? cc asm
          .call                         # self cc asm fn? cc asm
          drop sset00 goto ]            # self cc asm
        [                               # self cc asm 0 cc
          sget02 .memcpy drop           # self cc asm 0 cc
          sset00 goto ]                 # self cc asm
        if call

        .sset .1
        .drop .goto

      .compile                          # self cc fn
      sset01 goto                       # fn },

    "nil?" => bin q{const0 sset01 goto},
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

    "key==" => bin q{                   # k1 k2 self cc
      sget03 sget03 .==                 # k1 k2 self cc ==?
      sset03 sset01 drop goto           # ==? },

    "key==_fn" => bin q{                # self cc
      $strcmp_fn sset01 goto            # fn },

    keys => bin q{                      # self cc
      goto                              # self },

    kv_pairs => bin q{                  # self cc
      goto                              # self },

    "{}" => bin q{                      # k self cc
      sget01 .name sget03 .==           # k self cc ==?
      [ sset01 swap goto ]              # self
      [ sget02 sget02 .tail .{}         # k self cc link
        sset02 sset00 goto ]            # link
      if goto                           # link });


=head2 Struct linking functions

=cut


use constant struct_link_test_fn => phi::allocation
  ->constant(bin q{                     # cc
    $cons_struct_link_class drop        # cc
    goto                                # })
  ->named('struct_link_test_fn') >> heap;


1;
