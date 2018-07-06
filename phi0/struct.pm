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


use constant nil_struct_link_class => phi::class->new('nil_struct_link',
  maybe_nil_protocol,
  struct_link_protocol)

  ->def(
    "nil?"          => bin q{const1 sset01 goto},

    size            => bin q{const0 sset01 goto},
    left_offset     => bin q{const0 sset01 goto},
    right_offset    => bin q{const0 sset01 goto},

    size_fn         => bin q{%k0_fn sset01 goto},
    left_offset_fn  => bin q{%k0_fn sset01 goto},
    right_offset_fn => bin q{%k0_fn sset01 goto});


use constant nil_struct_link_instance => phi::allocation
  ->constant(pack Q => nil_struct_link_class->vtable >> heap)
  ->named('nil_struct_link_instance') >> heap;


=head3 Struct cons links
Struct definition:

  struct cons_struct_link               # size = 96
  {
    hereptr              vtable;        # offset = 0
    baseptr<struct_link> tail;          # offset = 8
    baseptr<string>      name;          # offset = 16
    baseptr<fn>          fget_fn;       # offset = 24
    baseptr<fn>          fset_fn;       # offset = 32
    baseptr<class>       class;         # offset = 40

    cell                 left_offset;   # offset = 48; -1 if computed
    cell                 size;          # offset = 56; -1 if computed

    baseptr<fn>          size_fn;           # offset = 64
    baseptr<fn>          right_offset_fn;   # offset = 72

    baseptr<fn>          getter_fn;     # offset = 80
    baseptr<fn>          setter_fn;     # offset = 88
  }

C<class> is a pointer to the class object that this field's value belongs to.

C<fget_fn> and C<fset_fn> are used to form C<getter_fn> and C<setter_fn>. Their
signatures are:

  fget_fn : (&field -> val)
  fset_fn : (val &field fieldsize ->)

We need these functions to implement arrays of things or other inline
variable-sized allocations.

C<left_offset_fn> and C<right_offset_fn> are cached elements that you don't
specify. We just store them to eliminate reallocations if we ask for them
multiple times.

WARNING: C<size_fn> returns the size of the rightmost _field_, not the struct
itself. The struct's size is returned by C<right_offset> and C<right_offset_fn>.
=cut


use constant cons_struct_link_class => phi::class->new('cons_struct_link',
  list_protocol,
  cons_protocol,
  cons_relinkable_protocol,
  maybe_nil_protocol,
  struct_link_protocol,
  map_protocol,
  cons_struct_link_protocol)

  ->def(
    tail        => bin q{swap const8  iplus m64get swap goto},
    name        => bin q{swap const16 iplus m64get swap goto},
    fget_fn     => bin q{swap const24 iplus m64get swap goto},
    fset_fn     => bin q{swap const32 iplus m64get swap goto},
    class       => bin q{swap cell8+5 iplus m64get swap goto},

    left_offset => bin q{swap cell8+6 iplus m64get swap goto},
    size        => bin q{swap cell8+7 iplus m64get swap goto},

    with_tail   => bin q{               # t self cc
      # Allocate a new struct link with our values, but erase all cached fields
      # because the new tail will specify different offsets.

      lit8+96 i.heap_allocate           # t self cc new
      sget02 m64get   sget01 m64set     # t self cc new [.vtable=]
      sget03          sget01 const8  iplus m64set     # [.tail=]
      sget02 .name    sget01 const16 iplus m64set     # [.name=]
      sget02 .fget_fn sget01 const24 iplus m64set     # [.fget_fn=]
      sget02 .fset_fn sget01 const32 iplus m64set     # [.fset_fn=]
      sget02 .class   sget01 cell8+5 iplus m64set     # [.class=]
      sget03 .right_offset
                      sget01 cell8+6 iplus m64set     # [.left_offset=]
      sget02 .size    sget01 cell8+7 iplus m64set     # [.size=]

      sget02 .size_fn sget01 cell8+8 iplus m64set     # [.size_fn=]
      sget02 .right_offset_fn
                      sget01 cell8+9 iplus m64set     # [.right_offset_fn=]

      const0          sget01 cell8+10 iplus m64set    # [.getter_fn=]
      const0          sget01 cell8+11 iplus m64set    # [.setter_fn=]

      sset02 sset00 goto                # new },

    right_offset => bin q{              # self cc
      # Return -1 if our left offset or our size is computed.
      sget01 .left_offset               # self cc loff
      sget02 .size                      # self cc loff size
      sget01 const1 ineg ieq            # self cc loff size offc?
      sget01 const1 ineg ieq ior        # self cc loff size computed?

      [ drop drop const1 ineg           # self cc -1
        sset01 goto ]                   # -1
      [ iplus sset01 goto ]             # loff+size
      if goto                           # roff },

    size_fn => bin q{                   # self cc
      # If someone is asking for this function and we don't have one, generate a
      # function that returns the correct constant and save that in the field.
      sget01 cell8+8 iplus dup m64get   # self cc &f f
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

      sget01 cell8+9 iplus dup m64get   # self cc &f f
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
            swap bswap32 swap .l32
            .sset .1 .goto
          swap goto ]                   # self cc &f asm

        if call                         # self cc &f asm
        .compile sget01 m64set          # self cc &f
        m64get sset01 goto              # f
      ]
      if goto                           # f },

    getter_fn => bin q{                 # self cc
      sget01 cell8+10 iplus dup m64get  # self cc &g g
      [ m64get sset01 goto ]            # g
      [ sget02 .generate_getter_fn      # self cc &g g
        sget01 m64set                   # self cc &g
        m64get sset01 goto ]            # g
      if goto                           # g },

    setter_fn => bin q{                 # self cc
      sget01 cell8+11 iplus dup m64get  # self cc &s s
      [ m64get sset01 goto ]            # s
      [ sget02 .generate_setter_fn      # self cc &s s
        sget01 m64set                   # self cc &s
        m64get sset01 goto ]            # s
      if goto                           # s },

    get => bin q{                       # &struct self cc
      sget02 sget02 .getter_fn .here    # &s self cc &s gfn
      call                              # &s self cc v
      sset02 sset00 goto                # v },

    set => bin q{                       # v &struct self cc
      sget03 sget03 sget03              # v &s self cc v &s self
      .setter_fn .here call             # v &s self cc
      sset02 drop drop goto             # },

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
        sget02 .fget_fn dup             # self cc asm fn? fn?
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

    "key==_fn" => bin q{                # self cc
      $strcmp_fn sset01 goto            # fn },

    keys => bin q{                      # self cc
      goto                              # self },

    kv_pairs => bin q{                  # self cc
      goto                              # self },

    "{}" => bin q{                      # k self cc
      sget01 .name sget03 .==           # k self cc ==?
      [ sset01 swap goto ]              # self
      [ swap .tail swap                 # k tail cc
        sget01 m64get :{} goto ]        # tail.{}
      if goto                           # link });


=head2 Struct linking functions
We need a few constructors to build struct objects:

  <tail>               "name" size fixed_field
  <tail>     fget fset "name" size fixed_getset_field
  <tail> "repfield" repsize "name" array_field
  <tail>                           here_marker_field

=cut


use constant setup_struct_link_globals_fn => phi::allocation
  ->constant(bin q{                     # cc
    # Getters
    asm .swap .m8get  .swap .goto .compile "int8_get"  i.def
    asm .swap .m16get .swap .goto .compile "int16_get" i.def
    asm .swap .m32get .swap .goto .compile "int32_get" i.def
    asm .swap .m64get .swap .goto .compile "int64_get" i.def

    # Setters: (v &f ->)
    asm .sget .2 .sget .2 .m8set  .sset .1 .drop .goto .compile "int8_set" i.def
    asm .sget .2 .sget .2 .m16set .sset .1 .drop .goto .compile "int16_set" i.def
    asm .sget .2 .sget .2 .m32set .sset .1 .drop .goto .compile "int32_set" i.def
    asm .sget .2 .sget .2 .m64set .sset .1 .drop .goto .compile "int64_set" i.def

    asm .const0 .sset .1 .goto .compile "k0_fn" i.def

    goto                                # })
  ->named('setup_struct_link_globals_fn') >> heap;


use constant empty_cons_struct_link_fn => phi::allocation
  ->constant(bin q{                     # cc
    cell8+12 i.heap_allocate            # cc &l
    $cons_struct_link_class sget01 m64set     # [.vt=]
    $nil_struct_link_instance
           sget01 cell8+1  iplus m64set # [.tail=]
    const0 sget01 cell8+2  iplus m64set # [.name=]
    const0 sget01 cell8+3  iplus m64set # [.fget=]
    const0 sget01 cell8+4  iplus m64set # [.class=]
    const0 sget01 cell8+5  iplus m64set # [.fset=]
    const0 sget01 cell8+6  iplus m64set # [.left_offset=]
    const0 sget01 cell8+7  iplus m64set # [.size=]
    const0 sget01 cell8+8  iplus m64set # [.size_fn=]
    const0 sget01 cell8+9  iplus m64set # [.right_offset_fn=]
    const0 sget01 cell8+10 iplus m64set # [.getter_fn=]
    const0 sget01 cell8+11 iplus m64set # [.setter_fn=]
    swap goto                           # &l })
  ->named('empty_cons_struct_link_fn') >> heap;


use constant fixed_field_fn => phi::allocation
  ->constant(bin q{                     # tail name size cc
    $empty_cons_struct_link_fn call     # tail name size cc &l

    sget04 sget01 cell8+1 iplus m64set  # [.tail=]
    sget03 sget01 cell8+2 iplus m64set  # [.name=]
    sget04 .right_offset
           sget01 cell8+6 iplus m64set  # [.left_offset=]
    sget02 sget01 cell8+7 iplus m64set  # tail name size cc &l [.size=]
    sset03 sset01 drop goto             # &l })
  ->named('fixed_field_fn') >> heap;


use constant fixed_getset_field_fn => phi::allocation
  ->constant(bin q{                     # tail fget fset name size cc
    sget05 sget03 sget03                # t g s n z c t n z
    $fixed_field_fn call                # t g s n z c l
    sget05 sget01 cell8+3 iplus m64set  # [.fget=]
    sget04 sget01 cell8+4 iplus m64set  # [.fset=]
    sset05 sset03 drop drop drop goto   # l })
  ->named('fixed_getset_field_fn') >> heap;


use constant int8_field_fn => phi::allocation
  ->constant(bin q{                     # tail name cc
    sget02                              # tail name cc tail
    %int8_get %int8_set                 # t n cc t g s
    sget04 const1                       # t n cc t g s n 1
    $fixed_getset_field_fn call         # t n cc struct
    sset02 sset00 goto                  # struct })
  ->named('int8_field_fn') >> heap;

use constant int16_field_fn => phi::allocation
  ->constant(bin q{                     # tail name cc
    sget02                              # tail name cc tail
    %int16_get %int16_set               # t n cc t g s
    sget04 const2                       # t n cc t g s n 2
    $fixed_getset_field_fn call         # t n cc struct
    sset02 sset00 goto                  # struct })
  ->named('int16_field_fn') >> heap;

use constant int32_field_fn => phi::allocation
  ->constant(bin q{                     # tail name cc
    sget02                              # tail name cc tail
    %int32_get %int32_set               # t n cc t g s
    sget04 const4                       # t n cc t g s n 4
    $fixed_getset_field_fn call         # t n cc struct
    sset02 sset00 goto                  # struct })
  ->named('int32_field_fn') >> heap;

use constant int64_field_fn => phi::allocation
  ->constant(bin q{                     # tail name cc
    sget02                              # tail name cc tail
    %int64_get %int64_set               # t n cc t g s
    sget04 const8                       # t n cc t g s n 8
    $fixed_getset_field_fn call         # t n cc struct
    sset02 sset00 goto                  # struct })
  ->named('int64_field_fn') >> heap;


use constant array_field_fn => phi::allocation
  ->constant(bin q{                     # tail rname rsize name cc
    $empty_cons_struct_link_fn call     # t rn z n cc l

    sget05 sget01 cell8+1 iplus m64set  # [.tail=]
    sget02 sget01 cell8+2 iplus m64set  # [.name=]
    sget05 .right_offset
           sget01 cell8+6 iplus m64set  # [.left_offset=]

    const1 ineg
           sget01 cell8+7 iplus m64set  # [.size=]

    # Now assemble the size function. This involves using the getter of the
    # rname field, then multiplying that result by rsize:
    #
    #                                   # &struct cc (initial stack)
    #   sget01 <rname-getter> call      # &struct cc n
    #   lit(rsize) itimes               # &struct cc size
    #   sset01 goto                     # size

    asm                                 # t rn z n cc l asm
      .sget .1

      sget05 sget07 .{} .getter_fn .here
        swap .hereptr .call

      .lit32
      sget04 bswap32 swap .l32 .itimes

      .sset .1 .goto
    .compile                            # t rn z n cc l fn

    sget01 cell8+8 iplus m64set         # t rn z n cc l [.sizefn=]

    sset04 sset02 drop drop goto        # l })
  ->named('array_field_fn') >> heap;


BEGIN
{
  bin_macros->{struct} = bin q{$nil_struct_link_instance};
  bin_macros->{ff}     = bin q{$fixed_field_fn call};
  bin_macros->{i8f}    = bin q{$int8_field_fn call};
  bin_macros->{i16f}   = bin q{$int16_field_fn call};
  bin_macros->{i32f}   = bin q{$int32_field_fn call};
  bin_macros->{i64f}   = bin q{$int64_field_fn call};
  bin_macros->{arrf}   = bin q{$array_field_fn call};
}


use constant struct_link_test_fn => phi::allocation
  ->constant(bin q{                     # cc
    struct "foo" const8 ff              # cc struct
           "bar" const4 ff              # cc struct
           "bif" const4 ff              # cc struct

    dup .right_offset const16 ieq "roffset16" i.assert
    dup "foo" swap .{}                  # cc struct foofield
      dup .left_offset  const0 ieq "loffset0" i.assert
      dup .right_offset const8 ieq "roffset8" i.assert
      dup .size         const8 ieq "size8"    i.assert
      drop                              # cc struct

    dup "bar" swap .{}                  # cc struct barfield
      dup .left_offset  const8  ieq "loffset8"  i.assert
      dup .right_offset lit8+12 ieq "roffset12" i.assert
      dup .size         const4  ieq "size4"     i.assert
      drop                              # cc struct

    dup .right_offset_fn .here          # cc struct sfnh
      const0 swap call const16 ieq "roffsetfn16" i.assert

    dup .size_fn .here
      const0 swap call const4 ieq "sizefn4" i.assert

    drop                                # cc

    # Test variable-sized structs, e.g. bytecode objects
    struct              "vtable"      i64f
                        "nrefs"       i32f
                        "codesize"    i32f
        "nrefs" const16 "refs"        arrf
                        "here_marker" const2 ff
      "codesize" const1 "data"        arrf

                                        # cc struct

    # Sanity check for basic layout
    dup "vtable"   swap .{} .left_offset const0  ieq "vtloff0"   i.assert
    dup "nrefs"    swap .{} .left_offset const8  ieq "nrefsoff8" i.assert
    dup "codesize" swap .{} .left_offset lit8+12 ieq "csloff12"  i.assert

    # Build a bytecode with two refs
    asm
      lit64 'foobar32 swap .ptr     .call
      lit64 'barbif11 swap .hereptr .goto
    .compile                            # cc struct bytecode

    dup sget02 .getter_fn .here call    # cc struct bytecode &data
      dup m8get lit8 lit64 ieq "lit64=" i.assert
      dup const1 iplus m64get lit64 '23raboof ieq "23raboof" i.assert
      drop                              # cc struct bytecode

    dup sget02 "codesize" swap .{} .getter_fn .here call
        lit8+20 ieq "codesize20" i.assert

    dup sget02 "vtable"   swap .{} .getter_fn .here call
        $bytecode_class ieq "vtbcclass" i.assert

    dup sget02 "nrefs"    swap .{} .getter_fn .here call
        const2 ieq "nrefs2" i.assert

    $nil_struct_link_instance
      %int64_get const0 "vtable" const8 $fixed_getset_field_fn call
      %int32_get const0 "offset" const4 $fixed_getset_field_fn call
      %int32_get const0 "ptype"  const4 $fixed_getset_field_fn call

                                        # cc bstruct bc rstruct

    # NB: the ref list goes right->left because it's staged as a linked list.
    "refs" sget03 .{} .getter_fn .here  # cc bs bc rs f
      sget02 swap call                  # cc bs bc rs &ref0

      dup sget02 "vtable" swap .{} .get $ref_class ieq "vtrefclass1" i.assert
      dup sget02 "offset" swap .{} .get lit8+11    ieq "offset11"    i.assert
      dup sget02 "ptype"  swap .{} .get const1     ieq "ptype1"      i.assert

    sget01 .right_offset iplus          # cc bs bc rs &ref1
      dup sget02 "vtable" swap .{} .get $ref_class ieq "vtrefclass2" i.assert
      dup sget02 "offset" swap .{} .get const1     ieq "offset1"     i.assert
      dup sget02 "ptype"  swap .{} .get const0     ieq "ptype0"      i.assert

      drop drop                         # cc bs bc

    drop drop                           # cc

    goto                                # })
  ->named('struct_link_test_fn') >> heap;


1;
