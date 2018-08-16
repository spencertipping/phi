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


=head2 Classes
Structurally, classes consist of four things:

1. A struct describing the data layout of each instance
2. A string-map of method definitions (NB: assembler transforms)
3. A string-map of virtual functions
4. A list of protocols implemented by the class

In phi-land, "method" means "a thing a class does to the compiling assembler"
whereas "virtual" corresponds to the more common concept of functions provided
by classes. Classes use virtuals to implement protocol functionality.

Put differently, phi gives you as much leverage as it its type information
allows it to provide.


=head3 Protocol objects
Protocols are pretty simple: we just have a list of virtuals and a list of
classes.

  struct protocol
  {
    hereptr  vtable;
    strmap  *virtuals;          # NB: used as a set
    intmap  *classes;           # NB: used as a set
  }

Like classes, protocols are compilers.

Protocols don't play a structural role in phi1. For now they're a holdover from
a previous design in which I had been generating vtables, but now that we're
using symbolic methods they're nonessential.
=cut


use constant protocol_class => phi::class->new('protocol',
  ctti_protocol,
  protocol_protocol,
  mutable_protocol_protocol)

  ->def(
    virtuals => bin q{swap =8  iplus m64get swap goto},
    classes  => bin q{swap =16 iplus m64get swap goto},

    # Any protocol value exists at runtime, since we don't know the type it
    # points to. (Protocols are the entry point for RTTI.)
    "exists_at_runtime?" => bin q{=1 sset01 goto},
    parse                => bin q{      # i p self cc
      sset00                            # i p cc
      $fail_instance sset01 goto        # i fail },

    defvirtual => bin q{                # m self cc
      sget02 sget02 .virtuals .<<       # m self cc ms
      drop sset01 swap goto             # self },

    'implementors<<' => bin q{          # c self cc
      sget02 sget02 .classes .<<        # c self cc cs
      drop sset01 swap goto             # self },

    struct_link => bin q{               # struct name self cc
      sget03 sget03 sget03 objrf        # struct name self cc struct'
      sset03 sset01 drop goto           # struct' },

    symbolic_method => bin q{           # asm m self cc
      # Standard method call through the dispatch fn. Hash the method up front
      # and drop in the lit64 for it, then swap and call twice.
      sget02 method_hash bswap64        # asm m self cc mh
      sget04                            # asm m self cc mh asm
        .dup .m64get                    # [args... obj fn]
        .lit64 .l64 .swap               # [args... obj m fn]
        .call                           # [args... obj mf]
        .call                           # [result...]
                                        # asm m self cc asm'
      sset03 sset01 drop goto           # asm });


use constant empty_protocol_fn => phi::allocation
  ->constant(bin q{                     # cc
    =24 i.heap_allocate                 # cc p
    $protocol_class sget01 m64set           # [.vtable=]
    strmap          sget01 =8  iplus m64set # [.virtuals=]
    intmap          sget01 =16 iplus m64set # [.classes=]
    swap goto                           # p })
  ->named('empty_protocol_fn') >> heap;

BEGIN
{
  bin_macros->{protocol} = bin q{$empty_protocol_fn call};
}


=head3 C<class> struct
Here's what a class looks like:

  struct class
  {
    hereptr              vtable;
    struct              *fields;
    strmap<hereptr<fn>> *methods;
    strmap<hereptr<fn>> *virtuals;
    intmap<protocol*>   *protocols;     # NB: used as a set
  }

=cut


use constant class_class => phi::class->new('class',
  ctti_protocol,
  class_protocol,
  compilable_class_protocol,
  joinable_protocol,
  mutable_class_protocol)

  ->def(
    fields    => bin q{swap =8  iplus m64get swap goto},
    methods   => bin q{swap =16 iplus m64get swap goto},
    virtuals  => bin q{swap =24 iplus m64get swap goto},
    protocols => bin q{swap =32 iplus m64get swap goto},

    # We exist at runtime iff there are any fields defined for this class.
    "exists_at_runtime?" => bin q{swap .fields .right_offset swap goto},
    parse                => bin q{      # i p self cc
      sset00                            # i p cc
      $fail_instance sset01 goto        # i fail },

    '+' => bin q{                       # rhs self cc
      lit8+40 i.heap_allocate           # rhs self cc c
      sget02 m64get sget01 m64set       # [.vt=]

      # NB: fields are in reverse order
      sget02 .fields    sget04 .fields    .+ sget01 =8  iplus m64set
      sget03 .methods   sget03 .methods   .+ sget01 =16 iplus m64set
      sget03 .virtuals  sget03 .virtuals  .+ sget01 =24 iplus m64set
      sget03 .protocols sget03 .protocols .+ sget01 =32 iplus m64set
      sset02 sset00 goto                # c },

    defmethod => bin q{                 # fn name self cc
      sget03 sget03 sget03              # fn name self cc fn name self
      .methods .{}=                     # fn name self cc methods [{name}=value]
      drop sset01 sset01 goto           # self },

    defvirtual => bin q{                # fn name self cc
      sget03 sget03 sget03              # fn name self cc fn name self
      .virtuals .{}=                    # fn name self cc virtuals
      drop sset01 sset01 goto           # self },

    implement => bin q{                 # p self cc
      sget02 sget02 .protocols .<<      # p self cc protos
      drop sget01 sget03
                  .implementors<<       # p self cc proto
      drop sset01 swap goto             # self },

    struct_link => bin q{               # struct name self cc
      # Do we have any virtuals, or does our struct representation contain
      # multiple fields? If either is true then we're a reference type;
      # otherwise we're a value type.
      sget01 .fields .length =1 ilt     # s n self cc fn>1?
      sget02 .virtuals .length          # s n self cc fn>1? vs?
      ior                               # s n self cc reftype?

      [ sget03 sget03 sget03 objrf      # s n self cc s'
        sset03 sset01 drop goto ]       # s'

      [ sget03 sget03 i64f              # s n self cc s'
        sget02 sget01 =40 iplus m64set  # s n self cc s' [.class=]
        sset03 sset01 drop goto ]       # s'

      if goto                           # s' },

    dispatch_fn => bin q{               # self cc
      # First allocate the k/v lookup table for methods. This is just 16*n bytes
      # of memory, for now with no prefix. We'll add the here-marker stuff in
      # phi2 to make it a real object.
      sget01 .virtuals .length          # self cc n
      =4     ishl dup
      =8     iplus i.heap_allocate      # self cc offN mt
      swap                              # self cc mt offN

      # Set the topmost entry to k=0 to detect missing methods.
      sget01 iplus =0 swap m64set       # self cc mt [.k[-1]=0]

      dup                               # self cc mt mt
      sget03 .virtuals .kv_pairs        # self cc mt mt kv

      [                                 # self cc mt mt kv loop
        sget01 .nil?
        [ # Now we have the full k/v table built up. Assemble a function that
          # refers to it and issues the correct method call.
          drop drop drop                # self cc mt
          asm                           # self cc mt asm[m cc]
            .hereptr                    # self cc asm[m cc mt]
            .swap                       # [m mt cc]
            $mlookup_fn swap .hereptr   # [m mt cc mlookup]
            .goto                       # [f]
          .compile .here                # self cc fn
          sset01 goto ]                 # fn

        [ # Set the next entry in the table, bump to tail/next, and loop again.
          sget01 .key method_hash       # self cc mt mt kv loop kh
          sget03 m64set                 # [.kh=]

          sget01 .value                 # self cc mt mt kv loop v
          sget03 =8 iplus m64set        # self cc mt mt kv loop [.value=]

          sget01 .tail sset01           # kv=kv.tail
          sget02 =16 iplus sset02       # mt++
          dup goto ]                    # ->loop

        if goto ]                       # self cc mt mt kv loop

      dup goto                          # ->loop },

    symbolic_method => bin q{           # asm m self cc
      # If the method is virtual, link it directly (i.e. save the vtable lookup
      # and insert a constant). Otherwise, invoke the method function directly.
      #
      # If you want a true-virtual method call you'll need to use a protocol
      # object.

      sget02 sget02 .virtuals .contains?
      [ sget02 sget02 .virtuals .{}     # asm m self cc fn
        sget04 .hereptr .call           # asm m self cc asm'
        sset03 sset01 drop goto ]       # asm'

      [ sget02 sget02 .methods .{}      # asm m self cc fn
        sset01 sset01 goto ]            # ->fn(asm)

      if goto                           # asm' });


use constant class_fn => phi::allocation
  ->constant(bin q{                     # struct cc
    lit8+40 i.heap_allocate             # struct cc c
    $class_class sget01           m64set    # [.vtable=]
    sget02       sget01 =8  iplus m64set    # [.fields=]
    strmap       sget01 =16 iplus m64set    # [.methods=]
    strmap       sget01 =24 iplus m64set    # [.virtuals=]
    intmap       sget01 =32 iplus m64set    # [.protocols=]
    sset01 goto                         # c })
  ->named('class_fn') >> heap;

BEGIN
{
  bin_macros->{class} = bin q{$class_fn call};
}


use constant phi1_oop_linkage_test_fn => phi::allocation
  ->constant(bin q{                     # cc
    # Let's start by generating a function that calls .length on one of our
    # bootstrap-exported maps. We'll do this twice: first using the list
    # protocol (vtable-indirect), then using a direct class linkage.
    %class_map dup .length swap         # cc n cm

    asm                                 # cc n cm asm []
      .swap                             # [cc cm]
      "list" %protocol_map .{} .'length # [cc l]
      .swap .goto                       # [l]
    .compile .call                      # cc n cl
    ieq "length via prototype" i.assert # cc

    # Same thing, this time with a direct-linked class method call.
    %class_map dup .length swap         # cc n cm
    asm                                 # cc n cm asm []
      .swap                             # [cc cm]
      "linked_map" %class_map .{}
        .'length                        # [cc l]
      .swap .goto                       # [l]
    .compile .call                      # cc n cl
    ieq "length via class" i.assert     # cc

    goto                                # })
  ->named('phi1_oop_linkage_test_fn') >> heap;


use constant phi1_compile_linkage_test_fn => phi::allocation
  ->constant(bin q{                     # cc
    struct
    class
      [ =31 sset01 goto ] swap
      "length"            swap .defvirtual

    .dispatch_fn                        # cc f
    get_stackptr                        # cc f obj

    .length =31 ieq "l31" i.assert      # cc f
    drop goto                           # })
  ->named('phi1_compile_linkage_test_fn') >> heap;


use constant phi1_runtime_linkage_test_fn => phi::allocation
  ->constant(bin q{                     # cc
    # Basic test: define a protocol for an unapplied binary operation.
    protocol
      "apply" swap .defvirtual
      "lhs"   swap .defvirtual
      "rhs"   swap .defvirtual          # cc p

    dup                                 # cc p p

    # Now define a class that implements this protocol.
    struct
      "fn"  i64f
      "lhs" i64f
      "rhs" i64f
    class                               # cc p p c
      .implement                        # cc p c

      [ swap dup                        # cc self self
        =8  iplus m64get swap           # cc lhs self
        =16 iplus m64get iplus          # cc v
        swap goto ] swap
        "apply" swap .defvirtual        # cc p c

      [ swap =8  iplus m64get swap goto ] swap "lhs" swap .defvirtual
      [ swap =16 iplus m64get swap goto ] swap "rhs" swap .defvirtual

      [ swap                            # cc asm [self]
        .dup .lit8  =8  swap .l8        # cc asm [self self loff]
          .iplus .m64get                # cc asm [self lhs]
        .swap .lit8 =16 swap .l8        # cc asm [lhs self roff]
          .iplus .m64get                # cc asm [lhs rhs]
        .iplus                          # cc asm [lhs+rhs]
        swap goto ] swap
        "inline" swap .defmethod        # cc p c

    # Verify that we have the right object size and layout
    dup .fields .right_offset =24 ieq "class objsize" i.assert
    dup .fields "fn" swap .{}
      .left_offset =0 ieq "class &fn=0" i.assert

    # OK, allocate an instance of this class and make sure it works correctly.
    =24 i.heap_allocate                 # cc p c obj
      sget01 .dispatch_fn
      sget01 m64set                     # cc p c obj [.fn=]

    dup                                 # cc p c obj obj

    # Untyped (manual) method call
    asm                                 # cc p c obj obj asm
      .swap                             # [cc obj]

      .lit8 lit8+17 swap .l8            # [cc obj:p 17]
      =1 swap .sget .lit8 =8 swap .l8
        .iplus .m64set                  # [cc obj:p [.lhs=]]
      .lit8 lit8+30 swap .l8            # [cc obj:p 30]
      =1 swap .sget .lit8 =16 swap .l8
        .iplus .m64set                  # [cc obj:p [.rhs=]]

      .dup .m64get                      # [cc obj fn]
      "apply" method_hash bswap64 swap
        .lit64 .l64                     # [cc obj fn mh]
      .swap .call .call                 # [cc obj.apply]
      .swap .goto                       # [obj.apply]
    .compile .call

    lit8+47 ieq "m47" i.assert          # cc p c obj

    dup

    # Type the argument as a protocol
    sget03 asm                          # cc p c obj obj p asm
      .swap                             # [cc obj:p]

      .lit8 =17 swap .l8                # [cc obj:p 17]
      =1 swap .sget .lit8 =8 swap .l8
        .iplus .m64set                  # [cc obj:p [.lhs=]]
      .lit8 =30 swap .l8                # [cc obj:p 30]
      =1 swap .sget .lit8 =16 swap .l8
        .iplus .m64set                  # [cc obj:p [.rhs=]]

      swap .'apply                      # [cc obj.apply]

      .swap .goto                       # [obj.apply]

    .compile .call                      # cc p c obj 47

    lit8+47 ieq "p47" i.assert          # cc p c obj

    dup

    # Now do the same thing using a direct class method call
    sget02 asm                          # cc p c obj obj c asm
      .swap                             # [cc obj:c]

      .lit8 =17 swap .l8                # [cc obj:c 17]
      =1 swap .sget .lit8 =8 swap .l8
        .iplus .m64set                  # [cc obj:c [.lhs=]]

      .lit8 =30 swap .l8                # [cc obj:c 30]
      =1 swap .sget .lit8 =16 swap .l8
        .iplus .m64set                  # [cc obj:c [.rhs=]]

      swap .'apply                      # [cc obj.apply]
      .swap .goto                       # [obj.apply]
    .compile
    .call                               # cc p c obj 47

    lit8+47 ieq "c47" i.assert          # cc p c obj

    dup                                 # cc p c obj obj

    # Finally, do the same thing using native linkage
    sget02 asm                          # cc p c obj obj c asm
      .swap                             # [cc obj:c]

      .lit8 =17 swap .l8                # [cc obj:c 17]
      =1 swap .sget .lit8 =8 swap .l8
        .iplus .m64set                  # [cc obj:c [.lhs=]]

      .lit8 =30 swap .l8                # [cc obj:c 30]
      =1 swap .sget .lit8 =16 swap .l8
        .iplus .m64set                  # [cc obj:c [.rhs=]]

      swap .'inline                     # [cc obj.inline]
      .swap .goto                       # [obj.inline]
    .compile
    .call                               # cc p c obj 47

    lit8+47 ieq "i47" i.assert          # cc p c obj
    drop                                # cc p c

    drop drop                           # cc
    goto                                # })
  ->named('phi1_runtime_linkage_test_fn') >> heap;


=head3 Accessors
Let's define some logic to generate accessor virtuals. This is pretty simple:
structs already give us getter/setter functions that will apply to the object
(provided the dispatch function is represented in the object's field list, which
it should be).

All accessors allow both get/set, and you get those methods for every defined
field. phi1 doesn't provide access control because access control is for wimps
and good programmers.
=cut

use constant gen_accessor_fn => phi::allocation
  ->constant(bin q{                     # c f cc
    swap dup .name                      # c cc f name
    sget01 .getter_fn swap              # c cc f get name
    sget04 .defvirtual drop             # c cc f [c.getter]

    dup .name "=" swap .+               # c cc f name=
    sget01 .setter_fn swap              # c cc f set name
    sget04 .defvirtual drop             # c cc f [c.setter]

    drop goto                           # c })
  ->named('gen_accessor_fn') >> heap;

use constant gen_accessors_fn => phi::allocation
  ->constant(bin q{                     # c cc
    sget01                              # c cc c
    [                                   # f c cc
      sget01 sget03 $gen_accessor_fn call # f c cc c
      sset02 =0 sset01 goto ]           # c exit?=0
    sget01 .fields .reduce              # c cc c
    drop goto                           # c })
  ->named('gen_accessors_fn') >> heap;

BEGIN
{
  bin_macros->{accessors} = bin q{$gen_accessors_fn call};
}


# NB: this protocol isn't used for anything; it just exists to tell phi0 that
# it's ok to refer to .x and .x= and so forth. Removing it won't change any
# code; you'll just get phi0 warnings about missing methods.
use constant accessor_test_protocol => phi::protocol->new('accessor_test',
  qw/ dispatch_fn
      x
      y
      x=
      y= /);

use constant accessor_test_fn => phi::allocation
  ->constant(bin q{                     # cc
    struct "dispatch_fn" i64f
           "x"           i64f
           "y"           i64f
    class
      accessors
    .dispatch_fn                        # cc f

    =17 =9 sget02                       # cc f y x f
    get_stackptr                        # cc f y x f obj

    dup .dispatch_fn                    # cc f y x f obj f?
      sget02 ieq "dfn==" i.assert       # cc f y x f obj

    dup .y =17 ieq "y17" i.assert       # cc f y x f obj
    dup .x =9  ieq "x9"  i.assert       # cc f y x f obj

    =34 sget01 .x=                      # cc f y x f obj
    dup .x =34 ieq "x34" i.assert       # cc f y x f obj

    drop drop drop drop drop goto       # })
  ->named('accessor_test_fn') >> heap;


1;
