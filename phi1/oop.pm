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


use phi::class protocol =>
  symbolic_method_protocol,
  protocol_protocol,
  mutable_protocol_protocol,

  virtuals => bin q{swap =8  iplus m64get swap goto},
  classes  => bin q{swap =16 iplus m64get swap goto},

  defvirtual => bin q{                # m self cc
    sget02 sget02 .virtuals .<<       # m self cc ms
    drop sset01 swap goto             # self },

  'implementors<<' => bin q{          # c self cc
    sget02 sget02 .classes .<<        # c self cc cs
    drop sset01 swap goto             # self },

  struct_link => bin q{               # struct name self cc
    sget03 sget03_ sget03_ .ptr       # struct name self cc struct'
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
    sset03 sset01 drop goto           # asm };


use phi::fn protocol => bin q{          # cc
  =24 i.heap_allocate                   # cc p
  $protocol_class sget01 m64set           # [.vtable=]
  strmap          sget01 =8  iplus m64set # [.virtuals=]
  intmap          sget01 =16 iplus m64set # [.classes=]
  swap goto                             # p };


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


use phi::class class =>
  symbolic_method_protocol,
  class_protocol,
  compilable_class_protocol,
  joinable_protocol,
  mutable_class_protocol,

  fields    => bin q{swap =8  iplus m64get swap goto},
  methods   => bin q{swap =16 iplus m64get swap goto},
  virtuals  => bin q{swap =24 iplus m64get swap goto},
  protocols => bin q{swap =32 iplus m64get swap goto},

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

    [ sget03 sget03_ .ptr             # s n self cc s'
      sset03 sset01 drop goto ]       # s'

    [ sget03 sget03_ .i64             # s n self cc s'
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

    sget02 sget02 .methods .contains?
    [ sget02 sget02 .methods .{}      # asm m self cc fn
      sget02 sset03 _ sset01 goto ]   # ->fn(asm self cc)

    [ sget02 sget02 .virtuals .contains?
      [ sget02 sget02 .virtuals .{}   # asm m self cc fn
        sget04 .hereptr .call         # asm m self cc asm'
        sset03 sset01 drop goto ]     # asm'
      [ debug_trace
        sget02 "no virtual or method named " .+ i.die ]
      if goto ]

    if goto                           # asm' };


use phi::fn class => bin q{             # struct cc
  lit8+40 i.heap_allocate               # struct cc c
  $class_class sget01           m64set    # [.vtable=]
  sget02       sget01 =8  iplus m64set    # [.fields=]
  strmap       sget01 =16 iplus m64set    # [.methods=]
  strmap       sget01 =24 iplus m64set    # [.virtuals=]
  intmap       sget01 =32 iplus m64set    # [.protocols=]
  sset01 goto                           # c };


use phi::testfn phi1_oop_linkage => bin q{       #
  # Let's start by generating a function that calls .length on one of our
  # bootstrap-exported maps. We'll do this twice: first using the list
  # protocol (vtable-indirect), then using a direct class linkage.
  %class_map dup .length swap         # n cm

  asm                                 # n cm asm []
    .swap                             # [cc cm]
    "list" %protocol_map .{} .'length # [cc l]
    .swap .goto                       # [l]
  .compile .call                      # n cl
  ieq "length via prototype" i.assert #

  # Same thing, this time with a direct-linked class method call.
  %class_map dup .length swap         # n cm
  asm                                 # n cm asm []
    .swap                             # [cc cm]
    "linked_map" %class_map .{}
      .'length                        # [cc l]
    .swap .goto                       # [l]
  .compile .call                      # n cl
  ieq "length via class" i.assert     # };


use phi::testfn phi1_compile_linkage => bin q{     #
  struct
  class
    [ =31 sset01 goto ] swap
    "length"            swap .defvirtual

  .dispatch_fn                        # f
  get_stackptr                        # f obj

  .length =31 ieq "l31" i.assert      # f
  drop                                # };


use phi::testfn phi1_runtime_linkage => bin q{   #
  # Basic test: define a protocol for an unapplied binary operation.
  protocol
    "apply" swap .defvirtual
    "lhs"   swap .defvirtual
    "rhs"   swap .defvirtual          # p

  dup                                 # p p

  # Now define a class that implements this protocol.
  struct
    "fn"_  .i64
    "lhs"_ .i64
    "rhs"_ .i64
  class                               # p p c
    .implement                        # p c

    [ swap dup                        # self self
      =8  iplus m64get swap           # lhs self
      =16 iplus m64get iplus          # v
      swap goto ] swap
      "apply" swap .defvirtual        # p c

    [ swap =8  iplus m64get swap goto ] swap "lhs" swap .defvirtual
    [ swap =16 iplus m64get swap goto ] swap "rhs" swap .defvirtual

    [ sset00 swap                     # asm [self]
      .dup .lit8  =8  swap .l8        # asm [self self loff]
        .iplus .m64get                # asm [self lhs]
      .swap .lit8 =16 swap .l8        # asm [lhs self roff]
        .iplus .m64get                # asm [lhs rhs]
      .iplus                          # asm [lhs+rhs]
      swap goto ] swap
      "inline" swap .defmethod        # p c

  # OK, allocate an instance of this class and make sure it works correctly.
  =24 i.heap_allocate                 # p c obj
    sget01 .dispatch_fn
    sget01 m64set                     # p c obj [.fn=]

  dup                                 # p c obj obj

  # Untyped (manual) method call
  asm                                 # p c obj obj asm
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

  lit8+47 ieq "m47" i.assert          # p c obj

  dup

  # Type the argument as a protocol
  sget03 asm                          # p c obj obj p asm
    .swap                             # [cc obj:p]

    .lit8 =17 swap .l8                # [cc obj:p 17]
    =1 swap .sget .lit8 =8 swap .l8
      .iplus .m64set                  # [cc obj:p [.lhs=]]
    .lit8 =30 swap .l8                # [cc obj:p 30]
    =1 swap .sget .lit8 =16 swap .l8
      .iplus .m64set                  # [cc obj:p [.rhs=]]

    swap .'apply                      # [cc obj.apply]

    .swap .goto                       # [obj.apply]

  .compile .call                      # p c obj 47

  lit8+47 ieq "p47" i.assert          # p c obj

  dup

  # Now do the same thing using a direct class method call
  sget02 asm                          # p c obj obj c asm
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
  .call                               # p c obj 47

  lit8+47 ieq "c47" i.assert          # p c obj

  dup                                 # p c obj obj

  # Finally, do the same thing using native linkage
  sget02 asm                          # p c obj obj c asm
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
  .call                               # p c obj 47

  lit8+47 ieq "i47" i.assert          # p c obj
  drop                                # p c

  drop drop                           # };


=head3 Accessors
Let's define some logic to generate accessor virtuals. This is pretty simple:
structs already give us getter/setter functions that will apply to the object
(provided the dispatch function is represented in the object's field list, which
it should be).

All accessors allow both get/set, and you get those methods for every defined
field. phi1 doesn't provide access control because access control is for wimps
and responsible programmers.
=cut

use phi::fn monomorphic_accessor => bin q{    # c name cc
  _ dup                                 # c cc name name

  # Define inline methods, which are assembler transforms. This means we need to
  # invoke the struct's get() method on the macro assembler object.
  asm                                   # c cc n n asm[asm self cc]
    =2_ .sget                           # c cc n n asm[asm self cc asm]
    .ptr                                # c cc n asm[asm self cc asm name]
    sget03 .fields _ .ptr               # c cc n asm[asm self cc asm name s]
    .'get                               # c cc n asm[asm self cc asm]
    =2_ .sset =0_ .sset .goto           # c cc n asm[cc(asm)]
  .compile .here                        # c cc n fn
  sget01 sget04 .defmethod drop         # c cc n

  dup                                   # c cc n n
  asm                                   # c cc n n asm[asm self cc]
    =2_ .sget                           # c cc n n asm[asm self cc asm]
    .ptr                                # c cc n asm[asm self cc asm name]
    sget03 .fields _ .ptr               # c cc n asm[asm self cc asm name s]
    .'set                               # c cc n asm[asm self cc asm]
    =2_ .sset =0_ .sset .goto           # c cc n asm[cc(asm)]
  .compile .here                        # c cc n fn
  sget01 "="_ .+ sget04 .defmethod drop # c cc n

  drop goto                             # c };

use phi::fn virtual_accessor => bin q{  # c name cc
  _ dup                                 # c cc name name

  # Inline-assemble the field's accessors into custom functions, then bind those
  # as virtuals. We need to do a little bit of continuation-shuffling to make
  # this work.
  asm                                   # c cc name name asm[self cc]
    .swap                               # c cc name name asm[cc self]
  _ sget04 .fields .get                 # c cc name asm[cc val]
    .swap .goto                         # c cc name asm[val]
  .compile .here sget01 sget04          # c cc name asm[val] name c
  .defvirtual drop                      # c cc name

  dup
  asm                                   # c cc name name asm[v self cc]
    =2_ .sget =2_ .sget                 # c cc name name asm[v self cc v self]
  _ sget04 .fields .set                 # c cc name asm[v self cc]
    =1_ .sset .swap .goto               # c cc name asm[self]
  .compile .here sget01 "="_ .+ sget04  # c cc name asm[val] name= c
  .defvirtual drop                      # c cc name

  drop goto                             # c };

use phi::fn accessors => bin q{         # c cc
  sget01                                # c cc c
  [ sget01 sget03 .name                 # f c cc c name
    monomorphic_accessor drop           # f c cc
    sget01 sget03 .name                 # f c cc c name
    virtual_accessor                    # f c cc c
    sset02 =0 sset01 goto ]             # c exit?=0
  sget01 .fields .reduce                # c cc c
  drop goto                             # c };


# NB: this protocol isn't used for anything; it just exists to tell phi0 that
# it's ok to refer to .x and .x= and so forth. Removing it won't change any
# code, you'll just get phi0 warnings about missing methods.
use phi::protocol accessor_test =>
  qw/ dispatch_fn
      x
      y
      x=
      y= /;

use phi::testfn accessor => bin q{      #
  struct "dispatch_fn"_ .i64
         "x"_           .i64
         "y"_           .i64
  class
    accessors
  dup .dispatch_fn                      # c f

  =17 =9 sget02                         # c f y x f
  get_stackptr                          # c f y x f obj

  dup .dispatch_fn                      # c f y x f obj f?
    sget02 ieq "dfn==" i.assert         # c f y x f obj

  dup .y =17 ieq "y17" i.assert         # c f y x f obj
  dup .x =9  ieq "x9"  i.assert         # c f y x f obj

  =34 sget01 .x= drop                   # c f y x f obj
  dup .x =34 ieq "x34" i.assert         # c f y x f obj

  # Now test with an assembled function and direct method linkage.
  sget05 asm                            # c f y x f obj c asm
    .swap _ .'x .swap .goto
  .compile .here                        # c f y x f obj fn
  sget01 _ call =34 ieq "dx34" i.assert

  drop drop drop drop drop drop         # };


1;
