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


=head2 Jurisdictions
phi is portable, but not in the imperialistic way that Java is. Rather than
building up a full world of uniformity, phi uses domains where certain stated
assumptions hold and compiles code accordingly. These domains are called
jurisdictions.

Code in the same jurisdiction can interoperate efficiently. Anything going on
outside a jurisdiction requires a mediating object to translate between calling
and value storage conventions.

Structurally, a jurisdiction governs the following behaviors:

1. Size and endianness of integers
2. Backend memory model: managed vs flat
3. Backend native compilation characteristics
4. Backend language
5. Object method addressing protocol
6. How many stack slots are used to encode each value

Jurisdictions and runtimes aren't the same thing: a runtime can have multiple
jurisdictions for various reasons. You'd likely do this in Javascript if you
were targeting ASM.js or WebAssembly, for example.


=head3 Jurisdictions and compilation
All code exists within some jurisdiction that governs aspects of that code's
semantics. For example, let's compile a simple function into the current
jurisdiction. We can get that from the interpreter.

  i.jurisdiction                        # j
  tasm                                  # asm [xs cc]
    .swap       $cons_class swap .typed # [cc xs:cons]
    .dup  .'head $int_class swap .typed # [cc xs h:int]
    .swap .'tail $int_class swap .typed # [cc h:int t:int]
    .'+                                 # [cc h+t]
    .swap .goto                         # [h+t]
  .compile                              # fn
  const1 const2 ::                      # fn xs
  swap .call                            # 3

It's common to use the hosting jurisdiction like this to build an assembler that
JITs new functions into the current environment.


=head3 Method calls and typed assembler interop
The cons/int example above involves some delegation from the asm object to the
jurisdiction, specifically around two things:

1. Stack manipulation of pointer values
2. Method calls against pointer values (or more generally, reference types)

(1) is almost always a passthrough; pointer values should be 1:1 mapped to stack
slots unless you're doing something quite elaborate like referring to remote
objects with compound addresses (although if you are, you should be using
proxies rather than doing it this way). I may reduce this to a global invariant
depending on how much it complicates the implementation.

(2) involves translating a symbolic method call to a compiled one. The stack is
assumed to contain C<...args receiver> and should end up as C<...return>; this
is invariant across jurisdictions. Any code that gets us between those states is
fair game.

Internally, here's how the typed assembler asks to generate a method call:

  asm "foo" $protocol i.jurisdiction    # asm m p j
    .protocol_call                      # asm

You can also ask the jurisdiction for a monomorphic class-oriented method call:

  asm "foo" $class i.jurisdiction       # asm m c j
    .class_call                         # asm


=head3 Base objects, metaclass-driven inheritance, and allocation
A jurisdiction specifies a set of metaclasses that are applied to produce
objects that comply with the method calling convention. For example, the
C<vtable> attribute present on all of the phi1 bootstrap classes would normally
be installed by the AMD64 native jurisdiction's "polymorphic reference type"
metaclass.

Jurisdictions manage object allocation. There are a few reasons for this: first,
there may be no directly-accessible heap, for instance if the backend uses
managed memory. Second, instances that function in a polymorphic context, e.g.
with vtables, need to have the vtable field in place immediately after the
memory is heap-allocated. Third, there's no guarantee that we allocate memory
into the heap at all.


=head2 AMD64 native jurisdiction with vtable polymorphism
...basically, the one we've been assuming throughout phi1. It collects classes
and compiles vtables on initialization, then uses metaclasses to store those
vtables on instances.

A natural question here is, how does the jurisdiction ask the interpreter for
memory? We need to compile a method call with respect to the _hosting_
jurisdiction, which importantly will be different from this one. In other words,
this jurisdiction creates a boundary: method calls inside it are governed by the
jurisdiction's allocation and conventions, while its interaction with the
outside world is governed by the outer jurisdiction.

This means that for phi1 bootstrapping purposes we have two instances of the
native jurisdiction. One contains the boot protocol method table and the other
generates its own vtable allocations.


=head3 Receiver-parameterized method resolution
We ultimately don't want a mapping from just method name to index. It's more
efficient to parameterize on the receiver type to get better optimization; two
different protocols could specify the same method name, and we don't want to
force the maximum method index in those cases. We need to treat those methods as
separate vtable allocations.

I'll implement this optimally with bitset unions in C<phi2>, but for now I'll
keep it simple and use a single namespace. This means that the C<phi1>
implementation of jurisdictions incorrectly ignores the C<class>/C<protocol>
arguments specified for C<class_call> and C<protocol_call>.
=cut

use constant protocols_to_method_mapping_fn => phi::allocation
  ->constant(bin q{                     # ps cc
    # First, sort protocols by descending number of implementing classes
    swap                                # cc ps
    [ sget01 .classes .length           # pr pl cc pn1
      sget03 .classes .length ilt       # pr pl cc pn1>pn2?
      sset02 sset00 goto ]              # pn1>pn2?
    $sort_fn call                       # cc sort(ps)

    # Now go through each one and allocate its methods
    strmap swap                         # cc m sort(ps)
    [ sget01                            # p m cc m
      [ sget02 sget02 .<< sset02        # m m cc
        const0 sset01 goto ]            # p m cc m f
      sget04 .virtuals .reduce          # p m cc m
      sset02 const0 sset01 goto ]       # m exit?=0
    swap .reduce                        # cc m
    swap goto                           # m })
  ->named('protocols_to_method_mapping_fn') >> heap;


=head3 vtables
Here's the struct:

  struct amd64_native_vtable            # size = 26 + 8*n
  {
    hereptr     vtable;                 # offset = 0
    class      *source_class;           # offset = 8
    int         n;                      # offset = 16
    here_marker;                        # offset = 24
    hereptr<fn> fns[n];                 # offset = 26
  }

=cut

use constant vtable_class => phi::class->new('vtable',
  vtable_protocol,
  here_protocol,
  list_protocol)

  ->def(
    source_class => bin q{swap const8  iplus m64get swap goto},
    length       => bin q{swap const16 iplus m64get swap goto},
    here         => bin q{swap lit8+26 iplus        swap goto},

    '[]' => bin q{                      # i self cc
      sget02 lit8+3 ishl                # i self cc i*8
      lit8+26 iplus                     # i self cc i*8+26
      sget02 iplus m64get               # i self cc &fn
      sset02 sset00 goto                # &fn },

    reduce => bin q{
      "TODO: implement reduce() for vtables" i.die });

use constant class_to_vtable_fn => phi::allocation
  ->constant(bin q{                     # j c cc
    # Find the maximum allocated method index from this class. We'll need to
    # close over the jurisdiction object to make the reducer work, which in our
    # case entails assembling a custom function with that reference.

    sget01 .virtuals .keys              # j c cc ms
    asm                                 # [m max cc]
      const2 swap .sget                 # [m max cc m]
      sget03 swap .ptr                  # [m max cc m c]
      sget04 swap .ptr                  # [m max cc m c j]

      "resolve_class_method"
        "vtable_jurisdiction" %protocol_map .{}
        i.jurisdiction .protocol_call   # [m max cc mi]

      .dup lit8+3 swap .sget            # [m max cc mi mi max]
      .ilt                              # [m max cc mi max<mi?]

      .[ const2 swap .sset              # [mi max cc]
         .lit8 .0 const1 swap .sset     # [mi exit?=0 cc]
         .goto .]                       # [mi exit?=0]

      .[ .drop                          # [m max cc]
         const1 swap .sget              # [m max cc max]
         const2 swap .sset              # [max max cc]
         .lit8 .0 const1 swap .sset     # [max exit?=0 cc]
         .goto .]                       # [max exit?=0]

      .if .goto                         # ->branch
    .compile .here                      # j c cc ms fn

    sget01 const0 sset02                # j c cc 0 fn ms
    .reduce                             # j c cc max

    const1 iplus                        # j c cc n

    # Now we have the maximum method index; allocate the vtable object.
    dup lit8+3 ishl lit8+26 iplus
    i.heap_allocate                     # j c cc n &vt
    swap    sget01 const16 iplus m64set # j c cc &vt [.size=]
    $vtable_class sget01         m64set # [.vt=]
    sget02  sget01 const8  iplus m64set # [.source_class=]
    lit8+26 sget01 const24 iplus m16set # [.here_marker=]

    # Set each method defined by the class. Any undefined virtuals will be
    # uninitialized memory, which we hope segfaults if you hit them but really
    # it's anyone's guess.
    sget02 .virtuals .kv_pairs          # j c cc vt kvs
    [ sget01 .nil?                      # j c cc vt kvs loop nil?
      [ drop drop .here                 # j c cc vth
        sset02 sset00 goto ]            # vt
      [ sget01 .key                     # j c cc vt kvs loop k
        sget05 sget07                   # j c cc vt kvs loop k c j
        .resolve_class_method           # j c cc vt kvs loop mi

        lit8+3 ishl lit8+26 iplus       # j c cc vt kvs loop moff
        sget03 iplus                    # j c cc vt kvs loop &f
        sget02 .value swap m64set       # j c cc vt kvs loop [f=def]

        swap .tail swap                 # j c cc vt kvs.tail loop
        dup goto ]                      # ->loop
      if goto ]                         # j c cc vt kvs loop

    dup goto                            # ->loop })
  ->named('class_to_vtable_fn') >> heap;


=head3 Native jurisdiction base classes
These should be added as the LHS before compiling a class into a the native code
jurisdiction.
=cut

use constant initialize_native_jurisdiction_base_classes_fn => phi::allocation
  ->constant(bin q{                     # cc
    struct
      "vtable" i64f
    class "polymorphic_reference_base" i.def

    struct class "monomorphic_reference_base" i.def
    struct class "monomorphic_value_base"     i.def
    goto                                # })
  ->named('initialize_native_jurisdiction_base_classes_fn') >> heap;


=head3 Jurisdiction structure
Here's what the jurisdiction stores:

  struct amd64_native_vtable_jurisdiction
  {
    hereptr                       vtable;
    intmap<protocol*, _>         *protocols;            # NB: used as a set
    strmap<int>                  *vtable_allocation;
    map<class*, hereptr<vtable>> *class_vtables;
  }

=cut

use constant amd64_native_vtable_jurisdiction_class =>
  phi::class->new('amd64_native_vtable_jurisdiction',
    vtable_jurisdiction_protocol,
    shared_vtable_jurisdiction_protocol,
    jurisdiction_protocol)

  ->def(
    protocols             => bin q{swap const8  iplus m64get swap goto},
    method_allocation_map => bin q{swap const16 iplus m64get swap goto},
    class_vtable_map      => bin q{swap const24 iplus m64get swap goto},

    asm => bin q{                       # self cc
      swap tasm swap goto               # asm },

    monomorphic_value_type => bin q{
      %monomorphic_value_base sset01 goto },

    monomorphic_reference_type => bin q{
      %monomorphic_reference_base sset01 goto },

    polymorphic_reference_type => bin q{
      %polymorphic_reference_base sset01 goto },

    resolve_class_method => bin q{      # m c self cc
      sget03 sget02                     # m c self cc m self
      .method_allocation_map .{}        # m c self cc mi
      sset03 sset01 drop goto           # mi },

    resolve_protocol_method => bin q{   # m p self cc
      sget03 sget02                     # m p self cc m self
      .method_allocation_map .{}        # m p self cc mi
      sset03 sset01 drop goto           # mi },

    allocate_fixed => bin q{            # asm class self cc
      # The class's size is its right offset. Allocate that much memory within
      # the assembler.
      sget02 .fields .right_offset      # asm class self cc size

      dup const1 ineg ieq               # asm class self cc size -1?
      [ "cannot allocate_fixed a class whose size is computed" i.die ]
      [ goto ]
      if call                           # asm class self cc size

      sget02 swap                       # asm class self cc self size
      sset02 swap                       # asm class size self cc
      sget01 m64get :allocate_variable goto },

    allocate_variable => bin q{         # asm class size self cc
      swap sget02 sget01                # asm class size cc self size self
      sset03 sset00                     # asm class self cc size

      bswap32
      sget04                            # asm class self cc size asm
        .lit32 .l32                     # [size]
        .get_interpptr                  # [size i]
        "heap_allocate"
          "interpreter" %protocol_map .{}
          i.jurisdiction .protocol_call # asm class self cc asm [&obj]

      # Now assign the vtable if we have one.
      sget03 "vtable" swap .fields .contains?
      [ sget03 sget03                   # asm class self cc asm class self
        .class_vtable_map .{}           # asm class self cc asm vt
        swap
          .hereptr                      # asm class self cc asm [&obj vt]
          const1 swap .sget             # [&obj vt &obj]

        # NB: technically this is just a single m64set operation; if the class
        # has a vtable at all, it needs to be at offset 0. But doing it this way
        # is more indicative of the linkages involved and is a good test of our
        # struct stuff.
        sget03 .fields "vtable" swap .{} .setter_fn .here
          swap .hereptr                 # [&obj vt &obj setter]
          .call                         # asm class self cc asm [&obj]
        drop sset01 drop goto ]         # asm
      [ drop sset01 drop goto ]         # asm
      if goto                           # asm },

    protocol_call => bin q{             # asm m p self cc
      sget03 sget03 sget03              # asm m p self cc m p self
      .resolve_protocol_method          # asm m p self cc mi
      lit8+3 ishl bswap32               # asm m p self cc mi'

      sget05                            # asm m p self cc mi' asm
        .dup .m64get                    # [obj vt]
        .lit32 .l32                     # [obj vt mi']
        .iplus .m64get .call            # asm m p self cc asm [...]

      drop sset02 drop drop goto        # asm },

    class_call => bin q{                # asm m c self cc
      # Link directly to the here-fn mapped by the class; no vtable indirection
      # is required.
      sget03 sget03 sget03              # asm m c self cc m c self
      .resolve_class_method             # asm m c self cc mi

      # NB: we can't ask vtable objects for the function; i.e. we can't take
      # this vtable and resolve its here-ptr to a base-ptr, then use .[] -- our
      # phi0-exported vtables are fake.
      lit8+3 ishl                       # asm m c self cc mi'

      sget03 sget03 .class_vtable_map   # asm m c self cc mi' c c->v
      .{}                               # asm m c self cc mi' vth
      iplus m64get                      # asm m c self cc fn

      sget05 .hereptr .call             # asm m c self cc asm [fn call]
      drop sset02 drop drop goto        # asm });


use constant amd64_native_jurisdiction_fn => phi::allocation
  ->constant(bin q{                     # ps cc
    # Allocate method slots for the set of protocols
    sget01 $protocols_to_method_mapping_fn call             # ps cc ms

    # Create the jurisdiction object up front. We use a partially initialized
    # version of it to generate the class-to-vtable mapping it ultimately relies
    # on.
    const32 i.heap_allocate             # ps cc ms j
    $amd64_native_vtable_jurisdiction_class sget01 m64set   # [.vt=]
    intmap                              # ps cc ms j pm
      [                                 # p pm cc
        sget02 sget02 .<< sset02        # pm pm cc
        const0 sset01 goto ]            # ps cc ms j pm f [pm exit?=0]
      sget05 .reduce                    # ps cc ms j pm
    sget01        const8  iplus m64set  # ps cc ms j [.protocols=]
    sget01 sget01 const16 iplus m64set  # [.vtable_allocation=]

    # Now generate a vtable for each implementing class. To do this, we first
    # need to collect all classes.
    intmap                              # ps cc ms j cmap
    sget04                              # ps cc ms j cmap ps
    [                                   # p cmap cc
      sget01 sget03 .classes            # p cmap cc cmap cs
      [ sget02 sget02 .<< sset02        # cmap cmap cc
        const0 sset01 goto ]            # cmap exit?=0
      swap .reduce                      # p cmap cc cmap
      sset02 const0 sset01 goto ]       # ps cc ms j cmap ps f
    swap .reduce                        # ps cc ms j cmap

    # We've got an intmap from class to integer; we can modify that in-place
    # into an intmap from class to vtable hereptr. Unfortunately, we can't use
    # kvmap reduce for this because that just gives us the key list. We'll have
    # to iterate over the kv cells manually.
    dup .kv_pairs                       # ps cc ms j cmap kv
    [ sget01 .nil?                      # ps cc ms j cmap kv loop nil?
      [ # Set the class->vtable mapping and we're done.
        sget02 sget04                   # ps cc ms j cmap kv loop cmap j
        const24 iplus m64set            # ps cc ms j cmap kv loop [.class_vts=]
        drop drop drop                  # ps cc ms j
        sset02 drop goto ]              # j
      [ sget01 dup .key                 # ps cc ms j cmap kv loop kv c
        sget05 swap                     # ps cc ms j cmap kv loop kv j c
        $class_to_vtable_fn call        # ps cc ms j cmap kv loop kv vt
        swap .value= drop               # ps cc ms j cmap kv loop
        sget01 .tail sset01             # ps cc ms j cmap kv' loop
        dup goto ]                      # ->loop
      if goto ]                         # ps cc ms j cmap kv loop

    dup goto                            # ->loop })
  ->named('amd64_native_jurisdiction_fn') >> heap;


=head2 Test code
The big question here is whether the boot jurisdiction generates method calls
that are compatible with our boot object set. Once that's tested, the next thing
to check is that we manage to produce any halfway reasonable method allocation
with the generator function.
=cut

use constant boot_jurisdiction_test_fn => phi::allocation
  ->constant(bin q{                     # cc
    # Let's start by generating a function that calls .length on one of our
    # bootstrap-exported maps. We'll do this twice: first using the list
    # protocol (vtable-indirect), then using a direct class linkage.
    %class_map dup .length swap         # cc n cm

    i.jurisdiction .asm                 # cc n cm asm []
      "list" %protocol_map .{}
                     swap .push         # [cm:list]
      $unknown_value swap .push         # [cm cc]

      .swap                             # [cc cm]
      .'length                          # [cc l]
      .swap .goto                       # [l]
    .compile .call                      # cc n cl
    ieq "length via prototype" i.assert # cc

    # Same thing, this time with a direct-linked class method call.
    %class_map dup .length swap         # cc n cm
    i.jurisdiction .asm                 # cc n cm asm []
      "linked_map" %class_map .{}
                     swap .push         # [cm:linked_map]
      $unknown_value swap .push         # [cm cc]
      .swap                             # [cc cm]
      .'length                          # [cc l]
      .swap .goto                       # [l]
    .compile .call                      # cc n cl
    ieq "length via class" i.assert     # cc

    goto                                # })
  ->named('boot_jurisdiction_test_fn') >> heap;


use constant native_jurisdiction_test_fn => phi::allocation
  ->constant(bin q{                     # cc
    # Basic test: define a protocol for an unapplied binary operation.
    protocol
      "apply" swap .defvirtual
      "lhs"   swap .defvirtual
      "rhs"   swap .defvirtual          # cc p

    dup                                 # cc p p

    # Now define a class that implements this protocol.
    struct
      "lhs" i64f
      "rhs" i64f
    class                               # cc p p c
      i.jurisdiction
        .polymorphic_reference_type .+  # cc p p c
      .implement                        # cc p c

      [ swap const8 iplus m64get swap goto ] swap
        "lhs" swap .defvirtual

      [ swap const16 iplus m64get swap goto ] swap
        "rhs" swap .defvirtual

      [ swap dup                        # cc self self
        const8 iplus m64get swap        # cc lhs self
        const16 iplus m64get iplus      # cc v
        swap goto ] swap
        "apply" swap .defvirtual        # cc p c

      [ swap                            # cc asm [self]
        .dup .lit8 const8 swap .l8      # cc asm [self self loff]
          .iplus .m64get                # cc asm [self lhs]
        .swap .lit8 const16 swap .l8    # cc asm [lhs self roff]
          .iplus .m64get                # cc asm [lhs rhs]
        .iplus                          # cc asm [lhs+rhs]
        swap goto ] swap
        "inline" swap .defmethod        # cc p c

    # Verify that we have the right object size and layout
    dup .fields .right_offset const24 ieq "class objsize" i.assert
    dup .fields "vtable" swap .{}
      .left_offset const0 ieq "class &vtable=0" i.assert

    sget01 intlist .<<                  # cc p c [p]

    $amd64_native_jurisdiction_fn call  # cc p c j

    # OK, allocate an instance of this class and make sure it works correctly.
    asm                                 # cc p c j asm [cc]
      sget02 sget02 .allocate_fixed     # [cc &obj]
      .swap .goto                       # [&obj]
    .compile .call                      # cc p c j obj

    # Verify some things about the vtable.
    dup m64get                          # cc p c j obj vth
      dup const2 ineg iplus
      m16get ineg iplus                 # cc p c j obj vt

    dup .length lit8+3 ieq "vtlen" i.assert
    drop                                # cc p c j obj

    dup                                 # cc p c j obj obj

    # Type the argument as a protocol
    sget04 sget03 .asm                  # cc p c j obj obj p asm
      # Push the initial stack contents. Right now we have the object and an
      # unknown.
      .push                             # [obj:p]
      $unknown_value swap .push         # [obj:p cc:unknown]

      .swap                             # [cc obj:p]

      .lit8 lit8+17 swap .l8            # [cc obj:p 17]
      const1 swap .sget .lit8 const8 swap .l8
        .iplus .m64set                  # [cc obj:p [.lhs=]]

      .lit8 lit8+30 swap .l8            # [cc obj:p 30]
      const1 swap .sget .lit8 const16 swap .l8
        .iplus .m64set                  # [cc obj:p [.rhs=]]

      .'apply                           # [cc obj.apply]

      .swap .goto                       # [obj.apply]

    .compile .call                      # cc p c j obj 47

    lit8+47 ieq "jp47" i.assert         # cc p c j obj

    dup

    # Now do the same thing using a direct class method call
    sget03 sget03 .asm                  # cc p c j obj obj p asm
      .push                             # [obj:c]
      $unknown_value swap .push         # [obj:c cc:unknown]
      .swap                             # [cc obj:c]

      .lit8 lit8+17 swap .l8            # [cc obj:c 17]
      const1 swap .sget .lit8 const8 swap .l8
        .iplus .m64set                  # [cc obj:c [.lhs=]]

      .lit8 lit8+30 swap .l8            # [cc obj:c 30]
      const1 swap .sget .lit8 const16 swap .l8
        .iplus .m64set                  # [cc obj:c [.rhs=]]

      .'apply                           # [cc obj.apply]
      .swap .goto                       # [obj.apply]
    .compile
    .call                               # cc p c j obj 47

    lit8+47 ieq "jc47" i.assert         # cc p c j obj

    dup                                 # cc p c j obj obj

    # Finally, do the same thing using native linkage
    sget03 sget03 .asm                  # cc p c j obj obj p asm
      .push                             # [obj:c]
      $unknown_value swap .push         # [obj:c cc:unknown]
      .swap                             # [cc obj:c]

      .lit8 lit8+17 swap .l8            # [cc obj:c 17]
      const1 swap .sget .lit8 const8 swap .l8
        .iplus .m64set                  # [cc obj:c [.lhs=]]

      .lit8 lit8+30 swap .l8            # [cc obj:c 30]
      const1 swap .sget .lit8 const16 swap .l8
        .iplus .m64set                  # [cc obj:c [.rhs=]]

      .'inline                          # [cc obj.inline]
      .swap .goto                       # [obj.inline]
    .compile
    .call                               # cc p c j obj 47

    lit8+47 ieq "ji47" i.assert         # cc p c j obj
    drop                                # cc p c j

    drop drop drop                      # cc
    goto                                # })
  ->named('native_jurisdiction_test_fn') >> heap;


1;
