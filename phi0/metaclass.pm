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


=head2 Regular classes
Structurally, classes consist of three things:

1. A struct describing the data layout of each instance
2. A string-map of method definitions
3. A list of protocols implemented by the class

Classes can also generate vtables, but they don't track those vtables; the
linkage goes only the other way (from the vtable to the class).

By themselves, classes don't know about any metaprogramming features like
accessor generation or GC. That stuff is managed by metaclasses, which are
functional transformations you apply to class objects.

Before I get into that in detail, let's talk about a basic aspect of classes,
the negotiation around protocols and vtable indexing.


=head3 Protocols and method allocation
A protocol is a set of methods you can use to address some object. The contract
is that any method within a protocol uses the same vtable index across all
implementing classes; so if we have something like this:

  protocol cons_like
  {
    head();
    tail();
  }

...then any class implementing C<head> will use the same vtable slot. This means
a method call to C<head> can be compiled down to a C<method> instruction with a
constant index. Method polymorphism is implemented by vtable polymorphism.

Our boot image has many protocol objects, but from a method allocation point of
view we're effectively having every object delegate all methods through a single
"possibly any method" protocol; that is, each method in the boot image gets a
separate vtable index. This strategy is simple and detects errors well, but it
doesn't scale well. There are plenty of cases where you'll want to have more
than 65536 total distinct methods, and in all likelihood you don't want the
500KiB vtables you'd get when you got close to that limit. Classes and protocols
need to minimize the overall vtable size when allocating vtables.

=head4 Naive allocation strategy: first available slot
We could start by asking the protocol for a vtable slot commitment for a method:

  cons_like.method_index_for("head")

C<cons_like> would then enumerate its implementing classes and offer method
indexes until all of those classes agree to accept some value.

This strategy works but isn't optimal; there's no guarantee that we're
minimizing the overall vtable size because the method allocation ordering
depends on the order of calls to C<method_index_for>. We can do better.

=head4 Global allocation strategy: allocate common methods first
Globally speaking, the combined size of all allocated vtables is determined by
the maximum method index of each (which is sort of obvious, but still worth
stating). Protocols must be disjoint when implemented by the same class, which
is the force that propels method indexes upwards.

Given this setup, a simple strategy is to start with a protocol, build the full
set of co-implemented protocols, and allocate their methods prioritized by the
number of classes implementing each. That is:

  method_index = 0
  all_protocols = p.transitive_closure(.classes.flatmap(.protocols))
  all_protocols.sort_descending_by(.classes.size)
               .each(.allocate_methods(&method_index))


=head3 Protocol objects
Protocols are pretty simple: we just have a list of methods and a list of
classes. The only point of any complexity is when we compile them down to
vtables, but that's managed by returning a separate object.

  struct protocol
  {
    hereptr  vtable;
    strmap  *methods;           # NB: used as a set
    intmap  *classes;           # NB: used as a set
  }

=cut


use constant protocol_class => phi::class->new('protocol',
  protocol_protocol,
  mutable_protocol_protocol,
  vtable_allocator_protocol)

  ->def(
    methods => bin q{swap const8  iplus m64get swap goto},
    classes => bin q{swap const16 iplus m64get swap goto},

    defmethod => bin q{                 # m self cc
      sget02 sget02 .methods .<<        # m self cc ms
      drop sset01 swap goto             # self },

    'implementors<<' => bin q{          # c self cc
      sget02 sget02 .classes .<<        # c self cc cs
      drop sset01 swap goto             # self },

    closure_set => bin q{               # set self cc
      sget01 sget03 .contains?          # set self cc contains?
      [ sset00 goto ]                   # set
      [ sget01 sget03 .<< drop          # set self cc [set<<self]
        sget01 .classes .kv_pairs       # set self cc cs
        [                               # set self cc cs loop
          sget01 .nil?                  # set self cc cs loop cs.nil?
          [ drop drop sset00 goto ]     # set
          [ sget01 dup .tail            # set self cc cs loop cs ct
            sset02 .head .protocols     # set self cc ct loop c.protos
            sget05 swap                 # set self cc ct loop set c.protos
            [                           # proto set cc
              sget01 sget03             # proto set cc set proto
              .closure_set sset02       # set set cc
              const0 sset01 goto ]      # [set exit?=0] ...set c.protos f
            swap .reduce drop           # set self cc ct loop
            dup goto ]                  # ->loop
          if goto ]                     # set self cc cs loop
        dup goto ]                      # set
      if goto                           # set },

    allocate_vtable_slots => bin q{     # self cc
      # Return a string map from method name to its allocated index.
      intmap sget02 .closure_set .keys  # self cc ps
      [ sget01 .classes .length         # pr pl cc pln
        sget03 .classes .length ilt     # pr pl cc pln>prn
        sset02 sset00 goto ]            # self cc ps f
      $sort_fn call                     # self cc sort(ps)

      # At this point sort(ps) contains the protocols in order of descending
      # number of implementing classes -- so we can number the methods
      # sequentially within each protocol and arrive at the correct solution.
      strmap swap                       # self cc m sort(ps)
      [ sget01                          # proto m cc m
        [ sget02 sget02 .<< sset02      # m m cc
          const0 sset01 goto ]          # [m exit?=0] proto m cc m f
        sget04 .methods .reduce         # proto m cc m
        sset02                          # m m cc
        const0 sset01 goto ]            # [m exit?=0] self cc m sort(ps) f
      swap .reduce                      # self cc m
      sset01 goto                       # m });


use constant empty_protocol_fn => phi::allocation
  ->constant(bin q{                     # cc
    const24 i.heap_allocate             # cc p
    $protocol_class sget01 m64set               # [.vtable=]
    strmap          sget01 const8  iplus m64set # [.methods=]
    intmap          sget01 const16 iplus m64set # [.classes=]
    swap goto                           # p })
  ->named('empty_protocol_fn') >> heap;

BEGIN
{
  bin_macros->{protocol} = bin q{$empty_protocol_fn call};
}


=head2 Metaclasses
Metaclasses are functions from classes to classes. For example, we could define
a metaclass that added a method for each field:

  class
    ...
    "*" field_getters

In this example C<field_getters> consumes the class, iterates through its field
elements, and conses on a new method for each field matching the pattern.


=head3 Garbage collection
phi relies on metaclasses right out of the gate to support garbage collection.
Unlike in most languages, phi garbage collection is implemented as a protocol
implemented by nearly every object; objects can then rewrite themselves into a
new heap when traced. GC behavior can be automatically generated for many
classes, so there's a metaclass that converts a bare object to one that is
GC-compliant:

  class
    "vtable" vtable
    ...
    gc                        # add GC support to this object

Structurally speaking, C<gc> enumerates class members and generates method
implementations that are hard-coded to trace those members. For this reason it's
important to include metaclasses like C<gc> _after_ all field definitions are
present; if you consed a field after C<gc>, only a subset of the class would be
copied into the new heap. Having to think about this is a major bummer, of
course, so classes store one additional element (the metaclass journal) to make
life a little easier.


=head3 Insertion points and journal replay
Metaclasses undermine the "classes are data" assertion by correlating data
elements. Take getter generation, for example: semantically speaking, it's more
correct to say "we have a class with two fields and accessors for those fields"
than it is to say "we have a class with two fields and two methods." The methods
exist only because the fields do; they're derived data.

This distinction matters from an API perspective. Rather than
applying-and-forgetting a function, phi classes store a list of metaclass
transformation objects that you've applied. This solves the field-after-GC
problem I mentioned above: if you add a new field to a class with metaclass
transformations, the field will be inserted into the class stack with all of the
class's other fields, then the metaclass journal will be replayed as though you
had added the field prior to applying any metaclasses.


=head3 C<class> struct
Here's what a class looks like:

  struct class
  {
    hereptr                  vtable;
    struct                  *fields;
    strmap<hereptr<fn>>     *methods;
    intmap<protocol*>       *protocols;     # NB: used as a set
    linked_list<metaclass*> *metaclasses;
  }

=cut


use constant class_class => phi::class->new('class',
  class_protocol,
  mutable_class_protocol)

  ->def(
    fields      => bin q{swap const8  iplus m64get swap goto},
    methods     => bin q{swap const16 iplus m64get swap goto},
    protocols   => bin q{swap const24 iplus m64get swap goto},
    metaclasses => bin q{swap const32 iplus m64get swap goto},

    defmethod => bin q{                 # fn name self cc
      sget03 sget03 sget03              # fn name self cc fn name self
      .methods .{}=                     # fn name self cc methods [{name}=value]
      drop sset01 sset01 goto           # self },

    implement => bin q{                 # p self cc
      sget02 sget02 .protocols .<<      # p self cc protos
      drop sget01 sget03
                  .implementors<<       # p self cc proto
      drop sset01 swap goto             # self },

    vtable => bin q{                    # mapping self cc
      # The method mapping contains everything from our protocols' closure set,
      # which could easily involve more methods than this class defines. We need
      # to find the maximum index of any method _we_ define to figure out how
      # much space the vtable should use.
      #
      # Due to the way the mapping is constructed, this maximum index will be
      # the first one in the kv list (only true for the boot image and linked
      # k/v maps, by the way).

      sget01 .methods sget03 .keys      # m self cc ms ks
      [ sget02 sget02 .contains?        # m ks cc contains?
        [ # Return this method as the reduced quantity
          const1 sset01 goto ]          # m exit?=1
        [ sget01 sset02                 # ks ks cc
          const0 sset01 goto ]          # ks exit?=0
        if goto ]                       # m self cc ms ks f
      swap .reduce                      # m self cc method
      sget03 .{}                        # m self cc maxi

      # Now we can allocate the vtable object. For now let's just allocate the
      # method implementation array -- i.e. the thing we'd normally here-point
      # to.
      const1 iplus lit8+3 ishl          # m self cc vtsize
      i.heap_allocate                   # m self cc vt
      sget02 .methods .kv_pairs         # m self cc vt kv
      [ sget01 .nil?                    # m self cc vt kv loop nil?
        [ drop drop                     # m self cc vt
          sset02 sset00 goto ]          # vt
        [ sget01 .key dup sget07 .{}    # m self cc vt kv loop k mi
          swap sget06 .methods .{}      # m self cc vt kv loop mi def
          swap lit8+03 ishl             # m self cc vt kv loop def mi*8
          sget04 iplus m64set           # m self cc vt kv loop [vt[mi]=def]
          sget01 .tail sset01           # m self cc vt kt loop
          dup goto ]                    # ->loop
        if goto ]                       # m self cc vt kv loop
      dup goto                          # ->loop });


use constant class_fn => phi::allocation
  ->constant(bin q{                     # struct cc
    cell8+5 i.heap_allocate             # struct cc c
    $class_class sget01               m64set    # [.vtable=]
    sget02       sget01 const8  iplus m64set    # [.fields=]
    strmap       sget01 const16 iplus m64set    # [.methods=]
    intmap       sget01 const24 iplus m64set    # [.protocols=]
    intlist      sget01 const32 iplus m64set    # [.metaclasses=]
    sset01 goto                         # c })
  ->named('class_fn') >> heap;

BEGIN
{
  bin_macros->{class} = bin q{$class_fn call};
}


use constant protocol_test_fn => phi::allocation
  ->constant(bin q{                     # cc
    protocol
      "a" swap .defmethod
      "b" swap .defmethod               # cc p

    dup intmap swap .closure_set        # cc p [p]
      dup .length const1 ieq
        "cs len1" i.assert              # cc p [p]
      dup const0 swap .[]               # cc p [p] p
          sget02 ieq
        "cs p[0]" i.assert              # cc p [p]
      drop                              # cc p

    dup .allocate_vtable_slots          # cc p m
      dup .length      const2 ieq "vta len2" i.assert
      dup "a" swap .{} const1 ieq "vta ma" i.assert
      dup "b" swap .{} const0 ieq "vta mb" i.assert
      drop                              # cc p

    # Now create a second protocol and bridge them with an implementing class.
    protocol
      "c" swap .defmethod               # cc p1 p2

    sget01 sget01                       # cc p1 p2 p1 p2

    struct
    class
      .implement
      .implement                        # cc p1 p2 c[p1,p2]

    # Each protocol should produce the same closure set, up to ordering.
    sget02 intmap swap .closure_set     # cc p1 p2 c cs1
      dup .length const2 ieq     "cs1 len2" i.assert
      dup sget04 swap .contains? "cs1p1"    i.assert
      dup sget03 swap .contains? "cs1p2"    i.assert
      drop                              # cc p1 p2 c

    sget01 intmap swap .closure_set     # cc p1 p2 c cs2
      dup .length const2 ieq     "cs2 len2" i.assert
      dup sget04 swap .contains? "cs2p1"    i.assert
      dup sget03 swap .contains? "cs2p2"    i.assert
      drop                              # cc p1 p2 c

    sget02 .allocate_vtable_slots       # cc p1 p2 c m
      dup .length lit8+3 ieq "p1ms len3" i.assert
      dup "a" swap .contains? "p1ms ca"  i.assert
      dup "b" swap .contains? "p1ms cb"  i.assert
      dup "c" swap .contains? "p1ms cc"  i.assert
      drop                              # cc p1 p2 c

    sget01 .allocate_vtable_slots       # cc p1 p2 c m
      dup .length lit8+3 ieq "p2ms len3" i.assert
      dup "a" swap .contains? "p2ms ca"  i.assert
      dup "b" swap .contains? "p2ms cb"  i.assert
      dup "c" swap .contains? "p2ms cc"  i.assert
      drop                              # cc p1 p2 c

    # Now define a second class that implements p2 and a new protocol p3.
    struct
    class                               # cc p1 p2 c1 c2
      sget02 swap .implement            # cc p1 p2 c1 c2

    protocol
      "d" swap .defmethod               # cc p1 p2 c1 c2 p3
    swap .implement                     # cc p1 p2 c1 c2

    drop sget02                         # cc p1 p2 c1 p1
    dup intmap swap .closure_set        # cc p1 p2 c1 p1 cs
      dup .length lit8+3 ieq "p1cs len3" i.assert
      drop

    .allocate_vtable_slots              # cc p1 p2 c1 m
      dup .length const4 ieq "p1ms len4" i.assert
      dup "a" swap .contains? "p1ms ca" i.assert
      dup "b" swap .contains? "p1ms cb" i.assert
      dup "c" swap .contains? "p1ms cc" i.assert
      dup "d" swap .contains? "p1ms cd" i.assert

      # At this point, methods "c" and "d" are implemented by two classes
      # whereas "a" and "b" are implemented by just one. So we expect c and d to
      # have lower method indexes.
      dup "a" swap .{}                  # cc p1 p2 c1 m ia
        sget01 "c" swap .{}             # cc p1 p2 c1 m ia ic
        ilt "c<a" i.assert              # cc p1 p2 c1 m

      dup "b" swap .{} sget01 "c" swap .{} ilt "c<b" i.assert
      dup "a" swap .{} sget01 "d" swap .{} ilt "d<a" i.assert
      dup "b" swap .{} sget01 "d" swap .{} ilt "d<b" i.assert

      drop

    sset01 drop                         # cc c

    drop                                # cc
    goto                                # })
  ->named('protocol_test_fn') >> heap;


use constant class_test_fn => phi::allocation
  ->constant(bin q{                     # cc
    # Test vtable generation. We should end up with a usable object, albeit not
    # one that uses the boot protocol.

    protocol
      "inc" swap .defmethod
      "dec" swap .defmethod             # cc p

    dup                                 # cc p p

    struct
    class                               # cc p p c
      .implement                        # cc p c
      asm
        .swap .const1 .iplus .swap .goto
      .compile .here                    # cc p c fnh
      swap "inc" swap .defmethod        # cc p c

      asm
        .swap .const1 .ineg .iplus .swap .goto
      .compile .here                    # cc p c fnh
      swap "dec" swap .defmethod        # cc p c

    # Now compile the vtable by asking the protocol to allocate methods.
    sget01 .allocate_vtable_slots       # cc p c ms

    dup sget02 .vtable                  # cc p c ms vt

    # Now we have a vtable. We should be able to use it to manipulate an object,
    # but we'll have to asm-compile the code because we don't have hard-coded
    # method offsets.
    asm                                 # ... ms vt asm [vt cc]
      .const0                           # ... ms vt asm [vt cc 0]
      .sget .2                          # ... ms vt asm [vt cc 0 vt]
      .method
        "inc" sget03 .{}
        bswap16 swap .l16               # ... ms vt asm [vt cc 0 .inc]
      .call                             # ... ms vt asm [vt cc 1]
      .swap
      .goto                             # ... ms vt asm [vt 1]
    .compile .call                      # cc p c ms vt 1

    const1 ieq "vtable inc 1" i.assert

    drop drop

    # Now do everything again, this time using the boot protocol method
    # allocation. We'll get a much larger vtable, but we'll be able to make
    # normal method calls against the resulting object.

    %method_vtable_mapping              # cc p c ms
    sget01 .vtable                      # cc p c vt

    const0 sget01 :inc call             # cc p c vt 1
      dup const1 ieq "0inc1" i.assert
    sget01 :inc call                    # cc p c vt 2
      dup const2 ieq "1inc2" i.assert
    sget01 :dec call                    # cc p c vt 1
      dup const1 ieq "2dec1" i.assert
    drop                                # cc p c vt

    drop drop drop                      # cc
    goto                                # })
  ->named('class_test_fn') >> heap;


1;
