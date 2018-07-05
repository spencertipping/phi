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
The overall size of generated vtables looks like this:

  ∑ max(method_indexes(c))
  c

TODO


=head2 Metaclasses
phi structs are built as data in the form of cons links, and phi classes are no
different. A class is a series of operations applied to a null origin value;
concatenatively:

  class
    "vtable"                vtable
    "x"        $int64_class field
    "y"        $int64_class field
    "distance" [...]        method
    $some_protocol          implement

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
=cut


use constant nil_class_class => phi::class->new('nil_class',
  maybe_nil_protocol,
  list_protocol,
  class_protocol)

  ->def(
    'nil?'    => bin q{const1 sset01 goto},
    protocols => bin q{$nil_instance sset01 goto},
    methods   => bin q{$nil_instance sset01 goto},
    fields    => bin q{$nil_instance sset01 goto},
    vtable    => bin q{"nil class has no vtable" i.die},
    struct    => bin q{struct sset01 goto},

    length    => bin q{const0 sset01 goto},
    '[]'      => bin q{"cannot call [] on nil" i.die},
    reduce    => bin q{sset01 drop goto},

    metaclass_journal => bin q{$nil_instance sset01 goto});

use constant nil_class_instance => phi::allocation
  ->constant(pack Q => nil_class_class->vtable >> heap)
  ->named('nil_class_instance') >> heap;


=head2 Class elements
Conceptually, classes behave as though you consed them in this order:

  nil
    fields...
    methods...
    protocols...
    metaclasses...

This ordering also applies to fields and methods produced by metaclasses; the
contract is that after a metaclass is applied, you can't tell the difference
between elements it added and elements that were manually added.

So, with all of this in mind, let's lay out some structs and define some stuff.


=head3 TODO: redo this a bit
The current API is pretty broken. A class isn't a single list of consed-up
things that are then interpreted as being in some order.

Instead, a class should be several cons/list/whatever link heads that can be
appended to using method calls. So we'd have a struct like this:

  struct class
  {
    struct      *fields;
    strmap      *methods;
    linked_list *protocols;
    linked_list *metaclasses;
  }

There's also no reason to have a separate management structure for fields;
structs serve the purpose just fine. (All we really need is some machinery to
automate the class annotations on struct links.)

We can drop C<attributes>; it serves no purpose.

Method definitions are pairs of C<name, bytecode> objects.

I think C<protocols> can just be a list of protocol objects; I don't think we
need names.

C<metaclasses> is the journal of _unapplied_ metaclasses. We'll have a method
that returns a new class where each metaclass has been applied. Application
order is right to left, consistent with other aspects of classes and structs.

TODO: let's drop the term "metaclass" in favor of "transformation" or some such.

Q: who manages vtables? It's tempting to have class objects cache them, but this
creates problems around mutability/invalidation. Do we store the mapping
externally?

Q: who owns protocol/class vtable slot negotiation? I need to finish the
algorithm spec above, which may help answer this question.


=head3 Field links
Conses a field onto a class:

  struct class_field
  {
    hereptr      vtable;
    class       *tail;
    strmap      *attributes;
    struct_link *struct_link;
  }

=cut

use constant class_field_class => phi::class->new('class_field',
  cons_protocol,
  list_protocol,
  map_protocol,
  kv_protocol,
  maybe_nil_protocol,
  class_protocol,
  class_field_protocol,
  class_element_protocol)

  ->def(
    'nil?' => bin q{const0 sset01 goto},
    head   => bin q{goto},
    tail   => bin q{swap const8 iplus m64get swap goto},

    name        => bin q{swap .struct_link .name swap goto},
    attributes  => bin q{swap const16 iplus m64get swap goto},
    struct_link => bin q{swap const24 iplus m64get swap goto},

    protocols         => bin q{swap .tail .protocols         swap goto},
    methods           => bin q{swap .tail .methods           swap goto},
    vtable            => bin q{swap .tail .vtable            swap goto},
    metaclass_journal => bin q{swap .tail .metaclass_journal swap goto},
    fields            => bin q{goto},

    struct            => bin q{         # self cc
      sget01 .tail .struct              # self cc ts
      sget02 .struct_link .with_tail    # self cc s
      sset01 goto                       # s },

    length => bin q{                    # self cc
      sget01 .tail .length              # self cc tl
      const1 iplus                      # self cc tl+1
      sset01 goto                       # tl+1 },

    '[]' => bin q{                      # i self cc
      sget02                            # i self cc i?
      [ sget02 const1 ineg iplus sset02 # i-1 self cc
        sget01 .tail sset01             # i-1 tail cc
        sget01 m64get :[] goto ]        # ->tail.[]
      [ sset01 swap goto ]              # self
      if goto },

    reduce => bin q{                    # x0 f self cc
      sget03 sget02 sget04 call         # x0 f self cc x0' exit?
      [ sset03 sset01 drop goto ]       # x0'
      [ sset03                          # x0' f self cc
        sget01 .tail sset01             # x0' f tail cc
        sget01 m64get :reduce goto ]    # ->tail.reduce
      if goto                           # x0' },

    key        => q{swap .name swap goto},
    value      => q{goto},

    'key==_fn' => q{$strcmp_fn sset01 goto},
    keys       => q{goto},
    kv_pairs   => q{goto},

    '{}' => bin q{                      # name self cc
      sget01 .name sget03 .==           # name self cc =?
      [ sset01 swap goto ]              # self
      [ sget01 .tail sset01             # name tail cc
        sget01 m64get :{} goto ]        # ->tail.{}
      if goto });


=head3 Method links


=cut


1;
