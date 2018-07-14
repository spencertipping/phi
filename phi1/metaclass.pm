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
2. A string-map of method definitions (NB: typed assembler transforms)
3. A string-map of virtual functions
4. A list of protocols implemented by the class

(2) is interesting. Classes don't define the literal function objects that go
into the vtable -- by which I mean they _can_, but by default you're operating
at a lower level: each method is a function that transforms a typed macro
assembler object (see L<phi1/compiler.pm>). This gives classes the ability to
inline methods into the call site.

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
    strmap  *virtuals;          # NB: used as a set
    intmap  *classes;           # NB: used as a set
  }

=cut


use constant protocol_class => phi::class->new('protocol',
  protocol_protocol,
  mutable_protocol_protocol)

  ->def(
    virtuals => bin q{swap const8  iplus m64get swap goto},
    classes  => bin q{swap const16 iplus m64get swap goto},

    defvirtual => bin q{                # m self cc
      sget02 sget02 .virtuals .<<       # m self cc ms
      drop sset01 swap goto             # self },

    'implementors<<' => bin q{          # c self cc
      sget02 sget02 .classes .<<        # c self cc cs
      drop sset01 swap goto             # self });


use constant empty_protocol_fn => phi::allocation
  ->constant(bin q{                     # cc
    const24 i.heap_allocate             # cc p
    $protocol_class sget01 m64set               # [.vtable=]
    strmap          sget01 const8  iplus m64set # [.virtuals=]
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


=head3 C<class> struct
Here's what a class looks like:

  struct class
  {
    hereptr                  vtable;
    struct                  *fields;
    strmap<hereptr<fn>>     *methods;
    strmap<hereptr<fn>>     *virtuals;
    intmap<protocol*>       *protocols;     # NB: used as a set
    linked_list<metaclass*> *metaclasses;
  }

=cut


use constant class_class => phi::class->new('class',
  class_protocol,
  joinable_protocol,
  mutable_class_protocol)

  ->def(
    fields      => bin q{swap const8  iplus m64get swap goto},
    methods     => bin q{swap const16 iplus m64get swap goto},
    virtuals    => bin q{swap const24 iplus m64get swap goto},
    protocols   => bin q{swap const32 iplus m64get swap goto},
    metaclasses => bin q{swap lit8+40 iplus m64get swap goto},

    '+' => bin q{                       # rhs self cc
      lit8+48 i.heap_allocate           # rhs self cc c
      sget02 m64get sget01 m64set       # [.vt=]
      sget03 .fields    sget03 .fields    .+ sget01 const8  iplus m64set
      sget03 .methods   sget03 .methods   .+ sget01 const16 iplus m64set
      sget03 .virtuals  sget03 .virtuals  .+ sget01 const24 iplus m64set
      sget03 .protocols sget03 .protocols .+ sget01 const32 iplus m64set

      # Metaclasses aren't additive; use just the ones from the LHS (self),
      # ignoring the ones on the RHS. We don't flatten the RHS here because
      # classes are data as much as they are compiled objects.
      #
      # NB: metaclasses are an abomination and phi2 doesn't support them.

      intlist sget03 .metaclasses         .+ sget01 lit8+40 iplus m64set

      sset02 sset00 goto                # c },

    flatten => bin q{                   # self cc
      # Apply each metaclass, right to left.
      sget01 dup .metaclasses           # self cc self ms
      .root_cons $nil_instance          # self cc self ms []
      $rev_fn call                      # self cc self rev(ms)
      [ sget01 sget03 .transform        # m c cc c'
        sset02 const0 sset01 goto ]     # c' exit?=0
      swap .reduce                      # self cc self'
      sset01 goto                       # self' },

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
      drop sset01 swap goto             # self });


use constant class_fn => phi::allocation
  ->constant(bin q{                     # struct cc
    lit8+48 i.heap_allocate             # struct cc c
    $class_class sget01               m64set    # [.vtable=]
    sget02       sget01 const8  iplus m64set    # [.fields=]
    strmap       sget01 const16 iplus m64set    # [.methods=]
    strmap       sget01 const24 iplus m64set    # [.virtuals=]
    intmap       sget01 const32 iplus m64set    # [.protocols=]
    intlist      sget01 lit8+40 iplus m64set    # [.metaclasses=]
    sset01 goto                         # c })
  ->named('class_fn') >> heap;

BEGIN
{
  bin_macros->{class} = bin q{$class_fn call};
}


1;
