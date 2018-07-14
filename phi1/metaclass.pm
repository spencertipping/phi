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
Structurally, classes consist of four things:

1. A struct describing the data layout of each instance
2. A string-map of method definitions (NB: typed assembler transforms)
3. A string-map of virtual functions
4. A list of protocols implemented by the class

In phi-land, "method" means "a thing a class does to the compiling assembler"
whereas "virtual" corresponds to the more common concept of functions provided
by classes. Classes use virtuals to implement protocol functionality.


=head3 Protocol objects
Protocols are pretty simple: we just have a list of virtuals and a list of
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
    hereptr              vtable;
    struct              *fields;
    strmap<hereptr<fn>> *methods;
    strmap<hereptr<fn>> *virtuals;
    intmap<protocol*>   *protocols;     # NB: used as a set
  }

=cut


use constant class_class => phi::class->new('class',
  class_protocol,
  joinable_protocol,
  mutable_class_protocol)

  ->def(
    fields    => bin q{swap const8  iplus m64get swap goto},
    methods   => bin q{swap const16 iplus m64get swap goto},
    virtuals  => bin q{swap const24 iplus m64get swap goto},
    protocols => bin q{swap const32 iplus m64get swap goto},

    '+' => bin q{                       # rhs self cc
      lit8+40 i.heap_allocate           # rhs self cc c
      sget02 m64get sget01 m64set       # [.vt=]

      # NB: fields are in reverse order
      sget02 .fields    sget04 .fields    .+ sget01 const8  iplus m64set
      sget03 .methods   sget03 .methods   .+ sget01 const16 iplus m64set
      sget03 .virtuals  sget03 .virtuals  .+ sget01 const24 iplus m64set
      sget03 .protocols sget03 .protocols .+ sget01 const32 iplus m64set
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
      drop sset01 swap goto             # self });


use constant class_fn => phi::allocation
  ->constant(bin q{                     # struct cc
    lit8+40 i.heap_allocate             # struct cc c
    $class_class sget01               m64set    # [.vtable=]
    sget02       sget01 const8  iplus m64set    # [.fields=]
    strmap       sget01 const16 iplus m64set    # [.methods=]
    strmap       sget01 const24 iplus m64set    # [.virtuals=]
    intmap       sget01 const32 iplus m64set    # [.protocols=]
    sset01 goto                         # c })
  ->named('class_fn') >> heap;

BEGIN
{
  bin_macros->{class} = bin q{$class_fn call};
}


1;
