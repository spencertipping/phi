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




1;
