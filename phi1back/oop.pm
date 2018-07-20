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
2. A string-map of method definitions (NB: typed assembler transforms)
3. A string-map of virtual functions
4. A list of protocols implemented by the class

In phi-land, "method" means "a thing a class does to the compiling assembler"
whereas "virtual" corresponds to the more common concept of functions provided
by classes. Classes use virtuals to implement protocol functionality.

Put differently, phi gives you as much leverage as it its type information
allows it to provide.

phi1 encodes all classes as RTTI instances and uses a single vtable-polymorphic
CTTI to address them.


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

Like classes, protocols are compilers. These end up being used by typed macro
assemblers to write bytecode. In this case we aren't about much: we just ask the
compiling jurisdiction to produce a method call against ourselves.
=cut


use constant protocol_class => phi::class->new('protocol',
  symbolic_method_protocol,
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
      drop sset01 swap goto             # self },

    symbolic_method => bin q{           # asm m self cc
      sget03 sget03 sget03 sget02       # asm m self cc asm m self asm
        .jurisdiction .protocol_call    # asm m self cc asm'
      sset03 sset01 drop goto           # asm' });


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
  symbolic_method_protocol,
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
      drop sset01 swap goto             # self },

    symbolic_method => bin q{           # asm m self cc
      # If the method is virtual, link it directly (i.e. save the vtable lookup
      # and insert a constant). Otherwise, invoke the method function directly.
      #
      # If you want a true-virtual method call you'll need to use a protocol
      # object.

      sget02 sget02 .virtuals .contains?
      [ sget03 sget03 sget03            # asm m self cc asm m self
        sget02 .jurisdiction            # asm m self cc asm m self j
        .class_call                     # asm m self cc asm'
        sset03 sset01 drop goto ]       # asm'

      [ sget02 sget02 .methods .{}      # asm m self cc fn
        sset01 sset01 goto ]            # ->fn(asm)

      if goto                           # asm' });


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