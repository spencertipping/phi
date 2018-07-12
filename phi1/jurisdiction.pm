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
    .prototype_call                     # asm

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
      sget04 .methods .reduce           # p m cc m
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
  list_protocol)

  ->def(
    source_class => bin q{swap const8  iplus m64get swap goto},
    length       => bin q{swap const16 iplus m64get swap goto},

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

    sget01 .methods .keys               # j c cc ms
    asm                                 # [max m cc]
      const1 swap .sget                 # [max m cc m]
      sget03 swap .ptr                  # [max m cc m c]
      sget04 swap .ptr                  # [max m cc m c j]

      # NB: use hosting method calling convention here; this code is
      # non-portable because we should be addressing the hosting jurisdiction
      # rather than the reflection-generated protocol mapping
      .dup .m64get                      # [max m cc m c j jvt]
        "resolve_class_method"
        %method_vtable_mapping .{}
        lit8+3 ishl bswap16 swap .l16   # [max m cc m c j jvt fndelta]
      .iplus .m64get .call              # [max m cc mi]

      .dup const4 swap .sget            # [max m cc mi mi max]
      .ilt                              # [max m cc mi max<mi?]

      .[ const2 swap .sset              # [mi m cc]
         .lit8 .0 const1 swap .sset     # [mi exit?=0 cc]
         .goto .]                       # [mi exit?=0]

      .[ .drop                          # [max m cc]
         .lit8 .0 const1 swap .sset     # [max exit?=0 cc]
         .goto .]                       # [max exit?=0]

      .if .goto                         # ->branch
    .compile .here                      # j c cc ms fn
    sget01 const0 sset02                # j c cc 0 fn ms
    .reduce                             # j c cc max

    # Now we have the maximum method index; allocate the vtable object.
    dup lit8+3 ishl lit8+26 iplus
    i.heap_allocate                     # j c cc max &vt
    swap    sget01 const16 iplus m64set # j c cc &vt [.size=]
    $vtable_class sget01         m64set # [.vt=]
    sget02  sget01 const8  iplus m64set # [.source_class=]
    lit8+26 sget01 const24 iplus m16set # [.here_marker=]

    # Set each method defined by the class. Any undefined methods will be
    # uninitialized memory, which we hope segfaults if you hit them but really
    # it's anyone's guess.
    sget02 .methods .kv_pairs           # j c cc vt kvs
    [ sget01 .nil?                      # j c cc vt kvs loop nil?
      [ drop drop                       # j c cc vt
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


=head3 Structure definitions
Here's what the jurisdiction stores:

  struct amd64_native_vtable_jurisdiction
  {
    hereptr               vtable;
    map<protocol*, _>    *protocols;            # NB: used as a set
    strmap<int>          *vtable_allocation;
    map<class*, vtable*> *class_vtables;
  }

=cut

use constant amd64_native_vtable_jurisdiction_class =>
  phi::class->new('amd64_native_vtable_jurisdiction',
    vtable_jurisdiction_protocol,
    jurisdiction_protocol)

  ->def(
    protocols             => bin q{swap const8  iplus m64get swap goto},
    method_allocation_map => bin q{swap const16 iplus m64get swap goto},
    class_vtable_map      => bin q{swap const24 iplus m64get swap goto},

    resolve_class_method => bin q{      # m c self cc
      sget03 sget02                     # m c self cc m self
      .method_allocation_map .{}        # m c self cc mi
      sset03 sset01 drop goto           # mi },

    resolve_protocol_method => bin q{   # m p self cc
      sget03 sget02                     # m p self cc m self
      .method_allocation_map .{}        # m p self cc mi
      sset03 sset01 drop goto           # mi },

    allocate_fixed => bin q{            # asm class self cc
      # TODO
      },

    allocate_variable => bin q{         # asm size class self cc
      # TODO
      },

    # NB: these methods assume stack layout (args... receiver vtable); that is,
    # you've unpacked the vtable already.
    protocol_call => bin q{             # asm m p self cc
      },

    class_call => bin q{                # asm m c self cc
      });


use constant amd64_native_jurisdiction_fn => phi::allocation
  ->constant(bin q{                     # ps cc
    # Allocate method slots for the set of protocols
    sget01 $protocols_to_method_mapping_fn call       # ps cc ms

    # Now generate a vtable for each implementing class. To do this, we first
    # need to collect all classes.
    intmap                              # ps cc ms cmap
    sget03                              # ps cc ms cmap ps
    # TODO

    })
  ->named('amd64_native_jurisdiction_fn') >> heap;


1;
