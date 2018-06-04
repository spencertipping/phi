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


=head2 phi base objects
We need to define the basic object types to host the data structures that make
up the interpreter. We don't need method implementations or protocol specifics
yet; this is just stuff to store the instance data.

Let's describe what a minimal interpreter needs to do.


=head3 Call frames, structs, and GC
FORTH uses an untyped data stack: that is, the hosting FORTH runtime doesn't
understand the semantic type of any given stack entry. This is fine in FORTH but
it would be a problem in phi because phi implements accurate garbage collection;
we need a way to build the set of things that are definitely referred to.
(Ultimately, this is why phi uses structured call frames and register-style
addressing instead of FORTH's more elegant stack model.)

phi gets around the GC problem by implementing a single stack of call frames,
very much like C. Each call frame is a proper object with its own vtable prefix,
and we have the invariant that C<%rbp> always points to the currently-executing
frame object. This means that for GC purposes, we can at any point issue a
C<mark-root> method call against the object in C<%rbp>.

phi provides the system stack for frame allocation, but you have no obligation
to use that exclusively -- nor should you if you want something more interesting
like forked stacks or coroutines. These are viable options because call frames
are addressible as objects with virtual methods; the usual operations like
C<leave> are implemented as method calls. I'll describe the mechanics of this in
more detail in the bytecode interpreter.


=head3 Bootup classes
We need to be able to describe a few types of objects:

1. C<phi::vtable>: a protocol-aware numeric allocation of methods for a class
2. C<phi::bytecode>: a bytecode function that is aware of value closure
3. C<phi::amd64native>: a machine code function with no closures
4. C<phi::class>: a source object that can generate a vtable
5. C<phi::protocol>: an API for a class; informs vtable slot allocation
6. C<phi::runtime>: the API for phi's runtime
7. C<phi::struct>: an object that generates field accessors

Every object begins with a herepointer into a vtable, and this is where the
object system bottoms out: C<a_vtable.vtable.vtable == a_vtable.vtable>.
Similarly, C<object.class.class.class == object.class.class>. This works because
C<.class> is non-operative in object terms; objects are implemented by vtables
rather than having the type of live method-dispatching mechanics provided by
dynamic languages like Ruby. (If we didn't have this degree of separation, then
class-as-an-instance-of-itself could loop forever trying to resolve methods.)
=cut

package phi::vtable {}
package phi::bytecode {}
package phi::amd64native {}
package phi::class {}
package phi::protocol {}
package phi::runtime {}
package phi::struct {}


=head2 Here pointers
There are cases where we'll want to refer to the middle of a structure for
linkage reasons. For example, references to native code should be C<jmp>-able,
so we want to point to the first byte of executable machine code, but that
machine code is owned and managed by a phi object whose base offset comes a bit
before. These cases are negotiated using C<here> pointers, each of which is
preceded by a two-byte struct offset. It would be like this in C:

  struct x *here = ...;
  struct x *base = (struct x*) ((char*) here - ((unsigned short*) here)[-1]);

In memory terms we have this:

                 here-pointer - heremarker = object root
                 |
  object root    |        here-pointer points here
  |              |        |
  V              V        V
  vtable ... heremarker | native code/whatever

We need to be aware of the fact that here pointers aren't the same as base
pointers, but if we can keep that distinction straight then we have full
polymorphism despite referring into the middle of a structure.
=cut

use phi::asm_macro here_marker => sub
{
  my $asm = shift;
  $asm->pS(phi::right - $asm);
};


=head2 vtable objects
We need to construct the method-dispatch herepointers for our base object
vtables so we can refer to them from core objects. The only tricky point here is
setting up the self-referential base vtable.
=cut

package phi::vtable
{
  sub new
  {
    my ($class, $name, $vtable) = @_;
    bless { name      => $name,
            vtable    => $vtable,
            asm       => undef,
            assembled => 0,
            methods   => [] }, $class;
  }

  sub assemble_header
  {
    my $self = shift;
    return $self if defined $$self{asm};
    $$self{asm} = phi::asm($self->name)
      ->pQ(defined $$self{vtable} ? $$self{vtable}->here : phi::l"mtablestart")
      ->pS(phi::l("mtableend") - phi::l"mtablestart")
      ->pS(length $self->name)
      ->lit($self->name)
      ->here_marker
      ->lmtablestart;
    $self;
  }

  sub assemble_full
  {
    my $self = shift->assemble_header;
    my $asm  = $$self{asm};
    return $self if $$self{assembled};

    $asm->pQ($_->here) for @{$$self{methods}};
    $asm->lmtableend;
    $$self{assembled} = 1;
    $self;
  }

  sub name { shift->{name} }
  sub base { shift->assemble_header->{asm} }
  sub here { shift->assemble_header->{asm}->resolve('mtablestart') }
}


# Explicit undef for the vtable arg means it will be self-referential
use phi::const vtable_vtable => phi::vtable->new('vtable_vtable', undef);
use phi::const all_vtables => [];

use phi::use 'phi::vtable' => sub
{
  my ($name) = @_;
  my $vt = phi::vtable->new($name, vtable_vtable);
  push @{+all_vtables}, $vt;
  $name => phi::const $vt;
};


use phi::vtable 'bytecode_vtable';
use phi::vtable 'amd64native_vtable';
use phi::vtable 'class_vtable';
use phi::vtable 'protocol_vtable';
use phi::vtable 'runtime_vtable';
use phi::vtable 'struct_vtable';


=head2 Machine code natives
These objects are simple enough because they don't refer to any constants -- so
they're really just strings of binary data. All we need to know is how long they
are.

Machine code natives are implementations of bytecode instructions, so we should
store the instruction index on each one so we can automatically build the
dispatch table later on.

The expectation is that these native objects are used from a bytecode context,
so we have a few invariants:

1. All but the low byte of C<%rax> is zeroed after each insn executes
2. Any insn addressing/args are consumed with C<lods> -- so C<%rsi> is fair game
3. Insns end with code to load and execute the next one

(3) is phi's continuation primitive, which corresponds to what Jonesforth calls
NEXT. phi's is only slightly more involved:

  # phi next                            # Jonesforth NEXT
  lodsb                                 lodsd
  jmp *(%rdi + 8*%rax)                  jmp %eax

This works because C<%rdi> stores a here-pointer to the runtime object's
bytecode dispatch table. We use C<lodsb> and bytecode because it ends up being
much smaller than full code pointers.
=cut

use phi::asm_macro zero_rax => sub
{
  shift->_4831o300;                     # xor %rax, %rax
};

use phi::asm_macro next => sub
{
  shift->_ac                            # lodsb
       ->_ffo044o307;                   # jmp *(%rdi + 8*%rax)
};


package phi::amd64native
{
  sub new
  {
    # NB: $code_asm should be finalized when you construct this object
    my ($class, $name, $index, $code_asm) = @_;
    my $self_asm = phi::asm $name;
    $self_asm->pQ(phi::amd64native_vtable->here)
             ->pS(phi::l("codeend") - phi::l"codestart")
             ->pS(length $name)
             ->pC($index)
             ->lit($name)
             ->here_marker
             ->lcodestart
             ->inline($code_asm)
             ->next
             ->lcodeend;

    bless { name  => $name,
            code  => $code_asm,
            index => $index,
            asm   => $self_asm }, $class;
  }

  sub name  { shift->{name} }
  sub index { shift->{index} }
  sub base  { shift->{asm} }
  sub here  { shift->{asm}->resolve('codestart') }
}


use phi::const all_amd64natives => [];

use phi::use 'phi::amd64native' => sub
{
  my ($name, $index, $asm) = @_;
  my $insn = phi::amd64native->new($name, $index, $asm);
  push @{+all_amd64natives}, $insn;
  $name => phi::const $insn;
};


=head2 Structs
Structs access memory, which may be structured or flat depending on the backend.
Structs are objects (class instances) that implement the C<struct> protocol,
which looks like this:

  protocol struct<args...>
  {
    fn allocator();                     # fn:    (args...) -> (struct*)
    fn sizer();                         # fn:    (struct*) -> (int)
    fn gc_marker();                     # fn:    (struct*) -> (struct*)
    fn getter(field);                   # fn:    (struct*) -> (x)
    fn setter(field);                   # fn: (x, struct*) -> ()
  }

TODO: should C<allocator> close over a memory allocator object of some sort, or
should we implicitly use the runtime's current heap for this?

TODO: how should this work for languages like Java that require some setup for
structs?


=head3 Struct variants
At a high level we have these variables:

1. Memory model (flat vs managed)
2. Storage disposition (inline vs reference; mostly relevant for managed memory)
3. High-level parameterization: array size, etc

TODO: more detail/design here. It's not clear whether structs should be at all
aware of this stuff, or whether we should just write functions that generate
custom structs.


=head3 For now: flat memory structs
I'll get into the above stuff later on, but for the moment let's keep it simple
and create a struct endpoint class that manages flat memory. We need enough
machinery to read the binary objects we're generating for phi's base image. That
entails:

1. Fixed-offset fields
2. Variable-offset fields calculated from fixed-offset fields
3. Here markers + here-pointer generation
4. Calculated size
5. GC marking/moving

No need to support array-style access yet; for now we can fudge that by doing
C-style pointer arithmetic.

TODO: do flat structs map to logical structs somehow, or are these separate
concepts?
=cut

package phi::struct
{
  sub new
  {
    my ($class, $name) = @_;
    bless { name   => $name,
            fields => [] }, $class;
  }

  # TODO
}

use phi::use 'phi::struct' => sub
{
  my ($name, @fields) = @_;
  # TODO
  ();
};


use phi::struct struct_struct => 0;


1;
