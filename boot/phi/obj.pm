#!/usr/bin/env perl

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


use phi::use 'phi::vtable' => sub
{
  my ($name) = @_;
  $name => phi::const phi::asm $name;
};


# We'll populate these a bit later, once we've defined the class/protocol
# machinery. We need the ASM objects now for forward referencing.
use phi::vtable 'vtable_vtable';
use phi::vtable 'bytecode_vtable';
use phi::vtable 'amd64native_vtable';
use phi::vtable 'class_vtable';
use phi::vtable 'protocol_vtable';
use phi::vtable 'runtime_vtable';


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

sub phi::asm::here_marker
{
  my $asm = shift;
  $asm->pS(phi::right - $asm);
}


=head2 Machine code natives
Let's start here. These objects are simple enough because they don't refer to
any constants -- so they're really just strings of binary data. All we need to
know is how long they are.
=cut

use phi::use 'phi::amd64native' => sub
{
  my ($name, $asm) = @_;
  $name => phi::const phi::amd64native->new($name, $asm);
};

package phi::amd64native
{
  sub new
  {
    # NB: $code_asm should be finalized when you construct this object
    my ($class, $name, $code_asm) = @_;
    my $self_asm = phi::asm($name)
                 ->pQ(phi::amd64native_vtable)
                 ->pS($code_asm->size)
                 ->here_marker
                 ->lcode
                 ->inline($code_asm);

    bless { name => $name,
            code => $code_asm,
            asm  => $self_asm }, $class;
  }

  sub base { shift->{asm} }
  sub here { shift->{asm}->resolve('code') }
}


1;
