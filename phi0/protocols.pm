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


=head2 Bootup strategy
How we get to the endpoint isn't the same as which endpoint we're going for; as
a result, the objects we use to boot phi aren't directly related to the set of
objects we ultimately want.

More specifically, phi ends up being a series of class and bytecode objects that
are here-linked to each other. We have three ways to get the bytecode:

1. Hand-write using C<bin>
2. Use Perl abstraction to compile it
3. Generate method calls to phi objects to ask them to generate it

Of those options, the latter gives us the most reusable leverage. So we need to
hand-write enough stuff to get phi objects that can compile other phi objects.
...and that, of course, means that we need to specify what phi is compiling.


=head3 phi bytecode gen
phi classes do two things: they generate instances, and they generate code to
interact with those instances. So let's suppose we have something simple like a
string type that expects to be addressed as a reference:

  class string*
  {
    // Instance state:
    //   uint32 size;
    //   byte   data[size];

    string* +(string *rhs);
    int     size();
    byte*   data();
  }

We can ask it to generate code for us by constructing a class compiler:

  lit(string*)                          # class_object
  .compiler                             # compiler_object
  .size                                 # size_code

The resulting code object implements the C<.size()> method call against an
instance of C<string*> on the stack (i.e. the pointer is on the stack, not the
instance itself).


=head3 Classes, frames, and GC atomicity
Generally speaking, class-generated code doesn't interact with frames -- it
doesn't know which frame slots are available. Complete non-interaction is a
nonstarter, though, because some methods allocate memory and will need to be
GC-atomic. Classes need to be aware of this because it's the callee's
responsibility to GC-atomize the receiver and arguments to a function.

The simple strategy, and the one classes tend to use, is to push a temporary
frame anytime atomicity is required. This is basically equivalent to making a
function call, except that the resulting bytecode remains inlined.


=head3 Core OOP protocols
The two most important protocols here are classes and interpreters. The
interpreter is crucial because it contains methods to heap-allocate memory (and
initialize the heap in the first place), and classes are used to generate boot
code.

The protocol protocol is involved in compiling virtual method calls, but only as
a lookup table.
=cut

use constant interpreter_protocol => phi::protocol->new('interpreter',
  qw/ heap_allocate
      map_heap
      print_char
      print_string
      pnl
      assert
      rdtsc
      exit /);

use constant class_protocol => phi::protocol->new('class',
  qw/ new
      protocols
      compiler
      vtable /);

use constant protocol_protocol => phi::protocol->new('protocol',
  qw/ classes
      method_index /);

use constant vtable_protocol => phi::protocol->new('vtable',
  qw/ class /);


=head3 Data structures
I want to keep this fairly minimal for now. We need enough stuff to encode the
structure of classes and bytecode objects, which I think can be built from lists
and maps.


=head4 Linked lists
The usual cons/nil arrangement. C<maybe_nil> is used by more than just cons
cells, so it gets its own protocol.

C<+> is expected to return a new list of the same type as the receiver.
=cut

use constant maybe_nil_protocol => phi::protocol->new('maybe_nil',
  qw/ nil? /);

use constant cons_protocol => phi::protocol->new('cons',
  qw/ head
      tail /);

use constant joinable_protocol => phi::protocol->new('joinable',
  qw/ + /);

use constant list_protocol => phi::protocol->new('list',
  qw/ length
      [] /);

use constant set_protocol => phi::protocol->new('set',
  qw/ contains? /);

use constant mutable_set_protocol => phi::protocol->new('mutable_set',
  qw/ << /);

use constant linked_list_protocol => phi::protocol->new('linked_list',
  qw/ element==
      element==_fn
      root_cons /);


=head4 Key/value maps
For now we can use a linked map structure, basically a cons setup where each
link contains both a key and a value.
=cut

use constant kv_protocol => phi::protocol->new('kv',
  qw/ key
      value /);

use constant map_protocol => phi::protocol->new('map',
  qw/ key==
      key==_fn
      keys
      {} /);

use constant mutable_map_protocol => phi::protocol->new('mutable_map',
  qw/ {}= /);

use constant linked_map_protocol => phi::protocol->new('linked_map',
  qw/ kvcell_for /);


=head3 Strings and bytecode
C<byte_string> is a low-level byte array, whereas C<string_buffer> is a
self-managing thing that lets you append efficiently using doubling allocation.
=cut

use constant compiled_protocol => phi::protocol->new('compiled',
  qw/ source /);

use constant eq_protocol => phi::protocol->new('eq',
  qw/ == /);

use constant byte_string_protocol => phi::protocol->new('byte_string',
  qw/ data
      size /);

use constant string_buffer_protocol => phi::protocol->new('string_buffer',
  qw/ append_string
      append_quad
      append_byte
      headroom
      capacity
      reallocate
      to_string /);


=head3 Macro assembler
phi code needs to be able to write phi code, because that's how I roll. This
means we'll end up reimplementing parts of C<bin> and various macros into a phi
object that can emit compiled code objects. At the core of this is the macro
assembler, which manages the intermediate buffers and provides a decently
friendly interface for writing functions and stuff.

We need to keep track of any external references we write into code. This is
done by maintaining a list of refs, each of which knows the offset of the
constant (relative to the beginning of the code buffer) and the vtable
corresponding to the ref that was pushed. The vtable matters for GC purposes: we
don't know whether the ref is a base pointer, here pointer, or something else.
=cut

use constant ref_protocol => phi::protocol->new('ref',
  qw/ offset
      pointer_type
      get
      set /);

use constant bytecode_protocol => phi::protocol->new('bytecode',
  qw/ here
      call_native /);

use constant fn_protocol => phi::protocol->new('fn',
  qw/ call
      goto /);

use constant macro_assembler_protocol => phi::protocol->new('macro_assembler',
  qw/ parent
      child
      refs
      code /,

  # Assembler macros
  qw/ l8
      l64
      ref<<
      ptr
      hereptr
      [
      ]
      compile /);


=head2 Method finalization
At this point we know enough to finalize the method index list, at least in the
boot-protocol world. Let's go ahead and commit to method indexes now so we can
make constant references to class vtables as soon as we have them.
=cut

phi::finalize_methods;


1;