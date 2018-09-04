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

Before we get there, though, we need to build up a world that's expressive
enough to make this not entirely miserable.


=head3 Interpreter protocol
This is a good place to start because it gives us heap allocation, which is
required for basically everything else. It also gives us some useful diagnostic
methods like C<print_string> and C<pnl> ("print with newline"). C<assert> is
used by unit tests and has signature C<< (cond message) -> ()|die >>.

The interpreter provides a mutable string-map of globals that we can use to name
stuff we generate at bootup. This is convenient because not all values exist
within phi0; sometimes we'll want to use phi1's constructor machinery to get to
an endpoint rather than trying prop stuff up in phi0.
=cut

use phi::protocol interpreter =>
  qw/ heap_allocate
      map_heap
      heap_usage
      heap_size
      globals
      globals=
      def
      global
      print_char
      print_string
      print_string_fd
      pnl
      pnl_err
      pnl_self
      read
      die
      assert
      exit /;


=head3 General object-management stuff
The boring details like C<to_string> and C<clone> that are normally factored
into base classes. This is all minimal because we're hand-coding these methods
where they exist (normally a lot of this would be generated by metaclasses).
=cut

use phi::protocol clone =>
  qw/ clone /;

use phi::protocol stringable =>
  qw/ to_s /;


=head3 Data structures
I want to keep this fairly minimal for now. We need enough stuff to encode the
structure of classes and bytecode objects, which I think can be built from lists
and maps.


=head4 Linked lists
The usual cons/nil arrangement. C<maybe_nil> is used by more than just cons
cells, so it gets its own protocol.

C<+> is expected to return a new list (or object in general) of the same type as
the receiver.
=cut

use phi::protocol maybe_nil =>
  qw/ nil? /;

use phi::protocol cons =>
  qw/ head
      tail /;

use phi::protocol cons_relinkable =>
  qw/ with_tail /;

use phi::protocol joinable =>
  qw/ + /;

use phi::protocol mutably_joinable =>
  qw/ += /;

use phi::protocol invertible =>
  qw/ ~ /;

use phi::protocol list =>
  qw/ length
      reduce
      [] /;

use phi::protocol set =>
  qw/ contains? /;

use phi::protocol mutable_list =>
  qw/ []=
      shift
      << /;

use phi::protocol mutable_set =>
  qw/ << /;

use phi::protocol linked_list =>
  qw/ element==_fn
      root_cons
      rev /;

use phi::protocol byte_set =>
  qw/ byte_bitset /;


=head4 Key/value maps
For now we can use a linked map structure, basically a cons setup where each
link contains both a key and a value.
=cut

use phi::protocol kv =>
  qw/ key
      value /;

use phi::protocol mutable_value =>
  qw/ value= /;

use phi::protocol map =>
  qw/ key==_fn
      keys
      kv_pairs
      {} /;

use phi::protocol mutable_map =>
  qw/ {}= /;

use phi::protocol linked_map =>
  qw/ kvcell_for /;


=head3 Strings and bytecode
C<byte_string> is a low-level byte array, whereas C<string_buffer> is a
self-managing thing that lets you append efficiently using doubling allocation.
=cut

use phi::protocol eq =>
  qw/ == /;

use phi::protocol byte_string =>
  qw/ data
      size /;

use phi::protocol string_buffer =>
  qw/ append_string
      append_hex
      append_int
      append_int64
      append_int32
      append_int16
      append_int8
      append_dec
      headroom
      capacity
      reallocate
      rewind
      to_string /;


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

use phi::protocol ref =>
  qw/ offset
      pointer_type
      get
      set /;

use phi::protocol here =>
  qw/ here /;

use phi::protocol bytecode =>
  qw/ call_native /;

use phi::protocol fn =>
  qw/ call
      goto /;

use phi::protocol macro_assembler =>
  qw/ parent
      child
      refs
      code /,

  # Assembler macros (plus some shorthands for numbers)
  # NB: pnl is a macro to simplify debugging. It inserts a literal string
  # followed by a call to i.pnl to print stuff.
  qw/ 0
      1
      2
      3
      4
      l8
      l16
      l32
      l64
      ref<<
      ptr
      hereptr
      dup
      pnl
      debug_trace
      [
      ]
      add_child_link
      inline
      compile /;

use phi::protocol insn_proxy =>
  sort keys %{+insns};


=head3 Classes and protocols
These are just enough to access the fields within the objects. C<phi1> then
generates new classes that provide compilation logic.
=cut

use phi::protocol protocol =>
  qw/ virtuals
      classes
      struct_link /;

use phi::protocol mutable_protocol =>
  qw/ defvirtual
      implementors<< /;

use phi::protocol class =>
  qw/ protocols
      methods
      virtuals
      fields
      struct_link /;

use phi::protocol compilable_class =>
  qw/ dispatch_fn /;

use phi::protocol mutable_class =>
  qw/ defmethod
      defvirtual
      implement /;


=head2 Backdoor reflective protocol
Objects need to implement this protocol so we can invoke their methods
symbolically; i.e. without knowing the vtable index of those methods.
=cut

use phi::protocol method_translator =>
  qw/ {} /;


use phi::protocol symbolic_method =>
  qw/ symbolic_method /;


1;
