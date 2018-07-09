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
      heap_usage
      globals
      globals=
      def
      global
      print_char
      print_string
      print_string_fd
      pnl
      die
      assert
      rdtsc
      exit /);


=head3 Data structures
I want to keep this fairly minimal for now. We need enough stuff to encode the
structure of classes and bytecode objects, which I think can be built from lists
and maps.


=head4 Linked lists
The usual cons/nil arrangement. C<maybe_nil> is used by more than just cons
cells, so it gets its own protocol.

C<+> is expected to return a new list of the same type as the receiver.
=cut

use constant parse_position_protocol => phi::protocol->new('parse_position',
  qw/ fail? /);

use constant string_position_protocol => phi::protocol->new('string_position',
  qw/ +
      index /);

use constant maybe_nil_protocol => phi::protocol->new('maybe_nil',
  qw/ nil? /);

use constant cons_protocol => phi::protocol->new('cons',
  qw/ head
      tail /);

use constant cons_relinkable_protocol => phi::protocol->new('cons_relinkable',
  qw/ with_tail /);

use constant joinable_protocol => phi::protocol->new('joinable',
  qw/ + /);

use constant invertible_protocol => phi::protocol->new('invertible',
  qw/ ~ /);

use constant list_protocol => phi::protocol->new('list',
  qw/ length
      reduce
      [] /);

use constant set_protocol => phi::protocol->new('set',
  qw/ contains? /);

use constant mutable_list_protocol => phi::protocol->new('mutable_list',
  qw/ []=
      << /);

use constant mutable_set_protocol => phi::protocol->new('mutable_set',
  qw/ << /);

use constant linked_list_protocol => phi::protocol->new('linked_list',
  qw/ element==_fn
      root_cons /);


=head4 Key/value maps
For now we can use a linked map structure, basically a cons setup where each
link contains both a key and a value.
=cut

use constant kv_protocol => phi::protocol->new('kv',
  qw/ key
      value /);

use constant mutable_value_protocol => phi::protocol->new('mutable_value',
  qw/ value= /);

use constant map_protocol => phi::protocol->new('map',
  qw/ key==_fn
      keys
      kv_pairs
      {} /);

use constant mutable_map_protocol => phi::protocol->new('mutable_map',
  qw/ {}= /);

use constant linked_map_protocol => phi::protocol->new('linked_map',
  qw/ kvcell_for /);


=head3 Strings and bytecode
C<byte_string> is a low-level byte array, whereas C<string_buffer> is a
self-managing thing that lets you append efficiently using doubling allocation.
=cut

use constant eq_protocol => phi::protocol->new('eq',
  qw/ == /);

use constant byte_string_protocol => phi::protocol->new('byte_string',
  qw/ data
      size /);

use constant string_buffer_protocol => phi::protocol->new('string_buffer',
  qw/ append_string
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

  # Assembler macros (plus some shorthands for numbers)
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
      [
      ]
      compile /);

use constant typed_macro_assembler_protocol =>
  phi::protocol->new('typed_macro_assembler',
    qw/ stack
        frame
        asm
        typed /);

use constant insn_proxy_protocol => phi::protocol->new('insn_proxy',
  sort keys %{+insns});


=head2 Parser protocols
Classes for these are defined in L<phi0/parsers.pm>.
=cut

use constant parser_protocol => phi::protocol->new('parser',
  qw/ parse /);

use constant string_parser_protocol => phi::protocol->new('string_parser',
  qw/ text /);

use constant binary_parser_protocol => phi::protocol->new('binary_parser',
  qw/ left
      right /);

use constant seq_parser_protocol => phi::protocol->new('seq_parser',
  qw/ combine
      combiner /);

use constant char_parser_protocol => phi::protocol->new('char_parser',
  qw/ chars /);

use constant repeat_parser_protocol => phi::protocol->new('repeat_parser',
  qw/ mincount /);

use constant parser_transform_protocol => phi::protocol->new('parser_transform',
  qw/ parser /);

use constant fn_parser_protocol => phi::protocol->new('fn_parser',
  qw/ fn /);


=head2 Metaclass protocols
Classes for these are defined in L<phi0/metaclasses.pm>.
=cut

use constant struct_link_protocol => phi::protocol->new('struct_link',
  qw/ size
      left_offset
      right_offset
      size_fn
      left_offset_fn
      right_offset_fn /);

use constant cons_struct_link_protocol => phi::protocol->new('cons_struct_link',
  qw/ name
      class
      getter_fn
      setter_fn
      get
      set
      fget_fn
      fset_fn
      generate_getter_fn
      generate_setter_fn /);


=head3 Classes and protocols
These are just enough to access the fields within the objects. C<phi1> then
generates new classes that provide compilation logic.
=cut

use constant protocol_protocol => phi::protocol->new('protocol',
  qw/ methods
      classes /);

use constant mutable_protocol_protocol => phi::protocol->new('mutable_protocol',
  qw/ implementors<< /);

use constant vtable_allocator_protocol => phi::protocol->new('vtable_allocator',
  qw/ closure_set
      allocate_vtable_slots /);

use constant class_protocol => phi::protocol->new('class',
  qw/ protocols
      methods
      vtable
      fields
      metaclasses
      compiler
      compiler_fn /);

use constant mutable_class_protocol => phi::protocol->new('mutable_class',
  qw/ defmethod
      implement
      compiler_fn= /);

use constant metaclass_protocol => phi::protocol->new('metaclass',
  qw/ transform /);


use constant class_test_incdec_protocol =>
  phi::protocol->new('class_test_incdec',
    qw/ inc
        dec /);


=head2 Backdoor reflective protocol
Objects need to implement this protocol so we can invoke their methods
symbolically; i.e. without knowing the vtable index of those methods.
=cut

use constant method_translator_protocol =>
  phi::protocol->new('method_translator',
    qw/ {} /);


use constant symbolic_method_protocol => phi::protocol->new('symbolic_method',
  qw/ symbolic_method /);


=head2 Method finalization
At this point we know enough to finalize the method index list, at least in the
boot-protocol world. Let's go ahead and commit to method indexes now so we can
make constant references to class vtables as soon as we have them.
=cut

phi::finalize_methods;


1;
