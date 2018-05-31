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

package phi::obj;

use strict;
use warnings;


=head2 Value types, pointers, and stacks
TODO: revise this section because we no longer trace the data stack

Let's talk about C for a second.

Idiomatic C can't be garbage collected accurately because we can't look at the
heap and know where the pointers are -- nor is there any guarantee that pointers
are even stored in any sane format. (XOR-linked lists, for example.) On the
bright side, C does provide some nice things like full-width unboxed integers
and floats.

C does have an understanding of pointer types, though; so if pointer/int casts
(and arbitrary memory access in general) were disallowed then it would be
possible to correctly GC it. C's stack layout is fully managed, so calculating
the root set is trivial.

FORTH is more challenging. The pointer/int casting rule isn't sufficient to add
GC because unlike in C, FORTH's stack types are generally unknown. That is, we
can't look at the data stack at a moment in time and understand which things on
it are pointers. If we wanted to be able to do this, we'd have to keep a bit of
separate information per entry indicating such.

Both languages are untyped in the sense that operators (mostly) reinterpret
their operands, but C is statically untyped while FORTH is dynamically untyped.

Polymorphic OOP, of course, doesn't work in an untyped world. We need some type
of runtime type information to be preserved per value in order to know which
variant of a method to resolve to. So our language becomes dynamically and
(probably) strongly typed. To the extent that coercion happens, it's driven by
the objects themselves -- objects are self-aware and self-managing.

If we take this principle to a logical extreme, there's no reason the garbage
collector needs to know anything about pointers or anything else. In fact,
there's no reason the language even needs a garbage collector. Objects with
direct memory access can be self-allocating, self-tracing, and self-relocating.

So ... how do we get to this wonderful paradise? The simplest strategy is to
kick things off with a concatenative, method-calling base with a few
instructions to rearrange the stack. Stack entries are value types, some of
which are pointers -- but the details of pointer dereferencing are delegated to
classes.

The last piece of this is that phi invokes all methods using vtables. Every
stack-allocated object is prefixed with a vtable pointer and we have an C<mcall>
primitive instruction to invoke the nth method from such a pointer.


=head3 Interpreter threading and method call mechanics
We haven't defined the interpreter yet because its primitives are specified as
objects (so we need the stuff in obj.pm first), but we can go ahead and define
some asm macros to implement method call threading.

phi's interpreter uses the same registers assignments that Jonesforth does, but
our threading model is a little simpler. We execute bytecode rather than
indirect-threaded machine code because it's much more compact for our use case.
I won't get into the exact bytecode definitions yet (see L<interp.pm>), but here
are the relevant registers:

  %rbp = return stack pointer
  %rsp = data stack pointer
  %rsi = instruction pointer
  %rdi = the interpreter object (not in Jonesforth)

Jonesforth loads the next machine instruction using C<lodsd>; we're executing
bytecode, so we use C<lodsb>. Let's write some macros for interpreter
advancement, return stack management, and method calls.

Using one byte per instruction means that our advancement primitive (NEXT in
Jonesforth) requires an additional instruction:

  # phi                                 # jonesforth
  xorq %rax, %rax                       lodsd
  lodsb                                 jmp *%eax
  jmp *(%rdi + 8*%rax)

C<%rdi> stores a here-pointer to the interpreter's bytecode dispatch table, so
there's no displacement. This reduces our overall code size.
=cut

sub phi::asm::next                      # 7 bytes
{
  # TODO: save an instruction by having each operator clear the high bits of
  # %rax before next-ing. Some instructions don't modify %rax at all, so no
  # sense in always clearing it.
  shift->_4831o300                      # xor %rax, %rax
       ->_ac                            # lodsb
       ->_ffo044o307;                   # jmp *(%rdi + 8*%rax)
}

sub phi::asm::enter                     # 8 bytes
{
  shift->_4883o305pc(-8)                # addq $-8, %rbp
       ->_4889o165pc(0);                # movq %rsi, *%rbp
  # FIXME: this obviously won't work because these aren't here-pointers, so we
  # won't be able to recover/rewrite references into the resulting code objects.
  # If we want a return stack at all, it will need to be inline-allocated with
  # polymorphic things just like the data stack currently is.
  #
  # NB: if we do take this approach, we can encode the offsets by having a
  # separate class per numeric offset into the code byte array... although it
  # may be worth measuring the absolute overhead of this approach before we
  # commit to it.
}

sub phi::asm::exit                      # 8 bytes
{
  shift->_488bo165pc(0)                 # movq *%rbp, %rsi
       ->_4883o305pc(8);                # addq $8, %rbp
}


=head3 By-value method calls
phi primitive objects are stack-allocated and treated as immutable-ish value
quantities that have no intrinsic identity. So the C<mcall> primitive
instructions apply to a bare vtable pointer as the stack top, followed by
immediate object data below that. In practice the object data is often a pointer
to a heap location; I explain the details below.
=cut

sub phi::asm::mcall
{
  shift->_58                            # pop %rax (the vtable)
       ->enter                          # save %rsi
       ->_488bo064o310;                 # movq *(%rax + 8*%rcx), %rsi
}


=head2 Pointers and value-objects
The phi stack usually looks like this:

          ...
          vtableN
          ...
          pointer3|data3
          vtable3
          pointer2a|data2a
          pointer2b|data2b
          vtable2
          pointer1|data1
  %rsp -> vtable1

These vtables serve as headers for stack-allocated objects.

TODO: how do we handle swapping/etc of these value types? This all seems wrong.
For example, we can ask stack[0] for its size -- which will be pushed onto the
stack. But how do we then ask stack[1] for its size? It would overwrite data in
stack[0] if we bumped C<%rsp>.
=cut


1;
