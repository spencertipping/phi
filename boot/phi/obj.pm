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
  shift->_4831o300                      # xor %rax, %rax
       ->_ac                            # lodsb
       ->_ffo044o307;                   # jmp *(%rdi + 8*%rax)
}

sub phi::asm::enter                     # 8 bytes
{
  shift->_4883o305pc(-8)                # addq $-8, %rbp
       ->_4889o165pc(0);                # movq %rsi, *%rbp
}

sub phi::asm::exit                      # 8 bytes
{
  shift->_488bo165pc(0)                 # movq *%rbp, %rsi
       ->_4883o305pc(8);                # addq $8, %rbp
}

sub phi::asm::mcall
{
  # Call method on an object _by value_!!!
  # %rsp = the object, %rcx = method number
  shift->_58                            # pop %rax (the vtable)
       ->enter                          # save %rsi
       ->_488bo064o310;                 # movq *(%rax + 8*%rcx), %rsi
}


=head2 Structs
Just like in C, structs manage blocks of memory. Unlike in C, though, phi
structs are implemented as first-class objects that can dynamically generate all
of the code required to support them, including GC interop (which itself is
managed through a first-class protocol).

phi structs are limited to memory management; OOP polymorphism is a separate
construct implemented by classes and protocols.


=head3 Example: code fragment objects
Let's fully spec this out. First, here's what a code fragment looks like in
memory:

  vtable:   object*
  source:   object*
  nlinks:   uint16_t
  codesize: uint16_t
  links:    (int8_t, uint16_t)[nlinks]
  heremark: uint16_t
  code:     byte[codesize]

Code fragments are reference types that adhere to a few protocols:

  protocol gc {
    # TODO: rewrite() needs to take some sort of context to indicate where the
    # object should be written
    object* rewrite();                  # receiver is an implicit arg
    int     size();                     # physical object size
  }

  protocol reference_type {
    object* resolve() = self;
  }

  protocol code_methods {
    object*              source();
    pointer[]            links();
    here_pointer<byte[]> code();
  }

  protocol assembler_methods {
    object* <<(byte[] code);
    object* <<(base_pointer ref);
    object* <<(here_pointer ref);
  }

  protocol reference_struct_reflection {
    name[] fields();
    object getfield(name);
    void   setfield(name, object);
  }

=cut


1;
