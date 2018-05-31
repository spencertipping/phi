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

There's a caveat to this, though: where do we bottom out to primitive
instructions? In phi's case we do this in the concatenative backend, which means
we do have an untyped domain. This means our programs need to make sure to
commit GC-traceable pointers to reliable root set entries before doing things
that allocate memory. I refer to this as "GC atomicity."


=head3 Interpreter threading and method call mechanics
phi's interpreter uses the same register assignments that Jonesforth does, but
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
  lodsb                                 jmp %eax
  jmp *(%rdi + 8*%rax)

In practice we can offload C<xorq> into the bytecode implementations; some
instructions don't modify C<%rax> at all, so they don't need to clear it. For
those instructions our advancement primitive is as fast as Jonesforth's on
Nehalem, and one cycle slower on Broadwell (due to the R/M difference).

NB: C<%rdi> stores a here-pointer to the interpreter's bytecode dispatch table,
so there's no displacement. This reduces our overall code size, which is
relevant because every bytecode instruction ends in NEXT.
=cut

sub phi::asm::next                      # 4 bytes
{
  shift->_ac                            # lodsb
       ->_ffo044o307;                   # jmp *(%rdi + 8*%rax)
}

sub phi::asm::enter                     # 8 bytes
{
  shift->_4883o305pc(-8)                # addq $-8, %rbp
       ->_4889o165pc(0);                # movq %rsi, *%rbp

  # FIXME: update this to use the new rstack init format -- but this involves
  # referring to a quoted constant (so we'll have a linkage) and dropping in a
  # reference to the calling code fragment.
  #
  # We'll have a reference to the calling code fragment if we push it onto the
  # rstack when _it_ is called. Then we have the invariant that the
  # currently-executing function is in the root set, which is kind of important!
  #
  # Q: how do we handle %rsi rewriting when we GC-mark the current code
  # fragment? Maybe we just add the location delta to %rsi -- that should be
  # fine actually.
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
  shift->_5a                            # pop %rdx (the vtable)
       ->enter                          # save %rsi
       ->_488bo064o312;                 # movq *(%rdx + 8*%rcx), %rsi
}

sub phi::asm::mgoto
{
  # FIXME: who's responsible for unwinding the memory in the calling stack
  # frame?
  shift->_5a                            # pop %rdx (the vtable)
       ->_488bo064o312;                 # movq *(%rdx + 8*%rcx), %rsi
}


1;
