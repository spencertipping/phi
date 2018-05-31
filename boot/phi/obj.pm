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


=head3 Return stack layout and GC
We need the return stack to maintain a GC-traceable reference to the
currently-executing function object. It's fine for this to be a here-pointer.
Because we're also storing the previous C<%rsi> address, though, which isn't
traceable, we need a vtable prefix for each return frame. So at a minimum we'd
have this:

          callee code object pointer
          caller %rsi
  %rbp -> vtable

Then GC tracing is a regular method call against the vtable, and frame
deallocation is a virtual tail call. So far so good, but let's talk about how
the rstack gets to be in the layout shown above.

TODO: the below is sort of correct, but more complicated than it needs to be. We
should be able to optimize in two ways: (1) receiver suffices as the callee code
fragment object (for tracing purposes); (2) _every_ method call is a method-goto
instruction -- all rstack frame allocation is managed by the function prolog.

First, we can't (or more precisely, don't want to) have an asm macro that drops
a constant reference into the code, so we can't make a hard reference to some
vtable somewhere. So it's up to the callee object to refer to this vtable.

This ends up being ideal because not all callees are going to want the same
frame class. Given the natural delegation, then, it seems like calling into a
function is basically a method call against the code object:

  lit(callee-object) .call()

...of course, this raises a bit of a problem: how do we invoke C<.call()>, which
is itself a code object? That method call needs to be "more primitive" than the
function we're calling into.

Luckily for us, "more primitive" is simple: push C<%rsi> and jump. This is a
method-goto instruction. So the above code would really be this:

  lit(callee-object) method-goto(call)

C<method-goto> requires help from the callee because it pushes the caller's
C<%rsi> onto the _data_ stack, not the return stack. This may seem strange, but
it ends up saving operations. C<call> sees this:

          ...
          caller-%rsi                 ...
  %rsp -> callee-object       %rbp -> caller-frame-vtable

Its minimum strategy, then, would look something like this:

  rpush rpush lit(frame-vtable) rpush <the-function>

Q: is it appropriate for C<method-goto> to use the data stack this way?

Q: is this overhead justifiable?

Q: is the above C<rpush> stuff a prolog -- so does C<call> inline the real
bytecode?
=cut


sub phi::asm::next                      # 4 bytes
{
  shift->_ac                            # lodsb
       ->_ffo044o307;                   # jmp *(%rdi + 8*%rax)
}


=head3 By-value method calls
TODO: method calls should now be by-reference; we don't need value types now
that we have frame allocation (i.e. we can subsume value types into frame
structs)

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
