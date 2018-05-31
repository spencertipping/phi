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


=head2 Stacks and GC roots
C and FORTH each have enough untyped-ness that it's difficult or impossible to
accurately garbage-collect them. This happens for two reasons: first, both
languages support pointer/int conversions; and second, neither language is
strongly typed.

phi is in an awkward position because we have an untyped data stack with
pointer/int ambiguity (and no int tagging), but we also guarantee accurate
mark/sweep GC -- so we need object-driven tracing. We can get both by
stack-allocating type-aware frame objects.

Before I get to that, though, let's talk about how bytecode is interpreted.


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


=head3 Return stack layout, calling convention, and GC
Any function that initiates a GC while owning a reference needs to link that
reference into a GC root object, which in phi's case is an rstack-allocated
frame. The frame can be created by a function's prolog:

  lit(size) lit(frame-vtable) rframe    # rframe == r+ <r >m64, give or take
  mcall(.init)                          # initialize frame object (important!)

Let's walk through this in more detail.

First, C<mcall()> drops the calling C<%rsi> onto the _data_ stack just below the
receiver:

          ...
          method receiver pointer
  %rsp -> caller %rsi

Next we stack up some arguments for C<rframe>:

          method receiver pointer       %rbp -> caller-frame-vtable
          caller %rsi
          frame-size
  %rsp -> frame-vtable herepointer

C<rframe> subtracts space from C<%rbp> and writes the vtable entry to make it an
object. It then drops the frame object onto the data stack.

          method receiver pointer               caller-frame-vtable
          caller %rsi                           <space>
  %rsp -> frame-pointer = %rbp          %rbp -> frame-vtable

Now we can invoke C<.init> on the frame object, which does two things:

1. Clears all pointer slots in the frame, which is critical
2. Stores the caller C<%rsi> in the frame

(1) is critical because if the frame contains uninitialized data that gets
interpreted as pointers, it is very likely to segfault when asked to GC-trace
itself.
=cut

sub phi::asm::next                      # 4 bytes
{
  shift->_ac                            # lodsb
       ->_ffo044o307;                   # jmp *(%rdi + 8*%rax)
}

sub phi::asm::mcall
{
  # NB: %rcx is the method number we want to invoke; receiver is top stack entry
  shift->_488bo024o044                  # movq *%rsp, %rdx (fetch object)
       ->_488bo032                      # movq *%rdx, %rbx (fetch the vtable)
       ->_56                            # push %rsi
       ->_488bo064o313;                 # movq *(%rbx + 8*%rcx), %rsi
}

sub phi::asm::goto
{
  shift->_5e;                           # pop into %rsi
}


=head2 Core classes
These are implemented in perl to simplify bootstrapping, then emitted into asm
objects when we generate the image.
=cut

package phi::vtable
{
  # TODO: fix this API: bind() method, autopopulate vtable attribute, take name
  # as ctor arg?

  sub new
  {
    bless { vtable   => undef,
            class    => undef,
            name     => undef,
            bindings => [] }, shift;
  }

  sub asm
  {
    my $self = shift;
    my $n    = scalar @{$$self{bindings}};
    my $asm  = phi::asm($$self{name})
             ->pQ($$self{vtable})
             ->pQ($$self{class})
             ->pS($n)
             ->here_marker
             ->lmstart;
    $asm->pQ($_ // 0) for @{$$self{bindings}};
    $asm->lmend;
  }
}

use phi::initblock vtable_vtable => sub
{
  my $vt = phi::vtable->new;
  $$vt{vtable} = $vt;
};

use phi::use 'phi::vtable' => sub
{
  my ($name, $vt) = @_;
  $$vt{name}   = $name;
  $$vt{vtable} = vtable_vtable;
  $name => $vt;
};


1;
