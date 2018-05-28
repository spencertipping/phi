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

package phi::interp;

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


=head3 Base classes
vtables serve a structural role in phi's OOP implementation, but they are
themselves objects. They need to be in order to be garbage-collected (phi has no
permgen; if it did, you'd never be able to reclaim the bootup class/code objects
after replacing them).


=head4 Here pointers
There are cases where we'll want to refer to the middle of a structure for
linkage reasons. For example, a lot of references to native code should be
C<jmp>-able, so we want to point to the first byte of executable machine code,
but that machine code is owned and managed by a phi object whose base offset
comes a bit before. These cases are negotiated using C<here> pointers, each of
which is preceded by a two-byte struct offset. It would be like this in C:

  struct x *here = ...;
  struct x *base = (struct x*) ((char*) here - ((unsigned short*) here)[-1]);

In memory terms we have this:

                 here-pointer - heremarker = object root
                 |
  object root    |        here-pointer points here
  |              |        |
  V              V        V
  vtable ... heremarker | native code/whatever

All we need to do is keep track of whether we have a here-pointer, a regular
pointer, or something else. I do address this point later on, I promise.
=cut

sub phi::asm::here_marker
{
  my $asm = shift;
  $asm->pS(phi::right - $asm);
}


=head4 Metaclasses and vtables
Any object-oriented language with reflection faces the bottoming-out problem:
what is C<x.class.class>? phi's model is that C<class> is an instance of itself.

This would normally be a problem because the method-call machinery would be
self-referential, but phi introduces the class->vtable compilation step as a
layer of indirection to fix this. Rather than having the class _perform_ the
method calls, the class _compiles_ method calls (technically, protocols compile
method calls, but the principle is the same). So classes are source objects that
get compiled into vtables, which are the low-level things that implement
polymorphism.
=cut

use phi::asm vtable_vtable => asm;

sub phi::asm::vtable_header
{
  shift->pQ(vtable_vtable)
       ->pS((l("mend") - l("mstart")) / 8)
       ->here_marker
       ->lmstart;
}

BEGIN
{
  vtable_vtable->vtable_header;
  vtable_vtable->{args}[0] = [vtable_vtable->resolve("mstart")];
}


use phi::use 'phi::vtable', sub { $_[0] => const asm($_[0])->vtable_header };

sub phi::asm::object
{
  my ($self, $vtable) = @_;
  $self->pQ($vtable->resolve("mstart"));
}


=head3 Instruction set
The simplest way to get this thing off the ground is to have a very small
handful of machine-code primitives that we then string together into functions.
These primitives don't need to be aware of OOP/memory management at all; they're
basically C-style int/pointer operations that operate in a concatenative way.

Why bother defining instructions above machine code? Because they're portable:
we want to minimize the processor-specific dependencies as much as we can. (None
of which precludes us from writing an optimizing compiler for any given backend
down the line.)

These instructions look almost exactly like FORTH primitives, and in fact I'm
going to use exactly the same register allocations that Jonesforth does:

  %rbp = return stack pointer
  %rsp = data stack pointer
  %rsi = instruction pointer
  %rdi = the interpreter object (not in Jonesforth; more below)

Our threading model is pretty different from Jonesforth. We don't execute
machine code directly; instead, we have a little bytecode interpreter that
includes some escapes for custom machine code sections. Specifically, here's our
instruction set (each is one byte):
=cut

use phi::const iinval => 0x00;          # 0x00 - 0x0f = invalid

use phi::const ilit8  => 0x10;          # two-byte insn
use phi::const ilit16 => 0x11;          # three-byte insn
use phi::const ilit32 => 0x12;          # five-byte insn
use phi::const ilit64 => 0x13;          # nine-byte insn

use phi::const iconst => 0x18;          # 0x18 - 0x1f are integer constants

use phi::const icall_native     => 0x20;
use phi::const icall_interp     => 0x21;
use phi::const imethod_call_nth => 0x22;
use phi::const iif              => 0x23;
use phi::const iiget            => 0x24;
use phi::const iiset            => 0x25;
use phi::const idget            => 0x26;
use phi::const idset            => 0x27;
use phi::const irget            => 0x28;
use phi::const irset            => 0x29;
use phi::const irpush           => 0x2a;
use phi::const irpop            => 0x2b;
use phi::const iret             => 0x2c;
use phi::const isyscall6        => 0x2d;

use phi::const iswap  => 0x30;
use phi::const idup   => 0x31;
use phi::const idrop  => 0x32;
use phi::const iover  => 0x33;
use phi::const irot3l => 0x34;
use phi::const igetn  => 0x35;          # two-byte insn
use phi::const isetn  => 0x36;          # two-byte insn

use phi::const igm8  => 0x40;
use phi::const ism8  => 0x41;
use phi::const igm64 => 0x42;
use phi::const ism64 => 0x43;
use phi::const imcpy => 0x44;

use phi::const iiplus  => 0x50;
use phi::const iitimes => 0x51;
use phi::const iishl   => 0x52;
use phi::const iisar   => 0x53;
use phi::const iishr   => 0x54;
use phi::const iiand   => 0x55;
use phi::const iior    => 0x56;
use phi::const iixor   => 0x57;
use phi::const iilt    => 0x58;
use phi::const iieq    => 0x59;
use phi::const iiinv   => 0x5a;
use phi::const iineg   => 0x5b;

use phi::const imcall1 => 0x80;
use phi::const imcall2 => 0xf0;         # two-byte insn
use phi::const imcall3 => 0xff;         # three-byte insn

=begin
TODO: instructions for managed languages? These all assume a flat memory model
with some amount of JIT capability. So we can target machine code, ASM.js, and
low-level bytecodes.

Using one byte per instruction means that our advancement primitive (NEXT in
Jonesforth) requires an additional instruction:

  # phi                                 # jonesforth
  xorq %rax, %rax                       lodsd
  lodsb                                 jmp *%eax
  jmp *(%rdi + 8*%rax)

C<%rdi> stores a here-pointer to the interpreter's bytecode dispatch table, so
there's no displacement. This reduces our overall code size.


=head4 Threading macros
These maintain registers and manage control flow. Every builtin instruction must
end with C<next> to advance the interpreter forwards.
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


=head4 Code objects
The interpreter contains a dispatch table with 256 entries, one per bytecode
instruction. Each entry is an absolute pointer to the linkable address of a
machine code object, which is itself independently GC-able and contains some
metadata about the operation it represents.
=cut

use phi::vtable 'amd64_code_vtable';

use phi::use 'phi::insn', sub
{
  my ($name, $asm) = @_;

  # TODO: refactor this into a perl -> phi struct compiler output
  "insn_$name" => const
    asm("insn_$name")
      ->pQ(amd64_code_vtable->resolve("mstart"))
      ->pC(length $name)
      ->lit($name)
      ->pS(l("codeend") - l"codestart")
      ->here_marker
      ->lcodestart
      ->inline(DEBUG_TRACE_INSNS ? asm->debug_print("INSN\t$name\n") : asm)
      ->inline($asm)
      ->next
      ->lcodeend;
};


=head4 Literal instructions
These are easy: we just use C<lods> to load the literal, then push it onto the
data stack.
=cut

use phi::insn lit8  => asm->  _ac_50;
use phi::insn lit16 => asm->_66ad_50;
use phi::insn lit32 => asm->  _ad_50;
use phi::insn lit64 => asm->_48ad_50;

use phi::insn litconst =>
  # %al stores the instruction, which is 0x18 higher than the literal int we
  # want to push.
  asm->_80o300pc(-0x18)                 # %al -= 0x18
     ->_50;                             # push %rax


=head4 Interpreter instructions
Most of the interesting stuff happens here.
=cut

use phi::insn call_native =>
  # Call directly into the specified address. Return address is stack top, data
  # stack follows that; the function can do anything it wants as long as it
  # restores the values of %rbp, %rsi, and %rdi (and doesn't totally mangle
  # %rsp).
  asm->_58_48ffo320;

use phi::insn call_interpreted =>
  asm->enter                            # save continuation
     ->_5a;                             # pop into %rsi

use phi::insn method_call_nth =>
  asm->_59                              # pop method number into %rcx
     ->mcall;                           # invoke it

use phi::insn if =>
  # Concatenatively, usage is this: <cond> [then] [else] if, where cond is an
  # integer. So we conditionally move "then" into %rsi.
  asm->enter                            # save continuation
     ->_5a                              # pop else into %rsi
     ->_58                              # pop then into %rax
     ->_59                              # pop cond into %rcx
     ->_4885o311                        # test %rcx, %rcx
     ->_480f45o360;                     # cmovnz %rax, %rsi

use phi::insn iget =>
  # We don't want to return the contents of %rdi directly, even though %rdi does
  # point into the interpreter. The problem is that %rdi is a here-pointer, so
  # we need to subtract the two-byte unsigned quantity immediately before it.
  asm->_57                              # push %rdi
     ->_480fb7o107pc(-2)                # movzxw *(%rdi - 2), %rax
     ->_4829o004o044;                   # sub %rcx, *%rsp

use phi::insn iset =>
  # WARNING: iset doesn't invert iget! iget returns a base pointer to the
  # interpreter, whereas iset expects a here-pointer to the bytecode dispatch
  # table.
  asm->_5f;

use phi::insn rget => asm->_55;
use phi::insn rset => asm->_5d;
use phi::insn dget => asm->_54;
use phi::insn dset => asm->_5c;

# TODO: optimize these by using memory-addressed push/pop
use phi::insn rpush =>
  asm->_58                              # pop %rax
     ->_4883o305pc(-8)                  # addq $-8, %rbp
     ->_4889o105pc(0);                  # movq %rax, *%rbp

use phi::insn rpop =>
  asm->_488bo105pc(0)                   # movq *%rbp, %rax
     ->_4883o305pc(8)                   # addq $8, %rbp
     ->_50;                             # push %rax

use phi::insn ret => asm->exit;

use phi::insn syscall6 => asm->syscall6;


=head4 Stack operations
Just enough to get things off the ground.
=cut

use phi::insn swap  => asm->_5859_5051;
use phi::insn dup   => asm->_58_5050;
use phi::insn drop  => asm->_58;
use phi::insn over  => asm->_5859_505150;
use phi::insn rot3l => asm->_58595a_515250;
use phi::insn getn  =>
  asm->_ac                              # lodsb for index
     ->_ffo064o304;                     # push stack[%rax]

use phi::insn setn  =>
  asm->_ac                              # lodsb for index
     ->_59                              # pop value to set
     ->_4889o014o304;                   # mov %rcx, stack[%rax]


=head4 Memory instructions
These aren't always available; you need a non-managed backend.
=cut

use phi::insn mem8get  => asm->_58_8ao000_50;
use phi::insn mem8set  => asm->_5859_88o001;
use phi::insn mem64get => asm->_58_488bo000_50;
use phi::insn mem64set => asm->_5859_4889o001;

use phi::insn memcpy =>
  asm->_59                              # pop size -> %rcx
     ->_488bo337                        # mov %rdi, %rbx
     ->_5f                              # pop dest -> %rdi
     ->_488bo326                        # mov %rsi, %rdx
     ->_5e                              # pop source -> %rsi
     ->_f348a4                          # rep(%rcx) movsb
     ->_488bo373                        # restore %rdi
     ->_488bo362;                       # restore %rsi


=head4 Integer instructions
These always apply to full-width untagged integers. You're unlikely to use these
directly most of the time because the data stack needs to consist of
vtable-prefixed value types for GC purposes.
=cut

use phi::insn intplus  => asm->_58_4801o004o044;
use phi::insn inttimes => asm->_5859_480fafo301_50;
use phi::insn intshl   => asm->_59_48d3o044o044;
use phi::insn intsar   => asm->_59_48d3o074o044;
use phi::insn intshr   => asm->_59_48d3o054o044;
use phi::insn intand   => asm->_58_4821o004o044;
use phi::insn intor    => asm->_58_4809o004o044;
use phi::insn intxor   => asm->_58_4831o004o044;

use phi::insn intlt =>
  # Arg ordering is such that [1 2 <] is false.
  asm->_5859                            # pop %rax (lhs), %rcx (rhs)
     ->_4831o322                        # xor %rdx, %rdx
     ->_483bo301                        # cmp %rcx to %rax
     ->_0f9co302                        # setl %rdx
     ->_52;                             # push %rdx

use phi::insn inteq =>
  asm->_5859                            # pop %rax, %rcx
     ->_4831o322                        # xor %rdx, %rdx
     ->_4885o301                        # test %rax, %rcx
     ->_0f94o302                        # sete %rdx
     ->_52;                             # push %rdx

use phi::insn intinv => asm->_48f7o024o044;
use phi::insn intneg => asm->_48f7o034o044;


=head4 Primitive method calls
The "method call" instructions always address the object pointed to by C<%rsp>,
which, in stack terms, is a value rather than a reference. Calling the nth
method involves using its vtable to fetch a pointer to a phi bytecode list, then
calling into that list. In other words, C<mcall(n)> is a much faster version of
this:

  <mem64                                # get vtable pointer
  lit(n) lit(8) int* int+               # get code pointer
  call-interpreted                      # invoke the method

NB: the method call instruction internals require their own instruction byte to
be present in C<%al>. This is a natural consequence of C<next>, which uses
C<lodsb>, but it's worth pointing out that this is a formal requirement.
=cut

use phi::insn mcall1 =>
  # One-byte method calls start at 0x80, so subtract 128
  asm->_04pc(-128)                      # %al -= 128
     ->_488bo310                        # mov %rax, %rcx
     ->mcall;

use phi::insn mcall2 =>
  # Two-byte method calls start at 0xf0, so subtract 240 and left-shift by eight
  asm->_04pc(16)                        # addb $16, %al
     ->_c1o340pc(8)                     # shld $8, %eax
     ->_ac                              # lodsb
     ->_488bo310                        # mov %rax, %rcx
     ->mcall;

use phi::insn mcall3 =>
  # Three-byte method calls start at 0xff, so no variation: just load two more
  # bytes
  asm->_66ad                            # lodsw
     ->_488bo310                        # mov %rax, %rcx
     ->mcall;


=head3 Interpreter object
The instruction set isn't OOP-aware, but the interpreter is an object. This is
important for a couple of reasons. First, the fact that the interpreter is just
a regular object means that it can be replaced; and second, as an object, it
ends up following the normal GC protocol so this replacement can happen
automatically when you stop referring to it.
=cut

use phi::vtable 'interpreter_vtable';

use phi::val insns => sub
{
  # TODO: if we have an insn class, use the class's awareness of opcodes to make
  # these assignments
  my @insns = map asm->lcodestart
                     ->debug_print(sprintf "illegal insn 0x%02x\n", $_)
                     ->exit_constant(1),
                  0..255;

  @insns[0x10 .. 0x13] = (insn_lit8, insn_lit16, insn_lit32, insn_lit64);
  @insns[0x18 .. 0x1f] = (insn_litconst) x 8;

  @insns[0x20 .. 0x23] = (insn_call_native,
                          insn_call_interpreted,
                          insn_method_call_nth,
                          insn_if);

  @insns[0x24 .. 0x29] = (insn_iget, insn_iset,
                          insn_dget, insn_dset,
                          insn_rget, insn_rset);

  @insns[0x2c .. 0x2d] = (insn_ret, insn_syscall6);

  @insns[0x30 .. 0x36] = (insn_swap, insn_dup, insn_drop,
                          insn_over, insn_rot3l, insn_getn, insn_setn);

  @insns[0x40 .. 0x44] = (insn_mem8get,  insn_mem8set,
                          insn_mem64get, insn_mem64set,
                          insn_memcpy);

  @insns[0x50 .. 0x53] = (insn_intplus, insn_inttimes, insn_intshl, insn_intsar);
  @insns[0x54 .. 0x57] = (insn_intshr,  insn_intand,   insn_intor,  insn_intxor);
  @insns[0x58 .. 0x5b] = (insn_intlt,   insn_inteq,    insn_intinv, insn_intneg);

  @insns[0x80 .. 0xef] = (insn_mcall1) x (0xf0 - 0x80);
  @insns[0xf0 .. 0xfe] = (insn_mcall2) x (0xff - 0xf0);
  $insns[0xff]         = insn_mcall3;

  \@insns;
};


use phi::asm interpreter_object => asm
  ->pQ(interpreter_vtable->resolve("mstart"))
  ->here_marker
  ->litable
  ->pQ256(map $_->resolve("codestart"), @{+insns});


=head3 Backend variants
We want to be able to run within different languages, the set of which spans
things like memory models, OOP/functional, and other paradigm differences. Some
cases like ASM.js involve bridging that gap within the same runtime. Because all
of this involves hard limits (we don't want to emulate a flat memory model if
the runtime provides GC for us), phi provides an operation that tells you about
the features available on the current runtime. Then you'll know which
instructions are enabled.


=head4 C<gc>
This is the biggest question mark for a runtime: how is memory managed? There
are a few options:

1. C<flat>: no GC; you have direct memory access (e.g. C, C++, machine code)
2. C<named>: no references/pointers (e.g. Perl4, POSIX sh, sed)
3. C<refcount>: cycles need to be broken, finalizers happen instantly (Perl5)
4. C<sync_tracing>: synchronous (pausing) mark/sweep (OCaml, Ruby?)
5. C<async_tracing>: concurrent mark/sweep (Java)

TODO: finalizer support?


=head4 C<int_size>
Basically, how big an int can we work with before overflowing a register? Sadly,
the number of bits is not always an even power of two; V8, OCaml, and Lisp each
use some form of bit tagging, reducing the effective integer precision to
slightly fewer than the native number of bits. On a 64-bit machine, this usually
means we get anywhere from 52 to 63 bits to work with (52 comes up if the
runtime pushes ints into NaN float values).

C<int_size> uses eight bits to encode the number of bits available in integers.


=head4 C<jit>
This setting can take on a few different values:

1. C<free>: there is no JIT overhead at all, e.g. machine code
2. C<fast>: you can quickly compile code at runtime, e.g. C<eval>
3. C<slow>: you can compile code via linkage, e.g. with the JVM; expect permgen
4. C<none>: you can't JIT anything; you'd have to restart the process
=cut

# TODO: implement variant support, whatever that ends up being


=head2 Classes and protocols
In a world with only classes, we'd need to allocate a separate vtable index for
every single method, which would take up a lot of space:

  # the world without protocols: every class contains four vtable entries
  class A {
    method foo;     # vtable index 0
    method bar;     # vtable index 1
  }
  class B {
    method foo;     # vtable index 0
    _               # vtable index 1 is blank to avoid A::bar conflict
    method bif;     # vtable index 2
    method baz;     # vtable index 3
  }

This is equivalent to every class belonging to a single protocol that contains
the union of everyone's methods.

We can fix this by specifying which classes are eligible for which virtual
method calls, in this case by adding them to a protocol object:

  protocol has_foo {
    method foo;     # same vtable index within all member classes
  }
  class A : has_foo {
    method foo;     # vtable index 0
    method bar;     # vtable index 1
  }
  class B : has_foo {
    method foo;     # vtable index 0
    method bif;     # vtable index 1 (no collision with A::bar)
    method baz;     # vtable index 2
  }

This is basically how method resolution works in statically-typed languages like
Java or C++; phi also uses static typing when it comes to classes/methods, it
just has more support for runtime code generation.

Every virtual method invocation, then, addresses a protocol rather than a class.
Any class-focused method invocations are static function calls and are likely to
be inlined during compilation.

NB: we can't modify vtables once they're allocated, so compiled classes,
vtables, and dynamic method calls are all immutable once you start using them.
It's theoretically possible to enumerate heap objects and rewrite their vtables
if all you're doing is adding new methods to things, but that's a library thing
instead of something built into the object system. (Q: can we use the GC process
to upgrade classes?)


=head3 vtable construction algorithm
Before I get into the optimization end of this, let's talk about some
higher-level tradeoffs.

First, vtables themselves obviously trade flexibility for performance. Dynamic
dispatch using fixed offsets is basically free compared to any other
implementation, so it's a no-brainer to take a hit on customization rather than
on every method invocation.

That doesn't mean we can live in a static world, though. It should be possible
to define a new method and have some way to invoke it immediately, and it should
also be possible to modify existing classes/protocols and have those changes
fully compiled into the running image. phi is locally static, globally dynamic.

Q: how do we check for type errors: do we build disjoint protocols, or insert
bitset cast-checks?


=head3 Method call compilation environment
Concatenative instruction lists are compiled from slightly higher-level "source"
objects, which means that the concatenative lists are, in some sense, specific
versions of a higher-level binary heap access protocol. (TODO: is it useful to
look at it this way?)


=head3 Value classes
Every stack entry will end up being prefixed with a vtable pointer that
specifies how we should interpret that entry. This means our primitive
instructions are actually not how we will manipulate the stack at all; we'll end
up making method calls against stack things, which will push, pop, and rearrange
themselves for us.

Let's talk about how method calls work because it's not quite as simple as it
sounds.

First, the interpreter's C<mcall> primitives work against the object referred to
by C<%rsp>, which means it's (the most recently) stack-allocated value _by
value_. This is important because it's exactly the way you don't want to write
programs most of the time. For example, an object-oriented int would look like
this on the stack:

  %rsp+8 -> int value
  %rsp   -> int vtable

This works for GC and gives us full-width unboxed primitives, but what about
full objects? We don't want to stack-allocate them:

  %rsp+alot   -> interpreter bytecode 0xff      # NOPE
  %rsp+alot-8 -> interpreter bytecode 0xfe
  ...
  %rsp+8      -> interpreter bytecode 0x00
  %rsp        -> interpreter vtable

Instead, we want the stack to contain a by-value _pointer_ to the interpreter:

  %rsp+8 -> interpreter pointer
  %rsp   -> pointer vtable

Now we can make by-value method calls against the pointer. So far so good, but
we're not out of the woods yet.


=head4 Method forwarding
phi doesn't support vtable-backed proxy objects (no language could without
allocating humongous vtables). Ruby, Perl, and other languages with some type of
C<method_missing> handler allow you to have an object that forwards its method
calls to another receiver, but this works only because the object uses a
reflective method invocation protocol.

This matters for us because we want a generic "base pointer" value type against
which we can make method calls that address the referent. So how do we implement
this without method forwarding? We need to resolve the pointer before making the
method call.

Value and reference types do this differently. Any stack value-primitive that
participates in OOP needs to support a C<resolve()> method that removes itself
and pushes a value and then its vtable. So a full method invocation is just
prefixed with C<.resolve()> to get the right receiver and vtable.

That gives us enough polymorphism to handle base/here pointers and primitives
correctly. Classes are aware of their value/reference nature; reference types
expect their address on the stack as the receiver, whereas value types expect
direct allocation on the stack. For example, here's the same C<point> type
implemented both by value and by reference:

  # point: double x, double y
  point_by_value    .set_x = [ swap, drop, lit point_vtable ]
  point_by_reference.set_x = [ swap, lit 8, +, >mem64 ]

Also note the different idioms: value types are immutable so we pop the receiver
and push the result (effectively), whereas reference types are modified in
place and their stack entries are dropped.
=cut

use phi::vtable 'pointer_vtable';
use phi::vtable 'herepointer_vtable';
use phi::vtable 'int_vtable';
use phi::vtable 'real_vtable';


=head2 Build the image
This is pretty simple: we just write the entire heap contents into a file and
call it a day. This works because the heap begins with ELF header allocations
that locate it properly in memory.


=head3 ELF header
These behave like heap allocations because of page alignment. Basically, let's
say our virtual address space starts at 0x400000; because the ELF file is
memory-mapped rather than copied, we have the constraint that we can "move" the
file's contents only by increments of 4096 bytes -- whole pages. We can't assign
an arbitrary offset, e.g. to skip the ELF header.

This isn't much of a problem really. As far as phi is concerned, our initial
heap is maybe 120 bytes smaller than it would normally be because it's got some
mystery data (the ELF headers) prepended; then that will get garbage collected
because nothing refers into it. So the net impact of our ELF headers ends up
being zero, exactly what we want.
=cut

use constant elf_startaddr         => 0x400000;
use constant elf_return_stack_size => 0x100000;

use phi::asm entry_point => asm;
use phi::asm elf_hdr     => asm
  ->locate(elf_startaddr)
  ->pC16_SSL(
    0x7f, ord 'E', ord 'L', ord 'F',
    2, 1, 1, 0,
    0, 0, 0, 0,
    0, 0, 0, 0,

    2,                                  # e_type    = ET_EXEC
    62,                                 # e_machine = EM_X86_64
    1)                                  # e_version = EV_CURRENT
  ->pQ(entry_point)
  ->pQQ_LSS_SSSS(
    64,                                 # e_phoff
    0,                                  # e_shoff
    0,                                  # e_flags

    64,                                 # e_ehsize
    56,                                 # e_phentsize
    1,                                  # e_phnum

    0,                                  # e_shentsize
    0,                                  # e_shnum
    0)                                  # e_shstrndx
  ->pLLQQQQQQ(
    1,                                  # p_type = PT_LOAD
    7,                                  # p_flags = R|W|X (important)
    0,                                  # p_offset (must be page-aligned)
    elf_startaddr,                      # p_vaddr
    0,                                  # p_paddr
    l("iend") - l"istart",              # p_filesz
    l("iend") - l"istart",              # p_memsz
    0x1000)                             # p_align
  ->lelf_hdr_end;


=head3 Initial interpreter state
We need to create the interpreter object and set up registers. I'm going to do
something a little bit sneaky here.

C<%rbp> needs to point to some available memory for a return stack, but right
now the only non-code allocation we have is the OS-provided C<%rsp> data stack.
If we want to allocate more, we'll need to use C<syscall6>, which itself uses
the return stack to save register state. How do we bootstrap this?

Pure awesomeness, that's how. We actually _do_ have a few bytes that aren't
code: remember the ELF header? That got mapped into memory because it's in the
same page as our bootstrap code. It doesn't serve any purpose once it's loaded,
so let's set C<%rbp> to the top end of the ELF header, which is 120 bytes (15
stack entries) long. We can start there before we map a return stack in memory.
=cut

interpreter_vtable
  ->pQ(asm("interp.print_char")         # method 0 = debug_print_char
    ->pC(idrop)                         # drop self pointer
    ->pCQCC(ilit64,                     # lit64 to push native
            asm("interp.print_char_native")
              ->_59585150               # (ret, char) -> (char, ret)
              ->_488bo304               # mov %rsp, %rax
              ->_515151                 # args 4, 5, 6
              ->_6apc(1)                # count = 1
              ->_50                     # buf = &char on stack
              ->_6apc(1)                # fd = 1
              ->_6apc(1)                # n = 1
              ->syscall6                # now we have (n, char, ret) on stack
              ->_5858                   # now we have (ret)
              ->_c3,
            icall_native,               # call_native
            iret))                      # ret

  ->pQ(asm("interp.exit")               # method 1 = debug_exit
    ->pC(idrop)                         # drop self pointer
    ->pCQC(ilit64,                      # lit64 to push native
           asm("interp.exit_native")->exit_constant(42),
           icall_native));              # call_native (never returns)


use phi::asm initial_list => asm
  ->pC6(ilit8, ord"f",  iiget, idup, igm64, imcall1 + 0)
  ->pC6(ilit8, ord"o",  iiget, idup, igm64, imcall1 + 0)
  ->pC6(ilit8, ord"o",  iiget, idup, igm64, imcall1 + 0)
  ->pC6(ilit8, ord"\n", iiget, idup, igm64, imcall1 + 0)
  ->pC4(iiget, idup, igm64, imcall1 + 1);


entry_point
  # Set up initial %rbp
  ->_48c7o305pl(elf_hdr->resolve("elf_hdr_end"))

  ->_6apc(0)            # push offset = 0
  ->_6apc(-1)           # push fd = -1
  ->_6apc(0x22)         # push flags = MAP_ANONYMOUS | MAP_PRIVATE
  ->_6apc(7)            # push prot = R|W|X
  ->_68pl(elf_return_stack_size)
  ->_6apc(0)            # push addr = 0
  ->_6apc(9)            # push n = __NR_MMAP
  ->syscall6            # now beginning of region is on the data stack
  ->_5d                 # pop region -> %rbp

  ->_4881o305pl(elf_return_stack_size)                  # initialize %rbp
  ->_48bfpQ(interpreter_object->resolve("itable"))      # initialize %rdi
  ->_48bepQ(initial_list)                               # initialize %rsi
  ->next;                                               # start interpreter


=head3 vtable finalization
We need to put end labels onto each of our vtables. TODO: migrate this into a
more managed process.
=cut

vtable_vtable->lmend;
amd64_code_vtable->lmend;
interpreter_vtable->lmend;


=head3 Heap construction
Allocate everything into the heap, dependencies last. C<elf_hdr> must be the
first object we allocate in order to generate a valid executable.
=cut

elf_hdr->listart
       ->inline(entry_point)
       ->liend;

print elf_hdr->linked unless caller;


=head3 Debugging: build a symbol table file
This is useful to have around for GDB and general troubleshooting. We don't
really care what the addresses end up being if everything's working, but if we
get C<%rsi> pointing at 0x400bf1, it's good to be able to verify that we expect
to have valid bytecode there.
=cut

if (DEBUG_EMIT_SYMBOLS)
{
  open my $fh, '> phi.symbols';
  printf $fh "%d\t%d\t%06x\t%s\n", $phi::asm::all_located{$_}->location,
                                   $phi::asm::all_located{$_}->size,
                                   $phi::asm::all_located{$_}->location,
                                   $_
    for sort { $phi::asm::all_located{$a}->location
                  <=> $phi::asm::all_located{$b}->location
               || $a cmp $b }
             keys %phi::asm::all_located;
  close $fh;
}

1;
