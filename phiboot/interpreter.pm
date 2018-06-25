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

no warnings 'void';


=head2 Bytecode allocation
Bytecodes fall into a few groups:

  0x00 - 0x0f: reserved -- these will crash the interpreter
  0x10 - 0x1f: push a constant
  0x20 - 0x2f: interpreter-related things: call a function/native, etc
  0x30 - 0x3f: stack functions: swap/dup/drop/etc
  0x40 - 0x4f: memory functions, if the backend supports them
  0x50 - 0x5f: integer math
  0x60 - 0x6f: float math, if the backend supports them
  everything else: reserved

=cut

use constant bytecodes => [];
sub bcset
{
  die if @_ & 1;
  while (@_)
  {
    my $insn = shift;
    my $def  = shift;
    die "bcset: unknown instruction $insn" unless defined insns->{$insn};
    bytecodes->[hex insns->{$insn}] = $def;
  }
}


=head3 Constant-pushing functions
These each net one stack value, either embedded into the instruction or read as
a literal from the bytecode. Multibyte quantities are read in big-endian form.

All literals are unsigned, so no sign extension happens. You can encode negative
literals either using the full 64-bit form, or by using C<ineg> to negate a
positive integer.
=cut

bcset
  lit8  => bin"  ac        50 N",       # lodsb;                push %rax
  lit16 => bin"66ad 86o340 50 31o300 N",# lodsw; xchg %al, %ah; push %rax
  lit32 => bin"  ad 0fc8   50 31o300 N",# lodsd; bswap %eax;    push %rax
  lit64 => bin"48ad 480fc8 50 31o300 N",# lodsq; bswap %rax;    push %rax

  cell8 => bin"ac 488do014o30500000000 51 N",  # lodsb; lea (8*%rax), %rcx; ...

  const0  => bin"6a00 N", const1  => bin"6a01 N",
  const2  => bin"6a02 N", const4  => bin"6a04 N",
  const8  => bin"6a08 N", const16 => bin"6a10 N",
  const24 => bin"6a18 N", const32 => bin"6a20 N";


=head3 Interpreter functions
This is the most complicated section because we have some open-ended stuff going
on.

C<call_native> jumps straight into some machine code whose address you specify
(presumably with a C<here_pointer>). That machine code needs to preserve phi's
registers and manually advance the interpreter to return. The stack, C<%rsi>,
etc all come in unmodified.

C<call_bytecode> swaps the target here-pointer address with C<%rsi> on the
stack. It's up to you to make sure you have a way to preserve the callee for GC
purposes if you need GC atomicity -- in practice this tends to be managed by the
function prologue that allocates the stack frame.

C<get_interpptr> is unusual in that it assumes C<%rdi> is a here-pointer and
returns the _base_ pointer of the interpreter. C<set_interpptr>, on the other
hand, requires that you point directly to the dispatch table -- which presumably
is a here-pointer.
=cut

bcset
  call        => bin"4887o064o044 N",   # xchg *%rsp, %rsi
  call_native => bin"59 48ffo341",      # pop %rcx; jmp %rcx (no next required)

  mcall8      => bin"59                 # pop %rcx
                     ac                 # lodsb
                     56                 # push %rsi
                     488bo064o301 N     # movq *(%rcx + 8*%rax), %rsi",

  mcall16     => bin"59                 # pop %rcx
                     66ad               # lodsw
                     56                 # push %rsi
                     86o340             # xchg %al, %ah
                     488bo064o301       # movq *(%rcx + 8*%rax), %rsi
                     31o300 N           # xor %eax, %eax",

  if          => bin"595a5b             # else->%rcx, then->%rdx, cond->%rbx
                     4885o333           # test %rbx, %rbx
                     480f45o312         # cmovnz %rdx, %rcx
                     51 N               # push %rcx",

  syscall     => bin"4989o364           # movq %rsi, %r12
                     4889o373           # movq %rdi, %rbx
                     58                 # pop n -> %rax
                     5f5e5a             # args 1, 2, 3 -> (%rdi, %rsi, %rdx)
                     498fo302           # arg 4 -> %r10
                     498fo300           # arg 5 -> %r8
                     498fo301           # arg 6 -> %r9
                     0f05               # syscall
                     50                 # push %rax (syscall result)
                     31o300             # xor %eax, %eax
                     498bo364           # movq %r12, %rsi
                     488bo373 N         # movq %rbx, %rdi",

  get_frameptr  => bin"55 N",           # push %rbp
  set_frameptr  => bin"5d N",           # pop %rbp

  get_interpptr => bin"488bo317         # movq %rdi, %rcx
                       0fb7o121fe       # movzx *(%rcx - 2), %rdx
                       4829o321         # %rcx -= %rdx
                       51 N             # push %rcx",
  set_interpptr => bin"5f N",           # pop %rdi

  get_stackptr  => bin"54 N",           # push %rsp
  set_stackptr  => bin"5c N",           # pop %rsp
  get_insnptr   => bin"56 N",           # push %rsi
  goto          => bin"5e N";           # pop %rsi


=head3 Stack functions
Just enough stuff to manipulate operands for primitives. Most data accesses
address the current frame.

C<frame> allocates a frame of the requested size. You can do the same thing
using C<set_frameptr>, but C<frame> is faster if you know the frame size up
front.
=cut

bcset
  dup  => bin"595151 N",                # pop %rcx; push %rcx, %rcx
  drop => bin"59 N",                    # pop %rcx
  swap => bin"595a5152 N",              # pop %rcx, %rdx; push %rcx, %rdx
  fget => bin"ac ffo164o30500 N",       # lodsb; pushq *(%rbp + 8*%rax)
  fset => bin"ac 8fo104o30500 N",       # lodsb; popq  *(%rbp + 8*%rax)
  sget => bin"ac ffo064o304 N",         # lodsb; pushq *(%rsp + 8*%rax)
  sset => bin"ac 8fo004o304 N";         # lodsb; popq  *(%rsp + 8*%rax)


=head3 Memory functions
Memory get/set in various sizes. No endian-conversion happens here, so you'll
have to byteswap if you're writing constants into bytecode on x86.
=cut

bcset
  m8get  => bin"59 8ao001 50 N",        # pop %rcx; movb *%rcx, %al; push %rax
  m16get => bin"59 0fb7o011 51 N",      # pop %rcx; movzx *%rcx, %ecx; push %rcx
  m32get => bin"59 8bo011 51 N",        # pop %rcx; movd *%rcx, %ecx; push %rcx
  m64get => bin"59 ffo061 N",           # pop %rcx; pushq *%rcx

  m8set  => bin"5a59 88o012 N",         # pop %rdcx; movb %cl,  *%rdx
  m16set => bin"5a59 6689o012 N",       # pop %rdcx; movw %cx,  *%rdx
  m32set => bin"5a59 89o012 N",         # pop %rdcx; movd %ecx, *%rdx
  m64set => bin"5a59 4889o012 N",       # pop %rdcx; movq %rcx, *%rdx

  memcpy => bin"59                      # pop size -> %rcx
                488bo337                # movq %rdi, %rbx
                5f                      # pop dest -> %rdi
                488bo326                # movq %rsi, %rdx
                5e                      # pop source -> %rsi
                f348a4                  # rep(%rcx) movsb
                488bo362                # movq %rdx, %rsi
                488bo373 N              # movq %rbx, %rdi";


=head3 Integer instructions
The usual suspects, on full-width stack cells. Operations are signed by default.
=cut

bcset
  iplus  => bin"59 4801o014o044 N",     # pop %rcx; add %rcx, *%rsp
  itimes => bin"595a 480fafo321 52 N",  # pop %rcdx; imul %rcx, %rdx; push %rdx
  ishl   => bin"59 48d3o044o044 N",     # pop %rcx; shl(%rcx) *%rsp
  isar   => bin"59 48d3o074o044 N",     # pop %rcx; sar(%rcx) *%rsp
  ishr   => bin"59 48d3o054o044 N",     # pop %rcx; shr(%rcx) *%rsp
  iand   => bin"59 4821o014o044 N",     # pop %rcx; and %rcx, *%rsp
  ior    => bin"59 4809o014o044 N",     # pop %rcx; or %rcx, *%rsp
  ixor   => bin"59 4831o014o044 N",     # pop %rcx; xor %rcx, *%rsp

  ilt    => bin"595a                    # pop %rcx, %rdx
                31o333                  # xor %ebx, %ebx
                483bo312                # cmp %rcx to %rdx
                0f9co303                # setl %bl
                53 N                    # push %rbx",

  ieq    => bin"595a                    # pop %rcx, %rdx
                31o333                  # xor %ebx, %ebx
                4839o312                # cmpq %rcx, %rdx
                0f94o303                # sete %bl
                53 N                    # push %rbx",

  iinv => bin"48f7o024o044 N",          # not *%rsp
  ineg => bin"48f7o034o044 N",          # neg *%rsp

  bswap16 => bin"59 86o351 51 N",       # pop %rcx; xchg %cl, %ch; push %rcx
  bswap32 => bin"59   0fc9 51 N",       # pop %rcx; bswap %ecx; push %rcx
  bswap64 => bin"59 480fc9 51 N";       # pop %rcx; bswap %rcx; push %rcx


=head2 Interpreter dispatch table
Now we have enough stuff to assemble the interpreter dispatch table and store
that address into C<%rdi> at bootup.

First, go through the bytecode list and drop each instruction into the heap so
it has an address we can refer to.
=cut

use constant bytecode_allocations => [];
use constant insn_names           => {};

BEGIN
{
  insn_names->{hex insns->{$_}} = $_ for keys %{+insns};
}


sub insn_allocation($)
{
  my $insn = shift;
  my $trace_prefix = DEBUG_TRACE_INSNS
    ? debug_print sprintf("insn_%02x [%s]\n", $_, insn_names->{$insn}),
                  2
    : '';

  phi::allocation->constant($trace_prefix . bytecodes->[$insn])
                 ->named(sprintf "insn %s", insn_names->{$insn});
}


sub illegal_insn_allocation($)
{
  my $insn = shift;
  DEBUG_ILLEGAL_INSNS
    ? phi::allocation->constant(debug_die sprintf "illegal insn 0x%02x", $insn)
                     ->named(sprintf "illegal insn 0x%02x", $insn)
    : runtime_fail;
}


our ($max_bytecode) = grep defined bytecodes->[$_], reverse 0..255;

push @{+bytecode_allocations},
     (defined bytecodes->[$_] ? insn_allocation $_
                              : illegal_insn_allocation $_) >> heap
  for 0 .. (ILLEGAL_SEGFAULT_OK ? $max_bytecode : 255);


1;
