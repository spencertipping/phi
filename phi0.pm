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

use v5.14;
use strict;
use warnings;
use bytes;


=head1 ELF container
I'll get more into this later on, but for now all we need to know is that the
ELF header is a fixed size and our allocations can start at that point.
=cut

our $elf_heap = 0x400078;               # first available address
our @elf_heap;

sub heap_write
{
  my $addr = $elf_heap;
  push @elf_heap, $_[0];
  $elf_heap += length $_[0];
  $addr;
}


=head1 phi0 bytecode interpreter
A basic concatenative instruction set implemented in machine code. The mechanics
are just like Jonesforth, but always threaded through the bytecode interpreter:
we use C<%rsi> as the instruction pointer and C<%rdi> to store the instruction
offset table. The core mechanic here is C<next>, which is implemented as
C<lodsb; jmp *(%rdi + 8*%rax)>. This requires all but the lowest byte of C<%rax>
to be zero before we run it, which is why I avoid using C<%rax> in many
instructions.

phi's interpreter state is made of four pieces:

  %rsp = stack pointer
  %rbp = frame pointer
  %rsi = program pointer
  %rdi = interpreter pointer

We'll end up with a heap that we access using the interpreter object pointed to
by C<%rdi>. C<%rdi> is a here-pointer.

NB: C<call> produces a return address four bytes beyond the C<call> instruction,
effectively the four bytes immediately to its right. This is required for
garbage collection; see L<dev/history/8a20-continuation-type.pm>.
=cut

our $next = "\xac\xff\044\307";

our %bytecode_implementations =
( # Constants
  l8  =>             "\xac\x50$next",
  l16 => "\x66\xad\x86\340\x50\x31\300$next",
  l32 =>     "\xad\x0f\xc8\x50\x31\300$next",
  l64 => "\xad\x48\x0f\xc8\x50\x31\300$next",

  # Control flow / interpreter state
  gp => "\x56$next",                    # push %rsi (gp = "get program")
  gi => "\x57$next",                    # push %rdi (gi = "get interpreter")
  gs => "\x54$next",                    # push %rsp (gs = "get stack")
  fi => "\x66\xad\x86\340"              # lodsw; xchg %ah, %al
      . "\x48\x0f\xbf\310"              # movsx %ax, %rcx
      . "\x48\x03\315"                  # %rcx += %rbp
      . "\x51$next",                    # push %rcx

  go => "\x5e$next",                    # pop %rsi (go = goto)
  si => "\x5f$next",                    # pop %rdi
  ss => "\x5c$next",                    # pop %rsp
  sf => "\x5d$next",                    # pop %rbp (sf = "set frame")

  call => "\x48\x83\306\x04"            # %rsi += 4
        . "\x48\x87\064\044$next",      # xchg (%rsp), %rsi
  back => "\x59\x48\xff\341",           # pop %rcx; jmp %rcx
  if   => "\x59\x5a\x5b"                # else->%rcx, then->%rdx, cond->%rbx
        . "\x48\x85\333"                # test %rbx, %rbx
        . "\x48\x0f\x45\312"            # cmovnz %rdx, %rcx
        . "\x51$next",                  # push %rcx

  # Stack commands
  drop => "\x59$next",
  swap => "\x59\x5a\x51\x52$next",
  sget => "\xac\xff\064\304$next",
  sset => "\xac\x8f\004\304$next",

  # Memory commands
  g8  => "\x59\x8a\001\x50$next",
  g16 => "\x59\x0f\xb7\011\x51$next",
  g32 => "\x59\x7b\011\x51$next",
  g64 => "\x59\xff\061$next",

  s8  => "\x5a\x59\x88\012$next",
  s16 => "\x5a\x59\x66\x89\012$next",
  s32 => "\x5a\x59\x89\012$next",
  s64 => "\x5a\x59\x48\x89\012$next",

  mset => "\x59"                        # size -> %rcx
        . "\x48\x8b\337"                # movq %rdi, %rbx
        . "\x5f\x58"                    # dest -> %rdi, val -> %rax
        . "\xf3\xaa"                    # rep(%rcx) stosb
        . "\x48\x8b\373"                # movq %rbx, %rdi
        . "\x31\300$next",              # xor %rax, %rax

  mcpy => "\x59"                        # size -> %rcx
        . "\x48\x8b\337\x5f"            # movq %rdi, %rbx; pop dest -> %rdi
        . "\x48\x8b\326\x5e"            # movq %rsi, %rdx; pop src -> %rsi
        . "\xf3\x48\xa4"                # rep(%rcx) movsb
        . "\x48\x8b\362"                # movq %rdx, %rsi
        . "\x48\x8b\373$next",          # movq %rbx, %rdi

  # Integer operations
  iadd => "\x59\x48\x01\014\044$next",
  imul => "\x59\x5a\x48\x0f\xaf\321\x52$next",
  idiv => "\x59\x31\322\x58\x48\xf7\371\x50\x31\300\x52$next",

  ishl => "\x59\x48\xd3\044\044$next",
  isar => "\x59\x48\xd3\074\044$next",
  ishr => "\x59\x48\xd3\054\044$next",
  iand => "\x59\x48\x21\014\044$next",
  ior  => "\x59\x48\x09\014\044$next",
  ixor => "\x59\x48\x31\014\044$next",

  ilt  => "\x59\x5a\x31\333\x48\x3b\312\x0f\x9c\303\x53$next",
  ieq  => "\x59\x5a\x31\333\x48\x39\312\x0f\x94\303\x53$next",

  iinv => "\x48\xf7\024\044$next",
  ineg => "\x48\xf7\034\044$next",

  x16  =>     "\x59\x86\351\x51$next",
  x32  =>     "\x59\x0f\xc9\x51$next",
  x64  => "\x59\x48\x0f\xc9\x51$next" );

# Bytecodes start at 0x10 to simplify debugging.
our %bytecodes;
our $bytecode_table = pack Q16 => 0..15;

{
  our $bytecode_index = 0x10;
  for (sort keys %bytecode_implementations)
  {
    $bytecodes{$_} = $bytecode_index++;
    $bytecode_table .= pack Q => heap_write $bytecode_implementations{$_};
  }
  $bytecode_table .= pack "Q*" => $bytecode_index..255;
}


1;
