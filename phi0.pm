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


=head1 ELF image-as-heap
I'll get more into this later on, but for now all we need to know is that the
ELF header is a fixed size and our allocations can start at that point. We also
keep a list of patches, each of which is an address followed by data to
overwrite.
=cut

use constant heap_base => 0x400000;
our $heap = heap_base + 0x78;           # ELF header is 120 bytes long
our @heap;
our %patch;                             # heap address -> label name
our %labels;                            # name -> data

sub heap_write
{
  my $addr = $heap;
  push @heap, $_[0];
  $heap += length $_[0];
  $addr;
}

sub heap_patch { $patch{$_[0]}  = $_[1], shift, shift while @_ }
sub heap_label { $labels{$_[0]} = $_[1], shift, shift while @_ }


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
  l64 => "\x48\xad\x48\x0f\xc8\x50\x31\300$next",

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

  code => "\xad\x0f\xc8"                # lodsd; bswap %eax
        . "\x48\x83\306\x04"            # %rsi += 4
        . "\x5e"                        # push %rsi (code address)
        . "\x48\x01\360"                # %rsi += %rax
        . "\x31\300$next",              # %rax = 0

  # Stack commands
  drop => "\x59$next",
  swap => "\x59\x5a\x51\x52$next",
  sget => "\xac\xff\064\304$next",
  sset => "\xac\x8f\004\304$next",

  # Memory commands
  g8  => "\x59\x8a\001\x50$next",
  g16 => "\x59\x0f\xb7\011\x51$next",
  g32 => "\x59\x8b\011\x51$next",
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

  mcmp => "\x59"                        # size -> %rcx
        . "\x48\x8b\337\x5f"            # movq %rdi, %rbx; pop rhs -> %rdi
        . "\x48\x8b\326\x5e"            # movq %rsi, %rdx; pop lhs -> %rsi
        . "\xf2\x48\xa6"                # repne(%rcx) cmpsb
        . "\x51"                        # push %rcx (zero if operands equal)
        . "\x48\x8b\362"                # movq %rdx, %rsi
        . "\x48\x8b\373$next",          # movq %rbx, %rdi

  mfnd => "\x59"                        # size -> %rcx (size in quadwords)
        . "\x58"                        # val -> %rax (thing we're looking for)
        . "\x48\x8b\327\x5f"            # movq %rdi, %rdx; ptr -> %rdi
        . "\xf2\x48\xaf"                # repne(%rcx) scasq
        . "\x48\x0f\x44\317"            # cmovz %rdi, %rcx
        . "\x31\300"                    # %rax = 0
        . "\x48\x8b\372"                # movq %rdx, %rdi
        . "\x51$next",                  # push %rcx (zero if not found)

  unh4 => "\x59"                        # hereptr -> %rcx
        . "\x8b\121\xfc"                # %edx = *(%rcx - 4)
        . "\x48\x29\321"                # %rcx -= %rdx
        . "\x52\x51$next",              # push offset, base

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


# Real bytecodes start at 0x10 to simplify debugging (a random number being
# misused as a bytecode is more likely to be something like 0x00 than 0x10).
# Nonexistent bytecode addresses are just the bytecode numbers themselves plus a
# 0xbad marker; each will segfault, and gdb will be able to tell us which
# instruction we tried to use.
our %bytecodes;
our $bytecode_table = pack Q16 => map 0xbad << 8 | $_, 0..15;

{
  our $bytecode_index = 0x10;
  for (sort keys %bytecode_implementations)
  {
    $bytecodes{$_} = $bytecode_index++;
    $bytecode_table .= pack Q => heap_write $bytecode_implementations{$_};
  }
  $bytecode_table .= pack "Q*" => map 0xbad << 8 | $_, $bytecode_index..255;
}


=head1 Syscall wrapper and debugging functions
This is a good thing to define early on so we can test things more easily. It's
also the first thing we test in L<phi1test/syscall-native>.

Usage is to heap-allocate this somewhere, then C<l(address) back> to call into
it. It consumes the six syscall args plus the syscall number, then returns the
single syscall result.
=cut

our $syscall_native =
    "\x49\x89\364"                      # movq %rsi, %r12
  . "\x48\x89\373"                      # movq %rdi, %rbx
  . "\x58"                              # pop n -> %rax
  . "\x5f\x5e\x5a"                      # x1, x2, x3 -> %rdi, %rsi, %rdx
  . "\x49\x8f\302"                      # x4 -> %r10
  . "\x49\x8f\300"                      # x5 -> %r8
  . "\x49\x8f\301"                      # x6 -> %r9
  . "\x0f\x05"                          # syscall
  . "\x50\x31\300"                      # push %rax; %rax = 0
  . "\x49\x8b\364"                      # movq %r12, %rsi
  . "\x48\x8b\373$next";                # movq %rbx, %rdi


=head1 ELF generator
There isn't much to this. We just need a minimal bit of machine code to set up
C<%rsi> and C<%rdi>, then we can use C<$next> to jump straight into bytecode.

We'll also need to initialize C<%rbp> (the frame ptr), but we can do this from
inside bytecode using C<gs l(stacklimit_bytes) ineg iadd sf>.
=cut

sub heap_image($$)
{
  my ($interpreter_hereptr, $start_bytecode) = @_;
  my $heap_size     = $heap - heap_base;
  my $start_address = heap_write
      "\x31\300"
    . "\x48\xbe" . pack(Q => $start_bytecode)
    . "\x48\xbf" . pack(Q => $interpreter_hereptr)
    . $next;

  my $elf_header    = pack(C16SSL => 0x7f, ord"E", ord"L", ord"F",
                                     2, 1, 1, 0, (0) x 8,
                                     2, 62, 1)
                    . pack(Q      => $start_address)
                    . pack(QQLS6  => 64, 0, 0, 64, 56, 1, 0, 0, 0)
                    . pack(LLQQQ  => 1, 7, 0, heap_base, 0)
                    . pack(QQ     => $heap_size, $heap_size)
                    . pack(Q      => 0x1000);

  my $image = $elf_header . join"", @heap;
  while (my ($addr, $l) = each %patch)
  {
    substr($image, $addr - heap_base, length $labels{$l})
      = $labels{$l} // die "undefined label $l";
  }
  $image;
}


1;
