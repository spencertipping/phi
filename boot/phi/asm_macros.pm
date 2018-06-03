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

package phi::asm_macros;

use strict;
use warnings;

use phi::use 'phi::asm_macro' => sub
{
  no strict 'refs';
  my ($name, $sub) = @_;
  *{"phi::asm::$name"} = $sub;
  ();
};


=head2 Syscall macros
We end up issuing system calls to do basic setup and debugging, so it's worth
having these around and easy to access.

FIXME: this macro won't work anymore, and may break in horrible ways
=cut

use phi::asm_macro syscall6 => sub
{
  # Args are popped from the data stack in this layout:
  #
  #   arg6 arg5 ... arg1 syscallN syscall
  #
  # It's important to save Ferris, so allocate some space on the return stack to
  # stash %rsi and %rdi -- both of which will be destroyed when we run the
  # syscall instruction.

  shift->_4889o175pc(-8)                # movq %rdi, *(%rbp - 8)
       ->_4889o165pc(-16)               # movq %rsi, *(%rbp - 16)

       ->_58                            # pop n -> %rax
       ->_5f5e5a                        # pop args 1, 2, 3
       ->_498fo302_498fo300_498fo301    # pop args 4, 5, 6
       ->_0f05                          # syscall insn
       ->_50                            # push result

       ->_488bo175pc(-8)                # movq *(%rbp - 8),  %rdi
       ->_488bo165pc(-16);              # movq *(%rbp - 16), %rsi
};


=head2 Debugging code
Let's write some macros to make life much much easier. Basically, we want to
have some things we can drop into machine code to tell us what's going on inside
the image. We don't have libc, so int->string and such are functions we'll have
to write for ourselves if we want them.
=cut

use phi::asm_macro exit_constant => sub
{
  shift->_4831o300_b03c         # %rax = 0x3c (exit syscall)
       ->_48c7o307_pl(shift)    # %rdi = $code (exit code)
       ->_0f05;                 # syscall -- no return from here
};

use phi::asm_macro debug_print => sub
{
  my ($asm, $message, $fd) = @_;
  $fd //= 1;
  $asm->_50_51_52_56_57         # push %rax, %rcx, %rdx, %rsi, %rdi
      ->_4831o300_b001          # %rax (n) = 1 (write syscall)
      ->_e8pl(length $message)  # call sizeof(message)
      ->lit($message)           # the message
      ->_5e                     # pop &message -> %rsi (buf)
      ->_bapl(length $message)  # mov length -> %rdx (len)
      ->_48c7o307pl($fd)        # %rdi (fd) = $fd
      ->_0f05                   # syscall
      ->_5f_5e_5a_59_58;        # pop %rdi, %rsi, %rdx, %rcx, %rax
};


1;
