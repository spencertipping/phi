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


=head2 Interpreter class and memory layout
The interpreter struct looks like this:

  struct interpreter
  {
    hereptr  vtable;                    // offset = 0
    baseptr  heap_base;                 // offset = 8
    baseptr  heap_allocator;            // offset = 16
    baseptr  heap_limit;                // offset = 24
    strmap   globals;                   // offset = 32
    uint16_t here_marker;               // offset = 40
    hereptr  bytecode_insns[256];       // offset = 42
  }

=cut


use constant nl_string => phi::allocation
  ->constant(pack "QL/a" => byte_string_class->fn >> heap,
                            "\n")
  ->named('nl_string') >> heap;


use constant rdtsc_native => phi::allocation
  ->constant(bin q{
    0f 31                               # rdtsc -> %edx:%eax
    # %edx are high 32 bits, %eax are low 32 bits. Left-shift and OR them
    # into a single value, push that, then clear %eax and use the regular
    # advancement macro.
    48c1o342 +32                        # shlq 32, %rdx
    4809o302                            # %rdx |= %rax
    31o300                              # xor %eax, %eax
    52 N                                # push %rdx })
  ->named('rdtsc_native') >> heap;

BEGIN
{
  bin_macros->{rdtsc} = bin q{$rdtsc_native call_native};
}


use phi::class interpreter =>
  interpreter_protocol,

  map_heap => bin q{                  # size self cc
    =0 =1 ineg =34                    # ... offset=0 fd=-1 flags=PRIVATE|ANON
    =7                                # ... 0 -1 32 prot=7
    sget06 =0                         # ... 0 -1 32 7 length=size addr=0
    =9 syscall                        # ... addr

    dup =1 ineg ieq                   # ... addr error?

    [ "map_heap: mmap returned -1" i.die ]
    [ goto ]
    if call                           # size self cc addr

    dup sget03 =16 iplus m64set       # size self cc addr [.alloc=addr]
    dup sget03 =8  iplus m64set       # size self cc addr [.base=addr]
    sget03 iplus                      # size self cc limit
    sget02 =24     iplus m64set       # size self cc
    sset01 drop goto                  # },

  heap_allocate => bin q{             # size self cc
    sget 01                           # size self cc self
    =16 iplus                         # size self cc &allocator
    dup m64get                        # size self cc &alloc r
    dup sget 05 iplus                 # size self cc &alloc r alloc+size

    sget 04 =24 iplus m64get          # size self cc &alloc r alloc' limit
    sget 01 ilt                       # size self cc &alloc r alloc' ok?

    [ goto ]                          # nop
    [ ".heap_allocate: exceeded limit" i.die ]
    if call                           # size self cc &alloc r alloc'

    sget 02 m64set                    # size self cc &alloc r [alloc=alloc']
    sset 03                           # r    self cc &alloc
    drop swap drop goto               # r },

  heap_usage => bin q{                # self cc
    sget01 =16 iplus m64get           # self cc heap_allocator
    sget02 =8  iplus m64get           # self cc heap_allocator heap_start
    ineg iplus                        # self cc heap_usage
    sset01 goto                       # heap_usage },

  globals => bin"swap =32     iplus m64get swap goto",
  "globals=" => bin"                  # g' self cc
    sget 02 sget 02 =32     iplus m64set    # g' self cc [.globals=]
    sset 01 drop goto                 #",

  def => bin"                         # val name self cc
    sget 03 sget 03 sget 03 .globals  # v n self cc v n g
    .{}= drop sset 02 drop drop goto  #",

  global => bin"                      # name self cc
    sget 02 sget 02 .globals .{}      # n self cc v
    sset 02 sset 00 goto              # v",

  print_char => bin"                  # char self cc
    sset00                            # char cc
    swap                              # cc char
    get_stackptr                      # cc char &char
    =0 swap =0 swap                   # cc char 0 0 &char
    =0 swap =1 swap                   # cc char 0 0 0 1 &char
    =1 =1 syscall                     # cc char 1
    drop drop                         # cc
    goto                              #",

  print_string_fd => bin q{           # s fd self cc
    =0     =0     =0                  # s fd self cc 0 0 0
    sget06 .size                      # s fd self cc 0 0 0 n
    sget07 .data                      # s fd self cc 0 0 0 n &d
    sget07                            # s fd self cc 0 0 0 n &d fd
    =1     syscall                    # s fd self cc n
    sget04 .size ixor                 # s fd self cc partial-write?

    [ "print_string_fd: partial write" i.die ]
    [ goto ]
    if call                           # s fd self cc
    sset02 drop drop goto             # },

  print_string => bin q{              # s self cc
    sget02 =1     i.print_string_fd   # s self cc
    sset01 drop goto                  # },

  pnl => bin q{                       # s self cc
    sget02     i.print_string         # s self cc
    $nl_string i.print_string         # s self cc
    sset01 drop goto                  # },

  pnl_err => bin q{                   # s self cc
    sget02     =2     i.print_string_fd   # s self cc
    $nl_string =2     i.print_string_fd   # s self cc
    sset01 drop goto                  # },

  assert => bin q{                    # cond name self cc
    sget03                            # cond name self cc cond

    [ goto ]                          # cond name self cc
    [ drop                            # cond name self cc
      "" i.pnl                        # cond name self cc
      "FAIL" i.pnl                    # cond name self cc
      "  " i.print_string             # cond name self cc
      sget02 i.pnl                    # cond name self cc
      "" i.pnl                        # cond name self cc
      debug_trace                     # print calling address
      =2     i.exit ]                 # exit(2)
    if call                           # cond name self cc (or exit)

    sset02 drop drop goto             # },

  die => bin q{                       # message self cc
    "dying by request" i.pnl          # message self cc
    debug_trace drop                  # message self
    drop                              # message
    i.pnl                             #
    lit8 +3 i.exit                    # exit(3) },

  exit => bin"                        # code self cc
    drop drop                         # code
    =0     swap =0     swap           # 0 0 code
    =0     swap =0     swap           # 0 0 0 0 code
    =0     swap lit8 3c               # 0 0 0 0 0 code NR_exit
    syscall                           # never returns";


1;
