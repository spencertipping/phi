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

use constant nl_string    => str "\n";
use constant ansi_save    => str "\0337";
use constant ansi_restore => str "\0338";
use constant ansi_clear   => str "\033[J";


use phi::binmacro TODO => bin q{ "TODO" i.die };


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
    sget01 =8 iplus m64get            # size self cc &heapbase
    [ goto ]
    [ "ZOINKS: gotta map a heap before you allocate stuff!" i.pnl
      "Try putting this at the top of your phi1 script:" i.pnl
      "" i.pnl
      "  lit32 00100000 i.map_heap    # 1MB heap" i.pnl
      "" i.pnl
      =1 i.exit ]
    if call

    sget 01                           # size self cc self
    =16 iplus                         # size self cc &allocator
    dup m64get                        # size self cc &alloc r
    dup sget 05 iplus                 # size self cc &alloc r alloc+size

    sget 04 =24 iplus m64get          # size self cc &alloc r alloc' limit
    sget 01 ilt                       # size self cc &alloc r alloc' ok?

    [ goto ]                          # nop
    [ ".heap_allocate: exceeded limit" i.die ]
    if call                           # size self cc &alloc r alloc'

    # Align to 8-byte boundary
    =7 iplus =7 iinv iand             # size self cc &alloc r alloc'

    sget 02 m64set                    # size self cc &alloc r [alloc=alloc']
    sset 03                           # r    self cc &alloc
    drop swap drop goto               # r },

  heap_usage => bin q{                # self cc
    sget01 =16 iplus m64get           # self cc heap_allocator
    sget02 =8  iplus m64get           # self cc heap_allocator heap_start
    ineg iplus                        # self cc heap_usage
    sset01 goto                       # heap_usage },

  heap_size => bin q{                 # self cc
    sget01 =24 iplus m64get           # self cc heap_limit
    sget02 =8  iplus m64get           # self cc heap_limit heap_start
    ineg iplus                        # self cc heap_size
    sset01 goto                       # heap_size },

  globals => bin q{                   # self cc
    sget01 =32 iplus dup m64get       # self cc &gs &gs?
    [ m64get sset01 goto ]            # gs
    [ i64i sget01 m64set              # self cc &gs [gs=i64i]
      m64get sset01 goto ]            # gs
    if goto                           # gs },

  def => bin q{                       # val name self cc
    sget03 sget03 sget03 .globals     # v n s cc v n gs
    .{}= drop sset02 drop drop goto   # },

  global => bin q{                    # name self cc
    sget02 sget02 .globals .{}        # n self cc v
    sset02 sset00 goto                # v },

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

  pnl => bin q{                       # s self cc
    sget02     =1 i.print_string_fd   # s self cc
    $nl_string =1 i.print_string_fd   # s self cc
    sset01 drop goto                  # },

  pnl_err => bin q{                   # s self cc
    sget02     =2 i.print_string_fd   # s self cc
    $nl_string =2 i.print_string_fd   # s self cc
    sset01 drop goto                  # },

  read => bin q{                      # buf n self cc
    =0 =0 =0                          # buf n self cc 0 0 0
    sget05 sget07 =0                  # buf n self cc 0 0 0 n buf fd=0
    =0 syscall                        # buf n self cc nread
    sset03 sset01 drop goto           # nread },

  assert => bin q{                    # cond name self cc
    sget03                            # cond name self cc cond?

    [ $ansi_save    =2 i.print_string_fd
      sget02        =2 i.print_string_fd
      $ansi_clear   =2 i.print_string_fd
      $ansi_restore =2 i.print_string_fd
      sset02 drop drop goto ]         #

    [ $nl_string =2 i.print_string_fd # cond name self cc
      ""      i.pnl                   # cond name self cc
      "FAIL"  i.pnl                   # cond name self cc
      "  " =1 i.print_string_fd       # cond name self cc
      sget02  i.pnl                   # cond name self cc
      ""      i.pnl                   # cond name self cc
      debug_trace                     # print calling address
      =2 i.exit ]                     # exit(2)

    if goto                           # nothing (or exit) },

  die => bin q{                       # message self cc
    "" i.pnl_err                      # message self cc
    "dying by request" i.pnl          # message self cc
    debug_trace drop                  # message self
    drop                              # message
    i.pnl                             #
    lit8 +3 i.exit                    # exit(3) },

  exit => bin q{                      # code self cc
    drop drop =0 =0 =0 =0 =0          # code 0 0 0 0 0
    sget05 =60 syscall                # ->exit };


1;
