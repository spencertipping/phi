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
    uint16_t here_marker;               // offset = 32
    hereptr  bytecode_insns[256];       // offset = 34
  }

=cut

use constant interpreter_class => phi::class->new(
  'interpreter',
  interpreter_protocol)         # TODO: more protocols for things like GC
  ->def(
    unmap_heap => bin"                  # self cc
      # TODO
      swap drop goto                    #",

    map_heap => bin"                    # size self cc
      get_frameptr                      # size self cc f
      get_stackptr set_frameptr         # size self cc f|

      const0 const1 ineg lit8 22        # | offset=0 fd=-1 flags=PRIVATE|ANON
      lit8 07                           # | 0 -1 32 prot=7
      fget 03 const0                    # | 0 -1 32 7 length=size addr=0
      lit8 09 syscall                   # | addr

      # TODO: check for success

      dup dup fget 02                   # | addr addr addr self
      const16 iplus m64set              # | addr addr [heap_allocator = addr]
      fget 02 const8 iplus m64set       # | addr [heap_base = addr]
      fget 03 iplus                     # | limit
      fget 02 const24 iplus m64set      # size self cc f| [heap_limit = limit]

      set_frameptr                      # size self cc
      sset 02 drop drop goto            #",

    heap_allocate => bin"               # size self cc
      sget 01                           # size self cc self
      const16 iplus                     # size self cc &allocator
      dup m64get                        # size self cc &alloc r
      dup sget 05 iplus                 # size self cc &alloc r alloc+size

      # TODO: check heap limit and trigger GC

      sget 02 m64set                    # size self cc &alloc r [alloc=alloc']
      sset 03                           # r    self cc &alloc
      drop swap drop goto               # r",

    print_char => bin"                  # char self cc
      swap drop                         # char cc
      swap                              # cc char
      get_stackptr                      # cc char &char
      const0 swap const0 swap           # cc char 0 0 &char
      const0 swap const1 swap           # cc char 0 0 0 1 &char
      const1 const1 syscall             # cc char 1
      drop drop                         # cc
      goto                              #",

    print_string => bin"                # s self cc
      swap drop swap                    # cc s
      const0 swap const0 swap           # cc 0 0 s
      const0 swap                       # cc 0 0 0 s
      dup .size                         # cc 0 0 0 s size
      swap .data                        # cc 0 0 0 size buf
      const1 const1                     # cc 0 0 0 size buf 1 1
      syscall                           # cc n

      # TODO: check return value for partial writes
      drop goto                         #",

    exit => bin"                        # code self cc
      drop drop                         # code
      const0 swap const0 swap           # 0 0 code
      const0 swap const0 swap           # 0 0 0 0 code
      const0 swap lit8 3c               # 0 0 0 0 0 code NR_exit
      syscall                           # never returns");


=head2 Byte strings
The simplest possible class: a byte string is just a length-prefixed series of
bytes. Specifically:

  struct byte_string
  {
    here_pointer vtable;
    uint32       length;
    byte         data[length];
  }

=cut

use constant byte_string_class => phi::class->new('byte_string',
  byte_string_protocol,
  list_protocol)

  ->def(
    "+" => bin"                         # rhs self cc
      sget 01 .size                     # rhs self cc n1
      sget 03 .size                     # rhs self cc n1 n2
      sget 01 sget 01 iplus             # rhs self cc n1 n2 n
      dup lit8 +12 iplus                # rhs self cc n1 n2 n size

      # TODO: be properly GC atomic by storing self and RHS in a frame (this is
      # always the callee's responsibility)
      get_interpptr .heap_allocate      # rhs self cc n1 n2 n r

      # Copy in the vtable from the LHS. We can't rely on the RHS here because
      # there's no reason the RHS needs to be a string (as long as it implements
      # .size and .data).
      sget 05 m64get sget 01 m64set     # rhs self cc n1 n2 n r [r.vtable]
      sget 01 sget 01                   # rhs self cc n1 n2 n r n r
      const8 iplus m32set               # rhs self cc n1 n2 n r [r.size]

      # Copy the LHS string data.
      dup .data                         # rhs self cc n1 n2 n r &data
      sget 06 .data                     # rhs self cc n1 n2 n r &data &s1d
      swap sget 05                      # rhs self cc n1 n2 n r &s1d &data n1
      memcpy                            # rhs self cc n1 n2 n r [s1d]

      # Copy RHS data.
      dup .data sget 04 iplus           # rhs self cc n1 n2 n r &rdata
      sget 07 .data                     # rhs self cc n1 n2 n r &rdata &s2d
      swap sget 04                      # rhs self cc n1 n2 n r &rdata &s2d n2
      memcpy                            # rhs self cc n1 n2 n r [s2d]

      # Now we're done: return the new string.
      sset 05                           # r self cc n1 n2 n
      drop drop drop swap drop goto     # r",

    "[]" => bin"                        # i self cc
      sget 01 .data                     # i self cc &data
      sget 03 iplus                     # i self cc &data[i]
      m8get                             # i self cc data[i]
      sset 02 swap drop goto            # i",

    "==" => bin"                        # rhs self cc
      sget 01 .size                     # rhs self cc n1
      sget 03 .size ieq                 # rhs self cc size=?

      # TODO: how do we conditionally jump over some code? I think we need a bin
      # macro that does static local linking of some sort.
      ",

    "<" => bin"                         # rhs self cc
      # TODO
      ",

    data => bin"                        # self cc
      swap lit8 +12 iplus swap goto     # &data",

    length => bin"                      # self cc
      swap .size swap goto              # self.size",

    size => bin"                        # self cc
      swap const8 iplus                 # cc self+8
      m32get swap goto                  # size");


sub str($)
{
  heap << byte_string_class->vtable;
  phi::allocation->constant(pack "QL/a" => byte_string_class->vtable, shift)
    >> heap;
}


=head2 Binary native/bytecode strings
We can share a class for these two things, which is useful because they have
similar constraints. Here's the structure:

  type ref = { uint8_t ref_type; uint16_t offset };

  baseptr<object> source;               # 8 bytes
  uint16_t nrefs;                       # 2 bytes
  uint16_t codesize;                    # 2 bytes
  ref[nrefs] refs;                      # 3*nrefs bytes
  uint16_t here_marker;                 # 2 bytes
  char code[codesize];                  # codesize bytes

This is more involved than bare bytestrings for two reasons:

1. Code objects support here-pointers to their data
2. Code objects can close over heap references

NB: if we ask the above structure for a C<here_pointer> to its code,
C<here_marker> will offset from C<source> -- _after_ the vtable prefix --
because code buffers are unaware of vtable encoding strategy; i.e. they're
untyped until we make them polymorphic and specify how.
=cut


1;
