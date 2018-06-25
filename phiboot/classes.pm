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

use Scalar::Util;

no warnings 'void';


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

      i.heap_allocate                   # rhs self cc n1 n2 n r

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
      dup sget 04 .size ieq             # rhs self cc n1 size=?

      [                                 # rhs self cc size
        # Strings are equal length, so loop over each byte:
        [                               # rhs self cc loop size i
          sget 01 sget 01 ilt           # rhs self cc loop size i i<size?
          [                             # rhs self cc loop size i
            dup sget 06 .[]             # rhs self cc loop size i rhs[i]
            sget 01 sget 06 .[]         # rhs self cc loop size i rhs[i] self[i]
            ieq                         # rhs self cc loop size i eq?
            [                           # rhs self cc loop size i
              const1 iplus              # rhs self cc loop size i+1
              sget 02 goto              # tail call into loop
            ]
            [                           # rhs self cc loop size i
              drop drop drop            # rhs self cc
              swap drop swap drop       # cc
              const0 swap goto          # 0
            ]
            if goto
          ]
          [                             # rhs self cc loop size i
            # i >= size: we're done, return 1
            drop drop drop              # rhs self cc
            swap drop swap drop         # cc
            const1 swap goto            # 1
          ]
          if goto
        ]                               # rhs self cc size loop
        swap                            # rhs self cc loop size
        const0                          # rhs self cc loop size 0
        sget 02 goto                    # tail call into loop
      ]
      [                                 # rhs self cc _
        drop                            # rhs self cc
        swap drop swap drop             # cc
        const0 swap goto                # 0
      ]
      if goto                           # 0|1",

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
  phi::allocation->constant(pack "QL/a" => byte_string_class->vtable, $_[0])
                 ->named("string constant \"" . ($_[0] =~ s/[[:cntrl:]]/./gr)
                                              . "\""
                                              . Scalar::Util::refaddr \$_[0])
    >> heap;
}


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
  interpreter_protocol)

  ->def(
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

    pnl => bin"                         # s self cc
      sget 02 sget 02 .print_string     # s self cc
      lit8 0a sget 02 .print_char       # s self cc
      sset 01 drop goto                 #",

    assert => bin"                      # cond self cc
      sget 02                           # cond self cc cond
      [goto]                            # cond self cc cond ok
      [                                 # cond self cc
        lit64 >pack 'Q>', str('assertion failed') >> heap
        i.print_string                  # cond self cc
        debug_trace                     # print calling address
        const2 i.exit                   # exit with failure
      ]
      if                                # cond self cc fn
      call                              # cond self cc (or exit)
      sset 01 drop goto                 #",

    exit => bin"                        # code self cc
      drop drop                         # code
      const0 swap const0 swap           # 0 0 code
      const0 swap const0 swap           # 0 0 0 0 code
      const0 swap lit8 3c               # 0 0 0 0 0 code NR_exit
      syscall                           # never returns");


=head2 Linked lists
For bootup we can define most things in terms of cons cells. It's inefficient
but simple. Here's the layout:

  struct cons
  {
    hereptr       vtable;               # offset = 0
    cell          head;                 # offset = 8
    baseptr<cons> tail;                 # offset = 16
  }

=cut


use constant nil_class => phi::class->new('nil',
  maybe_nil_protocol,
  list_protocol)

  ->def(
    "nil?" => bin"                      # self cc
      const1 sset01 goto                # 1",

    length => bin"                      # self cc
      const0 sset01 goto                # 0",

    "[]" => bin"                        # i self cc
      lit64 >pack 'Q>', str('[] on nil')
      get_interpptr .print_string

      const2 get_interpptr .exit        # boom",

    "+" => bin"                         # rhs self cc
      swap drop goto                    # rhs");


use constant cons_class => phi::class->new('cons',
  cons_protocol,
  maybe_nil_protocol,
  list_protocol)

  ->def(
    head => bin"                        # self cc
      swap const8 iplus m64get          # cc head
      swap goto                         # head",

    tail => bin"                        # self cc
      swap const16 iplus m64get         # cc tail
      swap goto                         # tail",

    "nil?" => bin"                      # self cc
      swap drop const0 swap goto        # 0",

    "+" => bin"                         # rhs self cc
      sget 02 .nil?                     # rhs self cc rhs.nil?
      [ sset 01 swap goto ]             # self
      [ const24 i.heap_allocate         # rhs self cc &cons
        sget 02 m64get                  # rhs self cc &cons vt
        sget 01 m64set                  # rhs self cc &cons [.vtable=]

        sget 02 .head                   # rhs self cc &cons self.h
        sget 01 const8 iplus m64set     # rhs self cc &cons [.head=]

        sget 03 sget 03                 # rhs self cc &cons rhs self
        .tail .+                        # rhs self cc &cons self.tail+rhs
        sget 01 const16 iplus m64set    # rhs self cc &cons [.tail=]
        sset 02 swap drop goto ]        # &cons
      if goto",

    "[]" => bin"                        # i self cc
      swap sget 02                      # i cc self i
      [ .tail sget 02                   # i cc self.t i
        const1 ineg iplus               # i cc self.t i-1
        swap .[]                        # i cc self.t[i-1]
        sset 01 goto ]                  # self.t[i-1]
      [ .head sset 01 goto ]            # self.h
      if goto",

    length => bin"                      # self cc
      swap .tail .length                # cc self.tail.length
      const1 iplus swap goto            # self.tail.length+1");


use constant nil_instance => phi::allocation
  ->constant(pack Q => nil_class->vtable >> heap)
  ->named("nil_instance");


use constant cons_fn => phi::allocation
  ->constant(bin"                       # t h cc
      const24 i.heap_allocate           # t h cc &cons
      lit64 >pack 'Q>', cons_class->vtable >> heap
      sget 01 m64set                    # t h cc &cons [.vt=]

      sget 02 sget 01 const8 iplus m64set       # t h cc &cons [.h=]
      sget 03 sget 01 const16 iplus m64set      # t h cc &cons [.t=]
      sset 02 swap                      # &cons cc h
      drop goto                         # &cons")

  ->named("cons_fn");


BEGIN
{
  bin_macros->{'nil'} = bin"            #
    lit64 >pack 'Q>', nil_instance >> heap";

  bin_macros->{'::'} = bin"             # t h
    lit64 >pack 'Q>', cons_fn >> heap   # t h cons_fn
    call                                # cons";
}


=head3 Linked list unit tests
Nothing too comprehensive, just enough to make sure we aren't totally off the
mark.
=cut

use constant linked_list_test_fn => phi::allocation
  ->constant(bin q{                     # cc
    nil                                 # cc nil
    dup .length const0 ieq i.assert

    const2 ::                           # cc 2::nil
    dup .length const1 ieq i.assert

    const1 ::                           # cc 1::2::nil
    dup .length const2 ieq i.assert

    dup .+                              # cc 1::2::1::2::nil
    dup .length const4 ieq i.assert
    dup const0 swap .[] const1 ieq i.assert
    dup const1 swap .[] const2 ieq i.assert
    dup const2 swap .[] const1 ieq i.assert
    dup lit8+3 swap .[] const2 ieq i.assert

    dup .tail .length lit8+3 ieq i.assert

    "linked list tests passed" i.pnl

    drop                                # cc
    goto                                # })
  ->named('linked list test fn') >> heap;


=head2 Linked associative maps
The only remarkable thing about this implementation is that the map object is
responsible for storing the key comparator function, which takes two key cells
and returns a number indicating equal/not equal. We're not doing any ordering in
the map; it's strictly a linear search.

Here are the struct definitions:

  struct kv_cons                        # size = 32 bytes
  {
    hereptr vtable;
    cell    key;
    cell    value;
    baseptr next;
  };

  struct linked_map                     # size = 24 bytes
  {
    hereptr          vtable;
    hereptr<fn>      key_eq_fn;
    baseptr<kv_cons> alist;
  };

Linked maps return their alist for the keyset; the KV cons links behave as a
list of keys if you address them using the list protocol.
=cut


use constant kv_cons_class => phi::class->new('kv_cons',
  list_protocol,
  cons_protocol,
  kv_protocol,
  maybe_nil_protocol)

  ->def(
    key   => bin"swap const8  iplus m64get swap goto",
    value => bin"swap const16 iplus m64get swap goto",
    head  => bin"swap const8  iplus m64get swap goto",
    tail  => bin"swap const24 iplus m64get swap goto",

    "nil?" => bin"const0 sset01 goto",

    "+" => bin"                         # rhs self cc
      sget 02 .nil?                     # rhs self cc rhs.nil?
      [ sset 01 swap goto ]             # self
      [ const32 i.heap_allocate         # rhs self cc &cons
        sget 02 m64get                  # rhs self cc &cons vt
        sget 01 m64set                  # rhs self cc &cons [.vtable=]

        sget 02 .key                    # rhs self cc &cons self.k
        sget 01 const8 iplus m64set     # rhs self cc &cons [.key=]

        sget 02 .value                  # rhs self cc &cons self.v
        sget 01 const16 iplus m64set    # rhs self cc &cons [.value=]

        sget 03 sget 03                 # rhs self cc &cons rhs self
        .tail .+                        # rhs self cc &cons self.tail+rhs
        sget 01 const32 iplus m64set    # rhs self cc &cons [.tail=]
        sset 02 swap drop goto ]        # &cons
      if goto",

    "[]" => bin"                        # i self cc
      swap sget 02                      # i cc self i
      [ .tail sget 02                   # i cc self.t i
        const1 ineg iplus               # i cc self.t i-1
        swap .[]                        # i cc self.t[i-1]
        sset 01 goto ]                  # self.t[i-1]
      [ .head sset 01 goto ]            # self.h
      if goto",

    length => bin"                      # self cc
      swap .tail .length                # cc self.tail.length
      const1 iplus swap goto            # self.tail.length+1");


use constant linked_map_class => phi::class->new('linked_map',
  map_protocol,
  linked_map_protocol,
  mutable_map_protocol)

  ->def(
    "key==" => bin"                     # k1 k2 self cc
      swap const8 iplus m64get          # k1 k2 cc keyeqfn
      sget 03 swap                      # k1 k2 cc k1 keyeqfn
      sget 03 swap                      # k1 k2 cc k1 k2 keyeqfn
      call                              # k1 k2 cc eq?
      sset 02 swap drop goto            # eq?",

    keys => bin"                        # self cc
      swap const16 iplus m64get         # cc self.alist
      swap goto                         # self.alist",

    kvcell_for => bin"                  # k self cc
      swap dup const8 iplus m64get      # k cc self keyeqfn
      swap    const16 iplus m64get      # k cc keyeqfn alist
      [                                 # k cc kfn loop alist|nil
        dup .nil?                       # k cc kfn loop alist nil?
        [                               # k cc kfn loop nil
          sset 03 drop drop goto        # nil
        ]
        [                               # k cc kfn loop kvcons
          dup .key                      # k cc kfn loop kvcons key
          sget 05                       # k cc kfn loop kvcons key k
          sget 04 call                  # k cc kfn loop kvcons eq?
          [                             # k cc kfn loop kvcons
            sset 03 drop drop goto      # kvcons
          ]
          [                             # k cc kfn loop kvcons
            .tail sget 01 goto          # ...
          ]
          if goto
        ]
        if goto
      ]                                 # k cc kfn alist loop
      swap sget 01                      # k cc kfn loop alist loop
      goto                              # kvcons|nil",

    "contains?" => bin"                 # k self cc
      sget 02 sget 02                   # k self cc k self
      .kvcell_for .nil? iinv            # k self cc contains?
      sset 02 swap drop goto            # contains?",

    "{}" => bin"                        # k self cc
      sget 01 sget 01                   # k self cc k self
      .kvcell_for .value                # k self cc v
      sset 02 swap drop goto            # v",

    "{}=" => bin"                       # v k self cc
      # Just cons up a new cell. The space leak doesn't matter because all of
      # this is happening pre-GC; all we care about is minimizing the number of
      # allocations.
      const32 i.heap_allocate           # v k self cc &kv

      lit64 >pack 'Q>', kv_cons_class->vtable >> heap
      sget 01 m64set                    # v k self cc &kv [.vt=]

      sget 03 sget 01 const8  iplus m64set      # .k=
      sget 04 sget 01 const16 iplus m64set      # .v=
      sget 02 const16 iplus m64get      # v k self cc &kv alist
      sget 01 const24 iplus m64set      # v k self cc &kv [.tail=]
      sget 02 const16 iplus m64set      # v k self cc [.alist=]

      sset 02 swap drop                 # cc self
      swap goto                         # self");


use constant linked_map_fn => phi::allocation
  ->constant(bin"                               # kfn cc
    const24 i.heap_allocate                     # kfn cc &map
    lit64 >pack 'Q>', linked_map_class->vtable >> heap
    sget 01 m64set                              # kfn cc &map [.vt=]

    sget 02 sget 01 const8  iplus m64set        # kfn cc &map [.kfn=]
    nil     sget 01 const16 iplus m64set        # kfn cc &map [.alist=]

    sset 01 goto                                # &map")

  ->named('linked_map_fn') >> heap;


BEGIN
{
  bin_macros->{map} = bin"
    [ ieq ]                             # kfn
    lit64 >pack 'Q>', linked_map_fn >> heap
    call                                # map";
}


use constant linked_map_test_fn => phi::allocation
  ->constant(bin q{                     # cc
    "starting linked map tests" i.pnl
    map                                 # cc {}
    "allocated a map" i.pnl

    dup .keys .length const0 ieq i.assert

    const2 swap const1 swap .{}=        # cc {1->2}
    "survived" i.pnl
    dup .keys .length const1 ieq i.assert
    dup .keys .head   const1 ieq i.assert
    dup .keys .value  const2 ieq i.assert
    dup const1 swap .contains?      i.assert
    dup const2 swap .contains? iinv i.assert

    "linked map tests passed" i.pnl     # cc {1->2}

    drop goto                           # })

  ->named('linked map test fn') >> heap;


1;
