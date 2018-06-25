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
      # Optimization: if the strings' base pointers are equal, then the contents
      # must also be.
      sget 02 sget 02 ieq               # rhs self cc identical?
      [ drop sset 01 drop const1 swap goto ]
      [ goto ]
      if call

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


our $str_index = 0;
sub str($)
{
  heap << byte_string_class->vtable;
  phi::allocation->constant(pack "QL/a" => byte_string_class->vtable, $_[0])
                 ->named("string constant \"" . ($_[0] =~ s/[[:cntrl:]]/./gr)
                                              . "\""
                                              . ++$str_index)
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

    rdtsc => bin"                       # self cc
      # Execute some inline machine code using the native call instruction.
      [
        0f 31                           # rdtsc -> %edx:%eax
        # %edx are high 32 bits, %eax are low 32 bits. Left-shift and OR them
        # into a single value, push that, then clear %eax and use the regular
        # advancement macro.
        48c1o342 +32                    # shlq 32, %rdx
        4809o302                        # %rdx |= %rax
        31o300                          # xor %eax, %eax
        52 N                            # push %rdx
      ]
      call_native                       # self cc tsc
      sset 01 goto                      # tsc",

    exit => bin"                        # code self cc
      drop drop                         # code
      const0 swap const0 swap           # 0 0 code
      const0 swap const0 swap           # 0 0 0 0 code
      const0 swap lit8 3c               # 0 0 0 0 0 code NR_exit
      syscall                           # never returns");


=head2 Key/element comparators
These are used by set-like things (both lists and maps) to determine whether
keys are considered equivalent.
=cut

use constant intcmp_fn => phi::allocation
  ->constant(bin"                       # k1 k2 cc
    sget 02 sget 02 ieq                 # k1 k2 cc eq?
    sset 02 swap drop goto              # eq?")
  ->named('intcmp_fn') >> heap;

use constant strcmp_fn => phi::allocation
  ->constant(bin"                       # s1 s2 cc
    sget 02 sget 02 .==                 # s1 s2 cc eq?
    sset 02 swap drop goto              # eq?")
  ->named('strcmp_fn') >> heap;


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
  set_protocol,
  list_protocol)

  ->def(
    "nil?" => bin"                      # self cc
      const1 sset01 goto                # 1",

    length => bin"                      # self cc
      const0 sset01 goto                # 0",

    "contains?" => bin"                 # x self cc
      sset 01 drop const0 swap goto     # 0",

    "[]" => bin q{                      # i self cc
      "illegal .[] on nil" i.pnl
      const2 get_interpptr .exit        # boom },

    "+" => bin"                         # rhs self cc
      swap drop goto                    # rhs");


# NB: this class doesn't implement set_protocol because we don't know what the
# key compare function should be. The only reason nil can implement it is that
# it always returns false.
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


=head3 Linked list managed wrapper
This is a mutable object that stores an element-compare function so you can use
it like a set. Here's the struct:

  struct linked_list
  {
    hereptr       vtable;
    hereptr<fn>   element_eq_fn;
    baseptr<cons> root_cons;
  };

=cut


use constant linked_list_class => phi::class->new('linked_list',
  list_protocol,
  set_protocol,
  mutable_set_protocol,
  linked_list_protocol)

  ->def(
    "+" => bin"                         # rhs self cc
      const24 i.heap_allocate           # rhs self cc &l
      sget 02 m64get sget 01 m64set     # rhs self cc &l [.vt=]
      sget 02 .element==_fn             # rhs self cc &l efn
      sget 01 const8 iplus m64set       # rhs self cc &l [.efn=]

      sget 03 .root_cons                # rhs self cc &l rhs.cons
      sget 03 .root_cons .+             # rhs self cc &l cons'
      sget 01 const16 iplus m64set      # rhs self cc &l [.root_cons=]
      sset 02 swap drop goto            # &l",

    length => bin"                      # self cc
      swap .root_cons .length swap goto # l",

    "[]" => bin"                        # i self cc
      sget 02 sget 02 .root_cons .[]    # i self cc x
      sset 02 swap drop goto            # x",

    "element==" => bin"                 # x1 x2 self cc
      sget 03 sget 03 sget 03           # x1 x2 self cc x1 x2 self
      .element==_fn call                # x1 x2 self cc eq?
      sset 03 sset 01 drop goto         # eq?",

    "element==_fn" => bin"swap const8  iplus m64get swap goto",
    root_cons      => bin"swap const16 iplus m64get swap goto",

    "contains?" => bin"                 # x self cc
      sget 01 .element==_fn             # x self cc fn
      sget 02 .root_cons                # x self cc fn l
      [                                 # x self cc fn loop l
        dup .nil?                       # x self cc fn loop l nil?
        [ drop drop drop sset 01 drop const0 swap goto ]
        [                               # x self cc fn loop l
          dup .head                     # x self cc fn loop l l.h
          sget 06 sget 04 call          # x self cc fn loop l eq?
          [ drop drop drop sset 01 drop const1 swap goto ]
          [ .tail sget 01 goto ]
          if goto
        ]
        if goto
      ]                                 # x self cc fn l loop
      swap sget 01 goto                 # contains?",

    "<<" => bin"                        # x self cc
      sget 01 .root_cons                # x self cc self.cons
      sget 03 ::                        # x self cc cons'
      sget 02 const16 iplus m64set      # x self cc [.root_cons=]
      sset 01 swap goto                 # self");


use constant linked_list_fn => phi::allocation
  ->constant(bin"                               # efn cc
    const24 i.heap_allocate                     # efn cc &list
    lit64 >pack 'Q>', linked_list_class->vtable >> heap
    sget 01 m64set                              # efn cc &list [.vt=]

    sget 02 sget 01 const8  iplus m64set        # efn cc &list [.efn=]
    nil     sget 01 const16 iplus m64set        # efn cc &list [.root_cons=]

    sset 01 goto                                # &list")

  ->named('linked_list_fn') >> heap;


BEGIN
{
  bin_macros->{intlist} = bin"
    lit64 >pack 'Q>', intcmp_fn >> heap
    lit64 >pack 'Q>', linked_list_fn >> heap
    call                                # list";

  bin_macros->{strlist} = bin"
    lit64 >pack 'Q>', strcmp_fn >> heap
    lit64 >pack 'Q>', linked_list_fn >> heap
    call                                # list";
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

    drop
    strlist                             # cc []
    "foo" swap .<<                      # cc ["foo"]
    dup "foo" swap .contains?      i.assert
    dup "bar" swap .contains? inot i.assert
    drop

    "linked list tests passed" i.pnl

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
  set_protocol,
  mutable_map_protocol,
  mutable_set_protocol,
  linked_map_protocol)

  ->def(
    "key==" => bin"                     # k1 k2 self cc
      swap const8 iplus m64get          # k1 k2 cc keyeqfn
      sget 03 swap                      # k1 k2 cc k1 keyeqfn
      sget 03 swap                      # k1 k2 cc k1 k2 keyeqfn
      call                              # k1 k2 cc eq?
      sset 02 swap drop goto            # eq?",

    "key==_fn" => bin"                  # self cc
      swap const8 iplus m64get swap goto# fn",

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
      .kvcell_for .nil? inot            # k self cc contains?
      sset 02 swap drop goto            # contains?",

    "<<" => bin"                        # k self cc
      const0 sget 03 sget 03 .{}=       # k self cc self
      drop sset 01 swap goto            # self",

    "{}" => bin"                        # k self cc
      sget 02 sget 02                   # k self cc k self
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
  bin_macros->{intmap} = bin"
    lit64 >pack 'Q>', intcmp_fn >> heap
    lit64 >pack 'Q>', linked_map_fn >> heap
    call                                # map";

  bin_macros->{strmap} = bin"
    lit64 >pack 'Q>', strcmp_fn >> heap
    lit64 >pack 'Q>', linked_map_fn >> heap
    call                                # map";
}


use constant linked_map_test_fn => phi::allocation
  ->constant(bin q{                     # cc
    intmap                              # cc {}

    dup .keys .length const0 ieq i.assert

    const2 swap const1 swap .{}=        # cc {1->2}
    dup .keys .length const1 ieq i.assert
    dup .keys .head   const1 ieq i.assert
    dup .keys .value  const2 ieq i.assert
    dup const1 swap .contains?      i.assert
    dup const2 swap .contains? inot i.assert

    const8 swap const4 swap .{}=        # cc {1->2, 4->8}
    dup .keys .length const2 ieq i.assert
    dup const4 swap .contains?      i.assert
    dup const8 swap .contains? inot i.assert

    dup const1 swap .{} const2 ieq i.assert
    dup const4 swap .{} const8 ieq i.assert

    drop

    strmap                              # cc {}
    lit8 +55 swap "foo" swap .{}=       # cc {foo->55}
    lit8 +91 swap "bar" swap .{}=       # cc {foo->55, bar->91}

    dup "foo" swap .contains?      i.assert
    dup "bar" swap .contains?      i.assert
    dup "bif" swap .contains? inot i.assert
    dup "baz" swap .contains? inot i.assert

    dup "foo" swap .{} lit8+55 ieq i.assert
    dup "bar" swap .{} lit8+91 ieq i.assert

    dup "foo" swap .keys .head ieq inot i.assert

    "bif" swap .<<                      # cc {foo->55, bar->91, bif->1}
    dup "bif" swap .contains? i.assert

    drop

    "linked map tests passed" i.pnl     # cc

    goto                                # })

  ->named('linked map test fn') >> heap;


=head2 String buffer
The idea here is to maintain a buffer that has some headroom to minimize
reallocation. Each overflow doubles the buffer size. Here's what the struct
looks like:

  struct string_buffer
  {
    hereptr vtable;
    cell    size;
    cell    capacity;
    byte*   data;
  };

=cut


use constant string_buffer_class => phi::class->new('string_buffer',
  string_buffer_protocol)

  ->def(
    size     => bin"swap const8  iplus m64get swap goto",
    capacity => bin"swap const16 iplus m64get swap goto",
    data     => bin"swap const24 iplus m64get swap goto",

    headroom => bin"                    # self cc
      sget 01 .capacity                 # self cc c
      sget 02 .size ineg iplus          # self cc c-s
      sset 01 goto                      # c-s",

    append_byte => bin"                 # b self cc
      # Stack-allocate a string to hold the byte; then we have no heap impact.
      # Byte strings use an int32 for the length, so we want our stack entry to
      # look like this in memory:
      #
      #   01 00 00 00 <byte> 00 00 00

      sget 02 const32 ishl              # b self cc b<<32
      const1 ior                        # b self cc 000000bb_00000001
      lit64 >pack 'Q>', byte_string_class->vtable >> heap
      get_stackptr                      # b self cc b1 vt &s
      sget 04 .append_string            # b self cc b1 vt self
      drop drop drop sset 01            # cc self
      swap goto                         # self",

    append_quad => bin"                 # q self cc
      # Chop the quad into two halves. We're appending it in native-endianness,
      # so we want 0x8877665544332211 to be appended as 11 22 33 44 55 66 77 88.
      sget 02 const32 ishr              # q self cc q>>32
      sget 03 const32 ishl              # q self cc q>>32 q<<32
      const8 ior                        # q self cc q2 q1
      lit64 >pack 'Q>', byte_string_class->vtable >> heap
      get_stackptr                      # q self cc q2 q1 vt &s
      sget 05 .append_string            # q self cc q2 q1 vt self
      drop drop drop drop sset 01       # cc self
      swap goto                         # self",

    append_string => bin"               # x self cc
      sget 01 .headroom                 # x self cc h
      sget 03 .size                     # x self cc h s
      swap ilt inot                     # x self cc s<=h?
      [
        sget 02 .data                   # x self cc from-ptr
        sget 02 .size                   # x self cc from selfsize
        sget 03 .data iplus             # x self cc from to
        sget 04 .size                   # x self cc from to bytes
        memcpy                          # x self cc [.data+=]
        sget 02 .size sget 02 .size iplus # x self cc size'
        sget 02 const8 iplus m64set     # x self cc [.size=]
        sset 01 swap goto               # self
      ]
      [                                 # x self cc
        sget 01 .capacity               # x self cc c
        const1 ishl                     # x self cc c*2
        sget 02 .reallocate             # x self cc self
        sget 03 swap .append_string     # x self cc self
        sset 02 sset 00 goto            # self
      ]
      if goto                           # self",

    reallocate => bin"                  # size self cc
      sget 01 .data                     # size self cc from
      sget 03 i.heap_allocate           # size self cc from to
      dup sget 04 const24 iplus m64set  # size self cc from to [.data=]
      sget 03 .size memcpy              # size self cc [copy]
      sget 02 sget 02 const16 iplus m64set  # size self cc [.capacity=]
      sset 01 swap goto                 # self",

    to_string => bin"                   # self cc
      sget 01 .size lit8 +12 iplus      # self cc ssize
      i.heap_allocate                   # self cc &s
      lit64 >pack 'Q>', byte_string_class->vtable >> heap
      sget 01 m64set                    # self cc &s [.vt=]

      sget 02 .size sget 01 const8 iplus m32set   # [.size=]
      sget 02 .data sget 01 lit8 +12 iplus        # self cc &s from to
      sget 04 .size memcpy                        # self cc &s [copy]

      sset 01 goto                      # &s");


use constant string_buffer_fn => phi::allocation
  ->constant(bin"                       # cc
    const32 i.heap_allocate             # cc &buf
    const32 i.heap_allocate             # cc &buf &data
    sget 01 const24 iplus m64set        # cc &buf [.data=]
    lit64 >pack 'Q>', string_buffer_class->vtable >> heap
    sget 01 m64set                      # cc &buf [.vt=]
    const0 sget 01 const8 iplus m64set  # cc &buf [.size=]
    const32 sget 01 const16 iplus m64set# cc &buf [.capacity=]
    swap goto                           # &buf")
  ->named('string_buffer_fn') >> heap;


BEGIN
{
  bin_macros->{strbuf} = bin"
    lit64 >pack 'Q>', string_buffer_fn >> heap
    call                                # buf";
}


use constant string_buffer_test_fn => phi::allocation
  ->constant(bin q{                     # cc
    strbuf                              # cc buf
    dup .to_string "" .== i.assert
    dup .size const0  ieq i.assert
    dup .capacity const32 ieq i.assert

    "foo" swap .append_string           # cc buf
    dup .size lit8+3 ieq i.assert
    dup .to_string "foo" .== i.assert

    "bar" swap .append_string           # cc buf
    dup .size lit8+6 ieq i.assert
    dup .to_string "foobar" .== i.assert

    "foobar" swap .append_string                  # len=12
    "0123456789012345678" swap .append_string     # len=31
    "9" swap .append_string                       # len=32

    dup .size     const32 ieq i.assert
    dup .capacity const32 ieq i.assert
    dup .to_string "foobarfoobar01234567890123456789" .== i.assert

    lit8 'x swap .append_byte           # cc buf
    dup .size     lit8 +33 ieq i.assert
    dup .capacity lit8 +64 ieq i.assert

    dup .to_string "foobarfoobar01234567890123456789x" .== i.assert

    lit64 'abcdefgh swap .append_quad   # cc buf
    dup .size     lit8 +41 ieq i.assert
    dup .capacity lit8 +64 ieq i.assert

    dup .to_string "foobarfoobar01234567890123456789xhgfedcba" .== i.assert

    drop

    "string buffer tests passed" i.pnl
    goto                                # })

  ->named('string buffer test fn') >> heap;


=head2 Macro assembler
This is our first composite class:

  struct macro_assembler
  {
    hereptr                   vtable;
    macro_assembler*          parent;
    linked_list<ref>*         refs;
    linked_list<byte_string>* data;
    string_buffer*            code;
  };

Note that this design is suboptimal; philosophically there's no reason to store
pointers to linked lists or string buffers since they're all fully owned values.
I'm indirecting here only to simplify the allocator and method calls.

Before I write the macro assembler, though, I first need a class for refs.
Here's the struct:

  struct ref
  {
    hereptr vtable;
    uint32  offset;
    uint32  pointer_type;
  };

Refs always refer to full 64-bit quantities in code. They manage endian
conversion on get/set, which for now is hard-coded to assume little-endian
native encoding.
=cut


use constant ref_class => phi::class->new('ref',
  ref_protocol)

  ->def(
    offset       => bin"swap const8  iplus m32get swap goto",
    pointer_type => bin"swap lit8+12 iplus m32get swap goto",

    get => bin"                         # buf self cc
      sget 01 .offset                   # buf self cc offset
      sget 03 .data iplus               # buf self cc &ref
      m64get bswap64                    # buf self cc rval
      sset 02 sset 00 goto              # rval",

    set => bin"                         # x buf self cc
      sget 03 bswap64                   # x buf self cc rval
      sget 02 .offset                   # x buf self cc rval offset
      sget 04 .data iplus               # x buf self cc rval &ref
      m64set sset 02 drop drop goto     #");


use constant macro_assembler_class => phi::class->new('macro_assembler',
  macro_assembler_protocol)

  ->def(
    parent => bin"swap const8  iplus m64get swap goto",
    refs   => bin"swap const16 iplus m64get swap goto",
    data   => bin"swap const24 iplus m64get swap goto",
    code   => bin"swap const32 iplus m64get swap goto",

    child => bin"                       # self cc
      lit8 +40 i.heap_allocate          # self cc &child
      sget 02 m64get sget 01 m64set     # self cc &c [.vt=]
      sget 02 sget 01 const8 iplus m64set   # [.parent=]
      intlist sget 01 const16 iplus m64set  # [.refs=]
      intlist sget 01 const24 iplus m64set  # [.data=]
      strbuf  sget 01 const32 iplus m64set  # [.code=]
      sset 01 goto                          # &c",

    l8 => bin"                              # byte self cc
      sget 02 sget 02 .code .append_byte    # byte self cc code
      drop sset 01 swap goto                # self",

    l64 => bin"                             # v self cc
      sget 02 sget 02 .code .append_quad    # v self cc code
      drop sset 01 swap goto                # self",

    "ref<<" => bin"                     # val type self cc
      # Appends a ref at the current insertion point.
      const16 i.heap_allocate           # val type self cc &r
      lit64 >pack 'Q>', ref_class->vtable >> heap
      sget 01 m64set                    # val type self cc &r [.vt=]

      sget 02 .code .size sget 01 const8 iplus m32set   # [.offset=]
      sget 03             sget 01 const8 iplus m32set   # [.type=]

      dup sget 03 .refs .<< drop        # val type self cc ref [.refs<<]
      const0 sget 03 .l64 drop          # val type self cc ref [.l64]
      sget 04 swap .set drop            # val type self cc [ref.set]

      sset 02 sset 00 goto              # self",

    ptr => bin"                         # &x self cc
      # Append code to push a base pointer onto the data stack. First we append
      # the lit64 byte, then create a ref to refer to the insertion point and
      # append the pointer value.
      #
      # Base pointers have type 0.

      lit8 lit64 sget 02 .l8 drop       # &x self cc [lit64 insn]
      sget 02 const0 sget 03 .ref<<     # &x self cc self
      sset 02 sset 00 goto              # self",

    hereptr => bin"                     # &x self cc
      # Append code to push a here-pointer onto the data stack. Identical to
      # ptr(), but we use a different pointer type.
      #
      # Here pointers have type 1.

      lit8 lit64 sget 02 .l8 drop       # &x self cc [lit64 insn]
      sget 02 const1 sget 03 .ref<<     # &x self cc self
      sset 02 sset 00 goto              # self",

    ownptr => bin"                      # &code self cc
      # Append code to push an owned pointer onto the data stack. These pointers
      # are never traced during GC and are relative to the current instruction
      # pointer. This means our generated code prefix looks like this:
      #
      #   get_insnptr lit64 <value> iplus
      #
      # Owned pointers have type 2.

      # TODO: figure this out better; it's kind of a mess and doesn't work
      lit8 get_insnptr sget 02 .l8 drop         # &x self cc [<<get_insnptr]
      lit8 lit64       sget 02 .l8 drop         # &x self cc [<<lit64]
      sget 02                                   # &x self cc &x
           const2   sget 03 .ref<< drop         # &x self cc [<<&x]

      sset 02 sset 00 goto              # self",

    "[" => bin"                         # self cc
      # Return a new linked buffer. The child will append its compiled self and
      # return this parent when any of its close-bracket methods are invoked.
      swap .child swap goto             # child",

    "]" => bin"                         # self cc
      # Heap-allocate the child buffer and link a here-pointer.
      sget 01 .parent                   # self cc parent
      sget 02 .compile .here            # self cc parent codeptr
      swap .hereptr                     # self cc parent [.hereptr]
      sset 01 goto                      # parent",

    "]i" => bin"                        # self cc
      # Inline-allocate the child buffer and link an owned pointer. This
      # involves appending an entry to the data list.
      #
      # This is a bit tricky because we don't yet know the address of the value
      # we want to write. For now, we can store the absolute address of the
      # compiled code fragment as though it were a here-pointer, but mark it as
      # being an owned pointer. Then we'll fix up the address when we compile.
      #
      # TODO: this sounds awful
      ",

    compile => bin"
      # TODO");


1;
