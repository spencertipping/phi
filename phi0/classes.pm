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


=head2 Byte strings
The simplest possible class: a byte string is just a length-prefixed series of
bytes. Specifically:

  struct byte_string
  {
    here_pointer vtable;
    uint32       length;
    byte         data[length];
  }

Byte strings behave as mutable bitsets if you address them with the set
protocol.
=cut

use constant byte_string_class => phi::class->new('byte_string',
  byte_string_protocol,
  eq_protocol,
  joinable_protocol,
  set_protocol,
  mutable_set_protocol,
  invertible_protocol,
  list_protocol)

  ->def(
    "contains?" => bin q{               # bit self cc
      sget02 lit8+3 ishr                # bit self cc bytei
      sget02 .[]                        # bit self cc byte
      const1 sget04 lit8+7 iand ishl    # bit self cc byte mask
      iand                              # bit self cc b
      sset02 sset00 goto                # b },

    "<<" => bin q{                      # bit self cc
      sget02 lit8+3 ishr                # bit self cc i
      sget02 .data iplus                # bit self cc &c
      const1 sget04 lit8+7 iand ishl    # bit self cc &c b
      sget01 m8get ior swap m8set       # bit self cc [c|=b]
      sset01 swap goto                  # self },

    "~" => bin q{                       # self cc
      sget01 .length lit8+12 iplus      # self cc size
      dup i.heap_allocate               # self cc size &s

      # Copy ourselves into the new object verbatim
      sget03 sget01 sget03 memcpy       # self cc size &s [copy]

      # Now invert each character in the new string
      sset02                            # s cc size
      drop sget01 .data                 # s cc &d
      sget02 .length                    # s cc &d l
      const0                            # s cc &d l i
      [                                 # s cc &d l i loop
        sget02 sget02 ilt               # s cc &d l i loop i<l?
        [ sget03 sget02 iplus           # s cc &d l i loop &d[i]
          dup m8get iinv swap m8set     # s cc &d l i loop [d[i]=~d[i]]
          swap const1 iplus swap        # s cc &d l i+1 loop
          dup goto ]                    # ->loop
        [ drop drop drop drop goto ]    # s
        if goto ]                       # s cc &d l i loop
      dup goto ]                        # ->loop },

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

    reduce => bin q{                    # x0 f self cc
      const0                            # x0 f self cc i
      sget02 .length                    # x0 f self cc i l
      sget03 .data                      # x0 f self cc i l d
      [                                 # x0 f self cc i l d loop
        sget02 sget04 ilt               # x0 f self cc i l d loop i<l?
        [
                                        # 8  7 6    5  4 3 2 1    0
          sget01 sget04 iplus m8get     # x0 f self cc i l d loop d[i]
          sget08 sget08                 # x0 f self cc i l d loop d[i] x0 f
          call                          # x0 f self cc i l d loop x0' exit?
          [
            # Early exit
            sset07 drop drop drop drop  # x0' f self cc
            sset01 drop goto            # x0'
          ]
          [
            # No early exit; continue normally by replacing x0 and i
            sset07                      # x0' f self cc i l d loop
            sget03 const1 iplus sset03  # x0' f self cc i+1 l d loop
            dup goto                    # ->loop
          ]
          if goto
        ]
        [                               # x0 f self cc i l d loop
          # No more list items: return x0
          drop drop drop drop           # x0 f self cc
          sset 01 drop goto             # x0
        ]
        if goto
      ]                                 # x0 f self cc i l d loop
      dup goto                          # ->loop },

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


sub str($)
{
  phi::allocation->constant(pack "QL/a" => byte_string_class, $_[0])
                 ->named("string constant \"" . ($_[0] =~ s/[[:cntrl:]]/./gr)
                                              . "\""
                                              . ++($phi::str_index //= 0))
    >> heap;
}


use constant memset_fn => phi::allocation
  ->constant(bin q{                     # c &m size cc
    const0                              # c &m size cc i
    [                                   # c &m size cc i loop
      sget03 sget02 ilt                 # c &m size cc i loop i<size?
      [ sget05 sget05 sget03 iplus      # c &m size cc i loop c &m[i]
        m8set                           # c &m size cc i loop [m[i]=c]
        swap const1 iplus swap          # c &m size cc i+1 loop
        dup goto ]                      # ->loop
      [ drop drop sset02 drop drop      # cc
        goto ]                          #
      if goto ]
    dup goto                            # })
  ->named('memset_fn') >> heap;

BEGIN
{
  bin_macros->{memset} = bin q{$memset_fn call};
}


use constant empty_bitset_fn => phi::allocation
  ->constant(bin q{                     # capacity cc
    sget01 lit8+7 iplus lit8+3 ishr     # capacity cc bytes
    dup lit8+12 iplus i.heap_allocate   # capacity cc bytes &s

    $byte_string_class sget01 m64set    # [.vt=]
    sget01 sget01 const8 iplus m32set   # [.length=]

    const0 sget01 .data sget03          # cap cc bytes &s 0 &data bytes
    memset                              # cap cc bytes &s
    sset02 drop goto                    # &s })
  ->named('empty_bitset_fn') >> heap;

BEGIN
{
  bin_macros->{bitset} = bin q{$empty_bitset_fn call};
}


use constant byte_string_test_fn => phi::allocation
  ->constant(bin q{                     # cc
    "foo" "bar" .+
    "barfoo" .== "barfoo" i.assert

    const0
    [                                   # total c cc
      sget02 sget02 iplus sset02        # total' c cc
      const0 sset01 goto ]              # total' 0
    "01" .reduce
    lit8+97 ieq "total97" i.assert

    "foo" .~ .~ "foo" .== "inv2" i.assert

    lit8+13 bitset                      # cc b
      const0 sget01 .contains? const0 ieq "bcontains0" i.assert
      const1 sget01 .contains? const0 ieq "bcontains1" i.assert
      const2 sget01 .contains? const0 ieq "bcontains2" i.assert
      const4 sget01 .contains? const0 ieq "bcontains4" i.assert
      const8 sget01 .contains? const0 ieq "bcontains8" i.assert

      .~
      const0 sget01 .contains? "~bcontains0" i.assert
      const1 sget01 .contains? "~bcontains1" i.assert
      const2 sget01 .contains? "~bcontains2" i.assert
      const4 sget01 .contains? "~bcontains4" i.assert
      const8 sget01 .contains? "~bcontains8" i.assert
      .~

                                        # cc b
      const0 sget01 .<<                 # cc b b
      const0 swap .contains? "bcontains0" i.assert

      const1 sget01 .contains? const0 ieq "bcontains1" i.assert
      const2 sget01 .contains? const0 ieq "bcontains2" i.assert
      const4 sget01 .contains? const0 ieq "bcontains4" i.assert
      const8 sget01 .contains? const0 ieq "bcontains8" i.assert

      const2 sget01 .<< const2 swap .contains? "bcontains2" i.assert
      const1 sget01 .contains? const0 ieq "bcontains1" i.assert
      const4 sget01 .contains? const0 ieq "bcontains4" i.assert
      const8 sget01 .contains? const0 ieq "bcontains8" i.assert

      const8 sget01 .<< const8 swap .contains? "bcontains8" i.assert
      const1 sget01 .contains? const0 ieq "bcontains1" i.assert
      const4 sget01 .contains? const0 ieq "bcontains4" i.assert

    drop
    goto                                # })
  ->named('byte_string_test_fn') >> heap;


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
  ->constant(pack "QL/a" => byte_string_class->vtable >> heap,
                            "\n")
  ->named('nl_string') >> heap;


use constant interpreter_class => phi::class->new(
  'interpreter',
  interpreter_protocol)

  ->def(
    map_heap => bin q{                  # size self cc
      get_frameptr                      # size self cc f
      get_stackptr set_frameptr         # size self cc f|

      const0 const1 ineg lit8 22        # | offset=0 fd=-1 flags=PRIVATE|ANON
      lit8 07                           # | 0 -1 32 prot=7
      fget 03 const0                    # | 0 -1 32 7 length=size addr=0
      lit8 09 syscall                   # | addr

      dup const1 ineg ieq               # | addr error?

      [ "map_heap: mmap returned -1" i.die ]
      [ goto ]
      if call                           # | addr

      dup dup fget 02                   # | addr addr addr self
      const16 iplus m64set              # | addr addr [heap_allocator = addr]
      fget 02 const8 iplus m64set       # | addr [heap_base = addr]
      fget 03 iplus                     # | limit
      fget 02 const24 iplus m64set      # size self cc f| [heap_limit = limit]

      set_frameptr                      # size self cc
      sset 02 drop drop goto            # },

    heap_allocate => bin q{             # size self cc
      sget 01                           # size self cc self
      const16 iplus                     # size self cc &allocator
      dup m64get                        # size self cc &alloc r
      dup sget 05 iplus                 # size self cc &alloc r alloc+size

      sget 04 const24 iplus m64get      # size self cc &alloc r alloc' limit
      sget 01 ilt                       # size self cc &alloc r alloc' ok?

      [ goto ]                          # nop
      [ ".heap_allocate: exceeded limit" i.die ]
      if call                           # size self cc &alloc r alloc'

      sget 02 m64set                    # size self cc &alloc r [alloc=alloc']
      sset 03                           # r    self cc &alloc
      drop swap drop goto               # r },

    heap_usage => bin q{                # self cc
      sget01 const16 iplus m64get       # self cc heap_allocator
      sget02 const8 iplus m64get        # self cc heap_allocator heap_start
      ineg iplus                        # self cc heap_usage
      sset01 goto                       # heap_usage },

    globals => bin"swap const32 iplus m64get swap goto",
    "globals=" => bin"                  # g' self cc
      sget 02 sget 02 const32 iplus m64set    # g' self cc [.globals=]
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
      const0 swap const0 swap           # cc char 0 0 &char
      const0 swap const1 swap           # cc char 0 0 0 1 &char
      const1 const1 syscall             # cc char 1
      drop drop                         # cc
      goto                              #",

    print_string_fd => bin q{           # s fd self cc
      const0 const0 const0              # s fd self cc 0 0 0
      sget06 .size                      # s fd self cc 0 0 0 n
      sget07 .data                      # s fd self cc 0 0 0 n &d
      sget07                            # s fd self cc 0 0 0 n &d fd
      const1 syscall                    # s fd self cc n
      sget04 .size ixor                 # s fd self cc partial-write?

      [ "print_string_fd: partial write" i.die ]
      [ goto ]
      if call                           # s fd self cc
      sset02 drop drop goto             # },

    print_string => bin q{              # s self cc
      sget02 const1 i.print_string_fd   # s self cc
      sset01 drop goto                  # },

    pnl => bin q{                       # s self cc
      sget02     i.print_string         # s self cc
      $nl_string i.print_string         # s self cc
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
        const2 i.exit ]                 # exit(2)
      if call                           # cond name self cc (or exit)

      sset02 drop drop goto             # },

    die => bin q{                       # message self cc
      "dying by request" i.pnl          # message self cc
      debug_trace drop                  # message self
      drop                              # message
      i.pnl                             #
      lit8 +3 i.exit                    # exit(3) },

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
  map_protocol,
  set_protocol,
  joinable_protocol,
  list_protocol)

  ->def(
    "nil?" => bin"                      # self cc
      const1 sset01 goto                # 1",

    length => bin"                      # self cc
      const0 sset01 goto                # 0",

    "contains?" => bin"                 # x self cc
      sset 01 drop const0 swap goto     # 0",

    "[]" => bin q{                      # i self cc
      "illegal .[] on nil" i.die        # boom },

    reduce => bin q{                    # x0 f self cc
      sset01 drop goto                  # x0 },

    "+" => bin"                         # rhs self cc
      swap drop goto                    # rhs",

    "key==_fn" => bin q{const0 sset01 goto},
    keys       => bin q{goto},
    kv_pairs   => bin q{goto},
    "{}"       => bin q{                # name self cc
      "illegal {} on nil" i.die         # boom });


# NB: this class doesn't implement set_protocol because we don't know what the
# key compare function should be. The only reason nil can implement it is that
# it always returns false.
use constant cons_class => phi::class->new('cons',
  cons_protocol,
  joinable_protocol,
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

    reduce => bin q{                    # x0 f self cc
      sget01 .head                      # x0 f self cc x
      sget04 sget04 call                # x0 f self cc x0' exit?
      [ sset03 sset01 drop goto ]       # x0'
      [ sset03                          # x0' f self cc
        swap .tail swap                 # x0' f tail cc
        sget01 m64get :reduce           # x0' f tail cc tail.reduce
        goto ]                          # tail-call
      if goto                           # r },

    length => bin"                      # self cc
      swap .tail .length                # cc self.tail.length
      const1 iplus swap goto            # self.tail.length+1");


use constant nil_instance => phi::allocation
  ->constant(pack Q => nil_class)
  ->named("nil_instance") >> heap;


sub list
{
  my $l = nil_instance >> heap;
  $l = phi::allocation->constant(pack QQQ => cons_class,
                                             pop >> heap,
                                             $l) >> heap
    while @_;
  $l;
}


use constant cons_fn => phi::allocation
  ->constant(bin"                       # t h cc
    const24 i.heap_allocate             # t h cc &cons
    \$cons_class sget 01 m64set         # t h cc &cons [.vt=]
    sget 02 sget 01 const8 iplus m64set # t h cc &cons [.h=]
    sget 03 sget 01 const16 iplus m64set# t h cc &cons [.t=]
    sset 02 swap                        # &cons cc h
    drop goto                           # &cons")

  ->named("cons_fn") >> heap;


BEGIN
{
  bin_macros->{'nil'} = bin '$nil_instance';
  bin_macros->{'::'}  = bin '$cons_fn call';
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
  joinable_protocol,
  set_protocol,
  mutable_list_protocol,
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

    reduce => bin q{                    # x0 f self cc
      swap .root_cons swap              # x0 f cons cc
      sget01 m64get :reduce goto        # tail-call },

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
  ->constant(bin q{                             # efn cc
    const24 i.heap_allocate                     # efn cc &list
    $linked_list_class sget 01 m64set           # efn cc &list [.vt=]
    sget 02 sget 01 const8  iplus m64set        # efn cc &list [.efn=]
    nil     sget 01 const16 iplus m64set        # efn cc &list [.root_cons=]
    sset 01 goto                                # &list })

  ->named('linked_list_fn') >> heap;


BEGIN
{
  bin_macros->{intlist} = bin '$intcmp_fn $linked_list_fn call';
  bin_macros->{strlist} = bin '$strcmp_fn $linked_list_fn call';
}


use constant rev_fn => phi::allocation
  ->constant(bin q{                     # xs t cc
    [                                   # xs t loop cc
      sget03 .nil?                      # xs t loop cc nil?
      [ sset02 drop swap goto ]         # t
      [ sget03 dup .tail                # xs t loop cc xs xt
        swap .head sget04               # xs t loop cc xt x t
        swap :: sset03 sset03           # xt x::t loop cc
        sget01 goto ]                   # ->loop
      if goto ]                         # xs t cc loop
    swap sget01 goto                    # ->loop })
  ->named('rev_fn') >> heap;


use constant sort_fn => phi::allocation
  ->constant(bin q{                     # xs cmp cc
    [ sget03 .length dup const0 ieq     # xs cmp recur cc l l==0?
      swap const1 ieq ior               # xs cmp r cc l==0|l==1?
      [ sset01 drop goto ]              # xs
      [ sget03 .head                    # xs cmp r cc pivot
        intlist intlist                 # xs cmp r cc p lt ge
        sget06 .tail                    # xs cmp r cc p lt ge xt
        [                               # xs cmp r cc p lt ge xs loop cc'
          sget02 .nil?                  # xs cmp r cc p lt ge xs l cc' nil?
          [ sset01 drop goto ]          # xs cmp r cc p lt ge
          [ sget02 dup .tail            # xs cmp r cc p lt ge xs l cc' xs xt
            sset03 .head dup            # xs cmp r cc p lt ge xt l cc' x x
            sget07 swap sget0b          # ... p lt ge xt l cc' x p x cmp
            call                        # ... p lt ge xt l cc' x x<p?
            sget06 sget06 if .<< drop   # ... p lt ge xt l cc' [<<x]
            sget01 goto ]               # ->loop
          if goto ]                     # xs cmp r cc p lt ge xs loop
        dup call                        # xs cmp r cc p lt ge

        .root_cons swap                 # ... gecons lt
        .root_cons swap                 # ... ltcons gecons

        sget05 sget05 dup call          # xs cmp r cc p lt sort(ge)
        sget02 ::                       # xs cmp r cc p lt p::sort(ge)
        swap sget05 sget05 dup call     # xs cmp r cc p p::sort(ge) sort(lt)
        .+                              # xs cmp r cc p sort(lt)+(p::sort(ge))
        sset04 drop sset01 drop goto ]  # sort(lt)+(p::sort(ge))
      if goto ]                         # xs cmp cc r
    swap sget01 goto                    # ->recur })
  ->named('sort_fn') >> heap;


=head3 Linked list unit tests
Nothing too comprehensive, just enough to make sure we aren't totally off the
mark.
=cut

use constant linked_list_test_fn => phi::allocation
  ->constant(bin q{                     # cc
    nil                                 # cc nil
    dup .length const0 ieq "ll len(0)" i.assert

    const2 ::                           # cc 2::nil
    dup .length const1 ieq "ll len(1)" i.assert

    const1 ::                           # cc 1::2::nil
    dup .length const2 ieq "ll len(2)" i.assert

    dup .+                              # cc 1::2::1::2::nil
    dup .length const4 ieq "ll len(4)" i.assert
    dup const0 swap .[] const1 ieq "ll[0] = 1" i.assert
    dup const1 swap .[] const2 ieq "ll[1] = 2" i.assert
    dup const2 swap .[] const1 ieq "ll[2] = 1" i.assert
    dup lit8+3 swap .[] const2 ieq "ll[3] = 2" i.assert

    dup .tail .length lit8+3 ieq "ll.tail len(3)" i.assert

    dup                                 # cc xs xs
    [                                   # r l cc
      sget02 sget02 ilt                 # r l cc l<r
      sset02 sset00 goto ]              # cc xs xs cmp
    $sort_fn call                       # cc xs sort(xs)

    dup .length const4 ieq "llS len(4)" i.assert
    dup const0 swap .[] const1 ieq "llS[0] = 1" i.assert
    dup const1 swap .[] const1 ieq "llS[1] = 1" i.assert
    dup const2 swap .[] const2 ieq "llS[2] = 2" i.assert
    dup lit8+3 swap .[] const2 ieq "llS[3] = 2" i.assert

    $nil_instance $rev_fn call          # cc xs rev(sort(xs))

    dup .length const4 ieq "rev len(4)" i.assert
    dup const0 swap .[] const2 ieq "rev[0] = 2" i.assert
    dup const1 swap .[] const2 ieq "rev[1] = 2" i.assert
    dup const2 swap .[] const1 ieq "rev[2] = 1" i.assert
    dup lit8+3 swap .[] const1 ieq "rev[3] = 1" i.assert

    drop                                # cc l

    dup                                 # cc l l
    const0 swap                         # cc l 0 l
    [                                   # x x0 cc
      sget02 sget02 iplus sset02        # x0' x0 cc
      const0 sset01                     # x0' 0 cc
      goto ] swap                       # cc l 0 [f] l
    .reduce lit8+6 ieq "sum = 6" i.assert     # cc l

    drop
    strlist                             # cc []
    "foo" swap .<<                      # cc ["foo"]
    dup "foo" swap .contains?      "contains(foo)"  i.assert
    dup "bar" swap .contains? inot "!contains(bar)" i.assert
    drop

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
  joinable_protocol,
  cons_protocol,
  kv_protocol,
  mutable_value_protocol,
  maybe_nil_protocol)

  ->def(
    key   => bin"swap const8  iplus m64get swap goto",
    value => bin"swap const16 iplus m64get swap goto",
    head  => bin"swap const8  iplus m64get swap goto",
    tail  => bin"swap const24 iplus m64get swap goto",

    'value=' => bin q{                  # v self cc
      sget02 sget02 const16 iplus       # v self cc v &value
      m64set sset01 swap goto           # self },

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

    reduce => bin q{                    # x0 f self cc
      sget01                            # x0 f self cc self
      sget04 sget04 call                # x0 f self cc x0' exit?
      [ sset03 sset01 drop goto ]       # x0'
      [ sset03                          # x0' f self cc
        swap .tail swap                 # x0' f tail cc
        sget01 m64get :reduce           # x0' f tail cc tail.reduce
        goto ]                          # tail-call
      if goto                           # r },

    length => bin"                      # self cc
      swap .tail .length                # cc self.tail.length
      const1 iplus swap goto            # self.tail.length+1");


use constant linked_map_class => phi::class->new('linked_map',
  map_protocol,
  set_protocol,
  list_protocol,
  mutable_map_protocol,
  mutable_set_protocol,
  linked_map_protocol)

  ->def(
    length => bin q{swap .keys .length swap goto},
    '[]'   => bin q{                    # i self cc
      sget02 sget02 .keys .[]           # i self cc keys[i]
      sset02 sset00 goto                # keys[i] },

    reduce => bin q{                    # x0 f self cc
      sget01 .keys sset01               # x0 f keys cc
      sget01 m64get :reduce goto        # ->keys.reduce },

    "key==_fn" => bin"                  # self cc
      swap const8 iplus m64get swap goto# fn",

    keys => bin"                        # self cc
      swap const16 iplus m64get         # cc self.alist
      swap goto                         # self.alist",

    kv_pairs => bin q{                  # self cc
      swap .keys swap goto              # keys },

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

    "<<" => bin q{                      # k self cc
      sget02 sget02 .contains?          # k self cc contains?
      [ sset01 swap goto ]              # self
      [ sget01 .length                  # k self cc len
        sget03 sget03 .{}=              # k self cc self [self{k}=len]
        drop sset01 swap goto ]         # self
      if goto                           # self },

    "{}" => bin"                        # k self cc
      sget 02 sget 02                   # k self cc k self
      .kvcell_for .value                # k self cc v
      sset 02 swap drop goto            # v",

    "{}=" => bin q{                     # v k self cc
      sget02 sget02 .kvcell_for         # v k self cc cell
      dup .nil?                         # v k self cc cell nil?

      [ drop                            # v k self cc
        const32 i.heap_allocate         # v k self cc &kv

        $kv_cons_class sget 01 m64set             # v k self cc &kv [.vt=]
        sget 03 sget 01 const8  iplus m64set      # [.k=]
        sget 04 sget 01 const16 iplus m64set      # [.v=]
        sget 02 const16 iplus m64get    # v k self cc &kv alist
        sget 01 const24 iplus m64set    # v k self cc &kv [.tail=]
        sget 02 const16 iplus m64set    # v k self cc [.alist=]

        sset 02 swap drop               # cc self
        swap goto ]                     # self

      [ # Existing cell: modify it in place
        sget04 swap .value= drop        # v k self cc
        sset01 sset01 goto ]            # self

      if goto                           # self});


sub kvmap
{
  my $cmp = shift;
  my $kvs = nil_instance;

  while (@_)
  {
    my $v = pop;
    my $k = pop;
    $kvs = phi::allocation->constant(pack QQQQ => kv_cons_class,
                                                  $k,
                                                  $v,
                                                  $kvs) >> heap;
  }

  phi::allocation->constant(pack QQQ => linked_map_class,
                                        $cmp,
                                        $kvs) >> heap;
}

sub int_kvmap { kvmap intcmp_fn, @_ }
sub str_kvmap { kvmap strcmp_fn, @_ }


use constant linked_map_fn => phi::allocation
  ->constant(bin q{                             # kfn cc
    const24 i.heap_allocate                     # kfn cc &map
    $linked_map_class sget 01 m64set            # kfn cc &map [.vt=]
    sget 02 sget 01 const8  iplus m64set        # kfn cc &map [.kfn=]
    nil     sget 01 const16 iplus m64set        # kfn cc &map [.alist=]
    sset 01 goto                                # &map })

  ->named('linked_map_fn') >> heap;


BEGIN
{
  bin_macros->{intmap} = bin '$intcmp_fn $linked_map_fn call';
  bin_macros->{strmap} = bin '$strcmp_fn $linked_map_fn call';
}


use constant linked_map_test_fn => phi::allocation
  ->constant(bin q{                     # cc
    intmap                              # cc {}

    dup .keys .length const0 ieq "keys len(0)" i.assert
    dup       .length const0 ieq "maplen(0)"   i.assert

    const2 swap const1 swap .{}=        # cc {1->2}
    dup .keys .length const1 ieq "keys len(1)"  i.assert
    dup .keys .head   const1 ieq "keys head(1)" i.assert
    dup .keys .value  const2 ieq "keys val(2)"  i.assert
    dup const1 swap .contains?      "contains(key 1)"  i.assert
    dup const2 swap .contains? inot "!contains(val 2)" i.assert

    dup .length const1 ieq "maplen(1)" i.assert

    const8 swap const4 swap .{}=        # cc {1->2, 4->8}
    dup .keys .length const2 ieq "keylen(2)" i.assert
    dup const4 swap .contains?      "contains(4)"  i.assert
    dup const8 swap .contains? inot "!contains(8)" i.assert
    dup .length const2 ieq "maplen(2)" i.assert

    dup const1 swap .{} const2 ieq "{1}=2" i.assert
    dup const4 swap .{} const8 ieq "{4}=8" i.assert

    # Assert key ordering since the map behaves like a list
    dup const0 swap .[] const4 ieq "[0]=4" i.assert
    dup const1 swap .[] const1 ieq "[1]=1" i.assert

    # Update an existing value and make sure we don't cons up a new entry
    const16 swap const4 swap .{}=       # cc {1->2, 4->16}

    dup .keys .length const2 ieq "keylen(2)" i.assert
    dup const4 swap .contains?      "contains(4)"  i.assert
    dup const8 swap .contains? inot "!contains(8)" i.assert
    dup .length const2 ieq "maplen(2)" i.assert

    dup const1 swap .{} const2  ieq "{1}=2"  i.assert
    dup const4 swap .{} const16 ieq "{4}=16" i.assert

    drop

    strmap                              # cc {}
    lit8 +55 swap "foo" swap .{}=       # cc {foo->55}
    lit8 +91 swap "bar" swap .{}=       # cc {foo->55, bar->91}

    dup "foo" swap .contains?      "contains(key foo)"  i.assert
    dup "bar" swap .contains?      "contains(key bar)"  i.assert
    dup "bif" swap .contains? inot "!contains(bif)"     i.assert
    dup "baz" swap .contains? inot "!contains(baz)"     i.assert

    dup "foo" swap .{} lit8+55 ieq "{foo}=55" i.assert
    dup "bar" swap .{} lit8+91 ieq "{bar}=91" i.assert

    dup "foo" swap .keys .head ieq inot "head!=foo" i.assert

    "bif" swap .<<                      # cc {foo->55, bar->91, bif->1}
    dup "bif" swap .contains? "contains(bif)" i.assert

    drop
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
  byte_string_protocol,
  string_buffer_protocol)

  ->def(
    size     => bin"swap const8  iplus m64get swap goto",
    capacity => bin"swap const16 iplus m64get swap goto",
    data     => bin"swap const24 iplus m64get swap goto",

    headroom => bin"                    # self cc
      sget 01 .capacity                 # self cc c
      sget 02 .size ineg iplus          # self cc c-s
      sset 01 goto                      # c-s",

    append_int8 => bin q{               # x self cc
      const1 sget03 sget03              # x self cc 1 x self
      .append_int drop sset01 swap goto # self },

    append_int16 => bin q{              # x self cc
      const2 sget03 sget03              # x self cc 2 x self
      .append_int drop sset01 swap goto # self },

    append_int32 => bin q{              # x self cc
      const4 sget03 sget03              # x self cc 4 x self
      .append_int drop sset01 swap goto # self },

    append_int64 => bin q{              # x self cc
      const8 sget03 sget03              # x self cc 8 x self
      .append_int drop sset01 swap goto # self },

    append_int => bin q{                # size_bytes x self cc
      # Chop the quad into two halves. We're appending it in native-endianness,
      # so we want 0x8877665544332211 to be appended as 11 22 33 44 55 66 77 88.
      #
      # Truncate the integer's length to the desired number of bytes.

      sget02 const32 ishr               # size q self cc q>>32
      sget03 const32 ishl               # size q self cc q>>32 q<<32
      sget05 ior                        # size q self cc q2 q1
      $byte_string_class                # size q self cc q2 q1 vt
      get_stackptr                      # size q self cc q2 q1 vt &s
      sget05 .append_string             # size q self cc q2 q1 vt self
      drop drop drop drop sset01        # size cc self
      sset01 goto                       # self },

    append_dec => bin q{                # n self cc
      # Emit a minus sign if the number is negative
      const0 sget03 ilt                 # n self cc n<0?
      [ lit8'- sget03 .append_int8 drop # n self cc cc'
        sget03 ineg sset03              # |n| self cc cc'
        goto ]                          # |n| self cc
      [ goto ]                          # |n| self cc
      if call                           # |n| self cc

      # If the number is zero, emit "0" and return directly
      sget02
      [ goto ]                          # n self cc
      [ lit8'0 sget03 .append_int8 drop # n self cc cc'
        drop sset01 swap goto ]         # self
      if call                           # n self cc

      # Search upwards to find the leading digit, then start subtracting powers
      # of ten.
      const1                            # n self cc p
      [ sget05 sget03 ilt               # n self cc p loop cc' p<n?
        [ sget02 lit8+10 itimes sset02  # n self cc p*10 loop cc'
          sget01 goto ]                 # ->loop
        [ sset00 goto ]                 # n self cc p
        if goto ]                       # n self cc p loop
      dup call                          # n self cc p'

      lit8+10 idivmod drop              # n self cc p'/10

      # Now we have a power of ten that is at least as large as the quantity.
      # Perform successive integer division until the base becomes 0, appending
      # a character each time.
      sget03                            # n self cc p' n'
      [                                 # n self cc p' n' loop
        sget02                          # n self cc p' n' loop p'?

        [ sget01 sget03 idivmod         # n self cc p' n' loop digit rem
          sset02                        # n self cc p' rem loop digit
          lit8'0 iplus                  # n self cc p' rem loop ascii
          sget05 .append_int8 drop      # n self cc p' rem loop

          # Divide p' by 10 to select the next digit
          sget02 lit8+10 idivmod drop   # n self cc p' rem loop p'/10
          sset02                        # n self cc p'/10 rem loop

          dup goto ]                    # ->loop

        [ drop drop drop                # n self cc
          sset01 swap goto ]            # self

        if goto ]                       # n self cc p' n' loop
      dup goto                          # self },

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

    rewind => bin q{                    # n self cc
      # Deallocate some bytes. This never causes the buffer's allocation to
      # change.
      sget01 .size sget03 ineg iplus    # n self cc size'
      sget02 const8 iplus m64set        # n self cc
      sset01 swap goto                  # self },

    to_string => bin q{                 # self cc
      sget 01 .size lit8 +12 iplus      # self cc ssize
      i.heap_allocate                   # self cc &s
      $byte_string_class sget 01 m64set           # self cc &s [.vt=]
      sget 02 .size sget 01 const8 iplus m32set   # [.size=]
      sget 02 .data sget 01 lit8 +12 iplus        # self cc &s from to
      sget 04 .size memcpy                        # self cc &s [copy]
      sset 01 goto                      # &s });


use constant string_buffer_fn => phi::allocation
  ->constant(bin q{                     # cc
    const32 i.heap_allocate             # cc &buf
    const32 i.heap_allocate             # cc &buf &data
    sget 01 const24 iplus m64set        # cc &buf [.data=]
    $string_buffer_class                # cc &buf vt
    sget 01 m64set                      # cc &buf [.vt=]
    const0 sget 01 const8 iplus m64set  # cc &buf [.size=]
    const32 sget 01 const16 iplus m64set# cc &buf [.capacity=]
    swap goto                           # &buf })
  ->named('string_buffer_fn') >> heap;


BEGIN
{
  bin_macros->{strbuf} = bin '$string_buffer_fn call';
}


use constant string_buffer_test_fn => phi::allocation
  ->constant(bin q{                     # cc
    strbuf                              # cc buf
    dup .to_string "" .== "empty tostring"   i.assert
    dup .size const0  ieq "size(0)"          i.assert
    dup .capacity const32 ieq "capacity(32)" i.assert

    "foo" swap .append_string           # cc buf
    dup .size lit8+3 ieq "size(3)"           i.assert
    dup .to_string "foo" .== "tostring(foo)" i.assert

    "bar" swap .append_string           # cc buf
    dup .size lit8+6 ieq "size(6)"                 i.assert
    dup .to_string "foobar" .== "tostring(foobar)" i.assert

    "foobar" swap .append_string                  # len=12
    "0123456789012345678" swap .append_string     # len=31
    "9" swap .append_string                       # len=32

    dup .size     const32 ieq "size(32)"     i.assert
    dup .capacity const32 ieq "capacity(32)" i.assert
    dup .to_string "foobarfoobar01234567890123456789" .== "tos(32)" i.assert

    lit8 'x swap .append_int8           # cc buf

    dup .size     lit8 +33 ieq "size(33)"     i.assert
    dup .capacity lit8 +64 ieq "capacity(64)" i.assert

    dup .to_string "foobarfoobar01234567890123456789x" .== "tos(33)" i.assert

    lit64 'abcdefgh swap .append_int64  # cc buf
    dup .size     lit8 +41 ieq "size(41)"     i.assert
    dup .capacity lit8 +64 ieq "capacity(64)" i.assert

    dup .to_string "foobarfoobar01234567890123456789xhgfedcba" .==
      "tos(41)" i.assert

    drop                                # cc

    # Decimal conversion
    lit8+137       strbuf .append_dec .to_string "137"     .== "dec1" i.assert
    lit8+0         strbuf .append_dec .to_string "0"       .== "dec2" i.assert
    lit8+137 ineg  strbuf .append_dec .to_string "-137"    .== "dec3" i.assert

    lit32 00100000 strbuf .append_dec .to_string "1048576" .== "dec4" i.assert

    goto                                # })

  ->named('string buffer test fn') >> heap;


=head2 Macro assembler
This is our first composite class:

  struct macro_assembler
  {
    hereptr           vtable;
    macro_assembler*  parent;
    linked_list<ref>* refs;
    string_buffer*    code;
  };

Note that this design is suboptimal; philosophically there's no reason to store
pointers to linked lists or string buffers since they're all fully owned values.
I'm indirecting here only to simplify the allocator and method calls.

Before I write the macro assembler, though, I first need a class for refs and
one for compiled code. Here they are:

  struct ref                            # 16 bytes
  {
    hereptr vtable;
    uint32  offset;
    uint32  pointer_type;
  };

  struct bytecode                       # size = 18 + 16*nrefs + codesize
  {
    hereptr        vtable;              # offset = 0
    uint32         nrefs;               # offset = 8
    uint32         codesize;            # offset = 12
    ref[nrefs]     refs;                # offset = 16
    here_marker;                        # offset = 16 + 16*nrefs
    byte[codesize] data;                # offset = 18 + 16*nrefs
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


# NB: as a list, bytecode is a series of refs; as a string-ish thing it's a
# bunch of code.
use constant bytecode_class => phi::class->new('bytecode',
  list_protocol,
  byte_string_protocol,
  fn_protocol,
  bytecode_protocol)

  ->def(
    here => bin"swap .data swap goto",
    size => bin"swap lit8+12 iplus m32get swap goto",
    data => bin"                        # self cc
      sget 01 .length const4 ishl       # self cc n<<4
      sget 02 iplus lit8+18 iplus       # self cc &data
      sset 01 goto                      # &data",

    # Living dangerously
    call        => bin"swap .here goto",
    goto        => bin"drop .here goto",
    call_native => bin"swap .here call_native",

    length => bin"swap const8 iplus m32get swap goto",
    "[]"   => bin"                      # i self cc
      sget 02 const4 ishl               # i self cc i<<4
      sget 02 const16 iplus iplus       # i self cc &refs[i]
      sset 02 sset 00 goto              # &refs[i]",

    reduce => bin q{                    # x0 f self cc
      swap dup .length const0           # x0 f cc self l i
      [ sget02 sget02 ilt               # x0 f cc self l i loop i<l?
        dup sget04 .[]                  # x0 f cc self l i loop self[i]
        sget07 sget07 call              # x0 f cc self l i loop x0' exit?
        [ sset06 drop drop drop drop    # x0' f cc
          sset00 swap goto ]            # x0'
        [ sset06 swap const1 iplus swap # x0' f cc self l i+1 loop
          dup goto ]                    # ->loop
        if goto ]                       # x0 f cc self l i loop
      dup goto                          # ->loop });


sub refless_bytecode($)
{
  phi::allocation->constant(
    pack 'QLLSa*' => bytecode_class,
                     0,
                     length $_[0],
                     18,
                     $_[0]) >> heap;
}


use constant macro_assembler_class => phi::class->new('macro_assembler',
  byte_string_protocol,
  macro_assembler_protocol,
  insn_proxy_protocol)

  ->def(
    map(($_ => bin"swap lit8 $_ swap .l8 swap goto"), sort keys %{+insns}),

    parent => bin"swap const8  iplus m64get swap goto",
    refs   => bin"swap const16 iplus m64get swap goto",
    code   => bin"swap const24 iplus m64get swap goto",

    data   => bin"swap .code .data swap goto",
    size   => bin"swap .code .size swap goto",

    child => bin"                       # self cc
      const32 i.heap_allocate           # self cc &child
      sget 02 m64get sget 01 m64set     # self cc &c [.vt=]
      sget 02 sget 01 const8 iplus m64set   # [.parent=]
      intlist sget 01 const16 iplus m64set  # [.refs=]
      strbuf  sget 01 const24 iplus m64set  # [.code=]
      sset 01 goto                          # &c",

    map(($_ => bin qq{                  # self cc
      lit8+$_ sget02 .l8 drop goto      # self }), qw/ 0 1 2 3 4 /),

    l8 => bin q{                        # byte self cc
      sget02 sget02 .code .append_int8  # byte self cc code
      drop sset01 swap goto             # self },

    l16 => bin q{                       # v self cc
      sget02 sget02 .code .append_int16 # v self cc code
      drop sset01 swap goto             # self },

    l32 => bin q{                       # v self cc
      sget02 sget02 .code .append_int32 # v self cc code
      drop sset01 swap goto             # self },

    l64 => bin q{                       # v self cc
      sget02 sget02 .code .append_int64 # v self cc code
      drop sset01 swap goto             # self },

    "ref<<" => bin q{                   # val type self cc
      # Appends a ref at the current insertion point.
      const16 i.heap_allocate           # val type self cc &r
      $ref_class sget 01 m64set         # val type self cc &r [.vt=]

      sget 02 .code .size sget 01 const8  iplus m32set  # [.offset=]
      sget 03             sget 01 lit8+12 iplus m32set  # [.type=]

      dup sget 03 .refs .<< drop        # val type self cc ref [.refs<<]
      const0 sget 03 .l64 drop          # val type self cc ref [.l64]
      sget 04 swap                      # val type self cc val ref
      sget 03 swap                      # val type self cc val self ref
      .set                              # val type self cc

      sset 01 sset 01 goto              # self },

    ptr => bin q{                       # &x self cc
      # Append code to push a base pointer onto the data stack. First we append
      # the lit64 byte, then create a ref to refer to the insertion point and
      # append the pointer value.
      #
      # Base pointers have type 0.

      lit8 lit64 sget 02 .l8 drop       # &x self cc [lit64 insn]
      sget 02 const0 sget 03 .ref<<     # &x self cc self
      sset 02 sset 00 goto              # self },

    hereptr => bin"                     # &x self cc
      # Append code to push a here-pointer onto the data stack. Identical to
      # ptr(), but we use a different pointer type.
      #
      # Here pointers have type 1.

      lit8 lit64 sget 02 .l8 drop       # &x self cc [lit64 insn]
      sget 02 const1 sget 03 .ref<<     # &x self cc self
      sset 02 sset 00 goto              # self",

    "[" => bin q{                       # self cc
      # Return a new linked buffer. The child will append a hereptr to its
      # compiled self and return this parent when any of its close-bracket
      # methods are invoked.
      swap .child swap goto             # child },

    "]" => bin"                         # self cc
      # Heap-allocate the child buffer and link a here-pointer.
      sget 01 .parent                   # self cc parent
      sget 02 .compile .here            # self cc parent codeptr
      swap .hereptr                     # self cc parent [.hereptr]
      sset 01 goto                      # parent",

    compile => bin q{                   # self cc
      sget 01 .refs .length             # self cc nrefs
      sget 02 .code .size               # self cc n size

      sget 01 const4 ishl sget 01 iplus # self cc n s netsize
      lit8+18 iplus i.heap_allocate     # self cc n s &o

      $bytecode_class sget 01 m64set          # self cc n s &o [.vt=]
      sget 02 sget 01 const8  iplus m32set    # [.nrefs=]
      sget 01 sget 01 lit8+12 iplus m32set    # [.codesize=]

      sget 04 .data                     # self cc n s &o &data
      sget 03 const4 ishl lit8+18 iplus # self cc n s &o &data off(data)
      dup sget 03 iplus                 # self cc n s &o &data od &o.data
      swap sget 01                      # self cc n s &o &data &o.d offd &o.d
      const2 ineg iplus m16set          # self cc n s &o &data &o.data [.here=]
      sget 03 memcpy                    # self cc n s &o [.data=]

      sset 01 drop                      # self cc &o

      # Now copy the refs into place.
      [                                 # self cc &o loop &or rl
        dup .nil?                       # self cc &o loop &or rl rnil?
        [ drop drop drop sset 01 goto ] # &o
        [ dup .head sget 02             # self cc &o loop &or rl r &or
          const16 memcpy                # self cc &o loop &or rl [o.r[i]=]
          .tail swap const16 iplus swap # self cc &o loop &or' rl'
          sget 02 goto ]                # tail-recursive loop
        if goto
      ]                                 # self cc &o loop
      sget 01 const16 iplus             # self cc &o loop &o.refs[0]
      sget 04 .refs .root_cons          # self cc &o loop &or reflist
      sget 02 goto                      # &o });


use constant macro_assembler_fn => phi::allocation
  ->constant(bin q{                     # cc
    $macro_assembler_class              # cc vt
    get_stackptr .child                 # cc vt child
    const0 sget 01 const8 iplus m64set  # cc vt child [.parent=0]
    sset 00 swap goto                   # child })
  ->named('macro_assembler_fn') >> heap;


BEGIN
{
  bin_macros->{asm} = bin '$macro_assembler_fn call';
}


use constant macro_assembler_test_fn => phi::allocation
  ->constant(bin q{                     # cc
    asm                                 # cc asm
      .swap
      .lit8
      .4
      .iplus
      .swap
      .goto
    .compile                            # cc fn
    dup .length const0 ieq "masm0"     i.assert
    dup .size   lit8+6 ieq "masmsize6" i.assert

    lit8 +31 swap                       # cc 31 fn
    .call                               # cc 35
    lit8 +35 ieq "masmc35" i.assert     # cc

    asm                                 # cc asm
      lit64 'abcdefgh swap .ptr         # cc asm[lit64 'hgfedcba]
      .swap
      .goto
    .compile                            # cc fn

    dup .length const1  ieq "masm1"      i.assert
    dup .size   lit8+11 ieq "masmsize11" i.assert
    dup const0 swap .[]                 # cc fn r[0]
        sget 01 swap .get               # cc fn 'abcdefgh
        lit64 'abcdefgh ieq "masmlit64" i.assert    # cc fn

    dup .here                           # cc fn fnhere
        dup const2 ineg iplus           # cc fn fnhere &hm
        m16get ineg iplus               # cc fn fn
        sget 01 ieq "masmhere" i.assert # cc fn

    .call                               # cc 'hgfedcba
    lit64 'abcdefgh ieq "masmcall2" i.assert    # cc

    # Last one. Assemble bracket stuff.
    asm                                 # cc asm[|]
    .const1                             # cc asm[1|]
    .[                                  # cc asm[1 [|]]
      .const32                          # cc asm[1 [32|]]
      .iplus
      .swap
      .goto
    .]                                  # cc asm[1 [32 + swap goto]|]
    .goto                               # cc asm[1 [32 + swap goto] goto|]
    .compile .call                      # cc 33

    lit8+33 ieq "masmcall3" i.assert

    goto                                # })

  ->named('macro_assembler_test_fn') >> heap;


1;
