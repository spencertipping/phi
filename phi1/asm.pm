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


=head2 Macro assembler
An extended i8 indirect array with extra methods. We store the parent pointer at
offset 40:

  struct macro_assembler
  {
    i8_indirect_array self;             # 40 bytes
    macro_assembler*  parent;           # offset=40
  };

=cut

use phi::protocol symbolic_method =>
  qw/ symbolic_method /;

use phi::protocol macro_assembler =>
  qw/ parent
      parent=
      child /,

  # Assembler macros (plus some shorthands for numbers)
  # NB: pnl is a macro to simplify debugging. It inserts a literal string
  # followed by a call to i.pnl to print stuff.
  qw/ 0
      1
      2
      3
      4
      l8
      l16
      l32
      l64
      const64
      ptr
      hereptr
      dup
      pnl
      debug_trace
      [
      ]
      add_child_link
      compile /;

use phi::protocol insn_proxy =>
  sort keys %{+insns};


use phi::class macro_assembler =>
  i8_indirect_array_class->protocols,
  macro_assembler_protocol,
  symbolic_method_protocol,
  insn_proxy_protocol,

  i8_indirect_array_class->methods_except(qw/ clone /),

  compile => bin q{sget01 m64get :to_direct goto},
  parent  => bin q{_=40 iplus m64get_ goto},

  'parent=' => bin q{                 # p' self cc
    sget02 sget02 =40 iplus m64set    # p' self cc
    sset01 _ goto                     # self },

  child => bin q{                     # self cc
    i8i =8 i.heap_allocate drop       # self cc new
    sget02 m64get sget01 m64set       # self cc new [new.class=self.class]
    sget02 sget01 =40 iplus m64set    # self cc new [new.parent=self]

    # Coding horror: i8i inline-allocates the buffer into the region where we
    # want to store *parent, so we need to bump it by eight. Because that's how
    # we do things around here.
    dup =24 iplus dup m64get =8 iplus _ m64set    # new.ks += 8
    sset01 goto                       # new },

  clone => bin q{                     # self cc
    =48 i.heap_allocate               # self cc new
    sget02 sget01 =48 memcpy          # self cc new [new=self]
    =0 sget01 =16 iplus m64set        # self cc new [new.n=new.cap=0]
    sget02 _ .+=                      # self cc new [new+=self]
    sset01 goto                       # new },

  map(($_ => bin"swap lit8 $_ swap .l8 swap goto"),
      grep !/^s[gs]et$/, sort keys %{+insns}),

  # NB: sget/sset are special in that they take the entry index as an
  # argument. This saves the literal byte emit and makes it easier to detect
  # overflows (which we don't do because that would be too easy).
  sget => bin q{                      # i self cc
    lit8 sget sget02 .l8 drop         # i self cc <<sget
    sget02    sget02 .l8 drop         # i self cc <<i
    sset01 swap goto                  # self },

  sset => bin q{                      # i self cc
    lit8 sset sget02 .l8 drop         # i self cc <<sset
    sget02    sget02 .l8 drop         # i self cc <<i
    sset01 swap goto                  # self },

  map(($_ => bin qq{                  # self cc
    lit8+$_ sget02 .l8 drop goto      # self }), qw/ 0 1 2 3 4 /),

  l8 => bin q{sget01 m64get :<< goto},

  l16 => bin q{                       # v self cc
    sget02         sget02 .<< drop    # v self cc
    sget02 =8 ishr sget02 .<< drop    # v self cc
    sset01 _ goto                     # self },

  l32 => bin q{                       # v self cc
    sget02          sget02 .<< drop
    sget02 =8  ishr sget02 .<< drop
    sget02 =16 ishr sget02 .<< drop
    sget02 =24 ishr sget02 .<< drop
    sset01 _ goto                     # self },

  l64 => bin q{                       # v self cc
    sget02          sget02 .<< drop
    sget02 =8  ishr sget02 .<< drop
    sget02 =16 ishr sget02 .<< drop
    sget02 =24 ishr sget02 .<< drop
    sget02 =32 ishr sget02 .<< drop
    sget02 =40 ishr sget02 .<< drop
    sget02 =48 ishr sget02 .<< drop
    sget02 =56 ishr sget02 .<< drop
    sset01 _ goto                     # self },

  dup => bin q{                       # self cc
    =0 sget02 .sget drop goto         # self },

  ptr     => bin q{sget01 m64get :const64 goto},
  hereptr => bin q{sget01 m64get :const64 goto},
  const64 => bin q{                   # x self cc
    _ .lit64 sget02 bswap64 _ .l64    # x cc self
    sset01 goto                       # self },

  symbolic_method => bin q{           # method self cc
    swap                              # method cc self[]
      .dup .m64get                    # method cc self[dup m64get]
      sget02 method_hash _ .const64   # method cc self[dup m64get =h]
      .swap .call .call               # method cc self[...]
    sset01 goto                       # self },

  pnl => bin q{                       # s self cc
    sget02 sget02                     # s self cc s self
      .ptr                            # s self cc self [s]
      .get_interpptr                  # s self cc self [s i]
      .'pnl
    sset02 sset00 goto                # self },

  debug_trace => bin q{               # self cc
    _ [ swap debug_trace swap goto ] _
    .hereptr .call
    _ goto                            # self },

  add_child_link => bin q{            # child self cc
    sget02 .compile .data             # child self cc fn
    sget02 .hereptr                   # child self cc self
    drop sset01 swap goto             # self },

  "[" => bin q{                       # self cc
    # Return a new linked buffer. The child will append a hereptr to its
    # compiled self and return this parent when any of its close-bracket
    # methods are invoked.
    _ .child _ goto                   # child },

  "]" => bin q{                       # self cc
    _ dup .parent .add_child_link     # cc parent
    _ goto                            # parent };


use phi::fn asm => bin q{               # cc
  i8i =8 i.heap_allocate drop           # cc asm
  $macro_assembler_class sget01 m64set  # cc asm [.class=]
  =0 sget01 =40 iplus m64set            # cc asm [.parent=0]

  # Coding horror: i8i inline-allocates the buffer into the region where we
  # want to store *parent, so we need to bump it by eight. Because that's how
  # we do things around here.
  dup =24 iplus dup m64get =8 iplus _ m64set    # new.ks += 8

  swap goto                             # asm };


=head2 Bytecode debugging
It's useful to be able to print the source for a bytecode as a string. This
involves parsing stack instructions, but that isn't too difficult. We just need
to know how many literal bytes follow each one.
=cut

use constant insn_follow_bytes => {
  lit8  => 1,
  lit16 => 2,
  lit32 => 4,
  lit64 => 8,

  framerel => 2,

  sget => 1,
  sset => 1,

  map +($_ => 0),
      qw/ call call_native if syscall

          set_frameptr
          get_interpptr set_interpptr
          get_stackptr  set_stackptr
          get_insnptr   goto

          drop swap

          m8get  m8set
          m16get m16set
          m32get m32set
          m64get m64set
          memset memcpy

          iplus itimes  idivmod ishl
          isar  ishr    iand    ior
          ixor  ilt     ieq     iinv
          ineg  bswap16 bswap32 bswap64 / };

use phi::genconst phi_insn_follow_bytes => bin
  join"\n",
    q{ =256 i8d                         # xs },
    map(sprintf(q{ dup =0_  lit8 %02x_ .[]= drop }, $_), 0..255),
    map(sprintf(q{ dup =%d_ lit8 %02x_ .[]= drop },
                insn_follow_bytes->{$_},
                insn_index $_),
        sort keys %{+insns});


use phi::genconst phi_insn_names => bin
  join"\n",
    q{ =256 i64d                        # m },
    map(sprintf(q{ dup "0x%02x"_ lit8 %02x_ .[]= drop }, $_, $_), 0..255),
    map(sprintf(q{ dup "%s"_     lit8 %02x_ .[]= drop }, $_, insn_index $_),
        sort keys %{+insns});


use phi::fn bytecode_to_hex => bin q{   # bytecode cc
  strbuf sget02                         # b cc s b
  [ sget02 =1 sget03 .<<hex =32_ .<< sset02
    =0 sset01 goto ]
  _.reduce                              # b cc s
  .to_string sset01 goto                # s };


use phi::fn bytecode_to_string => bin q{# bytecode cc
  strbuf                                # b cc s
  sget02 .size                          # b cc s n
  sget03 .data                          # b cc s n d
  [                                     # b cc s n d i loop
    sget03 sget02 ilt                   # b cc s n d i loop i<n?
    [ # Prepend the absolute hex address of each instruction. This is just d+i
      # as a number.
      sget02 sget02 iplus               # b cc s n d i loop &insn
      dup sget06 =8_ .<<hex             # b cc s n d i loop &insn s
      ": "_ .+=                         # b cc s n d i loop &insn s
      drop m8get                        # b cc s n d i loop insn

      # Retrieve the instruction's name and append that to the buffer.
      dup phi_insn_names .[]            # b cc s n d i loop insn iname
      sget06 .+=                        # b cc s n d i loop insn s
      " "_ .+=                          # b cc s n d i loop insn s

      sget03 =1 iplus sset03            # b cc s n d i+1 loop insn s

      # Now add following bytes if we have them. We can cheat a little here and
      # read 64 bits little-endian, then print only as many as we care about.
      # This is totally awful and prints in the wrong endianness, but it works.
      swap phi_insn_follow_bytes .[]    # b cc s n d i+1 loop s fbs
      sget04 sget04 iplus m64get        # b cc s n d i+1 loop s fbs data
      sget01 sget03 .<<hex drop         # b cc s n d i+1 loop s fbs

      sget03 iplus sset02               # b cc s n d i' loop s
      $nl_string _ .+= drop             # b cc s n d i' loop
      dup goto ]                        # ->loop(i')
    [ drop drop drop drop               # b cc s
      .to_string sset01 goto ]          # s.to_string
    if goto ]                           # b cc s n d loop
  =0_ dup goto                          # ->loop(0) };


=head2 ASM unit tests
Make sure the functions we produce behave as we expect, including their
here-pointer-ness.
=cut

use phi::testfn asm => bin q{           #
  asm                                 # asm
    .swap
    .lit8
    .4
    .iplus
    .swap
    .goto
  .compile                            # fn
  dup .size =6 ieq "masmsize6" i.assert

  lit8 +31 swap                       # 31 fn
  .data call                          # 35
  lit8 +35 ieq "masmc35" i.assert     #

  asm                                 # asm
    lit64 'abcdefgh _.const64         # asm[lit64 'hgfedcba]
    .swap
    .goto
  .compile                            # fn

  dup .size =11 ieq "masmsize11" i.assert
  dup .data unhere sget01 ieq "masmhere" i.assert  # fn

  .data call                          # 'hgfedcba
  lit64 'abcdefgh ieq "masmcall2" i.assert    #

  # Assemble some bracket stuff.
  asm                                 # asm[|]
  .lit8 .1                            # asm[1|]
  .[                                  # asm[1 [|]]
    .lit8 =32 _.l8                    # asm[1 [32|]]
    .iplus
    .swap
    .goto
  .]                                  # asm[1 [32 + swap goto]|]
  .goto                               # asm[1 [32 + swap goto] goto|]
  .compile .data call                 # 33

  lit8+33 ieq "masmcall3" i.assert

  # Now call back into a function defined using bin brackets.
  asm                                 # asm [cc]
    .lit8 .4                          # asm [cc 4]
    [ swap =1 iplus swap goto ]       # asm inc [cc 4]
    swap .hereptr                     # asm [cc 4 inc]
    .call                             # asm [cc 5]
    .swap
    .goto                             # asm [5]
  .compile .data call                 # 5

  lit8+5 ieq "masmfncall5" i.assert

  # Test inlining. We should be able to split an assembler in half and get
  # exactly the same result.
  =17
  asm
    .swap .ptr .iplus                 # asm1[swap =17 +]
    =34 asm .ptr .iplus               # asm1 asm2[=34 +]
        .compile swap
    .+=                               # asm[swap =17 + =34 +]
    .swap .goto                       # asm[...]
  .compile                            # code

  =8 sget01 .data call =59 ieq "59" i.assert

  drop                                # };


1;
