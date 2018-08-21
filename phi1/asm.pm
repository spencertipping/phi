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


=head2 Macro assembler
This is our first composite class:

  struct macro_assembler
  {
    hereptr            vtable;
    macro_assembler*   parent;
    linked_list<ref*>* refs;
    string_buffer*     code;
  };

Note that this design is suboptimal; philosophically there's no reason to store
pointers to linked lists or string buffers since they're all fully owned values.
I'm indirecting here only to simplify the allocator and method calls.
=cut


use phi::class macro_assembler =>
  clone_protocol,
  byte_string_protocol,
  macro_assembler_protocol,
  symbolic_method_protocol,
  insn_proxy_protocol,

  parent => bin"swap =8  iplus m64get swap goto",
  refs   => bin"swap =16 iplus m64get swap goto",
  code   => bin"swap =24 iplus m64get swap goto",

  data   => bin"swap .code .data swap goto",
  size   => bin"swap .code .size swap goto",

  clone => bin q{                     # self cc
    =32     i.heap_allocate           # self cc &asm
    sget02 m64get sget01 m64set       # self cc &asm [.vt=]

    # .parent is a nullable pointer, so clone only if it's nonzero.
    sget02 .parent dup
    [ swap .clone swap goto ]
    [ goto ]
    if call                           # self cc &asm p'

    sget01 =8 iplus m64set            # self cc &asm [.parent=]

    sget02 .refs .clone sget01 =16 iplus m64set   # [.refs=]
    sget02 .code .clone sget01 =24 iplus m64set   # [.code=]
    sset01 goto                       # &asm },

  # We need to be able to inline a bytecode object directly into this
  # assembler output. This is simple enough but we'll also need to copy the
  # refs, modifying each one to refer to the new offset.
  inline => bin q{                    # code self cc
    swap dup .size                    # code cc self delta
    sget03 =0                         # code cc self delta rl i
    [ sget03 .length sget03 ilt       # code cc self delta rl i loop cc i<n?
      [ =16 i.heap_allocate           # c cc s d rl i loop cc &r
        sget03 sget05 .[]             # c cc s d rl i loop cc &r &r0
        sget01 =16 memcpy             # c cc s d rl i loop cc &r

        dup =8 iplus m32get           # c cc s d rl i loop cc &r offset
        sget06 iplus                  # c cc s d rl i loop cc &r offset'
        sget01 =8 iplus m32set        # c cc s d rl i loop cc &r

        sget06 .refs .<< drop         # c cc s d rl i loop cc
        sget02 =1 iplus sset02        # c cc s d rl i+1 loop cc
        sget01 goto ]                 # ->loop

      [ sset03 drop drop drop goto ]  # c cc self

    if goto ]                         # code cc self delta rl i loop

    dup call                          # code cc self

    sget02 sget01 .code .append_string # code cc self self.code
    drop sset01 goto                  # self },

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

  child => bin"                       # self cc
    =32     i.heap_allocate           # self cc &child
    sget 02 m64get sget 01 m64set     # self cc &c [.vt=]
    sget 02 sget 01 =8  iplus m64set  # [.parent=]
    intlist sget 01 =16 iplus m64set  # [.refs=]
    strbuf  sget 01 =24 iplus m64set  # [.code=]
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

  dup => bin q{                       # self cc
    =0     sget02 .sget drop goto     # self },

  "ref<<" => bin q{                   # val type self cc
    # Appends a ref at the current insertion point.
    =16     i.heap_allocate           # val type self cc &r
    $ref_class sget 01 m64set         # val type self cc &r [.vt=]

    sget 02 .code .size sget 01 =8      iplus m32set  # [.offset=]
    sget 03             sget 01 lit8+12 iplus m32set  # [.type=]

    dup sget 03 .refs .<< drop        # val type self cc ref [.refs<<]
    =0     sget 03 .l64 drop          # val type self cc ref [.l64]
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
    sget 02 =0     sget 03 .ref<<     # &x self cc self
    sset 02 sset 00 goto              # self },

  hereptr => bin"                     # &x self cc
    # Append code to push a here-pointer onto the data stack. Identical to
    # ptr(), but we use a different pointer type.
    #
    # Here pointers have type 1.

    lit8 lit64 sget 02 .l8 drop       # &x self cc [lit64 insn]
    sget 02 =1     sget 03 .ref<<     # &x self cc self
    sset 02 sset 00 goto              # self",

  symbolic_method => bin q{           # method self cc
    swap                              # method cc self
      .dup .m64get
      .lit64
        sget02 method_hash
               bswap64 swap .l64      # method cc self
      .swap .call .call               # method cc self
    sset01 goto                       # self },

  pnl => bin q{                       # s self cc
    sget02 sget02                     # s self cc s self
      .ptr                            # s self cc self [s]
      .get_interpptr                  # s self cc self [s i]
      .'pnl
    sset02 sset00 goto                # self },

  debug_trace => bin q{               # self cc
    swap
    [ swap debug_trace swap goto ]
    swap .hereptr
    .call
    swap goto                         # self },

  add_child_link => bin q{            # child self cc
    sget02 .compile .here             # child self cc fn
    sget02 .hereptr                   # child self cc self
    drop sset01 swap goto             # self },

  "[" => bin q{                       # self cc
    # Return a new linked buffer. The child will append a hereptr to its
    # compiled self and return this parent when any of its close-bracket
    # methods are invoked.
    swap .child swap goto             # child },

  "]" => bin q{                       # self cc
    swap dup .parent                  # cc self parent
    .add_child_link                   # cc parent
    swap goto                         # parent },

  compile => bin q{                   # self cc
    sget 01 .refs .length             # self cc nrefs
    sget 02 .code .size               # self cc n size

    sget 01 =4     ishl sget 01 iplus # self cc n s netsize
    lit8+18 iplus i.heap_allocate     # self cc n s &o

    $bytecode_class sget 01 m64set          # self cc n s &o [.vt=]
    sget 02 sget 01 =8      iplus m32set    # [.nrefs=]
    sget 01 sget 01 lit8+12 iplus m32set    # [.codesize=]

    sget 04 .data                     # self cc n s &o &data
    sget 03 =4     ishl lit8+18 iplus # self cc n s &o &data off(data)
    dup sget 03 iplus                 # self cc n s &o &data od &o.data
    swap sget 01                      # self cc n s &o &data &o.d offd &o.d
    =2     ineg iplus m16set          # self cc n s &o &data &o.data [.here=]
    sget 03 memcpy                    # self cc n s &o [.data=]

    sset 01 drop                      # self cc &o

    # Now copy the refs into place.
    [                                 # self cc &o loop &or rl
      dup .nil?                       # self cc &o loop &or rl rnil?
      [ drop drop drop sset 01 goto ] # &o
      [ dup .head sget 02             # self cc &o loop &or rl r &or
        =16     memcpy                # self cc &o loop &or rl [o.r[i]=]
        .tail swap =16     iplus swap # self cc &o loop &or' rl'
        sget 02 goto ]                # tail-recursive loop
      if goto
    ]                                 # self cc &o loop
    sget 01 =16     iplus             # self cc &o loop &o.refs[0]
    sget 04 .refs .root_cons          # self cc &o loop &or reflist
    sget 02 goto                      # &o };


use phi::fn asm => bin q{               # cc
  $macro_assembler_class                # cc vt
  get_stackptr .child                   # cc vt child
  =0     sget01 =8     iplus m64set     # cc vt child [.parent=0]
  sset00 swap goto                      # child };


use phi::fn macro_assembler_test =>
  bin q{                                # cc
    asm                                 # cc asm
      .swap
      .lit8
      .4
      .iplus
      .swap
      .goto
    .compile                            # cc fn
    dup .length =0     ieq "masm0"     i.assert
    dup .size   lit8+6 ieq "masmsize6" i.assert

    lit8 +31 swap                       # cc 31 fn
    .call                               # cc 35
    lit8 +35 ieq "masmc35" i.assert     # cc

    asm                                 # cc asm
      lit64 'abcdefgh swap .ptr         # cc asm[lit64 'hgfedcba]
      .swap
      .goto
    .compile                            # cc fn

    dup .length =1      ieq "masm1"      i.assert
    dup .size   lit8+11 ieq "masmsize11" i.assert
    dup =0 swap .[]                     # cc fn r[0]
        sget 01 swap .get               # cc fn 'abcdefgh
        lit64 'abcdefgh ieq "masmlit64" i.assert    # cc fn

    dup .here                           # cc fn fnhere
        dup =2 ineg iplus               # cc fn fnhere &hm
        m16get ineg iplus               # cc fn fn
        sget 01 ieq "masmhere" i.assert # cc fn

    .call                               # cc 'hgfedcba
    lit64 'abcdefgh ieq "masmcall2" i.assert    # cc

    # Assemble some bracket stuff.
    asm                                 # cc asm[|]
    .lit8 .1                            # cc asm[1|]
    .[                                  # cc asm[1 [|]]
      .lit8 =32 swap .l8                # cc asm[1 [32|]]
      .iplus
      .swap
      .goto
    .]                                  # cc asm[1 [32 + swap goto]|]
    .goto                               # cc asm[1 [32 + swap goto] goto|]
    .compile .call                      # cc 33

    lit8+33 ieq "masmcall3" i.assert

    # Now call back into a function defined using bin brackets.
    asm                                 # cc asm [cc]
      .lit8 .4                          # cc asm [cc 4]
      [ swap =1 iplus swap goto ]       # cc asm inc [cc 4]
      swap .hereptr                     # cc asm [cc 4 inc]
      .call                             # cc asm [cc 5]
      .swap
      .goto                             # cc asm [5]
    .compile .call                      # cc 5

    lit8+5 ieq "masmfncall5" i.assert

    # Test inlining. We should be able to split an assembler in half and get
    # exactly the same result.
    =17
    asm
      .swap .ptr .iplus                 # cc asm1[swap =17 +]
      =34 asm .ptr .iplus               # cc asm1 asm2[=34 +]
          .compile swap
      .inline                           # cc asm[swap =17 + =34 +]
      .swap .goto                       # cc asm[...]
    .compile                            # cc code

    # Sanity checks
    dup .length =2 ieq "len2" i.assert
    =0 sget01 .[]                       # cc code ref
      sget01 swap .get =34 ieq "[0]34" i.assert
    =1 sget01 .[]                       # cc code ref
      sget01 swap .get =17 ieq "[1]17" i.assert
    =8 sget01 .call =59 ieq "59" i.assert

    # Now modify the references in place to make sure the offsets are correct.
    # I'm using small numbers every byte of which will be an illegal instruction
    # if it gets dropped into the wrong location.
    =5 =0 sget02 .[] sget02 swap .set   # cc code
    =9 =1 sget02 .[] sget02 swap .set   # cc code

    =8 sget01 .call =22 ieq "22" i.assert

    drop                                # cc
    goto                                # };


1;
