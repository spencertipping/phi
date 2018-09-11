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

use phi::class macro_assembler =>
  i8_indirect_array_class->protocols,
  macro_assembler_protocol,
  symbolic_method_protocol,
  insn_proxy_protocol,

  i8_indirect_array_class->methods_except(qw/ clone /),

  compile => bin q{sget01 m64get :to_direct goto},
  parent  => bin q{_=40 iplus m64get_ goto},

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


use phi::testfn asm =>
  bin q{                                #
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
