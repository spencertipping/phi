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


=head2 Typed assembler
A typed assembler behaves identically to a regular macro assembler with two
exceptions:

1. It introduces a new pseudo-instruction called C<typed>
2. It accepts symbolic methods and forwards those to values on a simulated stack

(1) is used by compilers to indicate the compile-time class of any given value
on the stack. Any unknown values will have type C<unknown>, which gives you no
method call support -- i.e. there is no specified calling convention for
interacting with an C<unknown> value.

(2) is how you interact with values on the stack. You can use the normal
primitive bytecode instructions (and C<typed>), but it's more common to delegate
to the CTTI classes by invoking their methods and allowing them to compile
specialized code.

This is also where we get protocols directly involved in compilation. Protocols
own the type transformation signatures of their classes (as per the monomorphism
guarantee above), so a polymorphic base pointer addressing a protocol-allocated
vtable map would both check the input argument types and add a C<typed>
instruction to any result values.


=head3 Retroactive specialization
We don't implement this here (it's in C<phi2> instead), but this is the
mechanism that uses compile-time return type coercion to reduce the footprint of
intermediate allocated data.


=head3 CTTI state space
A typed assembler needs to store the following:

1. A stack of value classes
2. The class of the current frame value
3. An assembler to compile into

Here's the struct:

  struct typed_assembler
  {
    hereptr              vtable;
    typed_assembler     *parent;        # offset = 8
    linked_list<class*> *stack_classes; # offset = 16
    class               *frame_class;   # offset = 24
    macro_assembler     *asm;           # offset = 32
  }

=cut


use constant empty_list => list;

use constant unknown_class => phi::class->new('unknown',
  dialect_negotiation_protocol,
  symbolic_method_protocol)

  ->def(
    dialect_channels => bin q{          # self cc
      $empty_list sset01 goto           # [] },

    inspect_channel => bin q{           # c self cc
      "unknown CTTI provides no dialect channels to inspect" i.die },

    symbolic_method => bin q{           # m self cc
      sget02 "invoked method ." .+
      " on an unknown value" swap .+
      i.die                             # fail });

use constant unknown_value => phi::allocation
  ->constant(pack Q => unknown_class)
  ->named('unknown_value') >> heap;


use constant typed_assembler_class => phi::class->new('typed_assembler',
  clone_protocol,
  symbolic_method_protocol,
  macro_assembler_protocol,
  typed_macro_assembler_protocol,
  insn_proxy_protocol)

  ->def(
    clone => bin q{                     # self cc
      lit8+40 i.heap_allocate           # self cc &asm
      sget02 m64get sget01 m64set       # [.vt=]
      sget02 .parent dup
      [ swap .clone swap goto ]
      [ goto ]
      if call                           # self cc &asm p'
      sget01 =8     iplus m64set        # self cc &asm [.parent=]

      sget02 .stack .clone sget01 =16     iplus m64set    # [.stack=]
      sget02 .frame        sget01 =24     iplus m64set    # [.frame=]
      sget02 .asm .clone   sget01 =32     iplus m64set    # [.asm=]

      sset01 goto                       # &asm },

    # Typed assembler protocol
    stack => bin q{swap =16     iplus m64get swap goto},
    frame => bin q{swap =24     iplus m64get swap goto},
    asm   => bin q{swap =32     iplus m64get swap goto},

    'stack=' => bin q{                  # s' self cc
      sget02 sget02 =16     iplus m64set# s' self cc [stack=]
      sset01 swap goto                  # self },

    typed => bin q{                     # t self cc
      # Set the type of the top stack entry.
      sget02 sget02 .stack =0           # t self cc t stack 0
      swap .[]=                         # t self cc stack
      drop sset01 swap goto             # self },

    push => bin q{                      # t self cc
      sget02 sget02 .stack .<< drop     # t self cc
      sset01 swap goto                  # self },

    pop => bin q{                       # self cc
      swap dup .stack .shift swap       # cc self v
      sget02 swap sset02 goto           # v self },

    # Macro assembler protocol
    parent => bin q{swap =8     iplus m64get swap goto},
    child  => bin q{                    # self cc
      # Start with an empty stack, an unknown frame pointer, and the child of
      # the current assembler.
      lit8+40 i.heap_allocate           # self cc child
      sget02 m64get  sget01               m64set    # [.vt=]
      sget02         sget01 =8      iplus m64set    # [.parent=]
      intlist        sget01 =16     iplus m64set    # [.stack=]
      $unknown_value sget01 =24     iplus m64set    # [.frame=]
      sget02 .asm .child
                     sget01 =32     iplus m64set    # [.asm=]

      sset01 goto                       # child },

    refs    => bin q{ swap .asm .refs    swap goto },
    code    => bin q{ swap .asm .code    swap goto },
    compile => bin q{ swap .asm .compile swap goto },

    '[' => bin q{ swap .child swap goto },
    ']' => bin q{                       # self cc
      sget01 .asm .]                    # self cc asm'
      drop swap .parent                 # cc self'

      # Append an unknown ref for the pointer pushed by the close-bracket. We
      # know it's a here-pointer to a function, but we don't know the function's
      # type yet so we can't do much with it.
      $unknown_value sget01 .stack .<< drop   # cc self'

      swap goto                         # self' },

    map(($_ => bin qq{ sget01 .asm .$_ drop goto }), qw/ 0 1 2 3 4 /),

    map(($_ => bin qq{                  # v self cc
      sget02 sget02 .asm .$_            # v self cc asm
      drop sset01 swap goto             # self }), qw/ l8 l16 l32 l64 /),

    'ref<<' => bin q{                   # v t self cc
      sget03 sget03 sget03 .asm .ref<< drop
      $unknown_value sget02 .stack .<< drop
      sset01 sset01 goto                # self },

    ptr => bin q{                       # x self cc
      sget02 sget02 .asm .ptr drop      # x self cc
      $unknown_value sget02 .stack .<< drop
      sset01 swap goto                  # self },

    hereptr => bin q{                   # x self cc
      sget02 sget02 .asm .hereptr drop  # x self cc
      $unknown_value sget02 .stack .<< drop
      sset01 swap goto                  # self },

    pnl => bin q{                       # s self cc
      sget02 sget02 .asm .pnl           # s self cc self
      drop sset01 swap goto             # self },

    # Symbolic method proxy
    symbolic_method => bin q{           # m self cc
      # The calling convention here is that the compiling class (top of data
      # stack) receives an assembler object and the method as its symbolic
      # method arguments.

      sget01 sget03 sget03 .stack       # m self cc self=asm m stack
      =0     swap .[]                   # m self cc self=asm m class
      .symbolic_method                  # m self cc asm'
      sset02 sset00 goto                # asm' },

    # Call/goto bytecodes
    goto => bin q{                      # self cc
      sget01 .stack .shift drop         # self cc
      sget01 .asm .goto drop            # self cc
      goto                              # self },

    call => bin q{                      # self cc
      sget01 .stack .shift drop         # self cc
      sget01 .asm .call drop            # self cc
      goto                              # self },

    call_native => bin q{               # self cc
      sget01 .stack .shift drop         # self cc
      sget01 .asm .call_native drop     # self cc
      goto                              # self },

    # Stack bytecodes
    dup => bin q{                       # self cc
      sget01 .asm .dup drop             # self cc
      sget01 .stack dup =0     swap .[] # self cc stack stack[0]
      swap .<< drop                     # self cc
      goto                              # self },

    drop => bin q{                      # self cc
      sget01 .asm .drop drop            # self cc
      sget01 .stack .shift drop         # self cc
      goto                              # self },

    swap => bin q{                      # self cc
      sget01 .asm .swap drop            # self cc
      sget01 .stack dup .shift swap     # self cc t0 stack
                    dup .shift swap     # self cc t0 t1 stack
      sget02 sget01 .<< drop            # self cc t0 t1 stack [<<t0]
      .<< drop drop                     # self cc
      goto                              # self },

    sget => bin q{                      # i self cc
      sget02 sget02 .asm .sget drop     # i self cc
      sget02 sget02 .stack .[]          # i self cc stack[i]
      sget02 .stack .<< drop            # i self cc
      sset01 swap goto                  # self },

    sset => bin q{                      # i self cc
      sget02 sget02 .asm .sset drop     # i self cc
      sget01 .stack .shift              # i self cc stack[0]
      sget03 sget03 .stack .[]= drop    # i self cc
      sset01 swap goto                  # self },

    # Frame/stack interop
    get_frameptr => bin q{              # self cc
      sget01 .asm .get_frameptr drop    # self cc
      sget01 .frame                     # self cc f
      sget02 .stack .<< drop            # self cc
      goto                              # self },

    set_frameptr => bin q{              # self cc
      sget01 .asm .set_frameptr drop    # self cc
      sget01 .stack .shift              # self cc f
      sget02 =24     iplus m64set       # self cc
      goto                              # self },

    # 0 -> 1 value emitting bytecodes
    map(($_ => bin qq{ sget01 .asm .$_ drop
                       \$unknown_value sget02 .stack .<< drop
                       goto }),
        qw/ lit8 lit16 lit32 lit64
            get_interpptr
            get_stackptr
            get_insnptr /),

    # 1 -> 0 value consuming bytecodes
    map(($_ => bin qq{ sget01 .asm .$_ drop
                       sget01 .stack .shift drop
                       goto }),
        qw/ set_interpptr
            set_stackptr /),

    # 1 -> 1 bytecodes
    map(($_ => bin qq{ sget01 .asm .$_ drop
                       \$unknown_value sget02 .stack =0     swap .[]= drop
                       goto }),
        qw/ iinv ineg bswap16 bswap32 bswap64
            m8get m16get m32get m64get /),

    # 2 -> 1 bytecodes
    map(($_ => bin qq{ sget01 .asm .$_ drop
                       sget01 .stack .shift drop
                       \$unknown_value sget02 .stack =0     swap .[]= drop
                       goto }),
        qw/ iplus itimes ishl isar ishr iand ior ixor ilt ieq /),

    # 2 -> 2 bytecode
    idivmod => bin q{ sget01 .asm .idivmod drop
                      $unknown_value sget02 .stack =0     swap .[]= drop
                      $unknown_value sget01 .stack =1     swap .[]= drop
                      goto },

    # 2 -> 0 bytecodes
    map(($_ => bin qq{ sget01 .asm .$_ drop
                       sget01 .stack .shift drop
                       sget01 .stack .shift drop
                       goto }),
        qw/ m8set m16set m32set m64set /),

    # 3 -> 1 bytecodes
    map(($_ => bin qq{ sget01 .asm .$_ drop
                       sget01 .stack .shift drop
                       sget01 .stack .shift drop
                       \$unknown_value sget02 .stack =0     swap .[] drop
                       goto }),
        qw/ if /),

    # 3 -> 0 bytecode
    memcpy => bin q{ sget01 .asm .memcpy drop
                     sget01 .stack .shift drop
                     sget01 .stack .shift drop
                     sget01 .stack .shift drop
                     goto },

    # 7 -> 1 bytecode
    syscall => bin q{ sget01 .asm .syscall drop
                      sget01 .stack .shift drop
                      sget01 .stack .shift drop
                      sget01 .stack .shift drop
                      sget01 .stack .shift drop
                      sget01 .stack .shift drop
                      sget01 .stack .shift drop
                      $unknown_value sget02 .stack =0     swap .[] drop
                      goto });


use constant typed_assembler_fn => phi::allocation
  ->constant(bin q{                     # cc
    lit8+40 i.heap_allocate             # cc &obj
    $typed_assembler_class sget01               m64set    # [.vt=]
    =0                     sget01 =8      iplus m64set    # [.parent=]
    intlist                sget01 =16     iplus m64set    # [.stack=]
    $unknown_value         sget01 =24     iplus m64set    # [.frame=]
    asm                    sget01 =32     iplus m64set    # [.asm=]

    swap goto                           # obj })

  ->named('typed_assembler_fn') >> heap;

BEGIN
{
  bin_macros->{tasm} = bin q{$typed_assembler_fn call};
}


1;
