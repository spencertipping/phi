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


=head2 Continuation flow
phi's compiler consists of a series of stack conversions, each adapting the set
of arguments passed into the continuation of the current expression. This set of
arguments is called the "refset," and we care about it because we end up using
it to generate the object stored in the frame pointer. This object is the GC
root.

Call frames can be stack or heap-allocated; the interpreter doesn't need to know
the difference. The frame object will handle GC slightly differently depending
on which it is (if stack-allocated, it won't try to write itself into the new
heap).

Flow assemblers are linked structures that proceed leftwards, unlike lists which
proceed rightwards. That is, suppose we want the following sequence:

  push_frame(X) update_frame(Y -> Z) pop_frame(Q)

The flow assembler will look like this:

  pop_frame_link(Q, tail =
    update_frame_link(Y, Z, tail =
      push_frame_link(X, tail =
        nil_flow_assembler)))

=cut


use constant flow_assembler_protocol => phi::protocol->new('flow_assembler',
  qw/ into_asm /);


use constant nil_flow_assembler_class => phi::class->new('nil_flow_assembler',
  maybe_nil_protocol,
  flow_assembler_protocol)

  ->def(
    "nil?"   => bin q{=1 sset01 goto},
    into_asm => bin q{sset00 goto});


use constant nil_flow_assembler_instance => phi::allocation
  ->constant(pack Q => nil_flow_assembler_class)
  ->named('nil_flow_assembler_instance') >> heap;


BEGIN
{
  bin_macros->{fasm} = bin q{$nil_flow_assembler_instance};
}


=head3 CTTI
Every language represents values as some mixture of CTTI (compile-time type
information) and RTTI (runtime type information), typically split as dictated by
the typing discipline. phi doesn't impose a global type system, so you have
fine-grained control over the line between compile-time and runtime.

Full evaluation is the upper bound of compile-time knowledge; at that point
there is no RTTI at all and we reduce a program to a constant or something
similarly trivial. If we refuse to follow function calls or constant-fold
conditionals, then we get a basic bottom-up type inference algorithm. If we do
no evaluation at all, we end up with something like Smalltalk: all data about
values is available as RTTI.


=head3 Basic blocks and linking
Each flow assembler behaves like C<bin> without support for C<[> and C<]>: it's
an uninterrupted chunk of code that runs from start to end. We can build
C<[...]> blocks by referring to other flow assembler objects. This is done using
a custom link-a-block link.


=head3 Stack/frame interfacing
Flow assemblers are made up of links, which fall into a few categories:


=head4 C<push_frame>
We need to know two things to create a frame:

1. The full set of ref IDs and their CTTIs
2. The incoming stack layout, in terms of this new refset

Frame classes in general are generated and managed by the flow assembler; this
coupling exists because the flow assembler is responsible for moving values
between the stack and the frame, which entails addressing the object somehow.

Q: is it worth designing this in terms of metaclasses or protocols or something?
We also need to tell the interpreter how to address the frame pointer for GC
purposes, so the abstraction escapes flow assemblers.

Here's the struct for this link:

  struct flow_push_frame_link
  {
    hereptr        class;
    flow_asm*      tail;
    strmap<ctti*>* frame_layout;
    list<string*>* stack_layout;
    class*         frame_class;
  }

=cut


use constant flow_push_frame_protocol => phi::protocol->new('flow_push_frame',
  qw/ frame_layout
      stack_layout
      frame_class
      defvar
      defarg /);


use constant flow_push_frame_class => phi::class->new('flow_push_frame',
  maybe_nil_protocol,
  cons_protocol,
  flow_assembler_protocol,
  flow_push_frame_protocol)

  ->def(
    "nil?"       => bin q{=0 sset01 goto},
    head         => bin q{goto},
    tail         => bin q{swap =8  iplus m64get swap goto},
    frame_layout => bin q{swap =16 iplus m64get swap goto},
    stack_layout => bin q{swap =24 iplus m64get swap goto},

    defvar       => bin q{              # name ctti self cc
      sget02 sget04 sget03              # n c s cc c n s
      .frame_layout .{}=                # n c s cc fl
      drop sset01 sset01 goto           # self },

    defarg       => bin q{              # name self cc
      sget02 sget02 .stack_layout .<<   # name self cc sl
      drop sset01 swap goto             # self },

    frame_class  => bin q{              # self cc
      sget01 =32 iplus dup m64get       # self cc &c c?
      [ m64get sset01 goto ]            # c

      [ # This is pretty straightforward. We need to generate a class by linking
        # a bunch of struct elements together, all of type i64f or objref
        # depending on the CTTI.
        struct
          "class"        i64f
          "parent_frame" i64f           # self cc &c struct

        sget03 .frame_layout .kv_pairs  # self cc &c struct fl
        [ sget01 .nil?                  # self cc &c struct fl loop flnil?
          [ drop drop                   # self cc &c struct
            class accessors             # self cc &c class
            sget01 m64set               # self cc &c
            m64get sset01 goto ]        # c
          [ sget02 sget02 .key          # self cc &c struct fl loop struct fname
            sget03 .value .struct_link  # self cc &c struct fl loop struct'
            sset02                      # self cc &c struct' fl loop
            swap .tail swap             # self cc &c struct' fl.tail loop
            dup goto ]                  # ->loop
          if goto ]                     # self cc &c struct fl loop
        dup goto ]                      # ->loop

      if goto                           # c },

    into_asm => bin q{                  # asm self cc
      sget02 sget02 .tail .into_asm     # asm self cc asm
      drop sget01 .frame_layout .length # asm self cc frame-slots

      # Stack-allocate the frame object. We'll need two additional slots, one
      # for the original frame pointer and one for the new frame's class
      # hereptr.
      #
      # The new frame lives below the stack pointer; i.e. there's no overlap.
      #
      # As an instance, the frame object is built of stack cells we initialize
      # to neutral values (where "neutral" is ultimately specified by the CTTI,
      # but for now we'll just use 0).
      =0                                # asm self cc fn i
      [                                 # asm self cc fn i loop cc
        sget03 sget03 ilt               # asm self cc fn i loop cc i<fn?
        [ sget06 .lit8 =0 swap .l8 drop # ... fn i loop cc
          sget02 =1 iplus sset02        # ... fn i+1 loop cc
          sget01 goto ]                 # ->loop
        [ sset01 drop goto ]            # ->cc
        if goto ]                       # asm self cc fn i loop
      dup call                          # asm self cc fn

      # Now we've added all of the frame data. We just need to push the original
      # frame and the new frame's class dispatch function. The dispatch function
      # is a hereptr to a class object.
      sget03 .get_frameptr              # asm self cc fn asm [get_frameptr]
      sget03 .frame_class .dispatch_fn
        swap .hereptr                   # asm self cc fn asm [... $dfn]
      .get_stackptr .set_frameptr       # asm self cc fn asm

      # Now grab the original entries and populate the frame data. We might have
      # enough frame cells that the original stack is beyond the one-byte offset
      # range of sget, so we need to use m64get rather than stack accessors.
      #
      # Here's how this works. We calculate the original stack pointer, then
      # emit a series of (dup =8 iplus swap m64get f .field=) sequences to
      # populate frame elements.

      swap =2 iplus =3 ishl swap        # asm self cc fsize asm
      .get_frameptr .lit16
        swap bswap16 swap .l16 .iplus   # asm self cc asm [...f &s]

      # Now iterate through the stack entries. These are sequential, in our case
      # referring to successive cells beginning inclusively with &s.
      [ swap                            # sname cc asm [&s]
          .dup .lit8 =8 swap .l8        # [&s &s 8]
          .iplus .swap .m64get          # [&s+8 s]
          .get_frameptr                 # [&s+8 s f]

          sget02 "=" swap .+ swap       # "sname=" [&s+8 s f]
          .symbolic_method              # [&s+8 s f .sname=]
        sset01                          # asm cc
        =0 swap goto ]                  # asm self cc asm fn
      sget03 .stack_layout .reduce      # asm self cc asm

      # Drop the stack pointer to complete the operation. Once we do this, the
      # new stack will be empty wrt the new frame.
      .drop
      drop sset00 goto                  # asm });


use constant flow_push_frame_fn => phi::allocation
  ->constant(bin q{                     # tail cc
    swap =40 i.heap_allocate            # cc tail &f
    $flow_push_frame_class sget01 m64set# [.class=]
    swap    sget01 =8  iplus m64set     # cc &f [.tail=]
    strmap  sget01 =16 iplus m64set     # [.frame_layout=]
    strlist sget01 =24 iplus m64set     # [.stack_layout=]
    =0      sget01 =32 iplus m64set     # [.frame_class=]
    swap goto                           # &f })
  ->named('flow_push_frame_fn') >> heap;

BEGIN
{
  bin_macros->{fpush} = bin q{$flow_push_frame_fn call};
}


=head4 C<update_frame>
Modifies one or more values stored in the refset. This link stores three things:

1. Initial stack layout, in terms of ref IDs
2. Concatenative code
3. Final stack layout, in terms of ref IDs

NB: it doesn't make sense for C<update_frame> links to be able to modify the
CTTI of a given slot. This introduces a dependency on control flow, which means
we're dealing with RTTI not CTTI.

Here's the struct:

  struct flow_update_frame_link
  {
    hereptr        class;
    flow_asm*      tail;
    list<string*>* initial_layout;
    list<string*>* final_layout;
    asm*           asm;
  }

=cut


use constant flow_update_frame_protocol =>
  phi::protocol->new('flow_update_frame',
    qw/ initial_layout
        final_layout
        add_child_link
        asm
        defin
        defout
        [
        code /);


use constant flow_update_frame_class => phi::class->new('flow_update_frame',
  maybe_nil_protocol,
  cons_protocol,
  flow_assembler_protocol,
  flow_update_frame_protocol)

  ->def(
    "nil?"         => bin q{=0 sset01 goto},
    head           => bin q{goto},
    tail           => bin q{swap =8  iplus m64get swap goto},
    initial_layout => bin q{swap =16 iplus m64get swap goto},
    final_layout   => bin q{swap =24 iplus m64get swap goto},
    asm            => bin q{swap =32 iplus m64get swap goto},
    code           => bin q{swap .asm .compile swap goto},

    '['            => bin q{swap .asm swap goto},

    defin          => bin q{            # name self cc
      sget02 sget02 .initial_layout .<< # name self cc il
      drop sset01 swap goto             # self },

    defout         => bin q{            # name self cc
      sget02 sget02 .final_layout .<<   # name self cc fl
      drop sset01 swap goto             # self },

    # NB: this is just used for bracket syntax; it's a nop.
    add_child_link => bin q{            # asm self cc
      sset01 swap goto                  # self },

    into_asm       => bin q{            # asm self cc
      sget02 sget02 .tail .into_asm     # asm self cc asm

      # Three steps here. First, append code to populate the initial stack
      # layout; then append the concatenative code; then store the final stack
      # back into the frame.
      sget02 .initial_layout            # asm self cc asm il
      [ swap                            # name cc asm
          .get_frameptr                 # name cc asm[f]
          sget02 swap .symbolic_method  # name cc asm[f.name]
        sset01                          # asm cc
        =0 swap goto ]                  # asm self cc asm il fn[asm exit?=0]
      swap .reduce                      # asm self cc asm

      # Now inline the code that addresses this stack.
      sget02 .code swap .inline         # asm self cc asm

      # ...and the last step, store the stack back into the frame.
      sget02 .final_layout              # asm self cc asm fl
      [ swap                            # name cc asm
          .get_frameptr
          sget02 "=" swap .+ swap .symbolic_method
        sset01                          # asm cc
        =0 swap goto ]                  # asm self cc asm fl fn[asm exit?=0]
      swap .reduce                      # asm self cc asm

      drop sset00 goto                  # asm });


use constant flow_update_frame_fn => phi::allocation
  ->constant(bin q{                     # tail cc
    swap =40 i.heap_allocate            # cc tail &f
    $flow_update_frame_class sget01 m64set # [.class=]
    swap    sget01 =8  iplus m64set     # cc &f [.tail=]
    strlist sget01 =16 iplus m64set     # [.initial_layout=]
    strlist sget01 =24 iplus m64set     # [.final_layout=]
    asm     sget01 =32 iplus m64set     # [.asm=]

    # Parent-link the asm to the frame update.
    dup dup .asm                        # cc &f &f asm
    =8 iplus m64set                     # cc &f [asm.parent=]

    swap goto                           # &f })
  ->named('flow_update_frame_fn') >> heap;

BEGIN
{
  bin_macros->{fupd} = bin q{$flow_update_frame_fn call};
}


=head4 C<link_code>
This stores a flow assembler pointer and gives you a way to add a code-hereptr
to the compiled result of a separate flow assembly. In other words, this makes
it possible to link multiple flow assemblers together.

C<link_code> produces a value compatible with the C<< hereptr<bytecode> >> CTTI.
That is, you can use it directly with a C<goto> or C<call> instruction, but the
result is addressable as an object.

Here's the struct:

  struct flow_link_code_link
  {
    hereptr   class;
    flow_asm* tail;
    flow_asm* code;
  }

=cut


use constant flow_link_code_protocol => phi::protocol->new('flow_link_code',
  qw/ code /);


use constant flow_link_code_class => phi::class->new('flow_link_code',
  maybe_nil_protocol,
  cons_protocol,
  flow_assembler_protocol,
  flow_link_code_protocol)

  ->def(
    "nil?" => bin q{=0 sset01 goto},
    head   => bin q{goto},
    tail   => bin q{swap =8  iplus m64get swap goto},
    code   => bin q{swap =16 iplus m64get swap goto},

    into_asm => bin q{                  # asm self cc
      sget02 sget02 .tail .into_asm     # asm self cc asm
      .[ sget02 .code .into_asm .]      # asm self cc asm'
      sset02 sset00 goto                # asm' });


use constant flow_link_code_fn => phi::allocation
  ->constant(bin q{                     # tail fasm cc
    =24 i.heap_allocate                 # tail fasm cc &f
    $flow_link_code_class sget01 m64set # [.class=]
    sget03 sget01 =8  iplus m64set      # [.tail=]
    sget02 sget01 =16 iplus m64set      # [.code=]
    sset02 sset00 goto                  # &f })
  ->named('flow_link_code_fn') >> heap;

BEGIN
{
  bin_macros->{fcode} = bin q{$flow_link_code_fn call};
}


=head4 C<pop_frame>
Flattens selected frame entries onto the stack and restores the parent frame
object. All we store is the final stack layout.

Here's the struct:

  struct flow_pop_frame_link
  {
    hereptr        class;
    flow_asm*      tail;
    list<string*>* stack_layout;
  }

=cut


use constant flow_pop_frame_protocol => phi::protocol->new('flow_pop_frame',
  qw/ defret
      stack_layout /);


use constant flow_pop_frame_class => phi::class->new('flow_pop_frame',
  maybe_nil_protocol,
  cons_protocol,
  flow_assembler_protocol,
  flow_pop_frame_protocol)

  ->def(
    "nil?"       => bin q{=0 sset01 goto},
    head         => bin q{goto},
    tail         => bin q{swap =8  iplus m64get swap goto},
    stack_layout => bin q{swap =16 iplus m64get swap goto},

    defret       => bin q{              # name self cc
      sget02 sget02 .stack_layout .<<   # name self cc sl
      drop sset01 swap goto             # self },

    into_asm => bin q{                  # asm self cc
      sget02 sget02 .tail .into_asm     # asm self cc asm

      sget02 .stack_layout              # asm self cc asm sl
      [ swap                            # name cc asm
          .get_frameptr                 # name cc asm[f]
          sget02 swap .symbolic_method  # name cc asm[f.name]
        sset01                          # asm cc
        =0 swap goto ]                  # asm self cc asm sl fn[asm exit?=0]
      swap .reduce                      # asm self cc asm

      # At this point we've copied the frame entries out, but we still need to
      # deallocate the frame itself. This entails moving these newly copied
      # entries up to immediately below the parent frame pointer.
      #
      # From the assembler's point of view, we calculate the frame position
      # delta and issue a constant-size memory copy; then set the frame and
      # stack pointers and we're done. Here's what the assembly will look like,
      # where N is the number of stack cells in the layout:
      #
      #   get_stackptr                  # &s
      #   f.parent_frame swap           # f1 &s
      #   sget01 f ineg iplus           # f1 &s delta
      #   sget01 iplus                  # f1 &s &s'
      #   =N =3 ishl memcpy             # f1
      #   dup set_frameptr              # f1
      #   =N =3 ishl ineg iplus         # s1
      #   set_stackptr                  #

      sget02 .stack_layout .length swap # asm self cc n asm
        .get_stackptr
        .get_frameptr .'parent_frame .swap
        =1 swap .sget .get_frameptr .ineg .iplus
        =1 swap .sget .iplus
        .lit8 sget01 swap .l8
        .lit8 =3     swap .l8
        .ishl .memcpy
        .dup .set_frameptr
        .lit8 sget01 swap .l8
        .lit8 =3     swap .l8
        .ishl .ineg .iplus
        .set_stackptr                   # asm self cc n asm

      sset03 drop sset00 goto           # asm });


use constant flow_pop_frame_fn => phi::allocation
  ->constant(bin q{                     # tail cc
    swap =24 i.heap_allocate            # cc tail &f
    $flow_pop_frame_class sget01 m64set # [.class=]
    swap    sget01 =8  iplus m64set     # [.tail=]
    strlist sget01 =16 iplus m64set     # [.stack_layout=]
    swap goto                           # &f })
  ->named('flow_pop_frame_fn') >> heap;

BEGIN
{
  bin_macros->{fpop} = bin q{$flow_pop_frame_fn call};
}


=head4 C<concatenative>
Adds some literal concatenative code, with the implied promise that the frame
isn't modified or used. This is often used to issue the final C<goto>
instruction after a C<pop_frame> link. Technically there's nothing stopping us
from using C<update_frame> for this, but that would be a little bit dishonest
because C<update_frame> makes no promises about the stack layout (and in fact it
expects the stack to be empty before+after itself). A C<concatenative> node
addresses the stack directly.

Here's the struct:

  struct flow_concatenative_link
  {
    hereptr   class;
    flow_asm* tail;
    asm*      asm;
  }

=cut


use constant flow_concatenative_protocol =>
  phi::protocol->new('flow_concatenative',
    qw/ asm
        add_child_link
        [
        code /);


use constant flow_concatenative_class => phi::class->new('flow_concatenative',
  maybe_nil_protocol,
  cons_protocol,
  flow_assembler_protocol,
  flow_concatenative_protocol)

  ->def(
    "nil?" => bin q{=0 sset01 goto},
    head   => bin q{goto},
    tail   => bin q{swap =8  iplus m64get swap goto},
    asm    => bin q{swap =16 iplus m64get swap goto},

    code   => bin q{swap .asm .compile swap goto},
    '['    => bin q{swap .asm swap goto},

    add_child_link => bin q{            # asm self cc
      sset01 swap goto                  # self },

    into_asm => bin q{                  # asm self cc
      sget02 sget02 .tail .into_asm     # asm self cc asm
      sget02 .code swap .inline         # asm self cc asm
      sset02 sset00 goto                # asm });


use constant flow_concatenative_fn => phi::allocation
  ->constant(bin q{                     # tail cc
    swap =24 i.heap_allocate            # cc tail &f
    $flow_concatenative_class sget01 m64set
    swap sget01 =8  iplus m64set        # cc &f [.tail=]
    asm  sget01 =16 iplus m64set        # [.asm=]

    # Parent-link the asm to the flow concatenative.
    dup dup .asm                        # cc &f &f asm
    =8 iplus m64set                     # cc &f [asm.parent=]

    swap goto                           # &f })
  ->named('flow_concatenative_fn') >> heap;

BEGIN
{
  bin_macros->{fcat} = bin q{$flow_concatenative_fn call};
}


=head2 Test code
Gotta test this stuff; there's a lot of new code being used here.
=cut

use constant fpush_test_protocol => phi::protocol->new('fpush_test',
  qw/ x
      y
      cc
      x=
      y=
      cc=
      parent_frame
      class /);


use constant flow_frame_class_test_fn => phi::allocation
  ->constant(bin q{                     # cc
    # First define a CTTI, in this case a trivial wrapper for int64.
    struct
      "value" i64f
    class                               # cc ctti

    # Now assemble the frame-push element of a flow function with this
    # signature:
    #
    # fn(i64 y, i64 cc)
    # {
    #   i64 x;
    # }

    fasm                                # cc ctti flow
    fpush                               # cc ctti flow
      "x"  swap sget02 swap .defvar     # cc ctti flow[x]
      "y"  swap sget02 swap .defvar     # cc ctti flow[x y]
      "cc" swap sget02 swap .defvar     # cc ctti flow[x y cc]

      "y"  swap .defarg
      "cc" swap .defarg                 # cc ctti flow[x y cc]

    sset00                              # cc flow
    .frame_class                        # cc fc
    dup .fields .right_offset =40 ieq "frame40" i.assert
    dup .fields "class"        swap .{} .left_offset =0  ieq "class0"  i.assert
    dup .fields "parent_frame" swap .{} .left_offset =8  ieq "pframe8" i.assert
    dup .fields "cc"           swap .{} .left_offset =16 ieq "cc16"    i.assert
    dup .fields "y"            swap .{} .left_offset =24 ieq "y24"     i.assert
    dup .fields "x"            swap .{} .left_offset =32 ieq "x32"     i.assert

    # Stack-instantiate the class just like the flow assembler will
    =3 =5 =7                            # cc fc x=3 y=5 cc=7
    get_frameptr                        # cc fc x=3 y=5 cc=7 f0
    sget04 .dispatch_fn                 # cc fc x=3 y=5 cc=7 f0 dfn
    get_stackptr                        # cc fc x=3 y=5 cc=7 f0 dfn &f

    # Verify that it functions correctly
    dup .parent_frame sget03 ieq "spframe" i.assert
    dup .class        sget02 ieq "sclass"  i.assert
    dup .x            =3     ieq "sx"      i.assert
    dup .y            =5     ieq "sy"      i.assert
    dup .cc           =7     ieq "scc"     i.assert

    =87 sget01 .x=
    =61 sget01 .cc=
    =54 sget01 .y=
    dup .x            =87    ieq "sx'"     i.assert
    dup .y            =54    ieq "sy'"     i.assert
    dup .cc           =61    ieq "scc'"    i.assert

    # Now address it as the frame pointer
    set_frameptr
    F.parent_frame sget02 ieq "fpframe" i.assert
    F.class        sget01 ieq "fclass"  i.assert
    F.x            =87    ieq "fx"      i.assert
    F.y            =54    ieq "fy"      i.assert
    F.cc           =61    ieq "fcc"     i.assert
    F.parent_frame set_frameptr

    drop drop drop drop drop            # cc fc
    drop                                # cc
    goto                                # })
  ->named('flow_frame_class_test_fn') >> heap;


use constant flow_asm_test_fn => phi::allocation
  ->constant(bin q{                     # cc
    # First define a CTTI, in this case a trivial wrapper for int64.
    struct
      "value" i64f
    class

    # sic: we want accessors because I need to make sure this doesn't modify
    # semantics of field assignment within the frame object. The above test does
    # the accessor-free case.
    accessors                           # cc ctti

    # Now assemble a small flow function. Here's roughly what we're doing:
    #
    # fn(y, cc)
    # {
    #   i64 x;
    #   x = y + 5;
    #   cc.return(x);
    # }

    fasm                                # cc ctti flow
    fpush                               # cc ctti flow
      "x"  swap sget02 swap .defvar     # cc ctti flow[x]
      "y"  swap sget02 swap .defvar     # cc ctti flow[x y]
      "z"  swap sget02 swap .defvar     # cc ctti flow[x y z]
      "cc" swap sget02 swap .defvar     # cc ctti flow[x y z cc]

      "y"  swap .defarg
      "cc" swap .defarg                 # cc ctti flow
    sset00                              # cc flow

    # nop node: copy y (= 87) to z
    fupd
      "y" swap .defin
      "z" swap .defout                  # cc flow

    fupd
      "y" swap .defin
      "x" swap .defout
    .[ .lit8 =5 swap .l8 .iplus .]      # cc flow

    fpop
      "cc" swap .defret
      "z"  swap .defret
      "y"  swap .defret
      "x"  swap .defret                 # cc flow

    fcat
    .[ .goto .]                         # cc flow

    asm swap .into_asm                  # cc asm
    .compile .here                      # cc fn

    # Important! Before we call this function, we need to set the frame pointer
    # to the position where we want the return values -- in this case, the
    # current stack location. Stash the previous frame pointer so we can restore
    # it.
    get_frameptr swap                   # cc f0 fn
    get_stackptr set_frameptr           # cc f0 fn|

    =87 sget01 call                     # cc f0 fn| 92 87 87
      =87 ieq "fz=87" i.assert          # cc f0 fn| 92 87
      =87 ieq "fy=87" i.assert          # cc f0 fn| 92
      =92 ieq "fx=92" i.assert          # cc f0 fn|

    drop set_frameptr                   # cc
    goto                                # })
  ->named('flow_asm_test_fn') >> heap;


1;
