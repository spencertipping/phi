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


=head2 phi2 pre-bytecode intermediate representation
phi1 bytecode is great, but it doesn't produce code that's GC-atomic by default.
If we want that to happen automatically, we need to use the frame pointer and
make sure stuff gets aligned correctly -- and that means we need a little more
structure around stack accesses. Let's talk about how this works.

phi2 is made out of imperative basic-blocks, each of which addresses frame
values as it builds up expressions. It's important to model the load/store cycle
of even linear subexpressions because every external function call needs to be
GC-atomic (consider, for instance, C<f(g(x), h(x))>: we need to persist C<g(x)>
during C<h(x)>). The phi2 IR is conservative about this at the representational
level; it's up to post-IR optimization layers to decide when any shortcuts can
be taken.


=head3 IR structure
IR containers define functions. Arguments and locals are referred to by index --
no names -- and store their compile-time types. Each function also stores a list
of basic blocks, each of which is an array of nodes of one of three types:

1. C<< val* [code] -> val* >>: run code and update one or more values
2. C<< goto (val ? then_branch : else_branch) >>
3. C<< return val* >>: pop the frame

C<val> and C<branch> quantities are indexes into the current function's local or
basic-block lists.

There's no mechanism here to refer to another function, and that's by design.
There are two ways to do it within the above framework: you can insert a literal
address using an C<ir_val> node, or you can pass the function pointer into the
function as an argument.

In the structs above, C<< array<X> >> can be either C<i64i> or C<i64d>. Nothing
is modified during IR compilation.


=head3 C<ir_node> protocol
The important thing here is that nodes dictate how they get compiled into
bytecode, but it's worth talking a bit about how that works.

First, nodes can refer to function-level data by index; so part of the
compilation context is "here's the function object driving this whole process."
Second, and more complicated, nodes can also refer to function-level IR nodes by
index. The expectation is that we can get code offsets for these nodes. For
example, C<ir_branch(cond=3, then=0, else=1)> refers to two basic blocks, stored
at C<1> and C<0> -- but we'll compile it using a template that looks more like
this:

  get_frameptr =X iplus m64get          # retrieve "cond"
  lit64 <then-addr>                     # push then-addr
  lit64 <else-addr>                     # push else-addr
  if goto                               # ->branch

There are two ways we can deal with this. One is to maintain a list of offsets
that contain addresses we'll need to fix up: assemble everything first, then go
back and patch in the right addresses once we know where our basic blocks are.
The other strategy, and the one we use here, is to ask every C<ir_node> to
commit to an exact compiled size before we start emitting bytecode. Then we know
the offsets up front.

C<ir_fn> isn't modified by compilation, so we can't store the code offset map
there. Instead, the code offset map is a third argument to C<ir_node.compile>;
we end up with this interface:

  ir_node.compiled_size(ir_fn* fn) -> int

  ir_node.compile_into(               asm,
                       array<hereptr> bb_offsets,
                       int            this_bb_index,
                       ir_fn*         fn) -> asm

=cut

use phi::protocol ir_node =>
  qw/ compiled_size
      compile_into /;

use phi::protocol ir_fn =>
  qw/ locals
      args
      blocks
      returns
      local_offset
      return_offset
      cc_offset
      [
      compile
      <<local
      >>arg
      >>return /;


=head3 C<ir_branch>
This is pretty simple; we just need to compile offsets to the other basic
blocks. The overall idea looks like this:

  get_frameptr =cond_index iplus m64get # size=8
  get_insnptr dup                       # ...size=10
    lit64 then-offset iplus swap        # ...size=21
    lit64 else-offset iplus             # ...size=31
  if goto                               # ...size=34

The struct is correspondingly straightforward:

  struct ir_branch : ir_node
  {
    hereptr class;
    i32     cond;                       # local index
    i32     then;                       # basic block index
    i32     else;                       # basic block index
  };

=cut

use phi::protocol ir_branch =>
  qw/ cond
      then
      else /;

use phi::class ir_branch =>
  ir_node_protocol,
  ir_branch_protocol,

  cond => bin q{ _ =8  iplus m32get _ goto },
  then => bin q{ _ =12 iplus m32get _ goto },
  else => bin q{ _ =16 iplus m32get _ goto },

  compiled_size => bin q{               # fn self cc
    sset01 drop =34 _ goto              # 34 },

  compile_into => bin q{                # asm bbs bi fn self cc
    sget01 .cond                        # asm bbs bi fn self cc cond
    sget03 .local_offset                # asm bbs bi fn self cc condoff
    sget06
      .get_frameptr
      .lit32 _ bswap32 _ .l32           # asm bbs bi fn self cc asm
      .iplus
      .m64get

    # Now emit the code to produce branch offsets. get_insnptr returns the
    # address of the next instruction, so that's where we need to start.
      .get_insnptr
    dup .size _                         # asm bbs bi fn self cc off asm
      .dup
    sget03 .then sget07 .[]             # asm bbs bi fn self cc off asm t
    sget02 ineg iplus _
      .const64 .iplus .swap             # asm bbs bi fn self cc off asm

    sget03 .else sget07 .[]             # asm bbs bi fn self cc off asm e
    sget02 ineg iplus _
      .const64 .iplus                   # asm bbs bi fn self cc off asm

      .if .goto

    drop drop sset03 drop drop drop goto# asm };

use phi::fn ir_branch => bin q{         # cond then else cc
  =20 i.heap_allocate                   # c t e cc new
  $ir_branch_class sget01 m64set        # [.class=]
  sget04 sget01 =8 iplus m32set         # [.cond=]
  sget03 sget01 =12 iplus m32set        # [.then=]
  sget02 sget01 =16 iplus m32set        # [.else=]
  sset03 sset01 drop goto               # new };


=head3 C<ir_return>
Emits instructions to push the specified set of indexed values onto the parent
stack, pop the frame, and return to the caller. The signature of the return node
must align with the function's declared return CTTI list.

Return code will end up looking like this:

  # stack up return values
  get_frameptr =val0offset iplus m64get # size=8
  get_frameptr =ret0offset iplus m64set # size=8
  ... (the above repeated per retval)

  # restore parent frame, reset stack pointer, and return to caller
  get_frameptr dup =8 iplus m64get      # size=7
    set_frameptr                        # ...size=8
    =ccoffset                           # ...size=13
    iplus set_stackptr                  # ...size=15
    goto                                # ...size=16

Here's the struct:

  struct ir_return : ir_node
  {
    hereptr     class;
    array<int>* vals;
  };

=cut

use phi::protocol ir_return =>
  qw/ >>ret
      retvals /;

use phi::class ir_return =>
  ir_node_protocol,
  ir_return_protocol,

  retvals => bin q{ _ =8 iplus m64get _ goto },
  '>>ret' => bin q{                     # r self cc
    sget02 sget02 .retvals .<< drop     # r self cc
    sset01 _ goto                       # self },

  compiled_size => bin q{               # fn self cc
    # Each returned value is 16 bytes, plus 16 for the final frame pop.
    _.retvals .n =16 itimes =16 iplus   # fn cc size
    sset01 goto                         # size },

  compile_into => bin q{                # asm bbs bi fn self cc
    =0                                  # asm bbs bi fn self cc rn
    F get_stackptr set_frameptr         # asm bbs bi fn self cc rn f0|
    sget07                              # asm bbs bi fn self cc rn f0| asm

    sget04 .retvals                     # asm bbs bi fn self cc rn f0| asm rvs

    # Stack return args
    [ F=32 iplus m64get                 # rvi asm cc fn
        F=8 iplus m64get _.return_offset
      F=32 iplus m64get                 # rvi asm cc retoff fn
        sget04 _.local_offset           # rvi asm cc retoff valoff

      # Increment rn counter
      F=8 iplus m64get =1 iplus F=8 iplus m64set

      sget03                            # rvi asm cc retoff valoff asm
        .get_frameptr .lit32 _bswap32_ .l32 .iplus .m64get
        .get_frameptr .lit32 _bswap32_ .l32 .iplus .m64set
      sset02 =0 sset01 goto ]           # asm exit?=0
    _.reduce                            # asm bbs bi fn self cc rn f0| asm

    # Pop frame and return. The parent frame is always stored at offset 8, and
    # we'll need to recalculate the return-continuation stack address (which is
    # retstack[0]).
    sget05 .cc_offset _                 # ...| ccoff asm
      .get_frameptr .dup .lit8 =8_ .l8 .iplus .m64get
      .set_frameptr
      .lit32 _bswap32_ .l32             # ...| asm
      .iplus .set_stackptr
      .goto                             # asm bbs bi fn self cc rn f0| asm

    drop set_frameptr drop              # asm bbs bi fn self cc
    sset03 drop drop drop goto          # asm };

use phi::fn ir_return => bin q{         # cc
  =16 i.heap_allocate                   # cc new
  $ir_return_class sget01 m64set        # [.class=]
  i64i sget01 =8 iplus m64set           # [.retvals=]
  _ goto                                # new };


=head3 C<ir_val>
Updates one or more values in the current frame by transferring them onto the
stack and running the specified bytecode. The compiled form looks like this:

  get_frameptr lit32 ival1off iplus m64get    # size=8
  ...
  code...                                     # size=N
  get_frameptr lit32 oval1off iplus m64set    # size=8
  ...

C<ivals> and C<ovals> are ordered oppositely: C<ivals[n-1]> and C<ovals[0]> both
refer to the top stack entry. Basically, they're in the ordering we use when
compiling.

  struct ir_val : ir_node
  {
    hereptr     class;
    array<int>* ivals;
    array<int>* ovals;
    i8d*        code;
  };

=cut

use phi::protocol ir_val =>
  qw/ ivals
      ovals
      <<ival
      >>oval
      add_child_link
      [
      code /;

use phi::class ir_val =>
  ir_node_protocol,
  ir_val_protocol,

  ivals => bin q{ _=8  iplus m64get_ goto },
  ovals => bin q{ _=16 iplus m64get_ goto },
  code  => bin q{ _=24 iplus m64get_ goto },

  '<<ival' => bin q{                    # i self cc
    sget02 sget02 .ivals .<< drop       # i self cc
    sset01 _ goto                       # self },

  '>>oval' => bin q{                    # o self cc
    sget02 sget02 .ovals .<< drop       # o self cc
    sset01 _ goto                       # self },

  '['            => bin q{ _ asm .parent= _ goto },
  add_child_link => bin q{              # asm self cc
    sget02 .compile                     # asm self cc code
    sget02 =24 iplus m64set             # asm self cc
    sset01 _ goto                       # self },

  compiled_size => bin q{               # fn self cc
    sget01 .ivals .n =8 itimes          # fn self cc size
    sget02 .ovals .n =8 itimes iplus    # fn self cc size'
    sget02 .code .size iplus            # fn self cc size''
    sset02 sset00 goto                  # size'' },

  compile_into => bin q{                # asm bbs bi fn self cc
    F get_stackptr set_frameptr         # asm bbs bi fn self cc f0|
    sget06                              # asm bbs bi fn self cc f0| asm

    sget03 .ivals                       # ...| asm ivs
    [ sget02                            # iv asm cc iv
      F=24 iplus m64get .local_offset   # iv asm cc ivoff
      sget02                            # iv asm cc ivoff asm
        .get_frameptr .lit32 _bswap32_ .l32 .iplus .m64get
      sset02 =0 sset01 goto ]           # asm exit?=0
    _.reduce                            # ...| asm

    sget03 .code _ .+=                  # ...| asm

    sget03 .ovals                       # ...| asm ovs
    [ sget02                            # ov asm cc ov
      F=24 iplus m64get .local_offset   # ov asm cc ovoff
      sget02                            # ov asm cc ovoff asm
        .get_frameptr .lit32 _bswap32_ .l32 .iplus .m64set
      sset02 =0 sset01 goto ]           # asm exit?=0
    _.reduce                            # asm bbs bi fn self cc f0| asm

    drop set_frameptr                   # asm bbs bi fn self cc
    sset03 drop drop drop goto          # asm };

use phi::fn ir_val => bin q{            # cc
  =32 i.heap_allocate                   # cc new
  $ir_val_class sget01 m64set           # [.class=]
  i64i sget01 =8  iplus m64set          # [.ivals=]
  i64i sget01 =16 iplus m64set          # [.ovals=]
  "" sget01 =24 iplus m64set            # [.code=]  (default: copy)
  _ goto                                # new };


=head3 C<ir_bb>
Basic blocks are pretty simple. They behave as nodes that aggregate their
children, although this only happens for one level. Every basic block is a
direct child of the enclosing function.

  struct ir_bb : ir_node
  {
    hereptr          class;
    ir_fn*           parent;            # NB: used just for editing
    int              index;
    array<ir_node*>* nodes;
  };

=cut

use phi::protocol ir_bb =>
  qw/ parent
      index
      nodes
      <<
      ] /;

use phi::class ir_bb =>
  ir_node_protocol,
  ir_bb_protocol,

  parent => bin q{ _ =8  iplus m64get _ goto },
  index  => bin q{ _ =16 iplus m64get _ goto },
  nodes  => bin q{ _ =24 iplus m64get _ goto },

  '<<' => bin q{                        # n self cc
    sget02 sget02 .nodes .<< drop       # n self cc
    sset01 _ goto                       # self },

  ']' => bin q{ _ .parent _ goto },

  compiled_size => bin q{               # fn self cc
    F get_stackptr set_frameptr         # fn self cc f0|
    =0 sget03 .nodes                    # fn self cc f0| size ns
    [ F=24 iplus m64get                 # n size cc fn
      sget03 .compiled_size             # n size cc nsize
      sget02 iplus sset02               # nsize size cc
      =0 sset01 goto ]                  # nsize exit?=0
    _.reduce                            # fn self cc f0| size
    sset03 set_frameptr sset00 goto     # size },

  compile_into => bin q{                # asm bs fn self cc
    F get_stackptr set_frameptr         # asm bs fn self cc f0|
    sget05 sget03 .nodes                # asm bs fn self cc f0| asm ns
    [ sget01 F=32 iplus m64get          # n asm cc asm bs
      F=16 iplus m64get .index          # n asm cc asm bs bi
      F=24 iplus m64get                 # n asm cc asm bs bi fn
      sget06 .compile_into              # n asm cc asm
      sset02 =0 sset01 goto ]           # asm exit?=0
    _.reduce                            # asm bs fn self cc f0| asm
    drop set_frameptr sset02            # asm cc fn self
    drop drop goto                      # asm };

use phi::fn ir_bb => bin q{             # cc
  =32 i.heap_allocate                   # cc new
  $ir_bb_class sget01 m64set            # [.class=]
  =0 sget01 =8 iplus m64set             # [.parent=]
  =1 ineg sget01 =16 iplus m64set       # [.index=-1]
  i64i sget01 =24 iplus m64set          # [.nodes=]
  _ goto                                # new };


=head3 C<ir_fn>
This is the compiler entry point. C<ir_fn> nodes provide some mutability so you
can stage changes incrementally.

Here's the struct:

  struct ir_fn
  {
    hereptr        class;
    array<ctti*>*  arg_cttis;
    array<ctti*>*  local_cttis;
    array<ctti*>*  return_cttis;
    array<ir_bb*>* basic_blocks;
  };

C<arg>s and C<local>s share an index range with C<arg>s allocated first. So if I
had a function like C<fn(x:int) { y:int }>, C<cc> would be at index 0, C<x> at
index 1, and C<y> at index 2 -- even though physical memory would look more like
this (assuming a single C<int> return value):

                     x_addr             # <- return value will go here
  parent_stackptr -> cc                 # address to return to (implied arg)
                     y_addr             # locals allocated below stack
                     parent_frameptr
  frameptr        -> frame_class_fn

All offsets are known at compile time.
=cut

use phi::protocol ir_fn_internals =>
  qw/ arg_stack_offset
      ret_stack_offset
      stack_n
      frame_size
      frame_class_fn
      block_offsets
      compile_frame_entry /;

use phi::class ir_fn =>
  ir_fn_protocol,
  ir_fn_internals_protocol,

  args    => bin q{ _ =8  iplus m64get _ goto },
  locals  => bin q{ _ =16 iplus m64get _ goto },
  returns => bin q{ _ =24 iplus m64get _ goto },
  blocks  => bin q{ _ =32 iplus m64get _ goto },

  # Editing methods
  '>>arg' => bin q{                     # ctti self cc
    # Die if we try to add args with basic blocks present: adding args changes
    # the indexes of all locals, effectively breaking everything.
    sget01 .blocks .n
    [ "can't >>arg a function with basic blocks" i.die ]
    [ goto ]
    if call                             # ctti self cc

    sget02 sget02 .args .<< drop        # ctti self cc
    sset01 _ goto                       # self },

  '<<local' => bin q{                   # ctti self cc
    sget02 sget02 .locals .<< drop      # ctti self cc
    sset01 _ goto                       # self },

  '>>return' => bin q{                  # ctti self cc
    sget02 sget02 .returns .<< drop     # ctti self cc
    sset01 _ goto                       # self },

  '[' => bin q{                         # self cc
    ir_bb                               # self cc bb
    sget02            sget01 =8  iplus m64set   # [.parent=self]
    sget02 .blocks .n sget01 =16 iplus m64set   # [.index=]
    dup sget03 .blocks .<< drop                 # [blocks<<bb]
    sset01 goto                         # bb },

  # Compiler methods
  frame_class_fn => bin q{              # self cc
    =0 sset01 goto                      # 0 },

  cc_offset => bin q{                   # self cc
    # Frame offset of the final continuation argument, which is just the arg
    # stack offset.
    _ .arg_stack_offset _ goto },

  local_offset => bin q{                # i self cc
    sget02 =8 itimes =16 iplus          # i self cc off
    sset02 sset00 goto                  # off },

  return_offset => bin q{               # i self cc
    _ .ret_stack_offset                 # i cc off
    sget02 =8 itimes iplus              # i cc off'
    sset01 goto                         # off' },

  stack_n => bin q{                     # self cc
    # How many stack slots do we need to store arg and return values? It's going
    # to be the max of those two numbers.
    sget01 .args .n                     # self cc argn
    sget02 .returns .n                  # self cc argn retn
    sget01 sget01 ilt                   # self cc argn retn argn>retn?
    if                                  # self cc max(argn,retn)
    sset01 goto                         # max },

  frame_size => bin q{                  # self cc
    # 16 bytes of overhead plus any locals and args we store.
    sget01 .locals .n sget02 .args .n iplus
    =8 itimes =16 iplus
    sset01 goto                         # size },

  arg_stack_offset => bin q{            # self cc
    # Frame size plus any difference between the arg stack size and the overall
    # stack size (e.g. if there are more return values than arguments).
    sget01 .args .n =8 itimes           # self cc argsize
    sget02 .stack_n =8 itimes           # self cc argsize ss
    _ ineg iplus                        # self cc ss-argsize
    sget02 .frame_size iplus            # self cc size
    sset01 goto                         # size },

  ret_stack_offset => bin q{            # self cc
    # Almost identical to arg_stack_offset; just with retstack instead.
    sget01 .returns .n =8 itimes        # self cc retsize
    sget02 .stack_n =8 itimes           # self cc retsize ss
    _ ineg iplus                        # self cc ss-retsize
    sget02 .frame_size iplus            # self cc size
    sset01 goto                         # size },

  block_offsets => bin q{               # asm self cc
    i64i                                # asm self cc as
    F get_stackptr set_frameptr         # asm self cc as f0|
    sget04 .size                        # asm self cc as f0| a0

    sget04 .blocks                      # asm self cc as f0| a0 bs
    [ sget01 F=8 iplus m64get .<< drop  # b a cc
      F=24 iplus m64get                 # b a cc self
      sget03 .compiled_size             # b a cc size
      sget02 iplus sset02               # a' a cc
      =0 sset01 goto ]                  # a' exit?=0
    _.reduce                            # asm self cc as f0| aN

    drop set_frameptr                   # asm self cc as
    sset02 sset00 goto                  # as },

  compile_frame_entry => bin q{         # asm self cc
    # First, align the stack to the size boundary; that is, reflect any padding
    # below the arguments. This memory doesn't need to be initialized because
    # the frame class doesn't know about it. If the gap is N bytes, then our
    # logic should look like this:
    #
    #   get_stackptr =N ineg iplus set_stackptr

    sget01 .frame_size ineg             # asm self cc -fs
    sget02 .arg_stack_offset iplus      # asm self cc delta
    sget03                              # asm self cc delta asm
      .get_stackptr .lit16 _bswap16_ .l16
      .ineg .iplus .set_stackptr        # asm self cc asm

    # Initialize all local slots to zero. If we don't do this, we run the risk
    # of following an uninitialized pointer if a GC happens.
    sget02 .locals                      # asm self cc asm ls
    [ sget01 .lit8 .0 sset02 =0 sset01 goto ]
    _.reduce                            # asm self cc asm

    # Copy arguments using sget. The indexes are constant and are calculated as
    # the number of locals plus the retstack surplus.
    F _ get_stackptr set_frameptr       # asm self cc f0 asm|
    sget03 .locals .n =1 ineg iplus
    sget04 .args .n iplus               # asm self cc f0 asm| lidx
    sget04 .stack_n ineg iplus
    sget04 .args .n iplus _             # asm self cc f0 idx| asm
    sget04 .args                        # asm self cc f0 idx| asm args
    [ F m64get sget02 .sget sset02 =0 sset01 goto ]
    _.reduce                            # asm self cc f0 idx| asm
    sset00 _ set_frameptr               # asm self cc asm

    # Last step: push the original frame pointer and the class dispatch fn, then
    # set the new frame pointer.
    sget02 .frame_class_fn _            # asm self cc ffn asm
      .get_frameptr
      .hereptr                          # asm self cc asm
      .get_stackptr .set_frameptr

    drop sset00 goto                    # asm },

  compile => bin q{                     # self cc
    asm                                 # self cc asm
    sget02 .compile_frame_entry         # self cc asm
    dup sget03 .block_offsets           # self cc asm bs
    F get_stackptr set_frameptr         # self cc asm bs f0|

    sget02 sget05 .blocks               # ...| asm bbs
    [ sget01 F=8 iplus m64get           # bb asm cc asm bs
      F=32 iplus m64get                 # bb asm cc asm bs self=fn
      sget05 .compile_into              # bb asm cc asm
      sset02 =0 sset01 goto ]           # asm exit?=0
    _.reduce                            # self cc asm bs f0| asm

    sset04 set_frameptr drop drop goto  # code };

use phi::fn ir_fn => bin q{             # cc
  =40 i.heap_allocate                   # cc new
  $ir_fn_class sget01 m64set            # [.class=]
  i64i sget01 =8 iplus m64set           # [.args=]
  i64i sget01 =16 iplus m64set          # [.locals=]
  i64i sget01 =24 iplus m64set          # [.returns=]
  i64i sget01 =32 iplus m64set          # [.blocks=]
  _ goto                                # new };


=head2 TORPEDO TIME
Let's sink this puppy. Or prove that it works. Or something.
=cut

use phi::testfn ir_trivial => bin q{    #
  ir_fn                                 # fn():
    =0_ .>>arg                          # fn(cc):
    =0_ .>>return                       # fn(cc){x}:cc
  .[                                    # bb0
    ir_return
      =0_ .>>ret _.<<                   # cc
  .] .compile .data
  =5_ call =5 ieq "still 5" i.assert    # };

use phi::testfn ir_inc => bin q{
  ir_fn
    =0_ .>>arg
    =0_ .>>arg
    =0_ .>>return
    =0_ .>>return
  .[
    ir_val
      =1_ .<<ival
      =1_ .>>oval
      .[ .lit8 .1 .iplus .] _.<<
    ir_return
      =0_ .>>ret
      =1_ .>>ret _ .<<
  .] .compile .data
  =5_ call =6 ieq "5->6" i.assert };

use phi::testfn ir_abs => bin q{
  ir_fn
    =0_ .>>arg                          # cc
    =0_ .>>arg                          # x
    =0_ .<<local                        # x<0?
    =0_ .>>return                       # cc
    =0_ .>>return                       # abs(x)
  .[
    ir_val
      =1_ .<<ival
      =2_ .>>oval
      .[ .lit8 .0 .swap .ilt .] _.<<
    =2 =1 =2 ir_branch _.<<
  .]
  .[                                    # then-branch: negative
    ir_val
      =1_ .<<ival
      =1_ .>>oval
      .[ .ineg .] _.<<
    ir_return
      =0_ .>>ret
      =1_ .>>ret _.<<
  .]
  .[                                    # else-branch: positive
    ir_return
      =0_ .>>ret
      =1_ .>>ret _.<<
  .] .compile .data                     # abs

  =87      sget01 call =87 ieq "abs87"  i.assert
  =87 ineg sget01 call =87 ieq "abs-87" i.assert
  =0       sget01 call =0  ieq "abs0"   i.assert

  drop };


1;
