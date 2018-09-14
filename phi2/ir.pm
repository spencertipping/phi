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

  ir_node.compiled_size(ir_fn* fn)                                -> int
  ir_node.compile_into(asm, array<hereptr> bb_offsets, ir_fn* fn) -> asm

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
      <<arg
      <<return /;


=head3 C<ir_bb>
Basic blocks are pretty simple. They behave as nodes that aggregate their
children, although this only happens for one level. Every basic block is a
direct child of the enclosing function.

C<parent> and C<index> help with editing, but aren't used during compilation.
Having these fields means we can implement C<]>, which will append the basic
block index to the calling node and return it. This simplifies branching nodes.

  struct ir_bb : ir_node
  {
    hereptr          class;
    ir_fn*           parent;
    int              index;
    array<ir_node*>* nodes;
  };

=cut

use phi::protocol ir_bb =>
  qw/ parent
      index
      nodes
      ] /;

use phi::class ir_bb =>
  ir_node_protocol,
  ir_bb_protocol,

  parent => bin q{ _ =8  iplus m64get _ goto },
  index  => bin q{ _ =16 iplus m64get _ goto },
  nodes  => bin q{ _ =24 iplus m64get _ goto },

  ']' => bin q{                         # self cc
    sget01 .index                       # self cc i
    sget02 .parent .<<                  # self cc parent
    sset01 goto                         # parent },

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
      F=24 iplus m64get                 # n asm cc asm bs fn
      sget05 .compile_into              # n asm cc asm
      sset02 =0 sset01 goto ]           # asm exit?=0
    _.reduce                            # asm bs fn self cc f0| asm
    drop set_frameptr sset02            # asm cc fn self
    drop drop goto                      # asm };


=head3 C<ir_branch>
This is probably the simplest node because the code template is a constant.
Every C<ir_branch> gets compiled like this:

  get_frameptr                          # size=1
    lit32 cond-offset                   # size=6
    iplus                               # size=7
    m64get                              # size=8
  lit64 then-addr                       # size=17
  lit64 else-addr                       # size=26
  if goto                               # size=28

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
    sset01 drop =28 _ goto              # 28 },

  compile_into => bin q{                # asm bbs fn self cc
    sget01 .cond                        # asm bbs fn self cc cond
    sget03 .local_offset                # asm bbs fn self cc condoff
    sget05
      .get_frameptr
      .lit32 _ bswap32 _ .l32           # asm bbs fn self cc asm
      .iplus
      .m64get
    drop                                # asm bbs fn self cc

    sget01 .else sget04 .[]             # asm bbs fn self cc else*
    sget02 .then sget05 .[]             # asm bbs fn self cc else* then*
    sget06
      .const64
      .const64                          # asm bbs fn self cc asm
      .if
      .goto                             # asm bbs fn self cc asm

    drop sset02 drop drop goto          # asm };


=head3 C<ir_return>
Emits instructions to push the specified set of indexed values onto the parent
stack, pop the frame, and return to the caller. The signature of the return node
must align with the function's declared return CTTI list.

Return code will end up looking like this:

  # push continuation address
  get_frameptr =ccoffset   iplus m64get # size=8
  get_frameptr =ret0offset iplus m64set # size=8

  # stack up return values
  get_frameptr =val1offset iplus m64get # size=8
  get_frameptr =ret1offset iplus m64set # size=8
  ... (the above repeated per retval)

  # restore parent frame, reset stack pointer, and return to caller
  get_frameptr =8 iplus m64get          # size=5
    dup                                 # ...size=6
    set_frameptr                        # ...size=7
    =retNoffset                         # ...size=12
    iplus set_stackptr                  # ...size=14
    goto                                # ...size=15

Here's the struct:

  struct ir_return : ir_node
  {
    hereptr     class;
    array<int>* vals;
  };

=cut

use phi::protocol ir_return =>
  qw/ retvals /;

use phi::class ir_return =>
  ir_node_protocol,
  ir_return_protocol,

  retvals => bin q{ _ =8 iplus m64get _ goto },

  compiled_size => bin q{               # fn self cc
    # Each returned value is 16 bytes, plus 16 constant for the continuation,
    # plus 15 for the final frame pop.
    _.n =16 itimes =31 iplus            # fn cc size
    sset01 goto                         # size },

  compile_into => bin q{                # asm bbs fn self cc
    =1                                  # asm bbs fn self cc rn
    F get_stackptr set_frameptr         # asm bbs fn self cc rn f0|
    sget06                              # asm bbs fn self cc rn f0| asm

    # Stack return continuation
    =0 sget06 .return_offset _          # asm bbs fn self cc rn f0| roff asm
    sget06 .cc_offset _                 # ...| roff ccoff asm
      .get_frameptr .lit32 _bswap32_ .l32 .iplus .m64get
      .get_frameptr .lit32 _bswap32_ .l32 .iplus .m64set    # ...| asm

    sget04 .retvals                     # asm bbs fn self cc rn f0| asm rvs

    # Stack return args
    [ F=32 iplus m64get                 # rvi asm cc fn
        F=8 iplus m64get _.return_offset
      F=32 iplus m64get                 # rvi asm cc retoff fn
        sget03 _.local_offset           # rvi asm cc retoff valoff

      # Increment rn counter
      F=8 iplus m64get =1 iplus F=8 iplus m64set

      sget03                            # rvi asm cc retoff valoff asm
        .get_frameptr .lit32 _bswap32_ .l32 .iplus .m64get
        .get_frameptr .lit32 _bswap32_ .l32 .iplus .m64set
      sset02 =0 sset01 goto ]           # asm exit?=0
    _.reduce                            # asm bbs fn self cc rn f0| asm

    # Pop frame and return. The parent frame is always stored at offset 8, and
    # we'll need to recalculate the return-continuation stack address (which is
    # retstack[0]).
    =0 sget06 .return_offset _          # ...| roff asm
      .get_frameptr .lit8 =8_ .l8 .iplus .m64get
      .dup .set_frameptr
      .lit32 _bswap32_ .l32             # ...| asm
      .iplus .set_stackptr
      .goto                             # asm bbs fn self cc rn f0| asm

    drop set_frameptr drop              # asm bbs fn self cc
    sset02 drop drop goto               # asm };


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
      code /;

use phi::class ir_val =>
  ir_node_protocol,
  ir_val_protocol,

  ivals => bin q{ _=8  iplus m64get_ goto },
  ovals => bin q{ _=16 iplus m64get_ goto },
  code  => bin q{ _=24 iplus m64get_ goto },

  compiled_size => bin q{               # fn self cc
    sget01 .ivals .n =8 itimes          # fn self cc size
    sget02 .ovals .n =8 itimes iplus    # fn self cc size'
    sget02 .code .size iplus            # fn self cc size''
    sset02 sset00 goto                  # size'' },

  compile_into => bin q{                # asm bbs fn self cc
    F get_stackptr set_frameptr         # asm bbs fn self cc f0|
    sget05                              # asm bbs fn self cc f0| asm

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
    _.reduce                            # asm bbs fn self cc f0| asm

    drop set_frameptr                   # asm bbs fn self cc
    sset02 drop drop goto               # asm };


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
had a function like C<fn(x:int) { y:int }>, C<x> would be at index 0 and C<y> at
index 1 -- even though physical memory would look more like this (assuming a
single C<int> return value):

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
      stack_size
      frame_size
      frame_class_fn
      block_addresses
      compile_frame_entry /;

use phi::class ir_fn =>
  ir_fn_protocol,
  ir_fn_internals_protocol,

  args    => bin q{ _ =8  iplus m64get _ goto },
  locals  => bin q{ _ =16 iplus m64get _ goto },
  blocks  => bin q{ _ =24 iplus m64get _ goto },
  returns => bin q{ _ =32 iplus m64get _ goto },

  # Editing methods
  '<<local' => bin q{ TODO },
  '<<arg'   => bin q{ TODO },

  '[' => bin q{                         # self cc
    # Push a new basic block, link it to this, and return it.
    TODO
    },

  # Compiler methods
  frame_class_fn => bin q{              # self cc
    # TODO: GC methods
    =0 sset01 goto                      # 0 },

  cc_offset => bin q{                   # self cc
    # Frame offset of the final continuation argument, which is just the arg
    # stack offset.
    _ .arg_stack_offset _ goto },

  local_offset => bin q{                # i self cc
    # Arg/local frame offset. If it's an arg offset, then it's relative to the
    # arg stack; otherwise it's frame + 16 + 8*n.
    sget01 .args .n sget03 ilt          # i self cc i<args?
    [ sget01 .arg_stack_offset          # i self cc off
      sget03 =8 itimes iplus            # i self cc off'
      sset02 sset00 goto ]              # off'
    [ sget02 =8 itimes =16 iplus        # i self cc off
      sset02 sset00 goto ]              # off
    if goto },

  return_offset => bin q{               # i self cc
    _ .ret_stack_offset                 # i cc off
    sget02 =8 itimes iplus              # i cc off'
    sset01 goto                         # off' },

  stack_size => bin q{                  # self cc
    # How many stack slots do we need to store arg and return values? It's going
    # to be the max of those two numbers.
    sget01 .args .n                     # self cc argn
    sget02 .returns .n                  # self cc argn retn
    sget01 sget01 ilt                   # self cc argn retn argn>retn?
    if                                  # self cc max(argn,retn)
    =8 itimes sset01 goto               # max*8 },

  frame_size => bin q{                  # self cc
    # 16 bytes of overhead plus any locals we store. Args and returns aren't
    # counted here.
    sget01 .locals .n =8 itimes =16 iplus
    sset01 goto                         # size },

  arg_stack_offset => bin q{            # self cc
    # Frame size plus any difference between the arg stack size and the overall
    # stack size (e.g. if there are more return values than arguments).
    sget01 .args .n =8 itimes           # self cc argsize
    sget02 .stack_size                  # self cc argsize ss
    _ ineg iplus                        # self cc ss-argsize
    sget02 .frame_size iplus            # self cc size
    sset01 goto                         # size },

  ret_stack_offset => bin q{            # self cc
    # Almost identical to arg_stack_offset; just with retstack instead.
    sget01 .returns .n =8 itimes        # self cc retsize
    sget02 .stack_size                  # self cc retsize ss
    _ ineg iplus                        # self cc ss-retsize
    sget02 .frame_size iplus            # self cc size
    sset01 goto                         # size },

  block_addresses => bin q{             # asm self cc
    i64i                                # asm self cc as
    F get_stackptr set_frameptr         # asm self cc as f0|
    sget04 .data                        # asm self cc as f0| a0

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
    dup sget03 .block_addresses         # self cc asm bs
    F get_stackptr set_frameptr         # self cc asm bs f0|

    sget02 sget05 .blocks               # ...| asm bbs
    [ sget01 F=8 iplus m64get           # bb asm cc asm bs
      F=32 iplus m64get                 # bb asm cc asm bs self=fn
      sget05 .compile_into              # bb asm cc asm
      sset02 =0 sset01 goto ]           # asm exit?=0
    _.reduce                            # self cc asm bs f0| asm

    sset04 set_frameptr drop drop goto  # asm };


1;
