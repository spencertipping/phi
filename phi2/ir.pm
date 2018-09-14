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
basic-block lists. Here are the struct definitions:

  struct ir_bb
  {
    hereptr          class;
    ir_fn*           parent;
    int              parent_index;
    array<ir_node*>* nodes;
  };

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
      arg_stack_offset
      ret_stack_offset
      frame_class_fn
      [
      compile
      <<local
      <<arg
      <<return /;


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

  # stack up return values
  get_frameptr =val1offset iplus m64get # size=8
  get_frameptr =ret1offset iplus m64set # size=8
  ... (the above repeated per retval)

  # push continuation address
  get_frameptr =ccoffset   iplus m64get # size=8
  get_frameptr =retNoffset iplus m64set # size=8

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
    =0                                  # asm bbs fn self cc rn
    F get_stackptr set_frameptr         # asm bbs fn self cc rn f0|
    sget05 sget03 .retvals              # asm bbs fn self cc rn f0| asm rvs

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

    # Stack return continuation
    sget02 sget06 .return_offset _      # asm bbs fn self cc rn f0| roff asm
    sget01 _                            # ...| roff roff asm
    sget07 .cc_offset _                 # ...| roff roff ccoff asm
      .get_frameptr .lit32 _bswap32_ .l32 .iplus .m64get
      .get_frameptr .lit32 _bswap32_ .l32 .iplus .m64set    # ...| roff asm

    # Pop frame and return. The parent frame is always stored at offset 8.
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
    TODO
    };


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

All offsets are known up front.
=cut

use phi::class ir_fn =>
  ir_fn_protocol,

  args    => bin q{ _ =8  iplus m64get _ goto },
  locals  => bin q{ _ =16 iplus m64get _ goto },
  blocks  => bin q{ _ =24 iplus m64get _ goto },
  returns => bin q{ _ =32 iplus m64get _ goto },

  frame_class_fn => bin q{ TODO },

  cc_offset => bin q{                   # self cc
    TODO
    },

  local_offset => bin q{                # i self cc
    # Return the offset of the given arg or local. This is all known at
    # compile-time.
    },

  return_offset => bin q{               # i self cc
    TODO
    },

  arg_stack_offset => bin q{            # self cc
    TODO
    },

  ret_stack_offset => bin q{            # self cc
    TODO
    },

  '[' => bin q{                         # self cc
    # Push a new basic block, link it to this, and return it.
    TODO
    },

  compile   => bin q{ TODO },
  '<<local' => bin q{ TODO },
  '<<arg'   => bin q{ TODO };


1;
