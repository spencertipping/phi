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


=head2 Expression-level scheduling
Not all control flow is managed at the side-effect level: short-circuit and
ternary operators are examples that modify the basic-block chain in IR terms.
Put differently, not all expressions can be represented using a single C<ir_bb>.
We need to manage linked structures of C<ir_bb>s as a single unit so we can
return that structure from the dialect's C<expression> parser.

For example, an expression like C<f(x) ? g(x) : h(x)> involves three basic
blocks: one to calculate C<f(x)> and then branch, one to calculate C<g(x)>, and
one to calculate C<h(x)>. The schedule for this expression allows us to then
specify its continuation.

A schedule also indicates which local slot represents its value. For instance,
C<f(g(x))> creates two local slots but only returns the one for C<f(...)>.

Here's the struct:

  struct schedule
  {
    hereptr class;
    ir_fn*  fn;
    int32   entry;                      # index of initial basic block
    int32   exit;                       # index of final basic block
    int32   val;                        # index of local variable
  };

=cut

use phi::protocol schedule =>
  qw/ fn
      entry
      exit
      entry=
      exit=
      ctti
      val
      val= /;

use phi::protocol schedule_linkage =>
  qw/ entry_block
      exit_block
      +=
      << /;


=head3 Control flow constructors
As much as possible, we want to use schedules without switching back and forth
to IR nodes to manage control flow. A handful of methods covers the basics
pretty well:

  s.if(then, else) -> s                 # receiver is condition
  s.while(body)    -> s                 # receiver is condition
  s.return()       -> s
  s.goto(bbi)      -> s                 # append unconditional goto

=cut

use phi::protocol schedule_flow =>
  qw/ if
      while
      return
      goto /;

use phi::class schedule =>
  schedule_protocol,
  schedule_linkage_protocol,
  schedule_flow_protocol,

  fn    => bin q{ _ =8  iplus m64get _ goto },
  entry => bin q{ _ =16 iplus m32get _ goto },
  exit  => bin q{ _ =20 iplus m32get _ goto },
  val   => bin q{ _ =24 iplus m32get _ goto },

  ctti  => bin q{                       # self cc
    sget01 .val                         # self cc li
    sget02 .fn .local_ctti              # self cc ctti
    sset01 goto                         # ctti },

  "entry=" => bin q{ sget02 sget02 =16 iplus m32set sset01 _ goto },
  "exit="  => bin q{ sget02 sget02 =20 iplus m32set sset01 _ goto },
  "val="   => bin q{ sget02 sget02 =24 iplus m32set sset01 _ goto },

  entry_block => bin q{ sget01 .entry sget02 .fn .blocks .[] sset01 goto },
  exit_block  => bin q{ sget01 .exit  sget02 .fn .blocks .[] sset01 goto },

  goto => bin q{                        # bbi self cc
    =0 sget03 sget04 ir_branch          # bbi self cc irb
    sget02 .<< sset02 sset00 goto       # self },

  if => bin q{                          # then else self cc
    sget01 .val                         # then else self cc cond
    sget04 .entry sget04 .entry         # then else self cc cond ti ei
    ir_branch                           # then else self cc irb
    sget02 .<< drop                     # then else self cc

    # Allocate a new local to be the return value of the conditional. Take the
    # CTTI from the then-branch.
    sget03 .ctti                        # then else self cc ctti
    sget02 .fn .<<local .last_local     # then else self cc li

    # Create the join block to collect control flow from the two branches and
    # update our exit.
    sget02 .fn .[ dup .] drop .index    # then else self cc li bbi
    sget03 .exit= sget01 _ .val= drop   # then else self cc li

    # Now, for each branch, copy the existing result into the join local and
    # unconditionally goto the join block.
    sget04                              # then else self cc li then
      dup .val
      ir_val .<<ival                    # then else self cc li then irv
        sget02 _ .>>oval .[ .]          # then else self cc li then irv
      _.<<                              # then else self cc li then
      sget03 .exit _ .goto drop         # then else self cc li

    sget03                              # then else self cc li else
      dup .val
      ir_val .<<ival                    # then else self cc li else irv
        sget02 _ .>>oval .[ .]          # then else self cc li else irv
      _.<<                              # then else self cc li else
      sget03 .exit _ .goto drop         # then else self cc li

    drop sset01 sset01 goto             # self },

  while => bin q{                       # body self cc
    # Structurally we have this:
    #
    # loop:
    #   cond << ir_branch(cond, body, end)
    # body:
    #   body << goto(loop)
    # end:
    #   (empty)
    #
    # The return value of a while loop is the condition, so every while loop
    # returns zero.

    sget01 .fn .[ dup .] drop .index    # body self cc endi
    sget02 .val                         # body self cc endi condi
    sget04 .entry sget02 ir_branch      # body self cc endi irb
    sget03 .<< .exit= drop              # body self cc

    sget01 .entry sget03 .goto          # body self cc body
    drop sset01 _ goto                  # self },

  return => bin q{                      # self cc
    ir_return                           # self cc ir
      =0_ .>>ret                        # self cc ir[cc]
      sget02 .val _ .>>ret              # self cc ir[cc val]
    sget02 .<< drop goto                # self },

  '<<' => bin q{ sget02 sget02 .exit_block .<< drop sset01 _ goto },
  '+=' => bin q{                        # rhs self cc
    # Link our exit block to the RHS's entry block and update this schedule to
    # return the RHS.
    sget02 .entry =0_ dup ir_branch     # rhs self cc if(0,bbi,bbi)
    sget02 .exit_block .<< drop         # rhs self cc
    sget02 .exit sget02 =20 iplus m32set  # [self.exit=]
    sget02 .val  sget02 =24 iplus m32set  # [self.val=]
    sset01 _ goto                       # self };

use phi::fn schedule => bin q{          # fn cc
  =28 i.heap_allocate                   # fn cc new
  $schedule_class sget01 m64set         # [.class=]
  sget02 sget01 =8 iplus m64set         # [.fn=]
  =1 ineg sget01 =16 iplus m32set       # [.entry=]
  =1 ineg sget01 =20 iplus m32set       # [.exit=]
  =1 ineg sget01 =24 iplus m32set       # [.val=]
  sset01 goto                           # new };


=head3 Constructors
There are a couple of obvious entry points here, one for constant values and one
for locals. Constant values allocate a new local slot and basic block to store
the result; local references create an empty basic block and mark the local
index on the schedule.
=cut

use phi::fn schedule_constant => bin q{ # v ctti fn cc
  # First, allocate a new local of the specified type and save its index.
  sget02 sget02 .<<local .last_local    # v ctti fn cc fn

  # Now create the ir_val node to generate it, emitting into the allocated
  # local.
  dup ir_val .>>oval sget05_            # v ctti fn cc li v irv
    .[ .const64 .]                      # v ctti fn cc li irv

  # Create a basic block to hold this value.
  sget03 .[                             # v ctti fn cc li irv bb
    .<<                                 # v ctti fn cc li bb
    dup .]                              # v ctti fn cc li bb fn

  # Now link everything into a schedule.
  schedule                              # v ctti fn cc li bb sc
    sget01 .index _ .entry=             # v ctti fn cc li bb sc
    sget01 .index _ .exit=              # v ctti fn cc li bb sc
    sset00 .val=                        # v ctti fn cc sc
  sset03 sset01 drop goto               # sc };

use phi::fn schedule_local => bin q{    # i fn cc
  # Create an empty basic block and refer to it.
  sget01 .[ dup .] drop .index          # i fn cc bbi

  # Now link a schedule that refers to the specified local.
  sget02 schedule                       # i fn cc bbi sc
    sget01 _ .entry= .exit=             # i fn cc sc
    sget03 _ .val=                      # i fn cc sc
  sset02 sset00 goto                    # sc };


=head3 Unit tests
Basically, make sure that various types of linkages work as advertised.
=cut

use phi::testfn schedule_inc => bin q{  #
  ir_fn
    =0_ .>>arg                          # cc (0)
    =0_ .>>arg                          # x (1)
    =0_ .>>return                       # cc (0)
    =0_ .>>return                       # x' (x + 1)
    dup "irfn" i.assert
  =0_ schedule_local                    # sc
    dup "local" i.assert                # sc

  # Now link two nodes, one to increment and one to return.
  ir_val
    =1_ .<<ival
    =1_ .>>oval
    .[ .lit8 .1 .iplus .]
  _.<< dup "irval" i.assert             # sc

  ir_return
    =0_ .>>ret
    =1_ .>>ret
  _.<< dup "irret" i.assert             # sc

  .fn      dup "scfn"   i.assert
  .compile dup "fncomp" i.assert
  .data    dup "fndata" i.assert        # f

  =19_ call =20 ieq "inc20" i.assert    # };


use phi::testfn schedule_call => bin q{
  ir_fn
    =0_ .>>arg                          # cc (0)
    =0_ .>>arg                          # x (1)
    =0_ .>>return                       # cc (0)
    =0_ .>>return                       # f(x)
    dup .locals .n =0 ieq "fnlocals0" i.assert

  [ _ =3 iplus =7 itimes _ goto ]_ =0_ schedule_constant
    dup .fn .locals .n =1 ieq "fnlocals1" i.assert
    dup .val =2 ieq "fnval2" i.assert

  # Now link a continuation to the constant node.
  ir_val
    =1_ .<<ival
    =2_ .<<ival
    =1_ .>>oval
    .[ .call .]
  _.<<
  ir_return
    =0_ .>>ret
    =1_ .>>ret
  _.<<

  .fn      dup "scfn"   i.assert
  .compile dup "fncomp" i.assert
  .data    dup "fndata" i.assert

  =4_ call =49 ieq "call49" i.assert    # };


use phi::testfn schedule_abs => bin q{
  ir_fn
    =0_ .>>arg                          # cc      (0)
    =0_ .>>arg                          # x       (1)
    =0_ .<<local                        # x<0?    (2)
    =0_ .<<local                        # abs(x)  (3)
    =0_ .>>return                       # cc      (0)
    =0_ .>>return                       # abs(x)  (1)

  dup =1_ schedule_local                # irf sc
    dup.val ir_val .<<ival
               =2_ .>>oval .[ .lit8 .0 .swap .ilt .]
    _.<< =2_ .val=                      # irf sc

  # Create a fork. We ultimately want to return local 1, but we can take two
  # different paths to get there and the goal is to manage this all using a
  # single schedule.
  sget01 =1_ schedule_local             # irf sc then
    dup.val ir_val .<<ival
               =3_ .>>oval .[ .ineg .]
    _.<< =3_ .val=                      # irf sc then
  sget02 =1_ schedule_local             # irf sc then else

  sget02 .if .return                    # irf sc sc
    .fn      dup "abs_fn"      i.assert
    .compile dup "abs_compile" i.assert
    .data    dup "abs_data"    i.assert # irf sc fn
  sset00 sset00                         # fn

  dup =5_      call =5 ieq "abs5"  i.assert
  dup =5 ineg_ call =5 ieq "abs-5" i.assert
  dup =0_      call =0 ieq "abs0"  i.assert
  drop                                  # };


use phi::testfn schedule_sum => bin q{
  ir_fn
    =0_ .>>arg                          # cc    (0)
    =0_ .>>arg                          # xs    (1)
    =0_ .<<local                        # i     (2)
    =0_ .<<local                        # n     (3)
    =0_ .<<local                        # total (4)
    =0_ .<<local                        # i<n?  (5)
    =0_ .<<local                        # xsi   (6)
    =0_ .>>return                       # cc    (0)
    =0_ .>>return                       # total (1)

  =0_ schedule_local                    # sc

  ir_val =1_ .<<ival =3_ .>>oval .[ .'n .] _.<<                 # n = xs.n

  dup.fn =5_ schedule_local             # sc loopcond
  ir_val =3_ .<<ival =2_ .<<ival =5_ .>>oval .[ .ilt .] _.<<    # i<n? = (i<n)

  dup.fn =6_ schedule_local             # sc loopcond loop
  ir_val =2_ .<<ival =1_ .<<ival =6_ .>>oval .[ .'[] .] _.<<    # xsi = xs[i]
  ir_val =4_ .<<ival =6_ .<<ival =4_ .>>oval .[ .iplus .] _.<<  # total += xsi
  ir_val =2_ .<<ival =2_ .>>oval .[ .lit8 .1 .iplus .] _.<<     # i += 1

  _.while                               # sc while
  _.+=                                  # sc
  =4_ .val= .return                     # sc

  .fn      dup "fn"      i.assert
  .compile dup "compile" i.assert
  .data    dup "data"    i.assert       # fn

  dup i64i =7_  .<< _ call =7 ieq "sum7" i.assert
  dup i64i =7_  .<<
           =10_ .<< _ call =17 ieq "sum17" i.assert

  drop                                  # };


1;
