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
      val
      val= /;

use phi::protocol schedule_linkage =>
  qw/ entry_block
      exit_block
      +=
      << /;

use phi::class schedule =>
  schedule_protocol,
  schedule_linkage_protocol,

  fn    => bin q{ _ =8  iplus m64get _ goto },
  entry => bin q{ _ =16 iplus m32get _ goto },
  exit  => bin q{ _ =20 iplus m32get _ goto },
  val   => bin q{ _ =24 iplus m32get _ goto },

  "entry=" => bin q{ sget02 sget02 =16 iplus m32set sset01 goto },
  "exit="  => bin q{ sget02 sget02 =20 iplus m32set sset01 goto },
  "val="   => bin q{ sget02 sget02 =24 iplus m32set sset01 goto },

  entry_block => bin q{ sget01 .entry sget02 .fn .blocks .[] sset01 goto },
  exit_block  => bin q{ sget01 .exit  sget02 .fn .blocks .[] sset01 goto },

  '<<' => bin q{ sget02 sget02 .exit_block .<< drop sset01 goto },
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
  =1 ineg sget01 =24 iplus m32set       # [.exit=]
  sset01 goto                           # new };


=head3 Constructors
There are a couple of obvious entry points here, one for constant values and one
for locals. Each one produces a single C<ir_val> node. Constant values allocate
a new local slot and basic block to store the result.
=cut

use phi::fn schedule_constant => bin q{ # v ctti fn cc
  # First, allocate a new local of the specified type and save its index.
  sget02 sget02 .<<local
    .locals .n =1 ineg iplus            # v ctti fn cc li

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
    sset01 .val=                        # v ctti fn cc sc
  sset03 sset01 drop goto               # sc };

use phi::fn schedule_local => bin q{    # i fn cc
  # Create an empty basic block and refer to it.
  sget01 .[ dup .] drop .index          # i fn cc bbi

  # Now link a schedule that refers to the specified local.
  schedule                              # i fn cc bbi sc
    sget01 _ .entry= .exit=             # i fn cc sc
    sget03 _ .val=                      # i fn cc sc
  sset02 sset00 goto                    # sc };


=head3 Unit tests
TODO
=cut


1;
