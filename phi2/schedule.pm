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
      val /;

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


1;
