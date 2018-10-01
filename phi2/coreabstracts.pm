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


=head2 Type-agnostic abstracts
Let's get some of the logic out of the way by handling common cases. Stack cells
are a constant size, so we can just assume a 1:1 mapping to locals. I'll
implement the abstracts for stack-allocated value types in phi3.

At a high level we have this set of things:

  av_local("name", ctti)
  av_setlocal("name", abstract)         # NB: you need to call do() on the arg
  av_checkpoint(abstract)               # store a transient frame-local
  av_return(abstract)
  av_seq([a1, a2, ..., aN])
  av_if(cond, then, else)
  av_method(receiver, "m")
  av_call(fn, [args])
  av_fn([arg_cttis], body)
  av_arg(i, ctti)

=cut

use phi::constQ av_method_ctor => 0;

use phi::class av_local =>
  @{+abstract_protocols},

  ctti             => bin q{ _ =16 iplus m64get _ goto },
  method           => bin q{ "TODO: wrap methods" i.die },
  stack            => bin q{ goto },
  as_local         => bin q{ sset01 _ goto },
  as_gc_atomic     => bin q{ goto },

  local_cttis_into => bin q{            # m self cc
    sget01 =8  iplus m64get             # m self cc name
    sget02 =16 iplus m64get _           # m self cc ctti name
    sget04 .{}= drop                    # m self cc
    sset00 goto                         # m },

  do                => bin q{ goto },
  "impacts_gse?"    => bin q{ =0 sset01 goto },
  "impacts_lse?"    => bin q{ =0 sset01 goto },
  "references_lse?" => bin q{ =1 sset01 goto },

  compile => bin q{                     # asm prior frame self cc
    sget01 =8 iplus m64get              # asm prior frame self cc name
    sget03 .{}                          # asm prior frame self cc offset
    sget05
      .get_frameptr                     # asm prior frame self cc offset asm[f]
      .const64                          # asm prior frame self cc asm[f offset]
      .iplus .m64get                    # asm prior frame self cc asm[v]
    drop sset02 drop drop goto          # asm };


1;
