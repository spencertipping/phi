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


=head2 Hashed gensyms
All names are stored as hashes, and all hashes have the lowest bit set. This
means we can produce gensyms by maintaining a sequential counter and
left-shifting by one -- or equivalently, incrementing by two instead of one.
=cut

use phi::constQ gensym_counter => 0;
use phi::fn     gensym_hash    => bin q{# cc
  gensym_counter m64get =2 iplus        # cc gs
  dup gensym_counter m64set             # cc gs
  _ goto                                # gs };


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

NB: all names are stored as hashed values; C<i64i> doesn't have a way to compare
strings to each other.
=cut

use phi::constQ av_method_ctor => 0;
use phi::constQ av_local_ctor  => 0;


use phi::class av_local =>
  @{+abstract_protocols},

  ctti             => bin q{ _ =16 iplus m64get _ goto },
  method           => bin q{ "TODO: wrap methods" i.die },
  stack            => bin q{ goto },
  as_local         => bin q{ sset01 _ goto },
  as_gc_atomic     => bin q{ goto },
  local_cttis_into => bin q{ sset00 goto },

  "impacts_gse?"    => bin q{ =0 sset01 goto },
  "impacts_lse?"    => bin q{ =0 sset01 goto },
  "references_lse?" => bin q{ =1 sset01 goto },

  compile => bin q{                     # asm prior frame self cc
    sget04 =0 sget04                    # asm prior frame self cc asm 0 frame
    sget05 .compile drop                # asm prior frame self cc

    sget01 =8 iplus m64get              # asm prior frame self cc name
    sget03 .{}                          # asm prior frame self cc offset
    sget05
      .get_frameptr                     # asm prior frame self cc offset asm[f]
      .const64                          # asm prior frame self cc asm[f offset]
      .iplus .m64get                    # asm prior frame self cc asm[v]
    drop sset02 drop drop goto          # asm };

use phi::fn av_local => bin q{          # ctti name cc
  $av_local_class _                     # ctti name class cc
  =24 i.heap_allocate                   # ctti name class cc new
  get_stackptr =16 iplus                # ctti name class cc new &c
  sget01 =24 memcpy                     # ctti name class cc new
  sset03 sset01 drop goto               # new };


use phi::class av_setlocal =>
  @{+abstract_protocols},

  ctti         => bin q{ =0 sset01 goto },
  method       => bin q{ "av_setlocal needs to be stacked" i.die },
  stack        => bin q{ "TODO: return an av_local" i.die },
  as_local     => bin q{ sset01 _ goto },
  as_gc_atomic => bin q{ goto },

  local_cttis_into => bin q{            # m self cc
    sget01 =16 iplus m64get .ctti       # m self cc ctti
    sget02 =8  iplus m64get             # m self cc ctti name
    sget04 .{}= drop                    # m self cc
    sset00 goto                         # m },

  "impacts_gse?"    => bin q{ _ =16 iplus m64get _
                              sget01 m64get :impacts_gse? goto },
  "impacts_lse?"    => bin q{ =1 sset01 goto },
  "references_lse?" => bin q{ =1 sset01 goto },

  compile => bin q{                     # asm prior frame self cc
    sget04 sget04 sget04                # asm prior frame self cc asm p f
    sget04 =16 iplus m64get .stack      # asm prior frame self cc asm p f s
    .compile                            # asm prior frame self cc asm
    sget02 =8 iplus m64get              # asm prior frame self cc asm name
    sget04 .{} _                        # asm prior frame self cc offset asm[v]
      .get_frameptr
      .const64
      .iplus .m64set                    # asm prior frame self cc asm[]
    drop sset02 drop drop goto          # asm };

use phi::fn av_setlocal => bin q{       # abs name cc
  $av_setlocal_class _                  # abs name class cc
  =24 i.heap_allocate                   # abs name class cc new
  get_stackptr =16 iplus                # abs name class cc new &c
  sget01 =24 memcpy                     # abs name class cc new
  sset03 sset01 drop goto               # new };


# NB: no GC in phi2, so checkpoints are passthrough
use phi::fn av_checkpoint => bin q{ goto };


use phi::class av_return =>
  @{+abstract_protocols},

  ctti              => bin q{ _ =8 iplus m64get .ctti _ goto },
  method            => bin q{ "av_return isn't a value node" i.die },
  stack             => bin q{ goto },
  as_local          => bin q{ "av_return isn't a value node" i.die },
  as_gc_atomic      => bin q{ goto },
  local_cttis_into  => bin q{ sset00 goto },

  "impacts_gse?"    => bin q{ _ =8 iplus .impacts_gse? _ goto },
  "impacts_lse?"    => bin q{ =1 sset01 goto },
  "references_lse?" => bin q{ =1 sset01 goto },

  compile => bin q{                     # asm prior frame self cc
    sget04 sget04 sget04
    sget04 =8 iplus m64get .stack .compile

    "TODO: frame pop code" i.die };

use phi::fn av_return => bin q{         # abs cc
  =16 i.heap_allocate                   # abs cc new
  $av_return_class sget01 m64set        # abs cc new [.class=]
  sget02 sget01 =8 iplus m64set         # abs cc new [.abs=]
  sset01 goto                           # new };


use phi::class av_seq =>
  @{+abstract_protocols},

  ctti => bin q{                        # self cc
    _ =8 iplus m64get                   # cc as
    dup .n =1 ineg iplus _ .[] .ctti    # cc ctti
    _ goto                              # ctti },

  method => bin q{ "TODO: wrap methods" i.die },
  stack  => bin q{ goto },
  as_local => bin q{                    # name self cc
    sget01 sget03 av_setlocal           # name self cc abs'
    sset02 sset00 goto                  # abs' },

  local_cttis_into => bin q{            # m self cc
    _ =8 iplus m64get                   # m cc as
    sget02 _                            # m cc m as
    [ sget01 sget03 .local_cttis_into   # a m cc m
      sset02 =0 sset01 goto ]
    _.reduce                            # m cc m
    drop goto                           # m },

  "impacts_gse?" => bin q{              # self cc
    _ =0_ =8 iplus m64get               # cc 0 as
    [ sget02 .impacts_gse? sget02 ior
      sset02 =0 sset01 goto ]
    _.reduce _ goto                     # i? },

  "impacts_lse?" => bin q{              # self cc
    _ =0_ =8 iplus m64get               # cc 0 as
    [ sget02 .impacts_lse? sget02 ior
      sset02 =0 sset01 goto ]
    _.reduce _ goto                     # i? },

  "references_lse?" => bin q{           # self cc
    _ =0_ =8 iplus m64get               # cc 0 as
    [ sget02 .references_lse? sget02 ior
      sset02 =0 sset01 goto ]
    _.reduce _ goto                     # r? },

  compile => bin q{                     # asm prior frame self cc
    _ =8 iplus m64get                   # asm prior frame cc as
    F get_stackptr set_frameptr         # asm prior frame cc as f0|
    sget04                              # asm prior frame cc as f0| prior
    [                                   # a prior cc
      F=40 iplus m64get                 # a prior cc asm
      sget02 F=24 iplus m64get          # a prior cc asm prior frame
      sget05 .compile                   # a prior cc 

      # OH SHIT ]

    # Fuck me, what do we do here
    };


1;
