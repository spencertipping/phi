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


=head2 Dialect frontend
Here's the struct:

  struct phi2_front
  {
    hereptr vtable;
    anf*    head_anf;
    anf*    tail_anf;
  };

C<head_anf> and C<tail_anf> refer to the beginning and ending ANF nodes of the
expression being parsed. ANF is ordered like CPS, so for an expression like
C<48.to_string(10 + 6)> we'd end up with this:

  phi2_front(head_anf = anf1, tail_anf = anf5)
  where:
    anf1 = anf_let("gensym1", int_ctti, [=48],      .tail=anf2)
    anf2 = anf_let("gensym2", int_ctti, [=10],      .tail=anf3)
    anf3 = anf_let("gensym3", int_ctti, [=6],       .tail=anf4)
    anf4 = anf_let("gensym4", int_ctti, [iplus],    .tail=anf5)
    anf5 = anf_let("gensym5", ptr_ctti, [tostring], .tail=0)

The tail ANF's CTTI dictates the parse, and the head ANF is used when linking
subexpressions (as was presumably done here with C<10 + 6>).
=cut

use phi::constQ phi2_front_parser => 0;
use phi::class phi2_front =>
  dialect_frontend_protocol,

  clone => bin q{                       # self cc
    =24 i.heap_allocate                 # self cc new
    sget02 sget01 =24 memcpy            # [new=self]
    sset01 goto                         # new },

  head_anf => bin q{_=8  iplus m64get_ goto},
  tail_anf => bin q{_=16 iplus m64get_ goto},

  link_return => bin q{                 # self cc
    # Side-effectfully link an ANF return onto the tail link here, then return
    # self.
    sget01 .tail_anf .name anf_return   # self cc rlink
    dup sget03 .tail_anf .tail= drop    # self cc rlink
    sget02 =16 iplus m64set goto        # self },

  link_new_tail => bin q{               # t' self cc
    # The new tail is itself a dialect frontend. We need to set our tail link's
    # tail to its head, then update our tail link to its tail.
    sget01 .tail_anf                    # t' self cc self.t
      sget03 .head_anf _ .tail= drop    # t' self cc [self.t.tail=t'.head]
    sget02 .tail_anf
      sget02 =16 iplus m64set           # t' self cc [self.tail=t'.tail]
    sset01 _ goto                       # self },

  parse => bin q{                       # in pos self cc
    phi2_front_parser m64get sset01     # in pos p cc
    sget01 m64get :parse goto           # ->parse(in pos p cc) };

use phi::fn phi2_atom_front => bin q{   # anfnode cc
  =24 i.heap_allocate                   # anf cc front
  $phi2_front_class sget01 m64set       # [.vtable=]
  sget02 sget01 =8 iplus m64set         # [.head_anf=anfnode]
  sget02 sget01 =16 iplus m64set        # [.tail_anf=anfnode]
  sset01 goto                           # front };


1;
