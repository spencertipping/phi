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


=head2 Method call compilation
There's some machinery that happens when method calls are compiled. First, some
dialects support CTTI-specialized methods: C<obj.f(int)> is different from
C<obj.f(string)> etc. phi2 has a standard way of approaching that problem, sort
of like name mangling but more civilized.

Second, we need to link the ANF continuations for the method arguments. If you
use these functions, the linkage always proceeds strictly left to right. I can't
imagine wanting some other default, although languages like C/C++ leave the
ordering unspecified for optimization purposes.
=cut

use phi::fn link_arglist => bin q{      # l lhs cc
  _                                     # l cc lhs
  [ sget02 sget02 .link_new_tail sset02
    =0 sset01 goto ]                    # l cc lhs fn
  sget03 .reduce                        # l cc lhs'
  sset01 goto ]                         # lhs' };

use phi::fn stack_arglist => bin q{     # l anf_let cc
  _                                     # l cc anf_let
  [ sget02 .tail_anf .name
    sget02 .defstack sset02
    =0 sset01 goto ]                    # l cc anf_let fn
  sget03 .reduce                        # l cc anf_let
  sset01 goto                           # anf_let };


=head3 Type signatures (name mangling)
This is pretty simple. phi2 uses a type suffix that is one of two things
depending on how you're compiling stuff:

  obj.method(int, int) -> "method:int,int"      # specialized mode
  obj.method(int, int) -> "method:#2"           # generic mode

A method's arity is absolutely non-negotiable in a concatenative world. If you
want Python or Ruby-style managed arglists, you'll need to either implement a
series of arity-specialized methods or pass in a data structure of some sort.
=cut

use phi::fn arglist_to_sig => bin q{    # l cc
  strbuf                                # l cc buf
  [ sget02 .tail_anf .ctti .name        # anf buf cc name
    sget02 .append_string
    ","_ .append_string sset02          # buf buf cc
    =0 sset01 goto ]                    # l cc buf f
  sget03 .reduce                        # l cc buf'
  dup .size
  [ =1_ .rewind .to_string sset01 goto ]
  [             .to_string sset01 goto ]
  if goto                               # buf' };

use phi::fn specialized_mname => bin q{ # m arglist cc
  strbuf sget03_                 .append_string
         ":"_                    .append_string
         sget02 arglist_to_sig _ .append_string
  .to_string                            # m arglist cc buf
  sset02 sset00 goto                    # buf };

use phi::fn generic_mname => bin q{     # m arglist cc
  strbuf sget03_          .append_string
         ":#"_            .append_string
         sget02 .length _ .append_dec
  .to_string                            # m arglist cc buf
  sset02 sset00 goto                    # buf };


use phi::fn compile_mcall => bin q{     # lhs m args cc
  sget02 sget02 specialized_mname       # lhs m args cc mname
  dup                                   # lhs m args cc mname mname

  sget05 .tail_anf .ctti .return_ctti   # lhs m args cc mname rctti
  anf_gensym                            # lhs m args cc mname ranf
    sget05 .tail_anf .name _ .defstack  # lhs m args cc mname ranf[lhs]
    sget03 .rev _ stack_arglist         # lhs m args cc mname ranf[lhs args]
  .[                                    # lhs m args cc mname ranf/asm
    _                                   # lhs m args cc ranf/asm mname
    sget05 .tail_anf .ctti
      .symbolic_method                  # lhs m args cc ranf/asm'
  .]                                    # lhs m args cc ranf
  anf_front                             # lhs m args cc rmf

  sget04 .clone                         # lhs m args cc rmf lhs'
    sget03_ link_arglist                # lhs m args cc rmf lhs'+args
    .link_new_tail                      # lhs m args cc lhs'+args+rmf
  sset03 sset01 drop goto               # lhs'+args+rmf };


1;
