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
  [ =1_ .rewind sset01 goto ]
  [             sset01 goto ]
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


=head3 Common special methods
There are some methods that don't really behave like normal functions. Some of
them are weird because they use continuations (e.g. C<&&>, C<?:>), while others
just present an awkward degree of CTTI variance (e.g. C<;>).
=cut

use phi::fn compile_semi => bin q{      # lhs m args cc
  sget01 .length =1 ieq
  [ goto ]
  [ "compile_semi expects exactly one argument in the arglist" i.die ]
  if call

  _ =0_ .[]                             # lhs m cc rhs

  # Link the RHS directly onto the LHS as a new tail, then return the newly
  # modified LHS.
  sget03 .clone .link_new_tail          # lhs m cc lhs'
  sset02 sset00 goto                    # lhs' };


use phi::fn compile_ternary => bin q{   # lhs then else cc
  # Assume the "then" CTTI is what gets returned even though we don't strictly
  # require it.
  sget02 .tail_anf .ctti                # lhs then else cc rctti
  sget03 .link_continuation             # lhs then else cc rc thenc
  sget03 .link_continuation             # lhs then else cc rc thenc elsec
  intlist .<< .<<                       # lhs then else cc rc args[thenc,elsec]

  sget05_                               # lhs t e cc rc lhs args
  "if"_ compile_mcall _                 # lhs t e cc lhs' rc
  anf_gensym .[ .call .]                # lhs t e cc lhs' rlet
  sget01 .tail_anf .name _ .defstack    # lhs t e cc lhs' rlet'
  anf_front _ .link_new_tail            # lhs t e cc lhs''
  sset03 sset01 drop goto               # lhs'' };


use phi::fn compile_ss_and => bin q{    # lhs m args cc
  # a && b is the same as a ? b : a, but with the second occurrence of "a"
  # cached; that is, we don't repeat side effects. To do this, we need to
  # let-bind "a" by snapshotting its tail.

  _ =0_ .[] sset01                      # lhs rhs cc

  sget02 .tail_anf dup .name _ .ctti    # lhs rhs cc ln lt
  anf_gensym .defstack .[ .] anf_front  # lhs rhs cc lhs'
  sget03 _                              # lhs rhs cc lhs lhs'
  sget03 _ compile_ternary              # lhs rhs cc branch
  sset02 sset00 goto                    # branch };

use phi::fn compile_ss_or => bin q{     # lhs m args cc
  _ =0_ .[] sset01                      # lhs rhs cc
  sget02 .tail_anf dup .name _ .ctti    # lhs rhs cc ln lt
  anf_gensym .defstack .[ .] anf_front  # lhs rhs cc lhs'
  sget03 _                              # lhs rhs cc lhs lhs'
  sget03 compile_ternary                # lhs rhs cc branch
  sset02 sset00 goto                    # branch };


1;
