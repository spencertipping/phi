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


=head2 phi2 language frontend
phi2 begins as three CTTIs:

1. C<baseptr>
2. C<hereptr>, which supports function behavior
3. C<int>: a primitive integer

C<baseptr> and C<hereptr> both support RTTI; C<int> doesn't, and all of its
methods are monomorphic and inline. This means C<int.+> compiles down to a
single C<iplus> instruction, plus any frame-argument addressing (which
unfortunately involves function calls at the moment, though they are direct).

You can use C<baseptr> and C<hereptr> to address phi1 objects, including classes
themselves. This is how you can define new classes/metaclasses. Pointer
arithmetic is possible by converting C<baseptr> instances to C<int> and vice
versa.

phi2 doesn't type-parameterize C<baseptr> and C<hereptr>, but it isn't difficult
to write parameterized versions of those CTTIs in phi2.


=head3 Applicative computed grammars and CTTI parse continuations
CTTIs provide a C<parse> method to parse their continuation. Counterintuitively,
that method doesn't parse the CTTI instance itself; instead, it parses things
you can do to the CTTI. For example:

  3 + 4
   |---| <- int.parse consumes this

phi2's base C<expr> parser produces C<3> and binds it to an ANF variable with
the C<int> CTTI. Mechanically speaking, the parse interacts with ANF nodes like
this:

  341 + 75
  ---                   <- anf_let(name=gen1, refstack=nil, code=[=341], tail=0)
        --              <- anf_let(name=gen2, refstack=nil, code=[=75], tail=0)
      ----              <- anf_let(name=gen3, refstack=[gen2, gen1],
                                   code=[iplus], tail=0) [gen1.tail=gen2,
                                                          gen2.tail=gen3]
  --------              <- gen3 is returned here

Parsers link ANF nodes as continuations are parsed; the return value from any
given parser is the node without a tail. As shown above, linkages follow CPS
parse ordering and not linear text ordering (which makes perfect sense given
that we're targeting concatenative).


=head3 phi2 dialect grammar
TODO

=cut

use phi::genconst phi2_whitespace => bin q{     # cc
  strbuf =32_ .append_int8
         =10_ .append_int8
         =9_  .append_int8
         .to_string
  pmanyof_
  goto };

use phi::genconst phi2_line_comment => bin q{   # cc
  strbuf =10_ .append_int8 .to_string
  .byte_bitset .~                               # cc not-nl
  =0 pmanyset_                                  # p cc
  goto                                          # p };

use phi::genconst phi2_ignore => bin q{ # cc
  phi2_whitespace
  phi2_line_comment palt prep_ignore    # cc ignore-many
  pnone palt                            # cc p
  _ goto                                # p };


1;
