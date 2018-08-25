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
Let's start by defining the dialect context; then we can write the core CTTIs.


=head3 Dialect context
phi2 maintains a single block-scope chain with no syntactic lexical capture. The
same scope is used for both compile-time and runtime quantities, although
compile-only values are omitted from the backend ANF.

The dialect context also maintains the current operator and filters following
operators accordingly. The "current operator" means "the one preceding the
expression whose continuation is now being parsed" and works like this:

  3 + 4 * 5 + 7
       ^   ^
       |   |
       |   current operator = *, then +, then none
       |
       current operator = +, then none

This is managed entirely by phi2 CTTI instances. Basically, each one records the
current operator coming into it and either accepts or rejects the following
parse on that basis. This causes some of the parse continuations to fail, which
kicks control back to lower-precedence expressions.

Here's the struct:

  struct phi2_context
  {
    hereptr          vtable;
    dialect_context* parent;
    string*          active_operator;
    phi2_scope*      scope;
  };

=cut

use phi::genconst phi2_operator_precedence => bin q{
  strmap =1_ "."_   .{}=
         =1_ "()"_  .{}=
         =2_ "*"_   .{}=
         =2_ "/"_   .{}=
         =2_ "%"_   .{}=
         =3_ "+"_   .{}=
         =3_ "-"_   .{}=
         =4_ "<<"_  .{}=
         =4_ ">>"_  .{}=
         =4_ ">>>"_ .{}=
         =5_ "<"_   .{}=
         =5_ "<="_  .{}=
         =5_ ">"_   .{}=
         =5_ ">="_  .{}=
         =6_ "=="_  .{}=
         =6_ "!="_  .{}=
         =7_ "&"_   .{}=
         =8_ "|"_   .{}=
         =8_ "^"_   .{}= };

use phi::genconst phi2_dialect_features => bin q{
  dialect_feature_infix_ops
  dialect_feature_symbol_resolution ior
  dialect_feature_expressions       ior };

use phi::protocol phi2_context =>
  qw/ active_operator
      with_active_operator
      operator_precedence
      scope
      with_scope /;

use phi::class phi2_context =>
  nested_dialect_protocol,
  dialect_negotiation_protocol,
  phi2_context_protocol,

  parent => bin q{_=8  iplus m64get_ goto},
  scope  => bin q{_=24 iplus m64get_ goto},

  feature_bitmask   => bin q{phi2_dialect_features sset01 goto},
  semantic_identity => bin q{hash_comment_ignore   sset01 goto},

  with_scope => bin q{                  # s self cc
    =32 i.heap_allocate                 # s self cc new
    sget02 sget01 =32 memcpy            # [new=self]
    sget03 sget01 =24 iplus m64set      # [new.scope=]
    sset02 sset00 goto                  # new },

  operator_precedence => bin q{         # op self cc
    sset00                              # op cc
    _ phi2_operator_precedence .{} _ goto },

  ;


=head3 Runtime CTTIs
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
=cut


1;
