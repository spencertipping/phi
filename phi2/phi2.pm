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


=head3 Dialect context structure
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
    hereptr             vtable;
    dialect_context*    parent;
    string*             active_operator;
    multichannel_scope* scope;
  };

=cut

# TODO: refactor operator precedence to use a scope channel that binds to CTTIs
# with parse continuations

use phi::genconst phi2_operator_precedence => bin q{
  strmap =1_  "."_   .{}=
         =1_  "()"_  .{}=
         =2_  "*"_   .{}=
         =2_  "/"_   .{}=
         =2_  "%"_   .{}=
         =3_  "+"_   .{}=
         =3_  "-"_   .{}=
         =4_  "<<"_  .{}=
         =4_  ">>"_  .{}=
         =4_  ">>>"_ .{}=
         =5_  "<"_   .{}=
         =5_  "<="_  .{}=
         =5_  ">"_   .{}=
         =5_  ">="_  .{}=
         =6_  "=="_  .{}=
         =6_  "!="_  .{}=
         =7_  "&"_   .{}=
         =8_  "|"_   .{}=
         =8_  "^"_   .{}=
         =9_  "&&"_  .{}=
         =10_ "||"_  .{}=
         =11_ "="_   .{}=
         =12_ "and"_ .{}=
         =13_ "or"_  .{}=
         =14_ ","_   .{}=
         =15_ ";"_   .{}= };

use phi::protocol phi2_context =>
  qw/ active_operator
      with_active_operator
      operator_precedence
      scope
      with_scope /;


=head3 Base parsers
phi2 resolves identifiers, operators, and delimiters as symbols, all using the
same scoping mechanism. Operators are resolved into CTTIs that implement prefix
unary things, e.g. C<-3> is parsed as C<-> with a high-precedence parse
continuation of C<3>.

Delimiters are also CTTIs that parse expressions and return them or some variant
of them. C<(3 + 4)> is parsed as C<(> (a CTTI) whose continuation is C<3 + 4>
followed by a required C<)>.
=cut

use phi::genconst phi2_symbol => bin q$
  ident_symbol
  "([{,;" poneof [ strbuf .append_int8 .to_string _ goto ] pmap palt
  ".*/%+-<>=!~&^|" poneof prep_bytes palt $;


=head3 Scope interaction
phi2 stores all atom-resolvable symbols in a single scope channel called "val",
so this is pretty straightforward. We can use a regular C<map> parser to do the
resolution because the parse state is the second argument (and second return
value).

We need to return a single value that provides a few pieces of information:

1. The CTTI of the value being encoded
2. The ANF symbol of that value, if one exists
3. The constant status of the CTTI (in this case, using C<ctti.fix>)

The contract here is that we use an ANF node iff the CTTI has any runtime
variance. This means we'll constant-fold as much as humanly possible, and it
means we have separate compile-time and runtime evaluation schedules.

NB: we'll need separate notations for "bind a constant" and "bind a runtime
value initialized to a constant"; sort of like C++ C<constexpr>.
=cut

use phi::fn phi2_resolve_val => bin q{  # state name cc
  sget01 sget03 .dialect_context .scope # state name cc name scope
  "val"_ .{}                            # state name cc scopelink
  sset01 goto                           # state scopelink cc };

use phi::genconst phi2_atom => bin q{
  # Q: what gets returned from here? It needs to be some sort of CTTI that knows
  # its ANF linkage or something; we can't use scope links because those don't
  # work for literals.
  phi2_symbol $phi2_resolve_val_fn pmap };


=head3 Parsing expressions
This is just an atom followed by its parse continuation.
=cut

use phi::genconst phi2_expression_parser => bin q{
  "TODO" };


use phi::genconst phi2_dialect_features => bin q{
  dialect_feature_infix_ops
  dialect_feature_symbol_resolution ior
  dialect_feature_expressions       ior };

use phi::class phi2_context =>
  nested_dialect_protocol,
  dialect_negotiation_protocol,
  phi2_context_protocol,

  parent          => bin q{_=8  iplus m64get_ goto},
  active_operator => bin q{_=16 iplus m64get_ goto},
  scope           => bin q{_=24 iplus m64get_ goto},

  feature_bitmask   => bin q{phi2_dialect_features  sset01 goto},
  semantic_identity => bin q{hash_comment_ignore    sset01 goto},
  expression_parser => bin q{phi2_expression_parser sset01 goto},

  with_active_operator => bin q{        # op self cc
    =32 i.heap_allocate                 # op self cc new
    sget02 sget01 =32 memcpy            # [new=self]
    sget03 sget01 =16 iplus m64set      # [new.op=]
    sset02 sset00 goto                  # new },

  with_scope => bin q{                  # s self cc
    =32 i.heap_allocate                 # s self cc new
    sget02 sget01 =32 memcpy            # [new=self]
    sget03 sget01 =24 iplus m64set      # [new.scope=]
    sset02 sset00 goto                  # new },

  # TODO: extend to full opgate so we're associativity-aware
  operator_precedence => bin q{         # op self cc
    sset00                              # op cc
    sget01 phi2_operator_precedence .contains?
    [ _ phi2_operator_precedence .{} _ goto ]
    [ =127 sset01 goto ]
    if goto                             # prec },

  "operator_allowed?" => bin q{         # op self cc
    _ dup.active_operator               # op cc self aop
    sget01 .operator_precedence         # op cc self ap
    _ sget03 _.operator_precedence      # op cc ap prec
    ilt sset01 goto                     # allowed? },

  identifier_to_anf => bin q{           # id self cc
    _ .scope sget02 _ .{} sset01 goto   # anf };


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
