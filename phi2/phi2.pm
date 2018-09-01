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
=cut

use phi::fn phi2_resolve_val => bin q{  # state name cc
  "phi2 resolve val" i.pnl
  sget01 sget03 .dialect_context .scope # state name cc name scope
  "val"_ .{} .ctti                      # state name cc ctti
  sset01 goto                           # state ctti cc };

use phi::fn phi2_int_rvalue => bin q{   # x cc
  "phi2 int rvalue" i.pnl
  int_ctti anf_gensym                   # x cc anf
  .[ sget02 bswap64_ .lit64 .l64 .]     # x cc anf
  sset01 goto                           # anf };


=head3 Dialect value-CTTI
Although technically phi2 has two distinct CTTIs, one for lvalues and one for
rvalues, I'm just defining one here. The only reason we differentiate between
the two is that rvalues don't parse C<=>; but nothing prevents us from
simplifying by using gensym-lvalues as rvalues.

This means everything I've said about const-by-default is a complete lie in
practice. It's more like "C<const>, wink wink nudge nudge."

Let's talk real quick about how these CTTIs interoperate with ANF. We have
logical CTTIs like C<int> and C<ptr> defined in L<phi2/ctti.pm>; those bind
methods that transform assemblers and provide C<return_ctti>s that tell us how
to parse the return values. All good. ANF CTTIs are always logical, not
dialect-specific.

phi2's dialect CTTI comes into play when we parse stuff or bind variables. In
either case, we get a dialect wrapper that adds phi2-specific syntactic
continuations to the grammar. Structurally it's related to ANF and logical CTTIs
like this:

  phi2_dialect_ctti
    "/phi2/anf" field = anf_let("anf_name", logical_ctti)

C</phi2/anf> is a fictitious field because we always know it at compile-time.
It's probably horrible to store this data this way, but that's never stopped me
before.
=cut

use phi::constQ phi2_expression_parser   => 0;
use phi::constQ phi2_unary_method_parser => 0;

use phi::fn phi2_lvalue_ctti_parse => bin q{   # in pos self cc
  "lvalue ctti parse" i.pnl
  # Let's kick this off by parsing .symbol(arg) for a unary method call.
  sget03 sget03                         # in pos self cc in pos
    phi2_unary_method_parser
      m64get .parse                     # in pos self cc pos'[method::arg]

  "unary method parser result" i.pnl

  dup .fail?
  [ sset03 sset01 drop goto ]           # pos' (fail)
  [                                     # in pos self cc pos'
    # Ok, we need to do some unpacking here. As described above, we can pull the
    # ANF node corresponding to "self" to get the receiver. We'll need that.
    sget02 .fields
      "/phi2/anf"_ .{} .cvalue          # in pos self cc pos' anfnode

    # Now let's get the arg ANF and build the method name.
    strbuf                              # in pos self cc pos' anf buf
      sget02 .value .head _ .append_string
      ":"_                  .append_string
      sget02 .value .tail
        .ctti .name _       .append_string
    .to_string                          # in pos self cc pos' anf mname

    # Build the new ANF node. To do this, we'll need the return CTTI for the
    # method (if it exists).
    dup sget02 .ctti .return_ctti       # in pos self cc pos' anf mname rctti
    anf_gensym                          # in pos self cc pos' anf mname anf'

    # Now assemble code into that ANF node. The argument is always below self on
    # the stack, just like it is in phi1.
    sget03 .value .tail .name _ .defarg
    sget02              .name _ .defarg
    .[
      _                                 # in pos self cc pos' anf anf'asm mname
      sget02 .ctti .symbolic_method     # in pos self cc pos' anf anf'asm
    .]                                  # in pos self cc pos' anf anf'

    # Last step: link ANF tails. Evaluation order should be self->arg->result.
    dup                                 # in pos self cc pos' anf anf' anf'
    sget02 .value .tail .tail=          # in pos self cc pos' anf anf' arg
    sget02 .tail= drop                  # in pos self cc pos' anf anf'
    sget02 .with_value                  # in pos self cc pos' anf pos''
    sset05 drop drop sset01 drop goto ] # pos''

  if goto };

use phi::fn phi2_lvalue_ctti => bin q{  # anf inner-ctti cc
  "phi2 lvalue ctti" i.pnl
  ctti                                  # anf inner cc outer
    sget02 .name _ .defname             # anf inner cc outer
    dup .fields "/phi2/type"_ .ptr      # anf inner cc outer fs
                "/phi2/anf"_  .ptr      # anf inner cc outer fs
      sget03 .fields _ .+=              # anf inner cc outer fs [+=inner.fields]
    drop                                # anf inner cc outer

    sget02_ "/phi2/type"_ .fix          # anf inner cc outer
    sget03_ "/phi2/anf"_  .fix          # anf inner cc outer

  # Now we can drop anf and inner; everything else is constant setup.
  sset01 sset01                         # cc outer

  $phi2_lvalue_ctti_parse_fn _ .defparserfn

  _ goto                                # outer };

use phi::fn phi2_rvalue_ctti => bin q{  # inner-ctti cc
  "rvalue ctti" i.pnl
  _ gensym phi2_lvalue_ctti _ goto      # ctti };


=head3 Parsing expressions
This is just an atom followed by its parse continuation. The atom parser returns
a dialect CTTI, so we can immediately invoke C<.parse> on that value using a
C<flatmap> parser.
=cut

use phi::genconst phi2_atom => bin q{
  decimal_integer
    $phi2_int_rvalue_fn  pmap
    $phi2_rvalue_ctti_fn pmap
  phi2_symbol
    $phi2_resolve_val_fn pmap
  palt };

use phi::genconst phi2_expression_parser_init => bin q{
  phi2_atom
  [ sget01 dup                          # in pos pos' cc pos' pos'
    sset03 .value sset01                # in pos' ctti cc
    sget01 m64get :parse goto ]         # ->ctti.parse(in pos' ctti cc)
  pflatmap
  phi2_expression_parser m64set
  =0 };

use phi::genconst phi2_unary_method_parser_init => bin q{
  pignore
    "." pstr                      pseq_ignore
    pignore                       pseq_ignore
    phi2_symbol                   pseq_return
    pignore                       pseq_ignore
    "(" pstr                      pseq_ignore
    pignore                       pseq_ignore
    phi2_expression_parser m64get pseq_cons
    pignore                       pseq_ignore
    ")" pstr                      pseq_ignore
  phi2_unary_method_parser m64set
  =0 };


=head3 Dialect and context
The last stuff we need before this all works. Or fails.

Here's the struct:

  struct phi2_context
  {
    hereptr             vtable;
    dialect_context*    parent;
    string*             active_operator;
    multichannel_scope* scope;
  };

=cut

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
  expression_parser => bin q{phi2_expression_parser m64get sset01 goto},

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

  identifier_to_ctti => bin q{          # id self cc
    _ .scope sget02 _ .{} sset01 goto   # ctti };

use phi::fn phi2_context => bin q{      # parent cc
  =32 i.heap_allocate                   # parent cc c
  $phi2_context_class sget01 m64set     # [.vtable=]
  sget02 sget01 =8 iplus m64set         # [.parent=]
  "(" sget01 =16 iplus m64set           # [.active_operator=]
  empty_multichannel_scope
    "val"_ .defchannel
    sget01 =24 iplus m64set             # [.scope=]
  sset01 goto                           # c };


=head2 Unit tests
Ready to see breakage? I'm ready to see breakage.
=cut

use phi::testfn phi2_dialect => bin q{
  # Oh god, let's just do it
  "3.+(4)"                              # in
  =0 phi2_context dialect_state         # in pos
  phi2_expression_parser m64get .parse  # pos'

  "got a parse result" i.pnl

  dup .fail? inot "phi2_fail" i.assert  # pos'

  drop
  };


1;
