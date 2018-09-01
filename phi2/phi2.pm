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
  sget01 sget03 .dialect_context .scope # state name cc name scope
  "val"_ .{} .ctti                      # state name cc ctti
  sset01 goto                           # state ctti cc };


=head3 Dialect frontend
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
    phi2_front_parser m64get goto       # ->parser(in pos self cc) };

use phi::fn phi2_atom_front => bin q{   # anfnode cc
  =24 i.heap_allocate                   # anf cc front
  $phi2_front_class sget01 m64set       # [.vtable=]
  sget02 sget01 =8 iplus m64set         # [.head_anf=anfnode]
  sget02 sget01 =16 iplus m64set        # [.tail_anf=anfnode]
  sset01 goto                           # front };


=head3 Parsing expressions
This is just an atom followed by its parse continuation. The atom parser returns
a dialect CTTI, so we can immediately invoke C<.parse> on that value using a
C<flatmap> parser.
=cut

use phi::genconst phi2_atom => bin q{
  decimal_integer
    [ int_ctti anf_gensym               # n cc anf
      .[ sget02 bswap64_ .lit64 .l64 .] # n cc anf[n]
      phi2_atom_front sset01 goto ]     # front
    pmap
  phi2_symbol
    $phi2_resolve_val_fn pmap
  palt };

use phi::genconst phi2_expression_parser => bin q{
  phi2_atom
  [ sget01 dup                          # in pos pos' cc pos' pos'
    sset03 .value sset01                # in pos' front cc
    sget01 m64get :parse goto ]         # ->front.parse(in pos' front cc)
  pflatmap };

use phi::genconst phi2_unary_method_parser => bin q{
  pignore
    "." pstr               pseq_ignore
    pignore                pseq_ignore
    phi2_symbol            pseq_return
    pignore                pseq_ignore
    "(" pstr               pseq_ignore
    pignore                pseq_ignore
    phi2_expression_parser pseq_cons
    pignore                pseq_ignore
    ")" pstr               pseq_ignore };

use phi::fn phi2_front_parse => bin q{  # in pos self cc
  # Let's kick this off by parsing .symbol(arg) for a unary method call.
  sget03 sget03                         # in pos self cc in pos
    phi2_unary_method_parser .parse     # in pos self cc pos'[arg::method]

  dup .fail?
  [ drop sset02 drop _ goto ]           # pos (succeed with no continuation)
  [                                     # in pos self cc pos'
    # First, ask our tail ANF's CTTI (our current value) for the return CTTI of
    # the method call. To do that, we'll need to build up the method name.
    strbuf                              # in pos self cc pos' buf
      sget01 .value .tail _ .append_string
      ":"_                  .append_string
      sget01 .value .head               # in pos self cc pos' buf arg_front
        .head_anf .ctti .name _ .append_string
    .to_string                          # in pos self cc pos' method

    dup sget04 .tail_anf .ctti
                         .return_ctti   # in pos self cc pos' method rctti

    # Now build up the new ANF node. We have the return type, so allocate a
    # gensym. We'll append this to our front after we append our argument.
    anf_gensym                          # in pos self cc pos' m ranf

    # Set up the stack for our method call
    sget04              .tail_anf .name _ .defstack
    sget02 .value .head .tail_anf .name _ .defstack

    # Awesome. Now we can assemble the method body by asking our CTTI to compile
    # the logic.
    .[                                  # in pos self cc pos' m ranf/asm
      _                                 # in pos self cc pos' ranf/asm m
      sget04 .tail_anf .ctti
        .symbolic_method                # in pos self cc pos' ranf/asm'
    .]                                  # in pos self cc pos' ranf
    phi2_atom_front                     # in pos self cc pos' rmethodfront

    # Last step: link stuff up. We'll clone self for this just so we don't
    # destroy anything.
    sget03 .clone                       # i p s cc pos' rmf rfront=self.clone
    sget02 .value .head _               # i p s cc pos' rmf argfront rfront
    .link_new_tail
    .link_new_tail                      # i p s cc pos' rfront
    _ .with_value                       # i p s cc pos''

    # Tail-call into our new value's continuation parser.
    dup .value                          # i p s cc pos'' self'
    sset02 sset02                       # i pos'' self' cc
    sget01 m64get :parse goto ]         # ->self'.parse(i pos'' self' cc)

  if goto };

use phi::genconst phi2_front_parser_init => bin q{
  $phi2_front_parse_fn phi2_front_parser m64set
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

  identifier_to_front => bin q{         # id self cc
    _ .scope sget02 _ .{} sset01 goto   # front };

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

use phi::fn phi2_dialect_test_case => bin q{    # str val cc
  get_stackptr set_frameptr
  sget02 =0 phi2_context dialect_state          # str val cc str pos
  phi2_expression_parser .parse                 # str val cc pos'

  dup .fail? inot sget04 "fail"_ .+ i.assert
  .value
    .link_return .head_anf
    anf_fn ptr_ctti_ "cc"_ .defarg
    .value call                                 # str val cc ret
    sget02 ieq                                  # str val cc ret==val?
    sget03 "val"_ .+ i.assert                   # str val cc

  sset01 drop goto                              # };

use phi::testfn phi2_dialect => bin q{
  get_stackptr set_frameptr
  "3"                =3  phi2_dialect_test_case
  "3.+(4)"           =7  phi2_dialect_test_case
  "3.+(4.*(6)).+(5)" =32 phi2_dialect_test_case
  "100.-(50)"        =50 phi2_dialect_test_case
  "1.<<(4)"          =16 phi2_dialect_test_case
  "1.<(2)"           =1  phi2_dialect_test_case
  "2.<(1)"           =0  phi2_dialect_test_case };


1;
