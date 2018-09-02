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
  qw/ operator_precedence /;


=head3 Base parsers
phi2 resolves identifiers, operators, and delimiters as symbols, all using the
same scoping mechanism. Operators are resolved into CTTIs that implement prefix
unary things, e.g. C<-3> is parsed as C<-> with a high-precedence parse
continuation of C<3>.

Delimiters are also CTTIs that parse expressions and return them or some variant
of them. C<(3 + 4)> is parsed as C<(> (a CTTI) whose continuation is C<3 + 4>
followed by a required C<)>.
=cut

use phi::genconst phi2_op_symbol => bin q{"*/%+-<>=!~&^|?:" poneof prep_bytes};
use phi::genconst phi2_symbol => bin q+
  ident_symbol
  "([{;" poneof [ strbuf .append_int8 .to_string _ goto ] pmap palt
  phi2_op_symbol palt +;


=head3 Parsing expressions
This is just an atom followed by its parse continuation. The atom parser returns
a dialect front, so we can immediately invoke C<.parse> on that value using a
C<flatmap> parser.
=cut

use phi::genconst phi2_paren_parser => bin q{
  pignore
  "(" pstr                  pseq_ignore
  pignore                   pseq_ignore
  "(" dialect_expression_op pseq_return
  pignore                   pseq_ignore
  ")" pstr                  pseq_ignore };

use phi::genconst phi2_int_literal => bin q{
  decimal_integer
    [ int_ctti anf_gensym               # n cc anf
      .[ sget02 bswap64_ .lit64 .l64 .] # n cc anf[n]
      anf_front sset01 goto ]           # front
    pmap };

use phi::genconst phi2_atom => bin q{
  phi2_int_literal
  phi2_paren_parser           palt
  phi2_symbol dialect_resolve palt };

use phi::fn phi2_parse_continuation => bin q{ # p cc
  _                                     # cc p
  [ sget01 dup                          # in pos pos' cc pos' pos'
    sset03 .value sset01                # in pos' front cc
    sget01 m64get :parse goto ]         # ->front.parse(in pos' front cc)
  pflatmap                              # cc p'
  _ goto                                # p' };

use phi::genconst phi2_expression_parser => bin q{
  pignore
  phi2_atom phi2_parse_continuation pseq_return };

use phi::fn phi2_arglist_to_sig => bin q{ # l cc
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

use phi::fn phi2_link_arglist => bin q{ # l lhs cc
  _                                     # l cc lhs
  [ sget02 sget02 .link_new_tail sset02
    =0 sset01 goto ]                    # l cc lhs fn
  sget03 .reduce                        # l cc lhs'
  sset01 goto ]                         # lhs' };

use phi::fn phi2_stack_arglist => bin q{# l anf_let cc
  _                                     # l cc anf_let
  [ sget02 .tail_anf .name
    sget02 .defstack sset02
    =0 sset01 goto ]                    # l cc anf_let fn
  sget03 .reduce                        # l cc anf_let
  sset01 goto                           # anf_let };

use phi::fn phi2_specialized_mname => bin q{    # m arglist cc
  strbuf sget03_                      .append_string
         ":"_                         .append_string
         sget02 phi2_arglist_to_sig _ .append_string
  .to_string                            # m arglist cc buf
  sset02 sset00 goto                    # buf };

use phi::fn phi2_generic_mname => bin q{# m arglist cc
  strbuf sget03_          .append_string
         ":#"_            .append_string
         sget02 .length _ .append_dec
  .to_string                            # m arglist cc buf
  sset02 sset00 goto                    # buf };

use phi::fn phi2_compile_mcall => bin q{# lhs m args cc
  sget02 sget02 phi2_specialized_mname  # lhs m args cc mname
  dup                                   # lhs m args cc mname mname

  sget05 .tail_anf .ctti .return_ctti   # lhs m args cc mname rctti
  anf_gensym                            # lhs m args cc mname ranf
    sget05 .tail_anf .name _ .defstack  # lhs m args cc mname ranf[lhs]
    sget03 .rev _ phi2_stack_arglist    # lhs m args cc mname ranf[lhs args]
  .[                                    # lhs m args cc mname ranf/asm
    _                                   # lhs m args cc ranf/asm mname
    sget05 .tail_anf .ctti
      .symbolic_method                  # lhs m args cc ranf/asm'
  .]                                    # lhs m args cc ranf
  anf_front                             # lhs m args cc rmf

  sget04 .clone                         # lhs m args cc rmf lhs'
    sget03_ phi2_link_arglist           # lhs m args cc rmf lhs'+args
    .link_new_tail                      # lhs m args cc lhs'+args+rmf
  sset03 sset01 drop goto               # lhs'+args+rmf };

use phi::genconst phi2_arglist_parser => bin q{
  "(" dialect_expression_op
    "," pstr pnone palt pseq_ignore
  prep_intlist };

use phi::genconst phi2_method_parser => bin q{
  pignore
    "." pstr            pseq_ignore
    pignore             pseq_ignore
    phi2_symbol         pseq_return

  pignore
    "(" pstr            pseq_ignore
    pignore             pseq_ignore
    phi2_arglist_parser pseq_return
    pignore             pseq_ignore
    ")" pstr            pseq_ignore
  pnone [ intlist sset01 goto ] pmap palt
  pseq_cons

  [ sget02 .value                       # in pos pos' cc lhs
    sget02 .value .tail                 # in pos pos' cc lhs m
    sget03 .value .head                 # in pos pos' cc lhs m args
    phi2_compile_mcall                  # in pos pos' cc lhs'
    sget02 .with_value                  # in pos pos' cc pos''
    sset03 sset01 drop goto ]           # pos''
  pflatmap };

use phi::genconst phi2_op_parser => bin q{
  pignore phi2_op_symbol pseq_return
  [ sget01 .value                       # in pos pos' cc op
    sget02 .dialect_context
           .operator_allowed?           # in pos pos' cc opok?
    [ # Parse the RHS using a modified dialect context to reflect the new
      # operator precedence.
      sget03 sget02 dup .value          # in pos pos' cc in pos' op
      dialect_expression_op .parse      # in pos pos' cc pos''

      dup .fail?
      [ drop sset03 sset01 drop goto ]
      [ goto ]
      if call

      # Compile the method call, returning the new front object.
      sget03 .value                     # in pos pos' cc pos'' lhs
      sget03 .value                     # in pos pos' cc pos'' lhs m
      sget02 .value intlist .<<         # in pos pos' cc pos'' lhs m args
      phi2_compile_mcall                # in pos pos' cc pos'' lhs'
      _ .with_value                     # in pos pos' cc pos'''
      sset03 sset01 drop goto ]         # pos'''
    [ sset02 drop drop $fail_instance _ goto ]
    if goto ]
  pflatmap };

use phi::class phi2_ctti_parser =>
  parser_protocol,

  parse => bin q{                       # in pos self cc
    sget02 .value .tail_anf .ctti sset01# in pos v cc
    sget01 m64get :parse goto           # ->v.parse(in pos v cc) };

use phi::constQ phi2_ctti_parser => phi2_ctti_parser_class->fn >> heap;

use phi::genconst phi2_front_parser => bin q{
  phi2_method_parser
  phi2_ctti_parser palt
  phi2_op_parser   palt phi2_parse_continuation
  pnone            palt };


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

  feature_bitmask    => bin q{phi2_dialect_features  sset01 goto},
  semantic_identity  => bin q{hash_comment_ignore    sset01 goto},
  expression_parser  => bin q{phi2_expression_parser sset01 goto},
  front_continuation => bin q{phi2_front_parser      sset01 goto},

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

  identifier_to_scopelink => bin q{     # id self cc
    _ .scope sget02_                    # id cc id scope
    "val"_ .{} sset01 goto              # link };

use phi::fn phi2_context => bin q{      # parent cc
  =32 i.heap_allocate                   # parent cc c
  $phi2_context_class sget01 m64set     # [.vtable=]
  sget02 sget01 =8 iplus m64set         # [.parent=]
  "(" sget01 =16 iplus m64set           # [.active_operator=]
  empty_multichannel_scope
    "val"_ .defchannel

    let_ctti "let" anf_let anf_front _
    "let"_ "val"_ .{}=

    sget01 =24 iplus m64set             # [.scope=]
  sset01 goto                           # c };


=head2 Unit tests
Ready to see breakage? I'm ready to see breakage.
=cut

use phi::fn phi2_dialect_expr_test_case => bin q{ # str val cc
  get_stackptr set_frameptr

  sget02 =0 phi2_context dialect_state          # str val cc str pos
  "(" dialect_expression_op .parse              # str val cc pos'

  dup .fail? inot sget04 "fail " .+ i.assert
  dup .index sget04 .size ieq sget04 "partial " .+ i.assert
  .value
    .link_return .head_anf
    anf_fn ptr_ctti_ "cc"_ .defarg
    .value call                                 # str val cc ret
    sget02                                      # str val cc ret val
    sget01 sget01 ieq
    [ sset01 drop goto ]
    [ strbuf                                    # str val cc ret val cc' buf
        =10_           .append_int8
        sget06_        .append_string
        ": expected "_ .append_string
        sget02_        .append_dec
        ", but got "_  .append_string
        sget03_        .append_dec
        =10_           .append_int8
      .to_string i.pnl_err                      # str val cc ret val cc'
      drop ieq
      sget03 "val " .+ i.assert ]               # str val cc
    if call

  sset01 drop goto                              # };

use phi::testfn phi2_dialect_expressions => bin q{
  get_stackptr set_frameptr
  "3"                =3  phi2_dialect_expr_test_case
  "3.+(4)"           =7  phi2_dialect_expr_test_case
  " 3 .   + ( 4 )"   =7  phi2_dialect_expr_test_case
  "3.+(4.*(6)).+(5)" =32 phi2_dialect_expr_test_case
  "100.-(50)"        =50 phi2_dialect_expr_test_case
  "1.<<(4)"          =16 phi2_dialect_expr_test_case
  "1.<(2)"           =1  phi2_dialect_expr_test_case
  "2.<(1)"           =0  phi2_dialect_expr_test_case

  "1.if(2,3)"        =2  phi2_dialect_expr_test_case
  "1.if(3+4, 5+6)"   =7  phi2_dialect_expr_test_case
  "0.if(3+4, 5+6)"   =11 phi2_dialect_expr_test_case

  "8.bswap16" lit16 0800 phi2_dialect_expr_test_case
  "8.bswap16.bswap16" =8 phi2_dialect_expr_test_case

  "3.to_ptr.to_int"        =3 phi2_dialect_expr_test_case
  "3.to_ptr.to_int.to_ptr" =3 phi2_dialect_expr_test_case

  "3+4"     =7  phi2_dialect_expr_test_case
  "3 + 4"   =7  phi2_dialect_expr_test_case
  "3+4*5"   =23 phi2_dialect_expr_test_case
  "3*4+5"   =17 phi2_dialect_expr_test_case
  "3*(4+5)" =27 phi2_dialect_expr_test_case

  "(3+4)*5"     =35 phi2_dialect_expr_test_case
  "(3 + 4) * 5" =35 phi2_dialect_expr_test_case

  "let x = 5 in x"                           =5 phi2_dialect_expr_test_case
  "let x = 5 in x + 1"                       =6 phi2_dialect_expr_test_case
  "let x = 5 in let y = 6     in x + y"     =11 phi2_dialect_expr_test_case
  "let x = 5 in let y = x + 1 in x + y"     =11 phi2_dialect_expr_test_case
  "1 + let x = 5 in let y = x + 1 in x + y" =12 phi2_dialect_expr_test_case };


1;
