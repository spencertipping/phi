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

use phi::genconst phi2_op_symbol => bin q{
  "*/%+-<>=!~&^|" poneof prep_bytes
  "()" pstr palt
  ";"  pstr palt };

use phi::genconst phi2_symbol => bin q+
  ident_symbol
  # "([{;" poneof [ strbuf .append_int8 .to_string _ goto ] pmap palt
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

use phi::genconst phi2_str_literal => bin q{
  escaped_string
  "'" pstr ident_symbol pseq_return palt
    [ _ phi1ctti_byte_string anf_gensym # cc s anf
      .[ .ptr .] anf_front _ goto ]     # front
    pmap };

use phi::genconst phi2_atom => bin q{
  phi2_int_literal
  phi2_str_literal            palt
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

use phi::genconst phi2_arglist_parser => bin q{
  "," dialect_expression_op
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
    compile_mcall                       # in pos pos' cc lhs'
    sget02 .with_value                  # in pos pos' cc pos''
    sset03 sset01 drop goto ]           # pos''
  pflatmap };

use phi::genconst phi2_call_parser => bin q{
  pignore
    "(" pstr            pseq_ignore
    pignore             pseq_ignore
    phi2_arglist_parser pseq_return
    pignore             pseq_ignore
    ")" pstr            pseq_ignore

  [ sget02 .value                       # in pos pos' cc lhs
    sget02 .value                       # in pos pos' cc lhs args
    "()"_ compile_mcall                 # in pos pos' cc lhs'
    sget02 .with_value                  # in pos pos' cc pos''
    sset03 sset01 drop goto ]           # pos''
  pflatmap };

use phi::genconst phi2_slice_parser => bin q{
  pignore
    "[" pstr            pseq_ignore
    pignore             pseq_ignore
    phi2_arglist_parser pseq_return
    pignore             pseq_ignore
    "]" pstr            pseq_ignore

  [ sget02 .value                       # in pos pos' cc lhs
    sget02 .value                       # in pos pos' cc lhs args
    "[]"_ compile_mcall                 # in pos pos' cc lhs'
    sget02 .with_value                  # in pos pos' cc pos''
    sset03 sset01 drop goto ]           # pos''
  pflatmap };

use phi::genconst phi2_sliceeq_parser => bin q{
  pignore
    "[" pstr                  pseq_ignore
    pignore                   pseq_ignore
    phi2_arglist_parser       pseq_return
    pignore                   pseq_ignore
    "]" pstr                  pseq_ignore
    pignore                   pseq_ignore
    "=" pstr                  pseq_ignore
    "=" dialect_expression_op pseq_cons

  [ sget02 .value                       # in pos pos' cc lhs
    sget02 .value                       # in pos pos' cc lhs rhs::args
    dup .head _ .tail .<<               # in pos pos' cc lhs [rhs]+args
    "[]="_ compile_mcall                # in pos pos' cc lhs'
    sget02 .with_value                  # in pos pos' cc pos''
    sset03 sset01 drop goto ]           # pos''
  pflatmap };

use phi::genconst phi2_ternary_parser => bin q{
  pignore
    "?" pstr                  pseq_ignore
    "(" dialect_expression_op pseq_return
    pignore                   pseq_ignore
    ":" pstr                  pseq_ignore
    "=" dialect_expression_op pseq_cons

  [ sget02 .value                       # in pos pos' cc lhs
    sget02 .value                       # in pos pos' cc lhs else::then
    dup .head _ .tail                   # in pos pos' cc lhs then else
    compile_ternary                     # in pos pos' cc lhs'
    sget02 .with_value
    sset03 sset01 drop goto ]           # pos''
  pflatmap };

use phi::genconst phi2_special_methods => bin q{
  strmap
    $compile_semi_fn _ ";"_ .{}= };

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

      # Is it a special method? If so, compile it specially.
      sget01 phi2_special_methods .contains?
      [ sget01 phi2_special_methods .{} # in pos pos' cc pos'' lhs m args fn
        call                            # in pos pos' cc pos'' lhs'
        _.with_value                    # in pos pos' cc pos'''
        sset03 sset01 drop goto ]       # pos'''
      [ compile_mcall                   # in pos pos' cc pos'' lhs'
        _ .with_value                   # in pos pos' cc pos'''
        sset03 sset01 drop goto ]       # pos'''
      if goto ]
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
  phi2_ctti_parser    palt
  phi2_call_parser    palt
  phi2_sliceeq_parser palt
  phi2_slice_parser   palt
  phi2_ternary_parser palt
  phi2_op_parser      palt phi2_parse_continuation
  pnone               palt };


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


use phi::fn bind_phi1ctti => bin q{     # scope v name cc
  phi1ctti_ctti sget02 anf_let          # scope v name cc let
  .[ sget03_ .ptr .] anf_front          # scope v name cc front
  sget02 sget05 "val"_ .{}=             # scope v name cc scope'
  sset03 sset01 drop goto               # scope' };

use phi::fn phi2_context => bin q{      # parent cc
  =32 i.heap_allocate                   # parent cc c
  $phi2_context_class sget01 m64set     # [.vtable=]
  sget02 sget01 =8 iplus m64set         # [.parent=]
  "(" sget01 =16 iplus m64set           # [.active_operator=]
  empty_multichannel_scope
    "val"_ .defchannel

    int_ctti      "int"  bind_phi1ctti
    ptr_ctti      "ptr"  bind_phi1ctti
    phi1ctti_list "list" bind_phi1ctti

    phi1ctti_interpreter "I" anf_let
      .[ .get_interpptr .] anf_front _ "I"_ "val"_ .{}=

    let_ctti       "let"  anf_let .[ .] anf_front _ "let"_  "val"_ .{}=
    fndef_ctti     "fn"   anf_let .[ .] anf_front _ "fn"_   "val"_ .{}=
    phi1_ctor_ctti "phi1" anf_let .[ .] anf_front _ "phi1"_ "val"_ .{}=

    sget01 =24 iplus m64set             # [.scope=]
  sset01 goto                           # c };


use phi::fn phi2_compile_fn => bin q{   # source args cc
  sget02 =0 phi2_context dialect_state
  "(" dialect_expression_op .parse      # source args cc pos
  .value
    .link_return .head_anf
    anf_fn                              # source args cc anf
    [ sget02 .head sget03 .tail         # ctti::argname anf cc ctti arg
      sget03 .defarg sset02             # anf anf cc
      =0 sset01 goto ]                  # source args cc anf rfn
    sget03 .reduce                      # source args cc anf'
    here_ctti_ "cc"_ .defarg            # source args cc anf''
  #dup strbuf_ .inspect .to_string i.pnl
  .compile
  #dup bytecode_to_string i.pnl
  .here                                 # source args cc f
  sset02 sset00 goto                    # f };


=head2 Unit tests
Ready to see breakage? I'm ready to see breakage.


=head3 Basic expression-level parse tests
Basically, does the grammar work? Do parse continuations work?
=cut

use phi::fn phi2_dialect_expr_test_case => bin q{ # str val cc
  get_stackptr set_frameptr

  sget02 =0 phi2_context dialect_state          # str val cc str pos
  "(" dialect_expression_op .parse              # str val cc pos'

  dup .fail? inot sget04 "fail " .+ i.assert
  dup .index sget04 .size ieq sget04 "partial " .+ i.assert
  .value
    .link_return .head_anf
    #dup strbuf_ .inspect .to_string i.pnl
    anf_fn ptr_ctti_ "cc"_ .defarg
    #dup strbuf_ .inspect .to_string i.pnl
    .compile .call                              # str val cc ret
    #.compile dup bytecode_to_string i.pnl .call
    sget02                                      # str val cc ret val
    sget01 sget01 ieq
    $ansi_clear =2 i.print_string_fd
    [ =1 sget06 "val " .+ i.assert
      sset01 drop goto ]
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

  "1;2"              =2  phi2_dialect_expr_test_case
  "1;2;3"            =3  phi2_dialect_expr_test_case

  "1.if(3+4, 5+6)"   =7  phi2_dialect_expr_test_case
  "0.if(3+4, 5+6)"   =11 phi2_dialect_expr_test_case

  "8.bswap16" lit16 0800 phi2_dialect_expr_test_case
  "8.bswap16.bswap16" =8 phi2_dialect_expr_test_case

  "3.to_ptr.to_int.to_ptr" =3 phi2_dialect_expr_test_case

  "'foo.size" =3 phi2_dialect_expr_test_case

  "3 + 4"         =7  phi2_dialect_expr_test_case
  "3 + 4 * 5"     =23 phi2_dialect_expr_test_case
  "3 * 4 + 5"     =17 phi2_dialect_expr_test_case
  "3 * ( 4 + 5 )" =27 phi2_dialect_expr_test_case

  "(3 + 4) * 5" =35 phi2_dialect_expr_test_case

  "1 ? 2 : 3"           =2 phi2_dialect_expr_test_case
  "1 ? I : 3"            i phi2_dialect_expr_test_case
  "0 ? I.die('bif) : 3" =3 phi2_dialect_expr_test_case
  "1 ? 2 : I.die('bar)" =2 phi2_dialect_expr_test_case

  "let x = 5"                        =5 phi2_dialect_expr_test_case

  "let x = 5; x"                     =5 phi2_dialect_expr_test_case
  "let x = 5; let y = x + 1; x + y" =11 phi2_dialect_expr_test_case

  "let q = let; q x = 5; x" =5 phi2_dialect_expr_test_case
  "let foo = let; foo bar = foo; bar x = 6; x * x" =36
    phi2_dialect_expr_test_case };


=head3 phi2 fn tests
Making sure function definition works as advertised.
=cut

use phi::testfn phi2_fns => bin q{
  get_stackptr set_frameptr
  "(fn(x:int) x).()(5)" intlist phi2_compile_fn
    call =5 ieq "phi2 identity" i.assert

  "(fn(x:int) x+1)(5)" intlist phi2_compile_fn
    call =6 ieq "basic phi2 fn" i.assert

  "let q = fn; let f = q(x:int) x + 1; f(10)" intlist phi2_compile_fn
    call =11 ieq "indirect fn stuff" i.assert

  "(fn(x:int, y:int) x + y * 2)(5, 6)" intlist phi2_compile_fn
    call =17 ieq "binary fn" i.assert

  # Make sure the scope doesn't escape
  "let x = 10; (fn(x:ptr) x.to_int)(5.to_ptr); x" intlist phi2_compile_fn
    call =10 ieq "fnscope" i.assert };


=head3 phi1 linkage tests
Can we interoperate with phi1 successfully? Let's try using the macro assembler
object and see where we get.
=cut

use phi::testfn phi2_phi1_interop => bin q{
  get_stackptr set_frameptr
  "phi1.asm.goto.compile.here" intlist phi2_compile_fn
    call =6_ call =6 ieq "fn6" i.assert

  "phi1.asm.swap.lit8.l8(5).iplus.swap.goto.compile.here" intlist
    phi2_compile_fn
    call =6_ call =11 ieq "fn11" i.assert

  "fn(xs:list) xs[0]" intlist phi2_compile_fn
    call
    intlist =3_ .<< _ call
    =3 ieq "fnx[0]" i.assert

  "fn(xs:list) (xs[0] = (xs[0].to_int + 1).to_ptr; xs)" intlist phi2_compile_fn
    call
    intlist =8_ .<< _ call
    =0_ .[] =9 ieq "fnx[0]=" i.assert

  strbuf
    "let asm = phi1.asm;"_   .append_string
    "asm.swap;"_             .append_string
    "asm.lit8.l8(5).iplus;"_ .append_string
    "asm.swap.goto;"_        .append_string
    "asm.compile.here"_      .append_string
  .to_string intlist phi2_compile_fn
    call =6_ call =11 ieq "fn11_let" i.assert };


1;
