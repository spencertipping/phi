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


=head2 Dialects and parse states
Dialects don't define parse states. Instead, the parse state contains a pointer
to an opaque dialect-owned object that maintains whatever state is required for
the dialect to work. For example, a C-style dialect might track the current
operator precedence, information about the runtime scope, a list of C<typedef>s,
and a dictionary of preprocessor C<define>s.

Dialect context objects provide some negotiation to manage handoff between
languages. There are a few degrees of integration:

1. Comment/whitespace syntax (semantic identity)
2. Symbol resolution, if applicable
3. Surrounding operator precedence, if applicable
4. Expression parsing, if applicable

The "if applicable" stuff is optional and negotiated by a "which features do you
support" protocol. I'm using a bitmask for this because there ultimately aren't
that many different language paradigms we'll need to consider, and because I
don't want things to break if I add a new one down the line.
=cut

use phi::genconst dialect_feature_infix_ops         => bin q{ =1 };
use phi::genconst dialect_feature_symbol_resolution => bin q{ =2 };
use phi::genconst dialect_feature_expressions       => bin q{ =4 };

use phi::protocol nested_dialect =>
  qw/ parent /;

use phi::protocol dialect_negotiation =>
  qw/ feature_bitmask
      semantic_identity
      identifier_to_scopelink
      front_continuation
      active_operator
      with_active_operator
      scope
      with_scope
      operator_allowed?
      expression_parser /;

use phi::protocol dialect_aware_parse_state =>
  qw/ dialect_context
      with_dialect_context /;


=head3 Dialect frontend
This is a small structure that frontends use to build up ANF while parsing. Each
frontend implements the protocol in a potentially different way.

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

use phi::protocol dialect_frontend =>
  qw/ clone
      head_anf
      tail_anf
      link_new_tail
      link_continuation
      link_return
      parse /;

use phi::class dialect_front =>
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

  link_continuation => bin q{           # self cc
    # Turn this expression into a continuation linkage, binding it as a gensym
    # and returning a new front for it.
    gensym                              # self cc cname
    sget02 .tail_anf .name              # self cc cname vname
    sget01 anf_endc                     # self cc cname endc
    sget03 .tail_anf .tail= drop        # self cc cname

    sget02 .head_anf _                  # self cc body cname
    continuation_ctti _ anf_let .[ .] .tail=  # self cc body'

    gensym anf_continuation             # self cc k
    sget02 .clone                       # self cc k new
    sget01 sget01 =8  iplus m64set      # [.head=]
    sget01 sget01 =16 iplus m64set      # [.tail=]
    sset02 drop goto                    # new },

  link_new_tail => bin q{               # t' self cc
    # The new tail is itself a dialect frontend. We need to set our tail link's
    # tail to its head, then update our tail link to its tail.
    sget01 .tail_anf                    # t' self cc self.t
      sget03 .head_anf _ .tail= drop    # t' self cc [self.t.tail=t'.head]
    sget02 .tail_anf
      sget02 =16 iplus m64set           # t' self cc [self.tail=t'.tail]
    sset01 _ goto                       # self },

  parse => bin q{                       # in pos self cc
    sget02 .dialect_context .front_continuation sset01
    sget01 m64get :parse goto           # ->parse(in pos p cc) };

use phi::fn anf_front => bin q{         # anfnode cc
  =24 i.heap_allocate                   # anf cc front
  $dialect_front_class sget01 m64set    # [.vtable=]
  sget02 sget01 =8 iplus m64set         # [.head_anf=anfnode]
  sget02 sget01 =16 iplus m64set        # [.tail_anf=anfnode]
  sset01 goto                           # front };


=head3 Dialect-independent expression parser
This negotiates with the active context to parse an expression if the dialect
supports it. If unsupported, this parser will return a failure state as though
nothing could be parsed.

Here's the struct:

  struct dialect_expression
  {
    hereptr vtable;
    *       active_op;                  # NB: if zero, op isn't modified
  };

=cut

use phi::protocol dialect_expression =>
  qw/ op /;

use phi::class dialect_expression =>
  parser_protocol,
  dialect_expression_protocol,

  op => bin q{_=8 iplus m64get_ goto},

  parse => bin q{                       # in pos self cc
    sget02 .dialect_context
           .feature_bitmask             # in pos self cc bits
    dup dialect_feature_expressions iand# in pos self cc bits exprs?

    [ dialect_feature_infix_ops iand =1 =0 if
      sget02 .op                     =1 =0 if iand
      dup                               # in pos self cc do-op? do-op?
      [ sget03 .op                      # in pos self cc do-op? cc' op
        sget05 .dialect_context
               .with_active_operator    # in pos self cc do-op? cc' ctx'
        sget05 .with_dialect_context    # in pos self cc do-op? cc' pos'
        _ goto ]                        # in pos self cc do-op? pos'
      [ sget04_ goto ]                  # in pos self cc do-op? pos
      if call                           # in pos self cc do-op? pos'

      sget05_                           # in pos self cc do-op? in pos'
      dup .dialect_context
      .expression_parser .parse         # in pos self cc do-op? pos''

      # ...which isn't possible if it's a failure state.
      dup .fail?
      [ sset04 drop sset01 drop goto ]  # pos''
      [ _                               # in pos self cc pos'' do-op?
        [                               # in pos self cc pos''
          sget03 .dialect_context
                 .active_operator       # in pos self cc pos'' op
          sget01 .dialect_context
                 .with_active_operator  # in pos self cc pos'' ctx'
          _.with_dialect_context        # in pos self cc pos'''
          sset03 sset01 drop goto ]     # pos'''
        [ sset03 sset01 drop goto ]     # pos'
        if goto ]
      if goto ]
    [ fail_instance sset03 sset01 drop goto ]
    if goto };

use phi::genconst dialect_expression => bin q{
  =16 i.heap_allocate
  $dialect_expression_class sget01 m64set
  =0                        sget01 =8 iplus m64set };

use phi::fn dialect_expression_op => bin q{   # op cc
  =16 i.heap_allocate
  $dialect_expression_class sget01 m64set
  sget02                    sget01 =8 iplus m64set
  sset01 goto                           # op };


=head3 Dialect-independent scope resolver
This flatmaps an existing parser whose return value should be the name of a
symbol to be resolved. This parser will return a failure state if the
surrounding dialect doesn't support name resolution, or if the name in question
couldn't be resolved.
=cut

use phi::fn dialect_resolve => bin q{   # p cc
  _ [ # First up: figure out whether any of this is at all supported.
      sget01 .dialect_context
             .feature_bitmask           # in pos pos' cc bitmask
        dialect_feature_symbol_resolution iand
      [ sget01 .value                   # in pos pos' cc name
        sget02 .dialect_context
               .identifier_to_scopelink # in pos pos' cc link
        dup .nil?
        [ fail_instance sset03 sset01 drop goto ]
        [ .val .tail_anf                # in pos pos' cc anf
          dup .anf_constant?            # in pos pos' cc anf const?

          # If the ANF node itself produces a constant, then create a new ANF
          # node that returns that same constant and CTTI.
          [ drop dup .ctti anf_gensym   # in pos pos' cc anf ranf
            _ .anf_cvalue _             # in pos pos' cc aval ranf
            .[ .ptr .] anf_front        # in pos pos' cc rfront
            sget02 .with_value          # in pos pos' cc pos''
            sset03 sset01 drop goto ]   # pos''
          [ goto ]
          if call                       # in pos pos' cc anf

          dup .name _ .ctti dup         # in pos pos' cc name ctti ctti
          anf_gensym _                  # in pos pos' cc name anf' ctti

          # If the node has a constant value, then use that instead of
          # retrieving anything from the frame.
          dup .constant?                # in pos pos' cc name anf' ctti const?
          [ _ .cvalue                   # in pos pos' cc name anf' cc' cval
            sget02 .[ .ptr .] sset01    # in pos pos' cc name anf' cc'
            sset01 _ goto ]             # in pos pos' cc anf'
          [ sset00 _                    # in pos pos' cc name cc' anf'
            sget02_ .defstack .[ .]     # in pos pos' cc name cc' anf'
            sset01 goto ]               # in pos pos' cc anf'
          if call

          anf_front sget02 .with_value  # in pos pos' cc pos''
          sset03 sset01 drop goto ]     # pos''
        if goto ]
      [ fail_instance sset03 sset01 drop goto ]
      if goto ] pflatmap
  _ goto };


=head3 Dialect-aware parse state
Pretty simple: this is just a string parse state with a dialect context object
attached to it.

  struct dialect_aware_string_parse_state
  {
    hereptr vtable;
    int64   index;
    *       value;
    *       dialect_context;
  }

=cut

use phi::class dialect_aware_string_parse_state =>
  clone_protocol,
  parse_position_protocol,
  linear_position_protocol,
  dialect_aware_parse_state_protocol,

  "fail?"         => bin q{=0 sset01 goto},
  index           => bin q{_=8  iplus m64get_ goto},
  value           => bin q{_=16 iplus m64get_ goto},
  dialect_context => bin q{_=24 iplus m64get_ goto},

  # NB: shallow clone
  clone => bin q{                       # self cc
    =32 i.heap_allocate                 # self cc new
    sget02 sget01 =32 memcpy            # self cc new [new=self]
    sset01 goto                         # new },

  "+" => bin q{                         # n v self cc
    _.clone                             # n v cc new
    sget02 sget01 =16 iplus m64set      # n v cc new [.value=]
    dup .index sget04 iplus             # n v cc new index'
    sget01 =8 iplus m64set              # n v cc new [.index=]
    sset02 sset00 goto                  # new },

  with_value => bin q{                  # v self cc
    _.clone                             # v cc new
    sget02 sget01 =16 iplus m64set      # v cc new [.value=]
    sset01 goto                         # new },

  with_dialect_context => bin q{        # c self cc
    _.clone                             # c cc new
    sget02 sget01 =24 iplus m64set      # c cc new [.dialect_context=]
    sset01 goto                         # new };

use phi::fn dialect_state => bin q{     # context cc
  =32 i.heap_allocate                   # context cc state
  $dialect_aware_string_parse_state_class sget01 m64set
  =0 sget01 =8  iplus m64set            # [.index=0]
  =0 sget01 =16 iplus m64set            # [.value=0]
  sget02 sget01 =24 iplus m64set        # [.dialect_context=]
  sset01 goto                           # state };


1;
