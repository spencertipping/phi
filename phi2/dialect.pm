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


=head2 Dialects
Simply put, a dialect is a type of parse state. It does the usual value+offset
pairing, but also provides a series of methods to construct parsers for the
language being modeled. For example, C<dialect.infix_op> returns a parser that
handles infix operators at the correct precedence. A CTTI would use its output
to map the operator to a method call.

There are several such methods that correspond to common syntactic patterns.
When a dialect doesn't support something, e.g. Lisp not supporting infix ops,
the dialect will return a parser that always fails.

Some details about the parsers dialects provide:

  whitespace   = anything with no semantic meaning, including comments
  identifier   = any name that could be bound to a value
  atom         = any value that parses the same way regardless of precedence
  expression   = any value in general
  infix_op(op) = any operator that can bind rightwards of an "op"
  infix_method = method calling syntax, e.g. "foo.bar()"

=cut

use phi::protocol dialect_parsers =>
  qw/ whitespace
      identifier
      atom
      expression
      infix_op
      infix_method /;


=head3 Dialects and scoping
In addition to structuring the language syntax, dialects also implement the
scoping model of the language they're defining. Normally the scope interaction
is entirely owned by dialect-specific CTTIs like C<int> (which for a C-style
dialect would bind to a C-specific type wrapper, since its syntax doesn't
generalize), but there are cases where a generic CTTI might want to modify a
scope. Let's talk about what that looks like by designing a hypothetical
extension.

=head4 Example: Super-Destructuring C<let> for every language
Most languages don't provide destructuring binds, but we deserve better. So we
want a dialect-independent C<let> CTTI that starts with this:

  let [x, y, z] = xs;

...and rewrites it as this:

  auto x = xs[0];
  auto y = xs[1];
  auto z = xs[2];

Now, C<let> isn't a syntactic transformation: unlike a Lisp macro, it doesn't
generate source code. Instead, it asks the dialect directly for new variable
bindings and emits IR nodes to initialize them. The goal is to be
indistinguishable from whatever the dialect's variable bindings would have been,
without C<let> needing to be aware of anything dialect-specific.

All of this is straightforward from the IR side: C<let> can add new
local-variable entries to the frame and create C<ir_val> nodes to address them.
Dialects and scopes are involved only because we need to create identifiers that
refer to those new IR-level entries. And that means dialects need to support
some sort of unified scope modification API.

=head4 Managing scoped bindings
From the CTTI's perspective, there are basically four things you might want to
do:

1. Bind a literal within an expression (then unbind it)
2. Bind a literal from now on (within the current scope, presumably)
3. Bind a runtime value within an expression
4. Bind a runtime value from now on

Some dialects may support additional nuance like global vs local, namespaces,
access modifiers, etc, but those aren't universal concepts.

  .child  = return a dialect whose scope is a child of this one
  .parent = return the dialect whose scope is the parent
  .fn     = the ir_fn whose locals we're mapping to

  .define(name, ctti) = request a new arg/local/global binding

Later on, parent/child will also refer to dialects themselves, e.g. for language
embeddings. For now there's only one type of dialect so this isn't really an
issue.
=cut

use phi::protocol dialect_scoping =>
  qw/ child
      parent
      fn
      resolve_constant
      resolve_local
      define /;


=head3 phi2 dialect
The dialect is at its core a parse state that links to an object that stores a
mutable dialect state. This mutability carries an important condition: any CTTI
that modifies that mutable state must fully commit to its parsing alternative.
If a C<palt> backtracks beyond a mutable update, then we'll have an incorrect
state and no way to detect this failure.

Let's start by defining the inner mutable object, which contains a few pieces of
state:

  struct phi2_state
  {
    hereptr           class;
    phi2_state*       parent;
    ir_fn*            fn;
    map<hash, ctti*>* args;
    map<hash, ctti*>* locals;           # NB: null if we're defining args
    map<hash, *>*     constant_values;
    map<hash, ctti*>* constant_cttis;
  };

NB: in the code below, I use "name" to refer to the hash of a name. It doesn't
strictly need to be a hash, it just needs to be some integer value that has a
1:1 mapping to strings.
=cut

use phi::protocol phi2_state =>
  qw/ parent
      parent=
      fn
      args
      locals
      constant_values
      constant_cttis
      finalize_args
      resolve_constant
      resolve_local
      defconst
      defvar /;

use phi::class phi2_state =>
  phi2_state_protocol,

  parent          => bin q{ _ =8  iplus m64get _ goto },
  fn              => bin q{ _ =16 iplus m64get _ goto },
  args            => bin q{ _ =24 iplus m64get _ goto },
  locals          => bin q{ _ =32 iplus m64get _ goto },
  constant_values => bin q{ _ =40 iplus m64get _ goto },
  constant_cttis  => bin q{ _ =48 iplus m64get _ goto },

  'parent=' => bin q{ sget02 sget02 =8 iplus m64set sset01 _ goto },

  finalize_args => bin q{               # self cc
    i64i sget02 =32 iplus m64set goto   # self cc [.locals=i64i] },

  resolve_constant => bin q{            # name self cc
    sget02 sget02 .constant_values .contains?
    [ sget02 sget02 .constant_values .{}  # name self cc v
      sget03 sget03 .constant_cttis .{} # name self cc v ctti
      sget03 .fn                        # name self cc v ctti fn
      schedule_constant                 # name self cc schedule
      sset02 sset00 goto ]              # schedule
    [ sget01 .parent
      [ sget01 .parent sset01           # name parent cc
        sget01 m64get :resolve_constant goto ]
      [ =0 sset02 sset00 goto ]         # 0
      if goto ]                         # schedule|0
    if goto                             # schedule|0 },

  resolve_local => bin q{               # name self cc
    sget02 sget02 .args .contains?
    [ sget02 sget02 .args .{}           # name self cc i
      sget02 .fn schedule_local         # name self cc schedule
      sset02 sset00 goto ]              # schedule
    [ sget01 .locals
      [ sget02 sget02 .locals .contains?
        [ sget02 sget02 .locals .{}     # name self cc i
          sget02 .fn schedule_local     # name self cc schedule
          sset02 sset00 goto ]          # schedule
        [ =0 sset02 sset00 goto ]       # 0
        if goto ]                       # schedule|0
      [ =0 sset02 sset00 goto ]         # 0
      if goto ]                         # schedule|0
    if goto                             # schedule|0 },

  defconst => bin q{                    # v ctti name self cc
    sget04 sget03 sget03 .constant_values .{}= drop
    sget03 sget03 sget03 .constant_cttis  .{}= drop
    sset02 sset02 drop goto             # self },

  defvar => bin q{                      # ctti name self cc
    sget01 .locals                      # ctti name self cc locals?
    [ sget03 sget02 .fn .<<local
        dup .locals .n =1 ineg iplus    # ctti name self cc fn local_i
        _   .args .n iplus              # ctti name self cc local_i'
      sget03 sget03 .locals .{}= drop   # ctti name self cc
      sset01 sset01 goto ]              # self
    [ sget03 sget02 .fn .>>arg
        .args .n =1 ineg iplus          # ctti name self cc arg_i
      sget03 sget03 .args .{}= drop     # ctti name self cc
      sset01 sset01 goto ]              # self
    if goto                             # self };

use phi::fn phi2_ir_fn => bin q{        # cc
  ir_fn                                 # cc fn
    =0_ .>>arg                          # cc fn(cc)
    =0_ .>>return                       # cc fn(cc):cc
  _ goto                                # fn };

use phi::fn phi2_state => bin q{        # cc
  =48 i.heap_allocate                   # cc new
  $phi2_state_class sget01 m64set       # [.class=]
  =0         sget01 =8  iplus m64set    # [.parent=0]
  phi2_ir_fn sget01 =16 iplus m64set    # [.fn=]
  i64i       sget01 =24 iplus m64set    # [.args=]
  =0         sget01 =32 iplus m64set    # [.locals=]
  i64i       sget01 =40 iplus m64set    # [.constants=]
  _ goto                                # new };


=head3 phi2 syntactic state
This object is an immutable parse state that links to the underlying mutable
state.

  struct phi2_syntactic_state
  {
    hereptr     class;
    schedule*   value;
    phi2_state* semantic;
    int32       index;
    int32       precedence;
  };

=cut

use phi::protocol phi2_syntactic_state =>
  qw/ semantic
      semantic=
      precedence
      precedence= /;

use phi::class phi2_syntactic_state =>
  clone_protocol,
  parse_position_protocol,
  linear_position_protocol,
  dialect_parsers_protocol,
  dialect_scoping_protocol,
  phi2_syntactic_state_protocol,

  clone => bin q{                       # self cc
    =32 i.heap_allocate                 # self cc new
    sget02 sget01 =32 memcpy            # self cc new [new=self]
    sset01 goto                         # new },

  'fail?'    => bin q{ =0 sset01 goto },
  value      => bin q{ _ =8  iplus m64get _ goto },
  semantic   => bin q{ _ =16 iplus m64get _ goto },
  index      => bin q{ _ =24 iplus m32get _ goto },
  precedence => bin q{ _ =28 iplus m32get _ goto },

  'semantic=' => bin q{                 # v self cc
    sget02 sget02 =16 iplus m64set      # v self cc
    sset01 _ goto                       # self },

  'precedence=' => bin q{               # v self cc
    sget02 sget02 =28 iplus m32set      # v self cc
    sset01 _ goto                       # self },

  with_value => bin q{                  # v' self cc
    _.clone                             # v' cc new
    sget02 sget01 =8 iplus m64set       # v' cc new
    sset01 goto                         # new },

  '+' => bin q{                         # n v self cc
    _.clone                             # n v cc new
    sget02 sget01 =8 iplus m64set       # n v cc new [.value=]
    sget03 sget01.index iplus           # n v cc new index'
      sget01 =24 iplus m32set           # n v cc new [.index=]
    sset02 sset00 goto                  # new },

  whitespace   => bin q{ hash_comment_ignore sset01 goto },
  identifier   => bin q{ ident_symbol        sset01 goto },

  atom         => bin q{ TODO },
  expression   => bin q{ TODO },
  infix_op     => bin q{ TODO },
  infix_method => bin q{ TODO },

  child => bin q{                       # self cc
    phi2_state                          # self cc s'
    sget02.semantic _.parent=           # self cc s'
    sget02.clone .semantic=             # self cc new
    sset01 goto                         # new },

  parent => bin q{                      # self cc
    sget01.clone                        # self cc new
    dup .semantic .parent _ .semantic=  # self cc new
    sset01 goto                         # new },

  fn => bin q{ _.semantic .fn _ goto },

  resolve_constant => bin q{            # name self cc
    sget01 .semantic sset01             # name semantic cc
    sget01 m64get :resolve_constant goto },

  resolve_local => bin q{               # name self cc
    sget01 .semantic sset01             # name semantic cc
    sget01 m64get :resolve_local goto },

  defconst => bin q{                    # v ctti name self cc
    sget04 sget04 sget04
      sget04 .semantic .defconst drop   # v ctti name self cc
    sset02 sset02 drop goto             # self },

  defvar => bin q{                      # ctti name self cc
    sget03 sget03
      sget03 .semantic .defvar drop     # ctti name self cc
    sset01 sset01 goto                  # self };


1;
