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


=head2 phi2 language definition
We need two things at a minimum to define phi2: a dialect and a value CTTI. phi2
(like most languages) also defines other CTTIs that play auxiliary grammar
roles, but phi2 keeps them to a minimum for simplicity.


=head3 Language syntax
phi2 looks sort of like C++, but is parsed more like OCaml:

  let f = int(int a, int b)             # "int" owns the parse, binds a fn
  (                                     # NB: parens instead of braces
    int     x = 10;                     # int == int64_t
    int.ptr y = x.addr;                 # int.ptr == int*
    let     z = y + 18;                 # let == auto

    x += 1;                             # vars are mutable

    # NB: semicolons are infix operators: do left then right, return right
    z > 10
      ? return 5
      : ( while x                       # while binds to a custom CTTI
            ( x -= 1; x.to_string.pnl );
          return y.deref ? 6 : 7 )
  );

You can define new CTTIs in phi2, typically using a metaclass like C<class>:

  let vec2 = class.new;                 # a new reftype CTTI
  real vec2::x;                         # define instance fields (the
  real vec2::y;                         #   "hereptr class" prefix is implied)

C<real vec2::x> is syntax that's recognized by the phi2 type meta-CTTI, and is
equivalent to C<vec2.field("x", real)>. A similar mechanism is used to define
methods:

  vec2 vec2::+(vec2 rhs)                # define a virtual method
    vec2.new(self.x + rhs.x, self.y + rhs.y);

This is a little more involved because C<+> will be joined with the argument
types to support type-based overloads:

  vec2.method("+", vec2(vec2 self, vec2 rhs)
                     vec2.new(self.x + rhs.x, self.y + rhs.y));


=head3 Meta-CTTI internals
Clearly most of the interesting stuff here is happening inside type CTTIs;
that's where we get rewriting and class construction. Let's talk a bit about how
this all works.

First, C<class> is a meta-CTTI: it is both an instance of and produces instances
of CTTIs. The CTTIs it returns are reference types, meaning that they are
pointers to values that have a C<hereptr class> prefix field. This makes it
possible to define virtual methods.

C<class.new> is a regular CTTI: it's an instance of CTTI, but instances of it
aren't themselves CTTIs (unless you basically reimplement CTTI's fields and
methods). Because C<class> is defined as a reference type constructor, the CTTI
you get from it is configured in a few ways:

1. It contains C<hereptr class> as mentioned above
2. It automatically defines accessors when you create fields
3. It automatically implements copying GC
4. Its dispatch function is indirect, so you can modify the class at runtime

(4) means that you can define new virtual methods and they'll immediately become
available on existing instances. This class modification also makes it possible
to profile the method call distribution and reorder the lookup table to place
frequently-called methods first.


=head3 Type-CTTI syntax
phi2's CTTI-wrapping CTTI (which is used for anything that functions as
typename, e.g. C<class>, C<vec2>, C<int>) defines some common syntactic behavior
that enables local variable, function, class field, and class method
definitions. The way this all works is a bit subtle.

The core problem is that C<int x> by itself is ambiguous: are we defining a
local or a function argument? Here's the difference:

  int x = 10;                           # local
  let f = int(int x) x + 1;             # fn arg

This is a challenge because C<int> interpolates the language grammar to consume
C<x>, but it doesn't know the context in which it's being used. We can't
selectively disable C<int>'s parse continuation to override the way arglists are
parsed: phi doesn't have any support for this aside from rebinding all CTTIs
within a child scope.

If we want this all to work, then, we need to conscript the dialect, which
maintains syntactic state. In particular, CTTIs talk to the dialect in order to
extend the current local scope; what we need is for some of those "local scopes"
to actually specify function arguments.


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
    hereptr          class;
    phi2_state*      parent;
    ir_fn*           fn;
    map<hash, ctti>* args;
    map<hash, ctti>* locals;            # NB: null if we're defining args
  };

=cut

use phi::protocol phi2_state =>
  qw/ parent
      parent=
      fn
      args
      locals
      finalize_args
      define /;

use phi::class phi2_state =>
  phi2_state_protocol,

  parent => bin q{ _ =8  iplus m64get _ goto },
  fn     => bin q{ _ =16 iplus m64get _ goto },
  args   => bin q{ _ =24 iplus m64get _ goto },
  locals => bin q{ _ =32 iplus m64get _ goto },

  "parent=" => bin q{ sget02 sget02 =8 iplus m64set sset01 _ goto },

  finalize_args => bin q{               # self cc
    i64i sget02 =32 iplus m64set goto   # self cc [.locals=i64i] },

  define => bin q{                      # ctti name self cc
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

use phi::fn phi2_state => bin q{        # cc
  =40 i.heap_allocate                   # cc new
  $phi2_state_class sget01 m64set       # [.class=]
  =0    sget01 =8  iplus m64set         # [.parent=0]
  ir_fn sget01 =16 iplus m64set         # [.fn=]
  i64i  sget01 =24 iplus m64set         # [.args=]
  =0    sget01 =32 iplus m64set         # [.locals=]
  _ goto                                # new };


=head3 phi2 syntactic state
This object is an immutable parse state that links to the underlying mutable
state.

  struct phi2_syntactic_state
  {
    hereptr     class;
    *           value;
    int         index;
    phi2_state* semantic;
  };

=cut

use phi::protocol phi2_syntactic_state =>
  qw/ semantic
      semantic= /;

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

  "fail?"  => bin q{ =0 sset01 goto },
  value    => bin q{ _ =8  iplus m64get _ goto },
  index    => bin q{ _ =16 iplus m64get _ goto },
  semantic => bin q{ _ =24 iplus m64get _ goto },

  with_value => bin q{                  # v' self cc
    _.clone                             # v' cc new
    sget02 sget01 =8 iplus m64set       # v' cc new
    sset01 goto                         # new },

  "+" => bin q{                         # n v self cc
    _.clone                             # n v cc new
    sget02 sget01 =8 iplus m64set       # n v cc new [.value=]
    sget03 sget01.index iplus           # n v cc new index'
      sget01 =16 iplus m64set           # n v cc new [.index=]
    sset02 sset00 goto                  # new },

  whitespace   => bin q{ hash_comment_ignore sset01 goto },
  constant     => bin q{ TODO },
  identifier   => bin q{ ident_symbol sset01 goto },
  atom         => bin q{ TODO },
  expression   => bin q{ TODO },
  statement    => bin q{ TODO },
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

  define => bin q{                      # ctti name self cc
    sget03 sget03
      sget03 .semantic .define drop     # ctti name self cc
    sset01 sset01 goto                  # self };


1;
