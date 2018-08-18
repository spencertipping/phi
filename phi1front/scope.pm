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


=head2 Parse state
Frontend parse states are slightly nontrivial because they have a bit of
negotiation to do. In particular, not all frontends support:

1. Lexical scoping and lambda-capture
2. Dynamic scoping
3. Prefix-free variable addressing
4. Variable addressing at all

We need to generalize across these differences so CTTIs that offer custom parse
continuations can refer to defined quantities by name. That makes it possible to
write things like this:

  char const *str = "hello";
  html_element *e =
    html <h1>                           // at this point html owns the parse
           <div>$str</div>              // <- reference to defined variable
         </h1>;

Another way to look at it is that CTTI and scopes are the two sides of the
semantic catalog phi offers to frontends -- just like the file tree and inode
semantics are the two sides of the catalog offered by filesystems.

Here's what the state struct looks like:

  struct phi2_parse_state
  {
    hereptr     class;
    int64       index;                  # string position
    phi2_scope* scope;
    ?*          context;                # opaque state, e.g. an opgate
  }

=cut

use phi::protocol phi2_parse_state =>
  qw/ scope
      context
      with_scope
      with_context
      with_local
      with_captured /;

use phi::class phi2_parse_state =>
  parse_position_protocol,
  linear_position_protocol,
  phi2_parse_state_protocol,

  index   => bin q{swap =8  iplus m64get goto},
  scope   => bin q{swap =16 iplus m64get goto},
  context => bin q{swap =24 iplus m64get goto},

  with_scope => bin q{                # scope self cc
    =32 i.heap_allocate               # scope self cc self'
    sget02 sget01 =32 memcpy          # [self'=self]
    sget03 sget01 =16 iplus m64set    # [.scope=]
    sset02 sset00 goto                # self' },

  with_context => bin q{              # c self cc
    =32 i.heap_allocate               # c self cc self'
    sget02 sget01 =32 memcpy          # [self'=self]
    sget03 sget01 =24 iplus m64set    # [.context=]
    sset02 sset00 goto                # self' },

  with_local => bin q{                # ctti name self cc
    sget03 sget03 sget03 .scope       # c n s cc c n scope
    .with_local                       # c n s cc scope'
    sget02 .with_scope                # c n s cc self'
    sset03 sset01 drop goto           # self' },

  with_captured => bin q{             # ctti name self cc
    sget03 sget03 sget03 .scope       # c n s cc c n scope
    .with_captured                    # c n s cc scope'
    sget02 .with_scope                # c n s cc self'
    sset03 sset01 drop goto           # self' },

  "fail?" => bin q{=0 sset01 goto},
  "+"     => bin q{                   # n self cc
    =32 i.heap_allocate               # n self cc self'
    sget02 sget01 =32 memcpy          # [self'=self]
    dup =8 iplus dup m64get           # n self cc self' &index index
    sget05 iplus swap m64set          # n self cc self' [.index+=n]
    sset02 sset00 goto                # self' };


=head3 Scopes and capture
Parsers store the compile-time scope in the parse state, so we have more or less
a C<< map<string*, ctti*> >> we can consult as a directory. So far so good, but
there are cases where we'll need a bit more firepower than that. For example,
how do we write a frontend that supports lexical capture? Even more confusingly,
how do we differentiate between that and dynamic scoping?

  (lambda (x)
    (lambda (y)
      (+ x y)))                 ; if scheme, x is lexical
                                ; if emacs lisp, x is dynamic

It gets worse. If we count the above ambiguity, we have a few different cases to
think about:

1. Lexical scope + "normal" capture: OCaml, Scheme, JS, Perl, Ruby, etc
2. Lexical scope + restricted, semi-explicit capture: Python
3. Lexical scope + restricted capture: C++
4. Dynamic scope: Emacs Lisp, bash, C preprocessor
5. Lexical+dynamic scope: gcc-flavored C
6. Block scope: standard C

(2) captures two weirdnesses about Python. First, the external scope state is
captured once, exactly when a child scope is defined; if you later extend that
external scope you won't change the child refset retroactively (which is good).
Second, all assignments are interpreted as local declarations unless you use
C<global> or C<nonlocal> (Python 3) to disambiguate. I don't think you can
assign into a lexical but not global parent in Python 2.

(5) is the intersection of the two scoping models: you get lexical scoping only
for values also within the dynamic scope
(L<https://gcc.gnu.org/onlinedocs/gcc/Nested-Functions.html>).

(5) and (6) can be implemented exactly the same way; structurally, a block is a
function you promise to call immediately, and that function can inherit the
calling frame object. This means blocks destructively modify the frame in which
they appear, adding slots for their block-scoped quantities. We'll rely on
general refset drops to prevent fictitious GC pins (which happens independently
of the frontend, for the record).

...again, all of this is less a commentary about how these things would be
implemented in frame terms than it is a question of how frontends should encode
the compile-time information into the parse state's scope objects.

Ok, I think we have enough information here to make some decisions. Let's define
the scope protocol. C<dynamic_ctti> might be a bit misleading, but it's useful:
when we know the calling frame's class, e.g. for an inline closure or a block,
we can hardlink variable accesses. Contrarily, references to unspecified dynamic
variables need to be resolved using some type of reflection.

NB: scope objects are immutable because they're owned by parse states.

NB: lexically captured quantities aren't typically stored in the frame as
locals. It's often more efficient to represent lexical closures as ad-hoc
classes than it is to dynamically assemble push-captured-variable code. In
practice this means we'll use a different protocol to access captured values.
It's up to the frontend to manage closure instantiation and capture access.

Here's the struct:

  struct phi2_scope
  {
    hereptr              class;
    phi2_scope*          lexical_parent;
    ctti*                dynamic_parent_ctti;
    map<string*, ctti*>* locals;
    map<string*, ctti*>* captured;
  }

=cut

use phi::protocol phi2_scope =>
  qw/ locals
      with_local
      captured
      with_captured
      lexical_parent
      dynamic_parent_ctti /;

use phi::class phi2_scope =>
  phi2_scope_protocol,

  lexical_parent      => bin q{swap =8  iplus m64get swap goto},
  dynamic_parent_ctti => bin q{swap =16 iplus m64get swap goto},
  locals              => bin q{swap =24 iplus m64get swap goto},
  captured            => bin q{swap =32 iplus m64get swap goto},

  with_local => bin q{                # ctti name self cc
    =40 i.heap_allocate               # ctti name self cc self'
    sget02 sget01 =40 memcpy          # [self'=self]

    sget04 sget04 sget02              # c n s cc s' c n s'
    .locals .clone .{}=               # c n s cc s' ls'
    sget01 =24 iplus m64set           # c n s cc s' [.locals=]
    sset03 sset01 drop goto           # self' },

  with_captured => bin q{             # ctti name self cc
    =40 i.heap_allocate               # ctti name self cc self'
    sget02 sget01 =40 memcpy          # [self'=self]

    sget04 sget04 sget02              # c n s cc s' c n s'
    .captured .clone .{}=             # c n s cc s' cap'
    sget01 =32 iplus m64set           # c n s cc s' [.captured=]
    sset03 sset01 drop goto           # self' };


use phi::fn phi2_scope => bin q{        # dynamic_ctti lexical_parent cc
  =40 i.heap_allocate                   # d l cc s
  $phi2_scope_class sget01 m64set       # [.class=]
  sget02 sget01 =8  iplus m64set        # [.lexical_parent=]
  sget03 sget01 =16 iplus m64set        # [.dynamic_parent_ctti=]
  strmap sget01 =24 iplus m64set        # [.locals=]
  strmap sget01 =32 iplus m64set        # [.captured=]
  sset02 sset00 goto                    # scope };

BEGIN
{
  bin_macros->{scope}       = bin q{=0 =0   phi2_scope};
  bin_macros->{child_scope} = bin q{=0 swap phi2_scope};
}


# TODO: tests?


=head3 Quick aside: optimizing non-escaping closures
I think if we're clever we can use escape analysis and use this to optimize
languages that use functional iterators, effectively inlining the block into the
parent scope:

  (1..5).each do |x|
    puts x                      # no reason to have a child frame here,
                                # given that the do-block doesn't escape
                                # from each()
  end

This works only if C<each> resolves to a CTTI that marks its lambda argument as
being a dynamic-valid call; that means lexical and dynamic scope will align, and
we can treat the whole child scope as a block-extension of the calling function.
It's sort of like a dynamic scope that skips the C<each> layer.

...then again, it may make more sense to have C<each> CTTI-analyze constant
blocks and inline them, rather than having the frontend know anything about
scope inlining. I think the latter involves locally overloading lambda
constructors or something, which seems like it might be awkward (and "local" in
this context may require more precision than we can easily get from our parsing
machinery).
=cut


1;
