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


=head2 Scopes
There are a lot of different scoping models, each of which places different
demands on implementors. Let's start with something simple like C:

  /* this is global scope */
  /* everything here is forward-accessible */
  void f(void)
  {
    /* local scope */
    /* can see globals + locals until this point */
  }

Aside from C<static> elements from other files (which we can manage manually by
modifying the parse state), C's scoping model is pretty straightforward. C++
makes things quite a bit more difficult:

  class foo
  {
    int x;                      // member variable scope
    void f(void) { /* x is visible */ }
  };
  void foo::g(void) { /* x is visible */ }
  void bar::g(void) { /* x isn't visible */ }

  class bif : public foo {};
  void bif::g(void) { /* x is visible */ }

In other words, C++ provides scopes that don't follow a lexical structure. In
order to implement this, we'll need to use the C<foo::> prefix to modify the
parse state to make member variables visible -- and that, in turn, means we'll
need to maintain a catalog of defined classes. We'd have to maintain that
catalog anyway for type-vs-expression resolution, but we'll need to also retain
the member variable scope for each one.


=head3 Ways we can implement this stuff
1. Try to generalize everything into an open-ended directory-like structure
2. Specialize the scope model per frontend
3. Provide some default scoping and extend as necessary with specialized classes

The reason to generalize, beyond code reuse, is that CTTI parsers will want to
include identifiers in various use cases. It's an open question whether they
need to know where these identifiers come from; if I'm writing a CTTI extension,
do I care whether the value coming in is a member variable or a local? Not if I
can emit accessor code for it, most likely. I might care about the type, but
that's about it.

...so maybe, then, the answer is to specialize by default. We can have a basic
scope structure that covers globals + lexical locals or something, and anything
else is a completely separate implementation. Any scope class just needs to
provide a standard API that a generic "value" parser can use.

I'm still not sold though. Is there a situation where a CTTI itself participates
in the scope chain somehow?

  ni *find(ni *stream, char const *pattern)
  {
    return stream r/$pattern/;          // no scope chain participation here
  }

  ni *prepend(ni *stream, char const *value)
  {
    // ::v[...] belongs to the stream's scope and should be visible elsewhere.
    // So globally speaking, we now have a mixed scope chain: part of it is C
    // and part is a ni stream thing.
    return stream ::v[i$value] p'r v.a, FR 1';
  }

Aha, that's the problem: there's no reason a CTTI can't introduce its own local
variables and even local scopes. We need to be able to mix those into the
hosting scope chain at some level.

NB: ideally, the ni CTTI integrates with line-ending idioms; C is
whitespace-insensitive, but Python/Ruby/bash should terminate a ni stream
modifier at EOL unless the line ends with C<\>. It's worth thinking about how
this might work.


=head3 More about scope mixing
Let's just get into every way this might work.

First, do we support bidirectional integration? Going back to the ni stream
example, this time in Ruby:

  def f filename
    ni ::v[$filename] m{|x| x + v}
  end

This is a bit of a contrived example, but we should be able to do things like
this. The CTTI's C<m> subgrammar interpolates back to Ruby to collect a lambda,
which idiomatically becomes a Ruby block (TODO: figure out how we would do
this).

That lambda block should see a scope in which it can refer to C<v>, a data
closure we bound earlier in the CTTI-owned expression. So the CTTI has modified
the scope chain that will be consumed by a Ruby lambda.

A natural question here is how Ruby should see that variable. The CTTI doesn't
necessarily know what mechanisms its surrounding language has to create child
scopes or bind things inside subexpressions. Does it ask Ruby's specialized
scope to bind-within and leave that up to the scope object?

What would this look like in C++?

  ni &f(std::string const &filename)
  {
    return ni ::v[$filename] m[&v](std::string const &x){ return x + v; };
  }

This might be a problem. First, it's unclear who's responsible for capturing
stuff and how: C<[&v]> as a lambda capture list suggests that C<v> is magically
available in a surrounding scope. Actually, that's not so bad at all -- of
course it would be; ni would have bound it.

...so ni's CTTI would implement C<m> by importing the set of defined data
closures into a child scope of unspecified nature -- could be a local or object
scope depending on the backend. In Java we might use a lambda:

  // CTTI-managed syntax:
  ni f(String filename)
  {
    return ni ::v[$filename] m(String x) -> x + v;
  }

  // equivalent implementation, taking some notational liberties:
  ni f(String filename)
  {
    return ((String filename) -> {
      final String v = ni.cat(filename);
      return ni.map((String x) -> x + v);
    })(filename);
  }

This is, to my knowledge, the _only_ way to implement the above. We have a few
constraints:

1. C<v> can't pollute the outer scope
2. C<v> can't be renamed, e.g. to a gensym (more below)
3. C<v> must be captured by Java, so it must be final
4. C<filename> must be capturable even though it isn't C<final> to start with

(2) is about the CTTI not asking the Java frontend for too much help. It's one
thing to construct a child scope, but it's another to tell a frontend "hey you
know C<v>? it's fictitious; rename it to this gensym so I can pretend I'm not
polluting the surrounding scope."

The goal here isn't to be Java-typesafe or anything; in all likelihood we won't
be at this level of abstraction. If we are in fact targeting Java, we'll do so
after bytecode conversion, so all of this will be compiled to a much lower-level
representation.

Regardless of backend, though, the C<ni> CTTI needs to do things that are
consistent with the frontend's scoping model. The above works pretty well in
that regard.

I guess this raises an interesting question: are ni lambdas off limits in a
straight-C dialect, even if we're targeting a backend that would make them easy
to implement? Lambdas per se probably are, although there's no reason we can't
parse a C expression with C<v> as a bound variable. We could do the GCC
nested-function thing I suppose. If we're being strict about standard C we don't
have any good options for introducing child scopes. I suppose at that point we'd
have a hard choice to make: either break out of C's scoping model, or give up
and say ni dataclosure names are just off limits.


=head3 Out-of-band capture
OK, let's suppose we're committed to having ni-lambdas work even in frontends
that don't provide any accessible scope mechanism to implement them. This means
every frontend that provides addressible variables of any sort will need to
provide a scope insertion point that lets us do two things:

1. Create a child scope that inherits local context
2. Bind a value in that child scope

There are advantages to this. First, it isn't a problem from a backend-targeting
point of view -- everything would go through bytecode first anyway so we can do
things that are technically disallowed as long as there's some way to implement
it in bytecode.

Second, we don't have to think about the often arbitrary limitations of
languages we're simulating. We can provide a base layer of high-functioning
sanity that gives CTTIs a uniform way to implement stuff: universal support more
or less for free.

Third, it might simplify CTTI propagation -- or at least it removes the
frontend's possibly broken scoping model from the line of fire.

I guess it's worth asking whether there are situations where we want to simulate
host-language brokenness or something. Like in Python, should we have to say
C<global filename>, or is the value bound locally? Or in Javascript, would
saying C<var filename> create a separate undefined local that shadows the one
the CTTI gives us?

Maybe the right approach is to have frontends be required to implement an "out
of band capture" scope (chain?) that provides magic rvalues by an unexplained
mechanism. You can shadow them; they don't behave like locals. The only promise
is that they're available by name as constants. I don't think we want to suggest
that you can modify them to interact with the CTTI ... but maybe?

How about this: OOB capture specifies the type of the argument, so we know the
CTTI. If that CTTI provides lvalue semantics then they're fair game. If we're
working with a language like Ruby that owns C<=>, then you'll either get an
error if you try to overwrite the OOB value, or it will behave like a regular
local variable. I think you should get an error because you'd most likely be
modifying the value's type.


=head3 Deliberately reductive: no special scopes allowed
We don't want CTTIs to think about complicated scoping models, nor even be aware
of their existence. In C++, for example, CTTIs can't query or modify class
member scopes -- such a CTTI wouldn't be at all portable to other frontends. It
can _inherit_ a scope but it can't _inspect_ one. (Or maybe it technically can,
but it probably shouldn't.)

...so whatever we end up doing, we should make sure that CTTIs don't need to be
specialized to the frontend. They should be able to get whatever functionality
they need purely through the OOB mechanism.

Q: can CTTIs modify the hosting scope, as opposed to creating an OOB child? For
example, does this work?

  class foo : public bar
  {
  public:
    int y;
    python def f(self, x):              # does this define a C++ method?
      return self.y + x

    int g(int x)
    {
      return f(x);                      // ...and can we say this?
    }
  };


=head3 Lexical scoping and capture
NB: this section is deprecated

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

TODO: redo this completely; I suspect it's totally wrong.
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


# TODO: tests


1;
