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
Here's where things start to get real. We use the machinery defined in phi1 to
parse phi2, which provides a proper infix language with parse-time CTTI
propagation, expression type inference, lexical closure, etc. phi2 is a simple
language that provides enough grammar extensibility to host anything else.

Let's talk a little about how the parsers work internally.


=head3 CTTI interop and parsing
CTTIs themselves don't need to provide parsers in order to participate in a
grammar. Most languages don't have computed grammars in the first place, so
there isn't really a precedent for classes to be syntax-aware; but beyond that,
we can get a lot of mileage from a CTTI's set of offered methods. If some of
those methods look like operators, then those can be integrated into the syntax
as such (and at the appropriate precedence for the dialect in question).

Put differently, CTTIs are at liberty to rely solely on the method/virtual
tables to convey functionality and they can be confident that any frontend will
present those in a sane way.


=head3 ...therefore, a frontend is a single parser
Now we can define a frontend with minimal machinery. The frontend parser can ask
the parse state for the CTTI for a given binding, and use that to parse
identifiers. Once it has such a CTTI, it can parse method/operator invocations
against a given value, compiling/linking those at parse-time. It can also
delegate the parse to any custom continuation for that CTTI (and should, unless
the language just can't accommodate custom extensions).

"Primitive" operations like C's C<+> and pointer-dereference and such are all
specified by one of the CTTIs. We don't need a base case if we have a CTTI that
defines non-virtual methods that bottom out into assembler instructions we can
link using C<symbolic_method>. (C<phi1::class.symbolic_method> takes care of
this for us.)


=head3 Assembling expressions
Most frontends (including phi2) convert expressions to something close to ANF to
flow-assemble them. So we'd have something like this:

  # if we compile this:
  x + y.bar(bif + baz, bok.bork())

  # ...we'd compile it like this:
  let tmp1 = bif + baz in
  let tmp2 = bok.bork() in
  let tmp3 = y.bar(tmp1, tmp2) in
  let tmp4 = x + tmp3 in
  tmp4

C<let>-bindings don't dynamically extend the scope; rather, phi2 finds them up
front and builds a frame class for the full set of referenced variables. That
makes it important for C<tmpN> variables to be unique gensyms in their own
namespace.

Because scopes aren't always defined in parse order, we use parsers to stage
flow assembler links. That is, parsers return objects that do two things:

1. Return a map of C<< name -> CTTI >> entries to describe their definition set
2. Return a set of C<name> entries to describe their reference set
3. Add new links to an existing flow assembler

NB: these links are right-inclusive, just like the tail of a cons cell. There is
no tail-tail or "rest of the list" otherwise.
=cut

use constant phi2_link_protocol => phi::protocol->new('phi2_link',
  qw/ defset
      refset
      link_onto /);


=head3 Parse state
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
=cut

use constant phi2_scope_protocol => phi::protocol->new('phi2_scope',
  qw/ locals
      with_local
      captured
      with_captured
      lexical_parent
      dynamic_ctti /);


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
