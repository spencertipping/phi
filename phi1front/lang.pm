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


=head2 Language frontends
phi classes compile things, but classes aren't addressible using any particular
syntax. That's where frontends come in. phi frontends are usually structures
imposed on text -- interactive editors, more or less. They can also parse
offline. (NB: phi1 doesn't implement editor interactivity; that's introduced in
phi2.)

There's no reason to define a constant grammar for a dynamically compiled
language; in fact, doing so creates all kinds of problems down the line. This
type of design is why Ruby is hard to use for shell scripting despite being very
expressive generally. In phi you could define an identifier called C<$> that
dictates its parse continuation, locally modifying the grammar to parse
something in shell notation before ending its local parse and returning control
to the hosting grammar. That would enable you to write something like this:

  x = 10;
  y = ($ ls | wc -l).as_int;
  (x + y).print;

Because these subordinate grammars are properly interpolated into the hosting
parser, you can use arbitrarily complex subexpressions and get autocompletion,
highlighting, and other support for them. This avoids the problems you get in
languages like Perl and Ruby, in which C<`> blocks require nested C<`>
characters to be escaped. Escaping in phi happens only for unstructured
constructs like strings, for which no subordinate grammar would contextualize
delimiters beyond the obvious C<\"> arrangement. (And for the record, C<`> is a
case where you'd still need escaping unless you did something clever like
looking at the balance of whitespace.)


=head3 Variable bindings, lexical scopes, and parse states
phi has no runtime-level knowledge about variables or scope chains; all of that
gets erased by the time we assemble things into bytecode.

Structurally speaking, the parse state stores the scope chain as well as which
variables are captured and which are defined locally. Symbols can then be
resolved against the scope chain, which contains pulldown logic to recursively
capture the variable down to the right scope.


=head3 Dialects
phi is designed to be able to inline grammars from other programming languages,
which creates some interesting design constraints. For example, let's suppose I
define a variable in a Python dialect and then use it from C. Assuming all of
the code is targeted at the same runtime (i.e. we're not creating an RPC
bridge between two distinct processes), we might have something like this:

  in python:
    global x
    x = "foo"                           # x = "foo" happens in python dialect

  in c
  {                                     # this block happens in C dialect
    printf("%s\n", x);                  # x is idiom-converted to (char const*)
  }

If C<x> bends the grammar, this should usually happen in a way that's consistent
with the dialect in which it appears -- and that means C<x> needs to not only
know which dialect that is, but also make sure its parse continuations are
similarly conforming. Given the use case above, C<x> can't have an intrinsic
preference about dialect; in every context it's being compiled down to the phi
backend, so in that sense dialects are equivalent. Instead, dialects are managed
and indicated by the parse state, which allows them to be handled lexically.


=head3 CTTI inflection and idiom mapping
phi maintains a stack of CTTI abstracts, the topmost of which drives the parse
at any given moment. These CTTI abstracts are semantic, though, unlike dialects;
this means that they have no reason to define (or even be aware of)
dialect-specific parsing details. We need to inflect the neutral CTTI into the
dialect specified by the parse state.

Both dialects and CTTI are open-ended; you can define new instances of each.
This complicates interoperation between the two by necessitating a reductive
negotiation protocol. Let's talk about what that needs to look like.

First, our life is pretty easy for most languages because most languages don't
use computed grammars. We may have any number of backend CTTI types, but almost
all of them are projected into the same inflected parser type (for C and Python,
it's probably a "generic value" type). We can integrate dialect-independent
parsing extensions if we want to -- for instance, if we have something like an
HTML generator that can parse literal HTML regardless of dialect, then C's
inflected values could C<alt> that continuation into the grammar, enabling code
like this:

  char const    *msg = "hi there";
  html_fragment *h   = html <p><div class='id'>$msg</div></p>;

There are two ways we can get this behavior:

1. Have C<html> resolve (via C scopes) to a non-generic value
2. Write C's generic-value inflection to inline out-of-band continuations

We have something interesting going on with C<$msg>: first, how does a custom
parse continuation access variables within scope; and second, who's doing the
type mapping from C<char const*> to a phi string?

C<html>'s parser is free to refer to state-dependent parsers like C<variable>
and C<expression>; these are parameterized on the scope chain, which in C is
just the set of currently-defined globals and locals. Variables won't be bound
to actual values yet, but we will know their declared types at parse-time.

Things like C<html> aren't aware that they're working within the C dialect, nor
should they have to care. So C<char const*> is meaningless at this point.
Luckily, C<char const*> isn't the declared type of C<msg>;
C<phi::const(phi::nt_string)> is. This is a dialect-neutral CTTI to simulate the
behavior of C's C<char const*> strings (C<nt_string> == "null-terminated
string"). The C dialect doesn't idiom-map every type like this, but as a general
rule anything with its own syntax comes with an idiom mapping into phi's
dialect-neutral CTTI space.

So, putting this all together, C<html> can define a dialect-independent grammar
extension that lets you interpolate values in a compile-time polymorphic and
dialect-neutral way, provided that those values can be mapped to a
dialect-neutral representation by idiom translation. If the value can't be
mapped, the extension can ask it to do an RTTI-driven conversion to an idiom
type; for instance, if we're writing in Java and you say something like this:

  MyCustomType t = ...;
  Html h = html <p><div class='id'>$t</div></p>;

C<$t> maps to C<phi::ref_fixed_class_bounded_by(MyCustomType)> or similar, which
has no compile-time overload that C<html> would know how to use. So C<html>
takes the generic branch and requests a coercion to something that implements
the C<phi::string> protocol. If the class defines a string conversion, this
works automatically; otherwise it emits an error or warning.

One thing worth noting is that these coercions happen in phi-semantics, not
Java-semantics. So when we request a coercion like this, we're asking a phi CTTI
for a non-parametric projection into another phi CTTI. The dialect is uninvolved
at this point. _Targeting_ Java also doesn't involve the Java dialect; dialects
are strictly for parsing.


=head3 Paradigm channels
Earlier I alluded to a reductive protocol between CTTI and dialects; we still
need this if we want deep integration between, for instance, phi objects and C++
or Python's operator overloading. I should be able to do this, for example:

  in c++
  {
    class my_int
    {
      ...
      my_int &operator+ (int x) const { ... }
    };
  }

  in python:
    x = my_int(1)
    print x + 1                         # should use operator+ implementation

This isn't as straightforward as it looks. The big issue is that dialects own
operators; Python and C++ not only have different sets of things you can
overload, but those operators don't have the same precedence. So not only are
types subject to idiom-conversion, operators are too. Both Python and C++
dialects (along with most other infix languages) implement the "operator
paradigm," which provides an idiom-conversion channel between dialect-owned
operators and phi-owned CTTI methods.

Some other paradigms include:

1. Receiver-dispatch method invocation
2. Explicit and implicit type coercion
3. Type hinting (required when we use dynamic code from static languages)
4. Argument-type function overloads

This stuff is managed by having CTTIs implement the C<dialect_negotiation>
protocol.


=head3 phi CTTI as an intermediate encoding
phi's goal is to bridge language semantics well enough that we can import real
code from various languages and use it without thinking about semantic barriers.
This means we're projecting other languages' semantics into phi, which opens the
possibility for some erasure.

That erasure has a dark side, though: every time we lose detail, we compromise
the dialect's ability to apply its own semantics. The dialect itself can't
maintain state that is later erased; if something can't be projected into phi's
CTTI space, it's lost immediately.

This places a substantial demand on phi's CTTI layer: we need to be able to
encode almost every semantic construct from every frontend language losslessly,
which sometimes means indulging languages' idiosyncrasies. For example, C++
draws a distinction between references and pointers:

  void f(double &x) {...}
  void f(double *x) {...}

These two functions are overload-distinct despite having argument types that are
structurally identical. If we want to retain this overload distinction we'll
need to provide two separate phi CTTIs:

  base_pointer<type>
  base_pointer_as_ref<type>

In practice we'd likely do this as a second type parameter for C<base_pointer>
-- but the same principle holds either way: phi's CTTI is as much about
preserving dialects' data as it is about emulating their semantics.


=head3 Resolving semantic conflicts
Ruby strictly rewrites C<x += 1> into C<x = x + 1>, which means you can't
overload C<+=> as an operator; as far as Ruby is concerned, C<+=> doesn't exist.
C++ and Perl, on the other hand, treat C<+=> as being distinct from C<+>. This
may seem like a trivial difference, but it has some fairly substantial
implications about how these languages work.

Let's start with Ruby, and let's suppose we've patched the language to support
C<+=> as its own operator rather than implementing it as a syntactic shorthand.
We'd need to patch core classes, for instance C<Fixnum>:

  def +=(rhs)
    @n = @n + rhs.to_i
  end

...and right there we've just introduced a bug:

  x = 10
  y = x
  x += 5                # y should still be 10, but it will be 15

The problem here is that Ruby thinks of C<Fixnum> as an immutable _reference_
type -- Ruby doesn't support primitive value types. If objects are required to
implement C<+=>, they are forced to be mutable. Perl and C++ support C<+=>
overloading by providing value type semantics and clone-on-assignment, which is
semantically incompatible with Ruby's assumptions.

phi has two options for using an overloaded C<+=> method from a dialect like
Ruby:

1. Give it an alternative name
2. Add value-type semantics to the Ruby dialect

phi does (2) unless it's impossible for some reason.


=head3 Defining classes
Let's talk about the mechanics of defining classes within parsed language, in
this case using the Ruby dialect:

  class Counter
    attr_reader :x
    def initialize
      @x = 0                    # type inference may happen here
    end

    def f
      @x += 1
    end
  end

Within the parser, C<class> resolves to a Ruby-dialect CTTI that assumes control
of the parse and ultimately binds C<Counter> to a compiled CTTI instance. The
idea here is that C<Counter> can be locally linked and simulated by the time we
hit the C<end> of its class.

...and naturally, nothing stops us from implementing classes that participate in
parsing. All we need to do is implement the right set of methods.


=head3 phi2 algebraic class elements
There's no reason to use symbolic methods in phi2. Instead, we can build classes
up from individually parameterized CTTI elements that we combine to form a
compilable entity.
=cut


1;
