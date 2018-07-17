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
offline.

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
  h->write_to(stdout);

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


=head3 Inspection and realtime feedback
This isn't implemented in phi1, but it wouldn't be that difficult to add it.
Once we get to phi2, the parse state will provide an C<editor> object that
describes the location of the edit cursor and provides callbacks for node
inspection.

Because C<editor> is shared across all parse state instances, we close over it
at the class level and bake it straight into the vtable. This saves a pointer
per object, which for parsing ends up being quite a bit of memory.
=cut


1;
