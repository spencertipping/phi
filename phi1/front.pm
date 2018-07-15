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


=head3 Inflected CTTI



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
