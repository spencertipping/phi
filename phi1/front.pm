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

There's no reason to define a constant grammar for a language; in fact, doing so
creates all kinds of problems down the line. This type of design is why Ruby is
hard to use for shell scripting despite being very expressive generally. In phi,
you can define an identifier called C<$> that dictates its parse continuation,
locally modifying the grammar to parse something in shell notation before ending
its local parse and returning control to the hosting grammar. That would enable
you to write something like this:

  x = 10;
  y = ($ ls | wc -l).as_int;
  (x + y).print;

Because these subordinate grammars are properly interpolated into the hosting
parser, you can use arbitrarily complex subexpressions and get autocompletion,
highlighting, and other support for them.


=head3 Variable bindings, lexical scopes, and parse states
phi has no language-level knowledge about variables or scope chains; all of that
gets erased by the time we assemble things into bytecode.

TODO: elaborate


=head3 Dialects and class-compiler interop
TODO


=head3 Inspection and realtime feedback
This isn't implemented in phi1, but it wouldn't be that difficult to add it.
Once we get to phi2, the parse state will provide an C<editor> object that
describes the location of the edit cursor and provides callbacks for node
inspection.
=cut


1;
