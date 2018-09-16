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
  literal      = syntax elements and constants with direct constructors
  atom         = any value that parses the same way regardless of precedence
  expression   = any value in general
  statement    = an expression that isn't a value
  infix_op(op) = any operator that can bind rightwards of an "op"
  infix_method = method calling syntax, e.g. "foo.bar()"

"Literal" binds basically everything that's a constant at parse time: literal
values, type names, keywords (which bind to dialect-specific CTTIs), etc. For
example:

  int x = 10;                           // "int" is parsed by literal
  if (x > 5) ...;                       // "if" is parsed by literal

The difference between "literal" and "atom" is that literals are fully specified
at compile time; the value bound to "if" doesn't generate any IR nodes to store
itself.
=cut

use phi::protocol dialect_parsers =>
  qw/ whitespace
      literal
      atom
      expression
      statement
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

=head4 Example: SQL In Any Language
Let's suppose you want to be able to write SQL expressions inside any existing
language. For example:

  vector<int> xs;
  vector<long> ys;
  select static_cast<long>(xs._ as x)
    into ys
    from xs
    where x > 10
    order by x desc
    limit 10;

The goal is to have the above be equivalent to something like this:

  for (auto _ : xs)
  {
    auto x = _;
    if (x > 10)
    {
      ys.push_back(static_cast<long>(x));
      if (ys.size() == 10) break;
    }
  }
  sort(ys.begin(), ys.end(), std::greater<long>());

C<where> and C<select> deal with C++-dialect expressions, which means C<x> is a
real variable -- but we didn't use C++-dialect type expressions to bind it
because C<select> doesn't know that we're writing in C++. In order for this all
to work, C<select> asks the dialect for a child scope containing C<x>.

TODO: none of this is true; C<as> requires that we implement a wrapping dialect
anyway, so that gives us the opportunity to alt-parse C<x> into C<atom> or
modify the scope using that type of mechanism.
=cut


1;
