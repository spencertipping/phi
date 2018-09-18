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
  constant     = values that are known at parse-time
  identifier   = any name that could be bound to a value
  atom         = any value that parses the same way regardless of precedence
  expression   = any value in general
  statement    = an expression that isn't a value
  infix_op(op) = any operator that can bind rightwards of an "op"
  infix_method = method calling syntax, e.g. "foo.bar()"

"Constant" binds basically everything that's a constant at parse time: literal
values, type names, keywords (which bind to dialect-specific CTTIs), etc. For
example:

  int x = 10;                           // "int" is parsed by constant
  if (x > 5) ...;                       // "if" is parsed by constant

=cut

use phi::protocol dialect_parsers =>
  qw/ whitespace
      constant
      identifier
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

=cut

use phi::protocol dialect_scoping =>
  qw/ child
      parent
      fn
      resolve_constant
      resolve_local
      define /;


1;
