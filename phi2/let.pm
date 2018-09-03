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


=head2 C<let> bindings
The word C<let> (or anything else) can be bound to a value that continues the
parse by binding an unknown followed by C<= val>. The parse state is left with a
new binding in the current scope chain, so C<let> is scope-side-effectful.
=cut

use phi::genconst let_ctti => bin q{
  ctti "let"_ .defname

  pignore
  ident_symbol              pseq_return
  pignore                   pseq_ignore
  "=" pstr                  pseq_ignore
  pignore                   pseq_ignore
  ";" dialect_expression_op pseq_cons

  [ sget01 .value                       # in pos pos' cc val::name
    dup .tail _ .head                   # in pos pos' cc name val

    # Snapshot the value at its last point.
    dup .tail_anf dup .name _ .ctti     # in pos pos' cc name val vname vctti
    sget03 anf_let                      # in pos pos' cc name val vname let
    .defstack .[ .] anf_front           # in pos pos' cc name val binding

    # Bind into the scope.
    dup sget05 .dialect_context .scope  # in pos pos' cc name val b b scope
    sget04_ "val"_ .{}=                 # in pos pos' cc name val b scope'
    sget05 .dialect_context .with_scope
    sget05 .with_dialect_context sset04 # in pos pos'' cc name val b

    _ .clone .link_new_tail sset00      # in pos pos'' cc val'

    sget02 .with_value                  # in pos pos'' cc pos'''

    sset03 sset01 drop goto ]           # pos'''

  pflatmap _ .defparser };


1;
