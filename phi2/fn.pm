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


=head2 C<fn> non-lambda functions
These kick off a new root ANF node, parse the body, and drop a reference to the
compiled bytecode. Technically, functions are objects with a C<call> method that
specifies their arity and argument types. Arguments types must match in order
for the function to operate correctly (for GC reasons).

The function body scope is a child of the enclosing scope even though variable
closure doesn't happen. We need it to be a child so that C<let>-bound CTTI
constants (and other constants) can be captured into the function body context.
This also makes it possible to refer to globals, which is pretty important.
=cut


use phi::genconst fnarg_parser => bin q{
  pignore
    ident_symbol              pseq_return
    pignore                   pseq_ignore
    ":" pstr                  pseq_ignore
    pignore                   pseq_ignore
    "," dialect_expression_op pseq_cons };

use phi::genconst fnargs_parser => bin q{
  fnarg_parser
    "," pstr pnone palt pseq_ignore
  prep_intlist };


use phi::fn make_fn_ctti => bin q{      # rctti argcttis cc
  ctti sget03 "fn->" .+ _ .defname      # rc acs cc fc
  strbuf
    "call:" _ .append_string            # rc acs cc fc buf
  [ sget02 .name sget02 .append_string
                   ","_ .append_string
    sset02 =0 sset01 goto ]             # rc acs cc fc buf fn
  sget04 .reduce                        # rc acs cc fc buf'

  sget03 .length
  [ _ =1_ .rewind _ goto ]              # rc acs cc fc buf'
  [ goto ]
  if call                               # rc acs cc fc buf'
  .to_string                            # rc acs cc fc mname

  sget04 _ sget02 .defreturnctti        # rc acs cc fc

  # TODO
  };


use phi::genconst fndef_ctti => bin q{
  ctti "fndef"_ .defname

  # TODO
  };


1;
