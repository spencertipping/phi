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


use phi::fn make_fn_ctti => bin q{      # rctti argcttis cc
  ctti sget03 .name "fn->".+ _ .defname # rc acs cc fc
    dup .fields "ptr"_ .hereptr drop

  # Make functions castable to here-pointers for phi1 interop
  here_ctti_       "to_here:"_ .defreturnctti
  [ sset00 goto ]_ "to_here:"_ .defmethod

  strbuf
    "():" _ .append_string              # rc acs cc fc buf
  [ sget02 .name sget02 .append_string
                   ","_ .append_string
    sset02 =0 sset01 goto ]             # rc acs cc fc buf fn
  sget04 .reduce                        # rc acs cc fc buf'

  sget03 .length
  [ _ =1_ .rewind _ goto ]              # rc acs cc fc buf'
  [ goto ]
  if call                               # rc acs cc fc buf'
  .to_string dup                        # rc acs cc fc mname mname

  sget05 _ sget03 .defreturnctti drop   # rc acs cc fc mname

  [ sset00 _ .call _ goto ]_            # rc acs cc fc fn mname
  sget02 .defmethod drop                # rc acs cc fc

  sset02 sset00 goto                    # fc };


use phi::genconst fnarg_parser => bin q{
  pignore
    ident_symbol pseq_return
    pignore      pseq_ignore
    ":" pstr     pseq_ignore
    pignore      pseq_ignore

  "," dialect_expression_op
    [ _ .tail_anf .anf_cvalue _ goto ] pmap pseq_cons };

use phi::genconst fnargs_parser => bin q{
  fnarg_parser
    "," pstr pnone palt pseq_ignore
  prep_intlist };

use phi::genconst fn_parser => bin q{
  pignore
    "(" pstr      pseq_ignore
    fnargs_parser pseq_return
    pignore       pseq_ignore
    ")" pstr      pseq_ignore

  [ # Set up the child scope to refer to the function arguments, then parse the
    # body with that scope. This makes lambda-style capturing possible but
    # ultimately invalid.
    sget01 .value                       # in pos pos' cc arglist
    sget02 .dialect_context .scope      # in pos pos' cc arglist scope
                     "val"_ .child _    # in pos pos' cc scope' arglist

    # For simplicity we'll use the same names for both function arguments and
    # ANFs. This results in an apparently nonsensical "let x = x" node, but we
    # need that because scope bindings are to let nodes, not to arguments.
    [ sget02 dup .tail _ .head          # arg scope' cc argname argctti
      sget01 anf_let .[ .]              # arg scope' cc argname arglet
      sget01 _ .defstack anf_front      # arg scope' cc argname argfront
      _ sget03 "val"_ .{}=              # arg scope' cc scope''
      sset02 =0 sset01 goto ] _ .reduce # in pos pos' cc scope''

    # Now let's parse the body using this child scope.
    sget02 .dialect_context .with_scope
    sget02 .with_dialect_context        # in pos pos' cc pos''
    sget04 _                            # in pos pos' cc in pos''
    ";" dialect_expression_op .parse    # in pos pos' cc pos'''

    dup .fail?
    [ drop sset03 sset01 drop goto ]    # pos'''
    [ goto ]
    if call

    # Restore the original scope
    dup .dialect_context .scope "val"_ .parent
    sget01 .dialect_context .with_scope
    _ .with_dialect_context             # in pos pos' cc pos'''

    dup .value dup                      # in pos pos' cc pos''' bfront bfront
      .tail_anf .ctti _                 # in pos pos' cc pos''' rctti bodyfront
      .link_return .head_anf anf_fn     # in pos pos' cc p3 rctti fn-anf

    # Define the stack arguments and compile the function into a constant value.
    # We'll return a let-binding to its compiled here-pointer.
    sget04 .value                       # in pos pos' cc p3 rctti fn-anf arglist
    [ sget02 dup .head _ .tail          # ctti::name fn cc ctti name
      sget03 .defarg sset02 =0 sset01 goto ] _ .reduce    # ...fn-anf
    here_ctti_ "cc"_ .defarg            # in pos pos' cc p3 rctti fn-anf

    .compile .here _                    # in pos pos' cc p3 fn rctti

    # Last step: let-bind the function and set its CTTI.
    sget04 .value intlist_              # in pos pos' cc p3 fn rctti acs arglist
    [ sget02 .head sget02 .<< sset02
      =0 sset01 goto ] _ .reduce .rev   # in pos pos' cc p3 fn rctti acttis
    make_fn_ctti anf_gensym             # in pos pos' cc p3 fn fn-let

    .[ .hereptr .] anf_front            # in pos pos' cc p3 fnfront
    _ .with_value                       # in pos pos' cc pos'''
    sset03 sset01 drop goto ]           # pos'''
  pflatmap };


use phi::genconst fndef_ctti => bin q{
  ctti "fndef"_ .defname
  fn_parser _ .defparser };


1;
