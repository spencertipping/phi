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


=head2 Dialects and parse states
Dialects don't define parse states. Instead, the parse state contains a pointer
to an opaque dialect-owned object that maintains whatever state is required for
the dialect to work. For example, a C-style dialect might track the current
operator precedence, information about the runtime scope, a list of C<typedef>s,
and a dictionary of preprocessor C<define>s.

Dialect context objects provide some negotiation to manage handoff between
languages. There are a few degrees of integration:

1. Comment/whitespace syntax (semantic identity)
2. Symbol resolution, if applicable
3. Surrounding operator precedence, if applicable
4. Expression parsing, if applicable

The "if applicable" stuff is optional and negotiated by a "which features do you
support" protocol. I'm using a bitmask for this because there ultimately aren't
that many different language paradigms we'll need to consider, and because I
don't want things to break if I add a new one down the line.
=cut

use phi::genconst dialect_feature_infix_ops         => bin q{ =1 };
use phi::genconst dialect_feature_symbol_resolution => bin q{ =2 };
use phi::genconst dialect_feature_expressions       => bin q{ =4 };

use phi::protocol nested_dialect =>
  qw/ parent /;

use phi::protocol dialect_negotiation =>
  qw/ feature_bitmask
      semantic_identity
      identifier_to_ctti
      operator_allowed?
      expression_parser /;

use phi::protocol dialect_aware_parse_state =>
  qw/ dialect_context
      with_dialect_context /;


=head3 Dialect-aware parse state
Pretty simple: this is just a string parse state with a dialect context object
attached to it.

  struct dialect_aware_string_parse_state
  {
    hereptr vtable;
    int64   index;
    *       value;
    *       dialect_context;
  }

=cut

use phi::class dialect_aware_string_parse_state =>
  clone_protocol,
  parse_position_protocol,
  linear_position_protocol,
  dialect_aware_parse_state_protocol,

  "fail?"         => bin q{=0 sset01 goto},
  index           => bin q{_=8  iplus m64get_ goto},
  value           => bin q{_=16 iplus m64get_ goto},
  dialect_context => bin q{_=24 iplus m64get_ goto},

  # NB: shallow clone
  clone => bin q{                       # self cc
    =32 i.heap_allocate                 # self cc new
    sget02 sget01 =32 memcpy            # self cc new [new=self]
    sset01 goto                         # new },

  "+" => bin q{                         # n self cc
    _.clone                             # n cc new
    dup .index sget03 iplus             # n cc new index'
    sget01 =8 iplus m64set              # n cc new [.index=]
    sset01 goto                         # new },

  with_value => bin q{                  # v self cc
    _.clone                             # v cc new
    sget02 sget01 =16 iplus m64set      # v cc new [.value=]
    sset01 goto                         # new },

  with_dialect_context => bin q{        # c self cc
    _.clone                             # c cc new
    sget02 sget01 =24 iplus m64set      # c cc new [.dialect_context=]
    sset01 goto                         # new };


1;
