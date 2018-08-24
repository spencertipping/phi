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


=head2 CTTI metaclass
This inherits from C<class> but supports CTTI-specific functionality including:

1. Runtime projection
2. Parse continuations
3. Method resolution

Here's the struct:

  struct ctti_class
  {
    hereptr              vtable;
    struct*              fields;
    strmap<hereptr<fn>>* methods;
    strmap<hereptr<fn>>* virtuals;
    intmap<protocol*>    protocols;
    hereptr<fn>          parser_fn;
    strmap<*>            dialect_metadata;
  };

=cut

use phi::class ctti =>
  ctti_protocol,
  class_protocol,
  compilable_class_protocol,
  mutable_class_protocol,

  class_class->methods_except('+'),

  "exists_at_runtime?" => bin q{        # self cc
    _.fields .right_offset inot inot_ goto },

  parser_fn => bin q{_=40 iplus m64get_ goto},
  parse     => bin q{                   # in pos self cc
    sget01 .parser_fn goto              # ->parser_fn };


1;
