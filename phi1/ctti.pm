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
This inherits from C<class> but supports CTTI-specific functionality including
runtime projection and parse continuations.

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

It's worth noting that the C<parser_fn> takes C<in pos self> as arguments,
rather than just the usual C<in pos>. This allows you to compute the grammar on
the receiver's value.
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
    sget01 .parser_fn goto              # ->parser_fn },

  dialect_metadata => bin q{_=48 iplus m64get_ goto};

use phi::fn ctti => bin q{              # struct pfn cc
  =56 i.heap_allocate                   # struct pfn cc c
  $ctti_class      sget01 m64set        # [.vtable=]
  sget03 sget01 =8  iplus m64set        # [.fields=]
  strmap sget01 =16 iplus m64set        # [.methods=]
  strmap sget01 =24 iplus m64set        # [.virtuals=]
  intmap sget01 =32 iplus m64set        # [.protocols=]
  sget02 sget01 =40 iplus m64set        # [.parser_fn=]
  strmap sget01 =48 iplus m64set        # [.dialect_metadata=]
  sset02 sset00 goto                    # c };


use phi::testfn ctti => bin q{          #
  struct
    =8 "foo" const_field
    =9 "bar" const_field
  [ drop pnone .parse ]
  ctti
    accessors                           # ctti

  dup .parser_fn m8get lit8 drop ieq "pfn drop" i.assert
  dup .exists_at_runtime? inot "ctti noruntime" i.assert
  dup .fields "foo"_ .{} =0_ .get =8 ieq "foo8" i.assert
  drop };


1;
