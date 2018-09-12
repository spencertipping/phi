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


=head2 Cons cells
Just a simple way to pair up a couple of things. This is useful when you don't
to define a more complicated structure.

  struct cons
  {
    hereptr class;
    *       head;
    *       tail;
  };

=cut

use phi::protocol cons =>
  qw/ head
      tail /;


use phi::class cons =>
  cons_protocol,
  head => bin q{_=8  iplus m64get_ goto},
  tail => bin q{_=16 iplus m64get_ goto};


use phi::fn cons => bin q{              # t h cc
  =24 i.heap_allocate                   # t h cc c
  $cons_class sget01 m64set             # [.class=]
  sget02 sget01 =8  iplus m64set        # [.head=]
  sget03 sget01 =16 iplus m64set        # [.tail=]
  sset02 sset00 goto                    # c };

use phi::binmacro '::' => bin q{cons};


1;
