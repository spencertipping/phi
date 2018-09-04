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


=head2 phi2-hosted string class
Instances are mostly binary-compatible with phi1 byte strings.
=cut

use phi2::val str => q{
  let str = phi1.ctti.defname('str);
  str.fields.i64('class)
            .i32('size)
            .array(1, 'size, 'data);
  str.accessors
     .defvirtual(
        (fn(i:int, self:byte_string) (self.data + i).m8get).to_here,
        "[]") };

use phi2::val str_dispatch => q{ str.dispatch_fn };

use phi2::val test_string => q{
  let p = I.heap_allocate(8 + 4 + 3);
  p.m64set(str_dispatch.to_int);
  (p + 8) .m32set(3);
  (p + 12).m8set(97);
  (p + 13).m8set(98);
  (p + 14).m8set(99);
  p.to_byte_string };

use phi2::val test_test_string => q{
  'abc == test_string || I.die("test string is something else") };


1;
