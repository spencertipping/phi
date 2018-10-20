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
use bytes;


=head1 OOP and method calls
In phi1, all OOP happens at runtime using polymorphic dispatch. The calling
convention is simple enough; each object begins with a hereptr to a function
that resolves method hashes to implementations. That is:

  (g64 object) :: hereptr (int -> hereptr (args... offset base -> ...))

Function addresses are always at least 4-byte aligned, and method hashes always
have their least-significant bit set. This means that, as quadwords, the two
value spaces are disjoint, which makes it possible to use C<mfnd> on a packed
array to fetch the function for a given method hash.

C<ptr> and C<hereptr> differ slightly in how we invoke methods:

  (ptr).method(...)     -> [ <ptr> l8(0) swap l64(hash) sget(1) call call ]
  (hereptr).method(...) -> [ <ptr> unh4       l64(hash) sget(1) call call ]


=head2 Method hashing
We need a way to convert method names to stable 64-bit values. I'm using a
murmur2-64 hash here, murmurhash2A to be specific, ported from
L<https://github.com/abrandoned/murmur2/blob/master/MurmurHash2.c>.
=cut

use constant murmur2_m => 0xc6a4a7935bd1e995;
use constant murmur2_r => 47;

sub murmur2a($$)
{
  use integer;
  use bytes;

  my $seed = shift;
  my $h    = $seed ^ length $_[0];

  for my $k (unpack 'Q<*', $_[0] . "\0\0\0\0\0\0\0")
  {
    $k *= murmur2_m;
    $k ^= $k >> murmur2_r;
    $k *= murmur2_m;

    $h ^= $k;
    $h *= murmur2_m;
  }

  $h;
}

# NB: always set LSB so we can differentiate between hashed values and base/here
# pointers (the latter are aligned)
sub method_hash($) { 1 | murmur2a 0, shift }


1;
