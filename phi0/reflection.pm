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


=head2 Reflective encoding
Let's take various parts of C<phi1> and reuse them for C<phi2> to save effort.
We now have data structures we can work with, so we can generate class instances
and link them into lists/etc.


=head3 Bytecode implementations
This is a good place to start. We can bundle each machine code snippet into a
bytecode object, then store a list of all of them.
=cut

use constant bytecode_native_list =>
  list map defined bytecodes->[$_]
             ? refless_bytecode(bytecodes->[$_])
             : 0, 0..255;


=head3 Protocols
These are pretty simple objects and they aren't backed by metaclasses at this
point. Instead, we just export list/map data structures that describe each
object.
=cut

sub protocol_to_phi($)
{
  list map str($_), shift->methods;
}

use constant protocol_list =>
  list map protocol_to_phi($_), @{+defined_protocols};


=head4 Method/vtable mapping
This makes it possible to symbolically link code to boot objects, and to
disassemble existing code with method name annotations. We export a single map
from method name to its vtable index.
=cut

use constant method_vtable_mapping =>
  str_kvmap map +(str($_) => method_lookup->{$_}),
                sort keys %{+method_lookup};


=head3 Classes
TODO
=cut


=head2 Tests
Just some sanity checks to make sure we've exported the globals properly.
=cut

use constant reflection_test_fn => phi::allocation
  ->constant(bin q{                     # cc
    %bytecode_natives .length lit16 0100 ieq i.assert
    %bytecode_natives lit8 lit64 swap .[]
      .here                             # cc &lit64-data
      dup m8get              lit8 48 ieq i.assert
      dup const1 iplus m8get lit8 ad ieq i.assert
      dup const2 iplus m8get lit8 48 ieq i.assert
      dup lit8+3 iplus m8get lit8 0f ieq i.assert
      drop

    # Check method index manually
    [ :length ] const1 iplus m16get bswap16
    "length" %method_vtable_mapping .{} ieq i.assert

    # Make a manual method call to the protocol list
    %protocol_list                      # cc plist
    dup .length swap                    # cc plen plist
    "length" %method_vtable_mapping .{} # cc plen plist :len

    asm
      .swap .dup .m64get .method        # cc plen plist :len asm
      swap bswap16 swap .l16            # cc plen plist asm
      .call .swap .goto
    .compile .here call                 # cc plen plen2
    ieq i.assert

    goto                                # })
  ->named('reflection_test_fn') >> heap;


1;
