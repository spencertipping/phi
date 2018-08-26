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


=head2 Base CTTIs
There are three of these:

1. C<baseptr>
2. C<hereptr>, which supports function behavior
3. C<int>: a primitive integer

C<baseptr> and C<hereptr> both support RTTI; C<int> doesn't, and all of its
methods are monomorphic and inline. This means C<int.+> compiles down to a
single C<iplus> instruction, plus any frame-argument addressing (which
unfortunately involves function calls at the moment, though they are direct).

You can use C<baseptr> and C<hereptr> to address phi1 objects, including classes
themselves. This is how you can define new classes/metaclasses. Pointer
arithmetic is possible by converting C<baseptr> instances to C<int> and vice
versa.

phi2 doesn't type-parameterize C<baseptr> and C<hereptr>, but it isn't difficult
to write parameterized versions of those CTTIs in phi2.
=cut

use phi::genconst int_ctti => bin q{
  struct
    "value"_ .i64
  class
    [ _ .iplus             _ goto ]_ "/binop/+"_   .defmethod
    [ _ .swap .ineg .iplus _ goto ]_ "/binop/-"_   .defmethod
    [ _ .itimes            _ goto ]_ "/binop/*"_   .defmethod
    [ _ .idivmod .drop     _ goto ]_ "/binop//"_   .defmethod
    [ _ .idivmod =0_ .sset _ goto ]_ "/binop/%"_   .defmethod
    [ _ .lit8 .0 .ieq      _ goto ]_ "/unop/!"_    .defmethod
    [ _ .ior               _ goto ]_ "/binop/|"_   .defmethod
    [ _ .iinv              _ goto ]_ "/unop/~"_    .defmethod
    [ _ .ineg              _ goto ]_ "/unop/-"_    .defmethod
    [ _ .iand              _ goto ]_ "/binop/&"_   .defmethod
    [ _ .ixor              _ goto ]_ "/binop/^"_   .defmethod
    [ _ .ishr              _ goto ]_ "/binop/>>>"_ .defmethod
    [ _ .isar              _ goto ]_ "/binop/>>"_  .defmethod
    [ _ .ishl              _ goto ]_ "/binop/<<"_  .defmethod

    [                        goto ]_ "/unop/+"_    .defmethod

    [ _ .m64get            _ goto ]_ "/unop/*"_    .defmethod
    [ _ .m64set            _ goto ]_ "/binop/:="_  .defmethod
    [ _ .swap
        .lit8 .3 .ishl
        .iplus .m64get     _ goto ]_ "/binop/[]"_  .defmethod

    [ _ .ieq                     _ goto ]_ "/binop/=="_  .defmethod
    [ _ .ieq .lit8 .0 .ieq       _ goto ]_ "/binop/!="_  .defmethod
    [ _ .ilt                     _ goto ]_ "/binop/<"_   .defmethod
    [ _ .swap .ilt               _ goto ]_ "/binop/>"_   .defmethod
    [ _ .ilt .lit8 .0 .ieq       _ goto ]_ "/binop/>="_  .defmethod
    [ _ .swap .ilt .lit8 .0 .ieq _ goto ]_ "/binop/<="_  .defmethod

    [ _ .if      _ goto ]_ "/m/if"_ .defmethod

    [ _ .bswap16 _ goto ]_ "/m/bswap16"_ .defmethod
    [ _ .bswap32 _ goto ]_ "/m/bswap32"_ .defmethod
    [ _ .bswap64 _ goto ]_ "/m/bswap64"_ .defmethod

    [ _ .m64get  _ goto ]_ "/m/m64get"_ .defmethod
    [ _ .m32get  _ goto ]_ "/m/m32get"_ .defmethod
    [ _ .m16get  _ goto ]_ "/m/m16get"_ .defmethod
    [ _ .m8get   _ goto ]_ "/m/m8get"_  .defmethod

    [ _ .m64set  _ goto ]_ "/m/m64set"_ .defmethod
    [ _ .m32set  _ goto ]_ "/m/m32set"_ .defmethod
    [ _ .m16set  _ goto ]_ "/m/m16set"_ .defmethod
    [ _ .m8set   _ goto ]_ "/m/m8set"_  .defmethod

    [ _ .memcpy  _ goto ]_ "/m/memcpy"_ .defmethod

    [ goto ]_ "/m/to_int"_     .defmethod
    [ goto ]_ "/m/to_baseptr"_ .defmethod
    [ goto ]_ "/m/to_hereptr"_ .defmethod

    # phi interop methods
    # UH OH THESE GO ON THE METACLASS TODO
    };


1;
