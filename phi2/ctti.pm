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


=head2 CTTIs and ANF
Let's talk about C<int x = 10> for a minute.

First, we expect the scope to end up with a binding from C<x> to ... something.
That binding needs to support a few different uses:

  x + 5                 # use x as an rvalue
  x = 11;               # use it as an lvalue

The difference is dictated by the parse continuation (or dialect-owned operator
set), which means two related things:

1. Scopes resolve names to CTTIs that provide lvalue and rvalue continuations
2. Those CTTIs are ANF-aware and implement the C<=> binop

In practice there are two ways a CTTI can implement C<=>. One is to treat the
assignment as a cell value copy, which is how most GC'd languages work: Java,
Python, Ruby, etc. The other approach is to go the C/Perl route and copy stuff
like structs and strings by value. phi2 takes the former approach; for any
reference values C<x> and C<y>, C<x = y> causes C<x> and C<y> to point to the
same object without modifying either object's contents. The objects are unaware
that the assignment has occurred (unless you're using a refcounting pointer CTTI
or something).

The same type of thing happens for C<baseref> and other nested types:

  baseref<int>          x = (some ref);
  baseref<baseref<int>> y = (another ref);

  x + 10;               # baseref delegates rvalue parse continuation
  y + 10;               # ...recursively
  x = (new ref value);  # set x to a different ref (unlike C++)

So ... how would you implement a C++-style "forward my lvalue-ness" ref? The key
is to bind a strictly-rvalue local variable that won't accept the C<=> operator.
This will cause the operator request to fail into the next layer, which is
presumably the referent.

  realref<int> x = (some ref);
  x = 10;               # forward "=" to the ref target; x isn't an lvalue

C<realref> is able to make this decision about C<x>'s lvalue-ness because
C<realref> is how the scope was modified in the first place.


=head3 The parse anatomy of C<int x = 10>
In C and C++, C<int> is a value that names a type. phi2 looks at it exactly the
same way: C<int> is an rvalue-only name for a CTTI, one of whose parse
continuations is C<symbol>. Using that parse continuation will bind a new local
variable of whatever type C<int> dictates. (We hope it's an integer.)

That means we know this much about how the parse works:

  int x = 10;
  ---                   <- dialect+scope resolves this to an rvalue CTTI
    |--                 <- "int" parses this symbol and binds it to an ANF CTTI

So far so good. Who parses C<= 10>? It turns out it doesn't matter for C<int>.
C<int> could provide a secondary C<symbol = init> continuation, or, since C<int>
is an lvalue type, it could immediately return the new ANF CTTI, which would
then parse C<= expr> as its own imperative assignment.

rvalue-only types are different. C<int const x = 10> needs to parse C<= 10> as
its own initialization assignment so it can reject future C<=> operator
requests.


=head3 ...so for C<int x>, what exactly does C<x> resolve to?
In phi2, the following will be true (although you won't write variable
definitions this way):

  int const x         # phi2_dialect(int_ctti)
  int x               # phi2_dialect(anf_lvalue("x", int_ctti))

  int *const x        # phi2_dialect(baseptr(int_ctti))
  int *x              # phi2_dialect(anf_lvalue("x", baseptr(int_ctti)))

  int &const x        # phi2_dialect(baseref(int_ctti))
  int &x              # phi2_dialect(anf_lvalue("x", baseref(int_ctti)))

C<phi2_dialect> overlays continuations that handle binary operators and
method-invocation notation specific to phi2 syntax. It then translates these
things into calls to C</binop/X>, C</unop/X>, or C</m/X>.

phi2 considers C<const>-ness to be a default; if you want an ANF-level lvalue,
you need to indicate that. The above C-style definitions would be written like
this in phi2:

  int x               # in C, "int const x"
  int mut x           # in C, "int x"

  int ptr x           # in C, "int *const x"
  int ptr mut x       # in C, "int *x"

phi2 doesn't support refs out of the box.
=cut


use phi::genconst int_ctti => bin q{
  ctti
  dup .fields "value"_ .i64 drop

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
  [ goto ]_ "/m/to_hereptr"_ .defmethod };


1;
