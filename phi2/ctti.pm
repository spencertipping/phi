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
Before I get into the semantic side of CTTIs, let's talk about how they work
with ANF. Suppose I write this:

  int x = 10;

If we assume C<int> parse-owns the unbound symbol C<x> and immediately delegates
to the new lvalue CTTI to parse C<=>, then the net result should be an ANF node
that looks like this:

  let x:int = [10]

So far so good. But C<x> can be used as an lvalue, since C<x = 5> later on is a
valid statement. Importantly, C<x = 5> never calls a getter on C<x>, which means
that C<=> specializes the access to C<x>. This, in turn, requires C<x> to encode
its ANF residency in the CTTI itself, since the CTTI alone is the LHS of C<=>.
And that means our lvalue CTTIs need to be storage-aware.

Luckily this isn't as bad as it sounds. We can build functions over CTTIs to
compose access and semantics, so we might have something like C<anf("x", int)>
for instance. This compound CTTI manages the ANF storage backend and implements
semantic passthrough.


=head3 C<let> vs C<int> (rvalue vs lvalue)
phi requires parse-time evaluation (really compile-time, since compilation is
single-pass), and that means we need a syntactic distinction between things that
exist at runtime and things that don't. It isn't quite as simple as
opportunistic constant-folding: we might care about the lvalue-ness of a
quantity even if its value is knowable at compile time.

This means we need two different variable allocators, which phi2 refers to as
C<let> and C<type>. Syntactically:

  let x = 5;            # rvalue only, compile-time eval
  int y = 5;            # lvalue, runtime eval
  x = 10;               # this will fail at parse-time: x has no = continuation
  y = 10;               # this will work
  let z = y;            # this will fail: y isn't knowable here

C<let> is a true parse-time evaluator, so you can use it to alias type names and
other CTTI instances. For example:

  let foo = int;
  foo x = 10;           # foo == int by this point, so it shares a parser


=head3 lvalue internals
Every value ultimately needs to live somewhere, if for no other reason than GC
atomicity. This is true even of syntactically linear values:

  foo.bar(              # takes two values:
    "bif" + "baz",      # this value needs to be GC-atomic...
    "bok" + "bork");    # ...in case this kicks off a GC

...and that means C<"bif" + "baz"> needs to be stored in the frame until after
we've computed C<"bok" + "bork"> and called into the method.

Anyway, let's talk about lvalues in general -- linear and otherwise. There are a
few different types, only one of which is ANF-backed:

1. C<anf>: C<int x>
2. C<baseptr>: C<int*>
3. C<baseref>: C<int&>

Since all of these are lvalues, the CTTI has two accessors: C<get> and C<set>.
Syntactically, though, the distinction is less clear:

  x + 10;               # x.get() + 10
  x = 10;               # x.set(10)

The translation happens in the binary operator layer, and C<get> is encoded by a
symbolic CTTI call to C<rvalue>. Here's the distinction in concatenative terms:

  x + 10;               # x.rvalue() -> let gensymN:int = [get_frameptr .x]
  x = 10;               # x.set(10)  -> let x:anf(int) = [=10]

C<rvalue> is called on every CTTI access, although rvalue CTTIs define it as a
nop.


=head3 Scope/CTTI/ANF
TODO

=cut


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
