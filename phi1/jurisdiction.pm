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


=head2 Jurisdictions
phi is portable, but not in the imperialistic way that Java is. Rather than
building up a full world of uniformity, phi uses domains where certain stated
assumptions hold and compiles code accordingly. These domains are called
jurisdictions.

Code in the same jurisdiction can interoperate efficiently. Anything going on
outside a jurisdiction requires a mediating object to translate between calling
and value storage conventions.

Structurally, a jurisdiction governs the following behaviors:

1. Size and endianness of integers
2. Backend memory model: managed vs flat
3. Backend native compilation characteristics
4. Backend language
5. Object method addressing protocol
6. How many stack slots are used to encode each value

Jurisdictions and runtimes aren't the same thing: a runtime can have multiple
jurisdictions for various reasons. You'd likely do this in Javascript if you
were targeting ASM.js or WebAssembly, for example.


=head3 Jurisdictions and compilation
All code exists within some jurisdiction that governs aspects of that code's
semantics. For example, let's compile a simple function into the current
jurisdiction. We can get that from the interpreter.

  i.jurisdiction                        # j
  tasm                                  # asm [xs cc]
    .swap       $cons_class swap .typed # [cc xs:cons]
    .dup  .'head $int_class swap .typed # [cc xs h:int]
    .swap .'tail $int_class swap .typed # [cc h:int t:int]
    .'+                                 # [cc h+t]
    .swap .goto                         # [h+t]
  .compile                              # fn
  const1 const2 ::                      # fn xs
  swap .call                            # 3

It's common to use the hosting jurisdiction like this to build an assembler that
JITs new functions into the current environment.


=head3 Method calls and typed assembler interop
The cons/int example above involves some delegation from the asm object to the
jurisdiction, specifically around two things:

1. Stack manipulation of pointer values
2. Method calls against pointer values (or more generally, reference types)

(1) is almost always a passthrough; pointer values should be 1:1 mapped to stack
slots unless you're doing something quite elaborate like referring to remote
objects with compound addresses (although if you are, you should be using
proxies rather than doing it this way). I may reduce this to a global invariant
depending on how much it complicates the implementation.

(2) involves translating a symbolic method call to a compiled one. The stack is
assumed to contain C<...args receiver> and should end up as C<...return>; this
is invariant across jurisdictions. Any code that gets us between those states is
fair game.

Internally, here's how the typed assembler asks to generate a method call:

  asm "foo" $type i.jurisdiction        # asm m t j
    .method_call                        # asm


=head3 Base objects and quasi-inheritance
A jurisdiction specifies a set of metaclasses that are applied to produce
objects that comply with the method calling convention. For example, the
C<vtable> attribute present on all of the phi1 bootstrap classes would normally
be installed by the AMD64 native jurisdiction's "polymorphic reference type"
metaclass.

Every class instance requires a monomorphic method call to C<construct()> upon
allocation. This call sets up the vtable, which enables the class to function in
a polymorphic context.

Timing for C<construct> is subtle: you must call it _before_ any further
allocations are made. The reason is GC atomicity: if you have an uninitialized
vtable slot and GC kicks in, the vtable method dispatch for the GC protocol will
cause a segfault.

TODO: do we want some type of negotiated allocation strategy?


=head2 AMD64 native jurisdiction with vtable polymorphism
...basically, the one we've been assuming throughout phi1. It collects classes
and compiles vtables on initialization, then uses metaclasses to store those
vtables on instances.
=cut




1;
