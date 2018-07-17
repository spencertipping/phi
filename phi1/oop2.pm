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


=head2 CTTI and RTTI
Most languages combine compile-time and runtime type information; for example,
C++ incorporates RTTI only when you use virtual methods on classes. CTTI is
fully erased at runtime and is an opportunity for metaprogramming if you have a
compile-time interpreter (e.g. Lisp macros).

Viewed in that light, RTTI is an interpretation of CTTI data fields added to an
object when we decide to make it virtual. Nothing about C++ requires us to use
its virtual method implementation, for instance. We could build our own vtable
structures and prepend a vtable pointer to each object.

Languages like Java and Ruby make expensive trades to provide ubiquitous RTTI.
Because "instance of X" really means "instance of X or any subclass," there's no
mechanism in Java to do what C and C++ let you do with struct value inlining.
RTTI as a language construct means you no longer own memory: every object is
required to speak the same inlined RTTI protocol.


=head3 Blurring the line
CTTI and RTTI aren't always different. I could easily imagine migrating things
between the two during a project, particularly if I decide that a previously
monomorphic abstraction now needs to be polymorphic for some reason. For
example, maybe I'm working with bytestrings and later want to introduce a
variant that represents its data differently. It's good practice to define the
contracts you want an object to provide before interoperating with it, which
gives you a good foundation for dynamic dispatch even if there's only one method
calling target.

...so given that, what is CTTI good for? As a method-invocation strategy its
only redeeming quality is marginally better performance -- specifically, one
cache miss on vtable dispatch.

CTTI's real advantage comes in when we use a macro assembler and allow CTTI to
dictate callee-side logic. Now pointers, integers, and pretty much every
primitive type can be implemented as a phi-owned type, types become their own
compilers, and we can define method calling conventions. CTTI isn't about
classes at all; it's about capturing a piece of the compiler's state, and
frontend language constructs are about modifying that state. We effectively get
an abstract interpreter for free.

In this world we should _differentiate_ CTTI from RTTI as much as we can. To the
extent that objects are typed at compile time, they're parameterizing the
compiler itself; RTTI is about any abstraction that even enables polymorphism: a
call to a linked subroutine as opposed to inline code, or a method call.
Recursion is forced to use unerased control flow because self-reference demands
simulated polymorphism for the fixed point.


=head3 Making RTTI fast + extensible
Let's start with extensibility. Smalltalk and Ruby prioritize this, which is why
you can add methods to classes at runtime and get features like
C<method_missing>. There's no expectation that you'll predefine any interaction
protocols beyond "I'll send you a method call."

C++ goes entirely in the other direction: you define the protocols in detail and
have absolutely no way to extend anything after compilation. The upside is that
it's only a few clock cycles slower than a direct function call.

Any middle ground usually involves some kind of JIT. Javascript/V8 will compile
vtables under some circumstances, but is also willing to deoptimize (I think).
Java supports runtime-extensible classes and interfaces through the egregious
ClassLoader API, although it gives you no way to modify existing classes.

If we want fast + extensible in phi, we have a few options:

1. RTTI pointer is a bytecode function that handles dispatch
2. RTTI pointer is a vtable with a symbolic-method backdoor (how to optimize?)
3. RTTI pointer is a symbol -> function dictionary with a hot-entry cache
4. RTTI pointer is a vtable indirect that's reallocated for every modification

Each of these implementations can be swapped in using CTTI selection, depending
on what we end up going for.

Here's how they compare to optimal static vtables:

=head4 Bytecode fn
This might be our best option. The function can inline not only the offset
table, but also the methods themselves for better cache locality. The dispatch
logic should be fast if we use byte-dereferencing code trees and assume no OOB
methods are issued. (C<goto> works as a pointer dereference if bytecode is
data.)

=head4 vtable + backdoor
Optimizing this requires recompilation and some type of GC-driven compaction.
This may be fine, but I'm not sure I want to rely on heap rewriting to make OOP
fast. I'm nixing this one.

=head4 Dictionary + cache
This is one strategy that (1) might take, but conveniently it lets us detect OOB
methods (unlike a recursive-goto approach). It's unclear how much the cache will
help us, although there's a good chance that ordered branch-and-compare ends up
being faster than a single jump -- particularly if those branches are correctly
predicted.

=head4 Indirect vtables
Simple, one extra dereference, a few extra cache misses but probably nothing
awful. This is probably the fastest option of the four. Inlined callee logic,
which is always good.


=head3 phi1, from phi2's point of view
We're going to end up coding several method calling conventions as CTTI into
phi2, so as long as phi1 does anything remotely sane we should be ok. I think
this means the existing phi0/phi1 OOP model will work just fine for
bootstrapping.

Jurisdictions aren't a great API and won't make it into phi2 -- but it should be
ok to keep them for phi1. All we need is a CTTI-compatible macro assembler,
which phi1 provides. (I'll probably change the documentation around
jurisdictions to reflect their new less-universal nature.)

TODO: integrate these docs into oop.pm or otherwise into the rest of phi1's
source.
=cut


1;
