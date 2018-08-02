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


=head2 CTTI assembly
Every language represents values as some mixture of CTTI (compile-time type
information) and RTTI (runtime type information), typically split as dictated by
the typing discipline. phi doesn't impose a global type system, so you have
fine-grained control over the line between compile-time and runtime.

Full evaluation is the upper bound of compile-time knowledge; at that point
there is no RTTI at all and we reduce a program to a constant or something
similarly trivial. If we refuse to follow function calls and constant-fold
conditionals, then we get a basic bottom-up type inference algorithm. If we do
no evaluation at all, we end up with something like Smalltalk: all data about
values is available as RTTI.

phi's CTTI is more involved than CTTI in most languages for two reasons:

1. CTTI bends the grammar at parse time
2. As a result, we can't trivially inspect expressions in a normal form

(2) matters because correct GC atomicity demands that we not only store any
subexpressions used by the continuation, but also, ideally, that we clear them
as soon as they _aren't_ used. (It also throws a number of monkey wrenches into
the picture for traditional compiler stuff like lambda closure conversion. I'll
get into how we deal with these problems a bit below.)


=head3 Assembler structure
Assemblers are linked lists, basically. You start with a nil base link and cons
new stuff onto it, holding onto those new links. These conses proceed
rightwards, so the interpretation is like this:

  (((nil swap) dup) drop)       -> [swap dup drop]

There are several different kinds of assembler links:

1. Low-level stack instruction (like the above)
2. Insert a constant
3. Set CTTI of stack
4. Set CTTI of frame
5. Set CTTI of interpreter

These link types are enough to fully represent the low-level operational
semantics of phi, but they're lossy: just as C<bin> lossily compiles method
calls down to their constituent C<dup m64get ...> operations, the links encoding
a method call don't contain information about which method call was made, or
even that one was made at all. Nor should they.
=cut


1;
