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


=head2 Continuation flow
phi's compiler consists of a series of stack conversions, each adapting the set
of arguments passed into the continuation of the current expression. This set of
arguments is called the "refset," and we care about it because we end up using
it to generate the object stored in the frame pointer. This object is the GC
root.

Call frames can be stack or heap-allocated; the interpreter doesn't need to know
the difference. The frame object will handle GC slightly differently depending
on which it is (if stack-allocated, it won't try to write itself into the new
heap).


=head3 Abstracts
Every refset entry is represented by an abstract, which is the compile-time
projection of a runtime value. Each refset transformation is a simple graph of
abstracts, or more precisely, a graph-cons operation. These graphs are distinct
and refer to previous stages using refset IDs (variable names, usually). phi
makes no distinction between variables and anonymous quantities, nor does it
differentiate between aliased and linear values. All scope-level GC is managed
by looking for continuation references to currently-defined refset values.


=head3 Refsets and frames
Refsets exist on a stack at compile time. When you write a function, you'll
typically tell the flow assembler to create a new refset, optionally inheriting
(always by value) some refs from the lexical parent. Lambdas are instances of
classes; it's up to the parsing abstracts to generate classes and instantiate
them.

You can create functions that don't allocate their own refsets/frames; this is
how C<if>, C<while>, and similar constructs are implemented. A new refset is
required only when a function is re-entrant with respect to the calling frame.
phi doesn't automatically detect this; it's up to you to manually specify --
although phi _does_ require that any function inheriting a refset be
non-escaping; that is, every caller must be known. The function is then inlined
at every call site, more or less.


=head3 CTTI
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
=cut


1;
