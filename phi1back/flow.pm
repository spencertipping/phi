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


=head3 Basic blocks and linking
Each flow assembler behaves like C<bin> without support for C<[> and C<]>: it's
an uninterrupted chunk of code that runs from start to end. We can build
C<[...]> blocks by referring to other flow assembler objects. This is done using
a custom link-a-block link.


=head3 Stack/frame interfacing
Flow assemblers are made up of links, which fall into a few categories:


=head4 C<push_frame>
We need to know two things to create a frame:

1. The full set of ref IDs and their CTTIs
2. The incoming stack layout, in terms of this new refset

Frame classes in general are generated and managed by the flow assembler; this
coupling exists because the flow assembler is responsible for moving values
between the stack and the frame, which entails addressing the object somehow.

Q: is it worth designing this in terms of metaclasses or protocols or something?
We also need to tell the interpreter how to address the frame pointer for GC
purposes, so the abstraction escapes flow assemblers.


=head4 C<update_frame>
Modifies one or more values stored in the refset. This link stores three things:

1. Initial stack layout, in terms of ref IDs
2. Concatenative code
3. Final stack layout, in terms of ref IDs

NB: it doesn't make sense for C<update_frame> links to be able to modify the
CTTI of a given slot. This introduces a dependency on control flow, which means
we're dealing with RTTI not CTTI.


=head4 C<link_code>
This stores a flow assembler pointer and gives you a way to add a code-hereptr
to the compiled result of a separate flow assembly. In other words, this makes
it possible to link multiple flow assemblers together.

C<link_code> produces a value compatible with the C<< hereptr<bytecode> >> CTTI.
That is, you can use it directly with a C<goto> or C<call> instruction, but the
result is addressable as an object.


=head4 C<pop_frame>
Flattens selected frame entries onto the stack and restores the parent frame
object. All we store is the final stack layout.


=head4 C<return>
This just emits a single C<goto> instruction. You would use this to execute the
return after using a C<pop_frame> link that placed the return continuation into
the topmost stack entry.


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
similarly trivial. If we refuse to follow function calls or constant-fold
conditionals, then we get a basic bottom-up type inference algorithm. If we do
no evaluation at all, we end up with something like Smalltalk: all data about
values is available as RTTI.
=cut


1;
