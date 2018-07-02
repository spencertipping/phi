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


=head2 Compilation
The goal here is to get to a world where phi can parse source code and generate
new data structures that exist within the heap. There are a few pieces involved:

1. Low-level parsers (in L<phi0/parsers.pm>)
2. High-level parse machinery (TODO)
3. Low-level codegen (macro assembler in L<phi0/classes.pm>)
4. High-level codegen (metaclasses TODO)

Code we generate via classes will look very different from the concatenative
stuff we've been writing. First, all functions will allocate their own local
frames to structure their stack state for GC. This and the surrounding
management code is automatic and relies on structs (L<phi0/metaclasses.pm>).

Second, inline bytecode functions won't use the C<get_insnptr>+offset hackery
they currently do. Instead, they'll refer to full bytecode objects as external
references -- this is both easier to generate and faster at runtime (although it
creates slightly more GC overhead until we implement allocation fusion).

Third, _there's no more C<cc> silliness preventing you from doing awesome stack
stuff_. Because functions have a frame pointer, the function prolog stashes the
calling continuation up front, does unadulterated datastack awesomeness, and
then restores it at the last minute before returning. The way Chuck Moore
intended.

This compiler implementation is the external base case of a fixed point: we'll
need to rewrite everything so far in our resulting syntax. That turns out not to
be difficult because much of the bulk of our existing code will end up being
automatically generated. phi is a much simpler language than its current library
state would suggest.

Structurally speaking, C<phi1>'s job is read C<phi2.phi> to emit C<phi2>, which
will then recompile itself to produce the real phi image. At that point phi is
properly bootstrapped and C<phi2> can compile further images as the language
evolves. (Every breaking revision to the language needs to be saved, though, so
we can automate the process of going from perl+phi0 to the latest version.)


=head3 Class/struct interfacing and metaclasses
Let's take a simple class like a cons cell:

  class cons<T>
  {
    struct
    {
      hereptr<vtable>  vtable;
      T                head;
      baseptr<cons<T>> tail;
    }

    T                head();
    baseptr<cons<T>> tail();
    int64            length();
    bool             nil?();
    cons<T>          +(cons<T> rhs);
    T                [](int64 index);
    <U> U            reduce(U x0, (U, T) -> U fn);
  }

This is API-compatible with C<phi1>'s implementation of C<cons>. We use type
parameters to influence GC semantics, particularly around C<baseptr> vs
C<hereptr>. phi doesn't require you to statically type your code, but you get
performance and layout advantages when you do.




=head3 Parse-time classes and codegen
Macro assemblers receive method calls and emit code. Codegen classes are just
one step beyond that: they participate in computed grammars and emit code using
a macro assembler.

TODO


=head3 Mono/poly containers
Let's start with bare structs:

  struct cons_data { head, tail }

Structs don't specify any semantics, but they do specify data layout and
allocation. Then we have two class wrappers:

  class mono_of(struct, vtable);        # poly-disabled monomorphic instance
  class vtable_poly_of(struct);         # poly-enabled monomorphic instance

Each of the resulting classes refers to a nested struct that stores any
additional data we need. This design implies some things:

1. Field access is symbolic (which we'd want for all kinds of reasons)
2. Structs support nesting and jointly-flat allocation
3. Ideally, structs support subfield flattening
4. Classes and structs are distinct abstractions
5. Classes are dynamically-compiled things given vtable closure
6. Class logic is trivially portable to different struct access models (via (4))

Overall I like this picture.
=cut


1;
