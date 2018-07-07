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


=head2 Classes are compilers
A class object serves two purposes:

1. To generate a vtable of itself for runtime polymorphism
2. To translate method call requests into compiled code

(1) is pretty trivial and already implemented in L<phi0/metaclass.pm>; (2) is
where things get both interesting and useful.

TODO


=head2 Defining classes
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
C<hereptr>. The type definitions themselves are less static than is implied by
the above code; any monomorphism is parameterized to the invocation site, not
globally. Classes are responsible for constructing compile-time proxy values
that encode known type information, when applicable.


=head3 Compiled code
Let's take a simple function like C<rev>, which reverses a list. The simplest
tail-recursive concatenative design looks like this (assuming a required second
arg of nil):

  rev = [                               # xs t cc
    [ sget02 .nil?                      # xs t cc loop nil?
      [ drop sset01 swap goto ]         # t
      [ sget02 sget04 .head :: sset02   # xs  x::t cc loop
        sget03 .tail sset03             # xs' x::t cc loop
        dup goto ]                      # ->loop
      if goto ]                         # xs t cc loop
    dup goto ]                          # ->loop

This design isn't GC-atomic, though, which it needs to be given that it
allocates cons cells. To fix this, we need to allocate frames. That code uses a
function prologue and looks like this:

  rev = [                               # xs t cc
    get_frameptr                        # xs t cc f

    [                                   # xs t cc f loop vt|
      get_frameptr .xs .nil?
      [ get_frameptr .t                 # |t
        get_frameptr .ret ]             # t (unwinds the frame)

      [ get_frameptr .t                 # |t
        get_frameptr .xs .head          # |t x
        ::                              # |x::t
        get_frameptr .t=                # |

        get_frameptr .xs .tail          # |xs'
        get_frameptr .xs=               # |

        # Reuse the same frame and tail-call
        get_frameptr .loop goto ]       # ->loop

      if goto ]                         # xs t cc f loop

    $rev_frame_vtable                   # xs t cc f loop vt
    get_stackptr set_frameptr           # xs t cc f loop vt|

    get_frameptr .loop goto ]           # ->loop

The above is a bit of a lie in that we actually don't make any polymorphic
method calls since we already know the vtable's class (i.e. we can inline all of
the methods) -- but otherwise that's what's going on. We do need the frame
vtable either way because the GC requires it. After inlining, we end up with
this:

  rev = [                               # xs t cc
    get_frameptr                        # xs t cc f
    [                                   # xs t cc f loop vt|
      fget05 .nil?
      [ fget04 fset05                   # t t cc f loop vt|
        drop drop set_frameptr          # t t cc
        sset00 goto ]                   # t
      [ fget04 fget05 .head :: fset04   # xs  x::t cc f loop vt|
        fget05 .tail fset05             # xs' x::t cc f loop vt|
        fget01 goto ]                   # ->loop
      if goto ]                         # xs t cc f loop

    $rev_frame_vtable                   # xs t cc f loop vt
    get_stackptr set_frameptr           # xs t cc f loop vt|
    fget01 goto ]                       # ->loop

Most of the structure in this function is identical to the concatenative
version, but uses C<fget>/C<fset> instead of C<sget>/C<sset>. In particular, the
tail-recursive loop is exactly the same number of operations in both
implementations; all of the overhead we've added is in the frame setup/teardown.


=head3 Class compiler API
If we want to produce the compiled C<rev> function by addressing classes, we
need to create a frame struct/class first. This class provides an entry point to
the function's logic:

  frame_class                           # c
  .[                                    # casm [xs t cc f 0 vt|]
    # TODO: how do we init loop?
    # TODO: how do we generate a call to ::, which isn't abstract?
    # TODO: how do we do if-branching?
  .]                                    # rev_fn


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
