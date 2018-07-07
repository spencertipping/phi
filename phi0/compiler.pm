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


=head2 Classes are compilers
A class object serves two purposes:

1. To generate a vtable of itself for runtime polymorphism
2. To translate method call requests into compiled code

(1) is pretty trivial and already implemented in L<phi0/metaclass.pm>; (2) is
where things get both interesting and useful.

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

I can ask this class object for a compiler; this object implements all of
C<cons>'s protocols, but compiles code into a macro assembler rather than
running the method calls directly. For example:

  # NB: this is broken
  asm                                   # asm[]
  $a_cons_instance swap .ptr            # asm[a_cons]
  $cons_class                           # asm[a_cons] class
    .compiler                           # asm[a_cons] compiler
    .head                               # asm[a_cons .head]

The above code _almost_ works. The only problem is that classes and compilers
aren't quite as closely related as that API suggests. Classes are mutable
objects, which means that we could at any point add a new protocol to a class
and potentially change the method allocation in the process. Because this would
break everything silently, we have one more layer of indirection:

  asm...                                # asm[]
  $cons_class .protocols
    const0 swap .[]                     # asm[...] proto
  .allocate_vtable_slots                # asm[...] mindexes
  $cons_class                           # asm[...] mindexes class
    .compiler                           # asm[...] compiler
    .head                               # asm[... .head]

This fixes the problem: C<.compiler> now creates a compiler for a specific
vtable allocation.


=head3 Pointer classes and other proxy objects
Pointers and here-pointers are classes rather than builtins. They end up
wrapping operations like C<m64get>, but in a more structured way; later on we'll
use this to generate the GC tracing algorithm for most objects (this happens in
C<phi2>).

Proxying involves generating a compiler that mirrors the functionality of the
pointer's referent; it's really more of a protocol translator than a compiler.
In the example above, the compiler converts from the boot protocol to the method
allocation strategy we generated -- i.e. from the calling to the callee
protocol.

What happens when we define new methods that don't have vtable allocations in
the calling protocol though? This is where we use symbolic methods. I've written
a notation for this in C<bin>:

  .'foo           # expands to "foo" swap .symbolic_method

This lets us break out of the protocols we've written in the C<phi0> bootstrap
code.
=cut

use constant symbolic_method_test_class =>
  phi::class->new('symbolic_method_test',
                  symbolic_method_protocol)

  ->def(
    symbolic_method => bin q{           # m self cc
      # Just return the method name as a string.
      sset00 goto                       # m });


use constant symbolic_method_test_fn => phi::allocation
  ->constant(bin q{                     # cc
    $symbolic_method_test_class         # cc vt
    get_stackptr                        # cc vt &vt

    dup .'foobar                        # cc vt &vt "foobar"
      "foobar" .== "'foobar" i.assert   # cc vt &vt

    dup .'symbolic_method
      "symbolic_method" .== "'symbolic_method" i.assert

    drop drop                           # cc
    goto                                # })
  ->named('symbolic_method_test_fn') >> heap;


=head3 Compilers and method linkage
Compilers determine whether a method call involves vtables or whether it turns
into a directly-linked function invocation (or is just inlined). This means that
compiler objects themselves are aware of how polymorphic their referents are.

Let's go ahead and write a couple of polymorphic pointer types that support
custom vtable allocation. Here are their structs:

  struct polymorphic_base_pointer_compiler
  {
    hereptr  vtable;
    strmap  *method_allocation;
  }

  struct polymorphic_here_pointer_compiler
  {
    hereptr  vtable;
    strmap  *method_allocation;
  }

Each of these classes uses symbolic methods and translates into vtable method
calls. Because we know the value is a pointer, they'll do what we've been doing
with C<bin> macros so far: C<.foo> duplicates the object, gets the vtable,
selects a method, and calls it.
=cut


use constant polymorphic_base_pointer_compiler_class =>
  phi::class->new('polymorphic_base_pointer_compiler',
    symbolic_method_protocol,
    method_translator_protocol)

  ->def(
    '{}' => bin q{                      # m self cc
      swap const8 iplus m64get          # m cc map
      sget02 swap .{}                   # m cc map{m}
      sset01 goto                       # map{m} },

    symbolic_method => bin q{           # asm m self cc
      sget02 sget02 .{}                 # asm m self cc mi
      sget04                            # asm m self cc mi asm
        .dup
        .m64get
        .method
        swap bswap16 swap .l16          # asm m self cc asm
        .call
      drop sset01 drop goto             # asm });


use constant polymorphic_base_pointer_compiler_test_fn => phi::allocation
  ->constant(bin q{                     # cc
    # TODO: build a class+protocol and compile them using symbolic methods
    "TODO: write this test" i.pnl
    goto                                # })
  ->named('polymorphic_base_pointer_compiler_test_fn') >> heap;


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

=cut


1;
