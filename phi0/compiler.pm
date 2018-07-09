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


use constant polymorphic_here_pointer_compiler_class =>
  phi::class->new('polymorphic_here_pointer_compiler',
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
        .dup                            # [hp hp]
        .const2 .ineg .iplus            # [hp &hmarker]
        .m16get .ineg .iplus            # [base]

        # Now do what the regular polymorphic pointer does.
        .dup
        .m64get
        .method
        swap bswap16 swap .l16
        .call
      drop sset01 drop goto             # asm });


use constant polymorphic_base_pointer_compiler_fn => phi::allocation
  ->constant(bin q{                     # m cc
    const16 i.heap_allocate             # m cc &c
    $polymorphic_base_pointer_compiler_class sget01 m64set    # [.vt=]
    sget02 sget01 const8 iplus m64set                         # [.m=]
    sset01 goto                         # m })
  ->named('polymorphic_base_pointer_compiler_fn') >> heap;

use constant polymorphic_here_pointer_compiler_fn => phi::allocation
  ->constant(bin q{                     # m cc
    const16 i.heap_allocate             # m cc &c
    $polymorphic_here_pointer_compiler_class sget01 m64set    # [.vt=]
    sget02 sget01 const8 iplus m64set                         # [.m=]
    sset01 goto                         # m })
  ->named('polymorphic_here_pointer_compiler_fn') >> heap;


use constant polymorphic_base_pointer_compiler_test_fn => phi::allocation
  ->constant(bin q{                     # cc
    protocol
      "head" swap .defmethod
      "tail" swap .defmethod
    dup                                 # cc p p

    struct
      "vtable" i64f
      "head"   i64f
      "tail"   i64f
    class
      .implement                        # cc p c
      $polymorphic_base_pointer_compiler_fn swap .compiler_fn=
      [ swap const8  iplus m64get swap goto ] swap "head" swap .defmethod
      [ swap const16 iplus m64get swap goto ] swap "tail" swap .defmethod
                                        # cc p c
    swap .allocate_vtable_slots         # cc c m
    dup sget02 .vtable                  # cc c m vt

    asm .swap                           # cc c m vt asm[swap]
    sget02 sget04 .compiler             # cc c m vt asm comp
    .'head                              # cc c m vt asm
    .swap .goto                         # cc c m vt asm[...goto]
    .compile                            # cc c m vt head-fn

    # Now stack-allocate an instance of this new cons class.
    lit8+57 lit8+84                     # cc c m vt head-fn t h
    sget03 get_stackptr sget04          # cc c m vt head-fn t h vt &cons head-fn
    .call                               # cc c m vt head-fn t h vt h

    lit8+84 ieq "cons.h==84" i.assert   # cc c m vt head-fn t h vt

    drop drop drop drop drop drop drop
    goto                                # })
  ->named('polymorphic_base_pointer_compiler_test_fn') >> heap;

use constant polymorphic_here_pointer_compiler_test_fn => phi::allocation
  ->constant(bin q{                     # cc
    # Let's do something sneaky. We can get a here-pointer to the interpreter by
    # writing some machine code to push %rdi; then we can use a polymorphic
    # hereptr class against the boot method protocol to address the interpreter
    # instance.
    #
    # In this case the goal is to use the interpreter object to fetch a global,
    # which I'll define here.

    const0 i.heap_allocate "polyhereptrtest_global" i.def

    asm                                 # cc asm [ih cc]
      .swap                             # cc asm [cc ih]
      "polyhereptrtest_global" swap
      .ptr                              # cc asm [cc ih "p..."]
      .swap                             # cc asm [cc "p..." ih]
      %method_vtable_mapping
        $polymorphic_here_pointer_compiler_fn
        call                            # cc asm hptrc
        .'global                        # cc asm [cc g]
      .swap
      .goto
    .compile                            # cc fn

    [ 57 N ] call_native                # cc fn interp_hereptr

    swap .call                          # cc g
    %polyhereptrtest_global ieq "hereptr ieq" i.assert

    goto                                # })
  ->named('polymorphic_here_pointer_compiler_test_fn') >> heap;


=head3 Monomorphic classes
Classes are used to represent value types as well, for instance bare integers.
These objects aren't addressed using pointers; instead, they are immediate stack
values whose types are fully erased. Method calls are simply function constants
that are pulled from the class definition. Here's the corresponding compiler:

  struct monomorphic_compiler
  {
    hereptr  vtable;
    strmap  *method_fns;
  }

NB: C<monomorphic_compiler> implements C<method_translator_protocol>, but
returns function here-pointers instead of integers. Monomorphic values have no
vtables, so we need to end up with a direct function linkage.
=cut


use constant monomorphic_compiler_class =>
  phi::class->new('monomorphic_compiler',
    method_translator_protocol,
    symbolic_method_protocol)

  ->def(
    '{}' => bin q{                      # m self cc
      swap const8 iplus m64get          # m cc map
      sget02 swap .{}                   # m cc map{m}
      sset01 goto                       # map{m} },

    symbolic_method => bin q{           # asm m self cc
      sget02 sget02 .{}                 # asm m self cc f
      sget04                            # asm m self cc f asm
        .hereptr                        # [lit(fn)]
        .call                           # [lit(fn) call]
      drop sset01 drop goto             # asm });


use constant monomorphic_compiler_fn => phi::allocation
  ->constant(bin q{                     # m cc
    const16 i.heap_allocate             # m cc &c
    $monomorphic_compiler_class sget01 m64set     # [.vt=]
    sget02 sget01 const8 iplus m64set             # [.m=]
    sset01 goto                         # m })
  ->named('monomorphic_compiler_fn') >> heap;


use constant monomorphic_compiler_test_fn => phi::allocation
  ->constant(bin q{                     # cc
    # Integer class
    struct
      "value" i64f
    class
      $monomorphic_compiler_fn
      swap .compiler_fn=

      [                                 # rhs self cc
        sget02 sget02 iplus             # rhs self cc v
        sset02 sset00 goto ]            # v
      swap "+" swap .defmethod

      [ sget02 sget02 ineg iplus
        sset02 sset00 goto ]
      swap "-" swap .defmethod

      [ sget02 sget02 itimes
        sset02 sset00 goto ]
      swap "*" swap .defmethod          # cc c

    dup .methods swap .compiler         # cc comp

    asm                                 # cc c asm
      .swap                             # [cc v]
      .const4                           # [cc v 4]
      sget01 .'+                        # [cc v+4]
      .const2                           # [cc v+4 2]
      .swap                             # [cc 2 v+4]
      sget01 .'-                        # [cc v+4-2]
      .swap                             # [v+4-2 cc]
      .goto                             # [v+4-2]
    .compile                            # cc c fn
    lit8+127 swap .call                 # cc c 125

    lit8+125 ieq "125" i.assert         # cc c

    drop
    goto                                # })
  ->named('monomorphic_compiler_test_fn') >> heap;


=head3 Compilers and type propagation
Right now compilers emit code, but they don't really help you in terms of
propagating type information. That is, let's suppose I have a C<< cons<int> >>;
if I call C<.head> on this, the top stack entry will now be a monomorphic value
C<int>, and I would need to know this in order to work with the result.

Put differently, not every value is runtime-polymorphic; so we need our
compiling environment to track more types than we ultimately end up with. The
simplest strategy here is to maintain a stack model in parallel with an assembly
object. The stack model contains the compiling class for each stack value.

This design introduces a few complications:

1. Compilers will need to _describe_ their operations, not just execute them
2. All polymorphic variants of an operation must share a type transformation
3. All conditional branches must share a type transformation

...in other words, every call site must be monomorphic in compiler-type terms:
we can have C<< int -> int >>, but we can't have C<< int -> int|string >> unless
C<int|string> encodes its own runtime polymorphism.

=head4 Solving (1)
There are a couple of ways we can do this. One obvious strategy is to parse the
bytecode coming out of an assembler -- but we'll inevitably lose some semantics
in the process. A better approach is to write an assembler object wrapper that
accepts type annotations and maintains stack state. Then compilers interact at a
higher level, mixing instructions and type annotations to form a more complete
picture.

=head4 Solving (2)
This is more complicated, and we have a few basic options:

1. Have the protocol assume everything is above board
2. Enumerate classes and construct polymorphic types as necessary
3. Enumerate classes and die if polymorphism is required (protocols own sigs)
4. Lift the unknown into a type-forked continuation (not really a solution)

(1) isn't really a solution; it's more about washing our hands and pushing the
problem onto the user. While I love the laissez-faire nature of doing things
this way, it's not clear to me that it should be the default behavior. There's a
saner default that involves less work for most use cases.

(2) is complicated and ultimately infeasible because it's unclear how/where the
decisional entropy should be stored. The only way for this to work is to have a
polymorphic type generator function stored in the assembler or the protocol, but
that seems clunky.

(3) is nice because we end up with predictable types at runtime. In some sense
it isn't a real solution; we're basically saying "your program needs to be
statically typed" -- so our type flow looks more like ML than it does like Ruby.
But this isn't a bad thing, particularly from a compilation perspective. We can
always push polymorphism into runtime types and address those
compilation-monomorphically. There's a case to be made for having a sharp
distinction between RTTI and CTTI.

(4) is cool but a terrible default. First, we'd be introducing a non-obvious
correlation between conditional variates and products (i.e. we're parameterizing
not only the conditional value itself, but also the continuation). Second, we
don't have an automatic way to merge these forked continuations unless we
opportunistically look for a moment where the types line up in the future. If we
didn't merge the continuations, we could end up with an exponential amount of
specialized code.

(3) gives us enough machinery to implement every other option through explicit
polymorphic delegation, so I'm going to go with that.

=head4 Solving (3)
This is an extension of (2), and our solution carries. It's exactly the same
situation we'd have in OCaml or Haskell if we used C<if> to return one of two
functions: the compiler would require the two functions to have the same type.


=head3 Typed assembler
A typed assembler behaves identically to a regular macro assembler with two
exceptions:

1. It introduces a new pseudo-instruction called C<typed>
2. It accepts symbolic methods and forwards those to values on a simulated stack

(1) is used by compilers to indicate the compile-time class of any given value
on the stack. Any unknown values will have type C<unknown>, which gives you no
method call support -- i.e. there is no specified calling convention for
interacting with an C<unknown> value.

(2) is how you interact with values on the stack. You can use the normal
primitive bytecode instructions (and C<typed>), but it's more common to delegate
to the CTTI classes by invoking their methods and allowing them to compile
specialized code.

This is also where we get protocols directly involved in compilation. Protocols
own the type transformation signatures of their classes (as per the monomorphism
guarantee above), so a polymorphic base pointer addressing a protocol-allocated
vtable map would both check the input argument types and add a C<typed>
instruction to any result values.

TODO: implement this puppy
=cut


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
