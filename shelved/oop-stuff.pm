=head2 Classes and protocols
In a world with only classes, we'd need to allocate a separate vtable index for
every single method, which would take up a lot of space:

  # the world without protocols: every class contains four vtable entries
  class A {
    method foo;     # vtable index 0
    method bar;     # vtable index 1
  }
  class B {
    method foo;     # vtable index 0
    _               # vtable index 1 is blank to avoid A::bar conflict
    method bif;     # vtable index 2
    method baz;     # vtable index 3
  }

This is equivalent to every class belonging to a single protocol that contains
the union of everyone's methods.

We can fix this by specifying which classes are eligible for which virtual
method calls, in this case by adding them to a protocol object:

  protocol has_foo {
    method foo;     # same vtable index within all member classes
  }
  class A : has_foo {
    method foo;     # vtable index 0
    method bar;     # vtable index 1
  }
  class B : has_foo {
    method foo;     # vtable index 0
    method bif;     # vtable index 1 (no collision with A::bar)
    method baz;     # vtable index 2
  }

This is basically how method resolution works in statically-typed languages like
Java or C++; phi also uses static typing when it comes to classes/methods, it
just has more support for runtime code generation.

Every virtual method invocation, then, addresses a protocol rather than a class.
Any class-focused method invocations are static function calls and are likely to
be inlined during compilation.

NB: we can't modify vtables once they're allocated, so compiled classes,
vtables, and dynamic method calls are all immutable once you start using them.
It's theoretically possible to enumerate heap objects and rewrite their vtables
if all you're doing is adding new methods to things, but that's a library thing
instead of something built into the object system. (Q: can we use the GC process
to upgrade classes?)


=head3 Classes and structs
phi separates these two ideas: classes specify the set of methods an object
supports, and structs are used by those methods to address the memory allocated
for the object. Technically, though, there's no requirement that a class and a
struct have anything to do with each other; you could write a class that
accessed memory directly if you wanted to (with the caveat that the class
wouldn't be portable to memory-managed backends).

In other words, phi's C<struct> is a library that will do your memory access and
help you build the garbage collection methods all classes are required to
support. You should use C<struct> because it's totally groovy, not because you
have to. Life's too short for fascist programming languages.


=head3 Structs
C's C<struct> implementation is simple and fairly limited. phi's structs need to
be a little beefier to support polymorphic the dynamically-linked objects
involved with JIT. Specifically, we support two things that C structs don't
provide:

1. "Here-pointers" into the middle of structs (from which you can find the base)
2. Inline-allocated variable-size fields

We also get some things for free because structs are implemented as a library.
For example, C++ doesn't support polymorphic inline object allocation; I can't
write this:

  struct A { /* pure virtual stuff */ };
  struct B : A { /* implementations */ };
  struct C : A { /* implementations */ };
  struct this_wont_work
  {
    A either_b_or_c;          // fields can't be abstract types
  };

phi C<struct> has no problem with the above as long as the set of possible
concrete implementations is known when we compile code; it does the obvious
thing and converts a class hierarchy into basically this:

  union a_storage
  {
    B as_b;
    C as_c;
  };

  enum a_variant
  {
    ITS_B;
    ITS_C;
  };

  struct this_works_in_phi
  {
    a_variant either_b_or_c_type;
    a_storage either_b_or_c_struct;
  };


=head4 A quick aside about ranged-int encoding
C<a_variant> doesn't use most of the integer range by itself; if enums are
stored in 32 or 64 bits, all but one is wasted space. If a struct has multiple
ranged integer values, phi can rearrange it to get more mileage out of those
bits; for example:

  struct efficient
  {
    intrange(5) x;              // x can take on three distinct values
    intrange(7) y;              // y can take on seven
    intrange(5) z;
  };

The representation of this struct depends on our space/time tradeoff preference.
If we want maximum performance, we'll get this:

  struct efficient_fast
  {
    char x;
    char y;
    char z;
  };

If we want maximum space efficiency, we'll get this:

  struct efficient_compact
  {
    char xyz;
    char x() { return xyz % 5; }
    char y() { return xyz / 5 % 7; }
    char z() { return xyz / 35; }
  };

And if we want something in the middle, we'll get the equivalent of a bitfield:

  struct efficient_bitfield
  {
    short x : 3;
    short y : 3;
    short z : 3;
  };

Structs are just libraries that compile code, so you can write your own
transformations like this to implement representational optimizations that make
sense for your programs.


=head4 Dimensional invariance
C++'s templates let you specify some things about a struct at compile-time; for
example:

  template<int N>
  struct int_array<N>
  {
    int xs[N];
  };

In the list of when-we-know-stuff, C<N> falls into the second case:

1. We know it immediately, e.g. C<int xs[5]>
2. We know it at compile-time, e.g. C<< int_array<5> >>
3. We know it at instantiation-time, e.g. C<malloc(n * sizeof(int))>
4. We have to store it somewhere because who knows really

C doesn't strongly differentiate between cases (3) and (4); technically the size
you pass into C<malloc> is internally managed like case (4) even though the API
looks more like case (3). If C supported JIT, you could runtime-specialize a
code branch to get template-style performance and space efficiency from a
runtime quantity.

phi gives you some leverage to deal with case (3), and not always using JIT. For
example:

  struct kv_pair
  {
    size_t const klen;
    size_t const vlen;
    char k[klen];
    char v[vlen];
  };

The split between C<k> and C<v> varies depending on the value of C<klen>, and
C<sizeof> is computed rather than constant. All of this is fine because phi
doesn't need to know the size of anything at compile-time; we don't precompile
stack frame offsets like C does. Stack frames, if we want them, are themselves
structs and they can have members with computed sizes and offsets.


=head4 Struct API and variable allocation
Being value types, structs can't change their size after you create them. You
have to know their size at allocation-time and those memory bounds apply
throughout the lifetime of the object.

=cut


package phi::struct
{
  sub new
  {
    my ($class, $name) = @_;

    # TODO: what do we store in fields[]? Do we need to mirror phi's object
    # hierarchy in Perl? If so, can we inherit that implementation into phi's
    # image so we get a head start on the perl backend?
    bless { fields => {},
            order  => [],
            name   => $name }, $class;
  }
}


package phi::class
{
}


package phi::protocol
{
}


=head3 vtable construction algorithm
Before I get into the optimization end of this, let's talk about some
higher-level tradeoffs.

First, vtables themselves obviously trade flexibility for performance. Dynamic
dispatch using fixed offsets is basically free compared to any other
implementation, so it's a no-brainer to take a hit on customization rather than
on every method invocation.

That doesn't mean we can live in a static world, though. It should be possible
to define a new method and have some way to invoke it immediately, and it should
also be possible to modify existing classes/protocols and have those changes
fully compiled into the running image. phi is locally static, globally dynamic.

Q: how do we check for type errors: do we build disjoint protocols, or insert
bitset cast-checks?


=head3 Method call compilation environment
Concatenative instruction lists are compiled from slightly higher-level "source"
objects, which means that the concatenative lists are, in some sense, specific
versions of a higher-level binary heap access protocol. (TODO: is it useful to
look at it this way?)


=head3 Value classes
Every stack entry will end up being prefixed with a vtable pointer that
specifies how we should interpret that entry. This means our primitive
instructions are actually not how we will manipulate the stack at all; we'll end
up making method calls against stack things, which will push, pop, and rearrange
themselves for us.

Let's talk about how method calls work because it's not quite as simple as it
sounds.

First, the interpreter's C<mcall> primitives work against the object referred to
by C<%rsp>, which means it's (the most recently) stack-allocated value _by
value_. This is important because it's exactly the way you don't want to write
programs most of the time. For example, an object-oriented int would look like
this on the stack:

  %rsp+8 -> int value
  %rsp   -> int vtable

This works for GC and gives us full-width unboxed primitives, but what about
full objects? We don't want to stack-allocate them:

  %rsp+alot   -> interpreter bytecode 0xff      # NOPE
  %rsp+alot-8 -> interpreter bytecode 0xfe
  ...
  %rsp+8      -> interpreter bytecode 0x00
  %rsp        -> interpreter vtable

Instead, we want the stack to contain a by-value _pointer_ to the interpreter:

  %rsp+8 -> interpreter pointer
  %rsp   -> pointer vtable

Now we can make by-value method calls against the pointer. So far so good, but
we're not out of the woods yet.


=head4 Method forwarding
phi doesn't support vtable-backed proxy objects (no language could without
allocating humongous vtables). Ruby, Perl, and other languages with some type of
C<method_missing> handler allow you to have an object that forwards its method
calls to another receiver, but this works only because the object uses a
reflective method invocation protocol.

This matters for us because we want a generic "base pointer" value type against
which we can make method calls that address the referent. So how do we implement
this without method forwarding? We need to resolve the pointer before making the
method call.

Value and reference types do this differently. Any stack value-primitive that
participates in OOP needs to support a C<resolve()> method that removes itself
and pushes a value and then its vtable. So a full method invocation is just
prefixed with C<.resolve()> to get the right receiver and vtable.

That gives us enough polymorphism to handle base/here pointers and primitives
correctly. Classes are aware of their value/reference nature; reference types
expect their address on the stack as the receiver, whereas value types expect
direct allocation on the stack. For example, here's the same C<point> type
implemented both by value and by reference:

  # point: double x, double y
  point_by_value    .set_x = [ swap, drop, lit point_vtable ]
  point_by_reference.set_x = [ swap, lit 8, +, >mem64 ]

Also note the different idioms: value types are immutable so we pop the receiver
and push the result (effectively), whereas reference types are modified in
place and their stack entries are dropped.
=cut
