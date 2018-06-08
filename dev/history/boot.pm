=head1 Booting phi
phi is the most self-referential project I've ever worked on, so it's not
remotely obvious to me where it bottoms out into a bootstrap script. Let's
figure this out.

First, the main goal of phi is to provide metaclass-aware OOP for a wide variety
of different runtimes with maximal code/data portability. So the language has a
few constraints:

1. Almost everything is written in a semantically consistent bytecode
2. Data layout is done with pre-idiom "logical structs", which are then compiled
3. Structs and classes are portable object instances
4. The interpreter implementation is replaceable and can be GC'd

(GC isn't independently a big deal; objects trace and collect themselves on
backends that don't provide memory management.)

(3) and (4) conspire to make our lives interesting. Jointly, they imply that
every aspect of our bytecode implementations and base objects/structs be
described in terms of objects. Combined with the other constraints, then, we
have three fixed point elements:

1. Classes are and implement objects
2. Bytecode functions are and implement classes
3. Interpreters are instances of and implement classes

phi is designed to implement a minimal solution to these fixed points while
providing a runtime you'd want to use for real problems.


=head2 Lies, damned lies, and dependencies
The world doesn't typically work in X way (for most values of X). So if you're a
sane person who wants some degree of consistency, you have two options: you can
insist that the world does X, or you can do some hacking to make the world
appear to do X. This is the dependency/portability tradeoff. phi, like C,
minimizes dependencies in favor of portability.

Doing this economically requires some parsimony; maintaining a set of C<n>
interconnected lies involves much more than C<O(n)> effort. phi implements the
minimal set of lies required to portably maintain its fixed points. This entails
offloading a lot of things like GC and vtable allocation into libraries.


=head2 Specializing phi to a backend, mechanically speaking
Let's assume machine code or something similarly unstructured for the sake of
argument.

Since the backend is unmanaged, phi needs to implement its own OOP and GC. GC is
entirely library-based; the only backend cooperation we need is direct memory
access and the ability to replace the interpreter.

OOP isn't quite so simple because objects involve behavior, which in turn
requires some set of primitives to perform actions. We can, however, reduce OOP
to a single abstraction: "here's an object with a vtable; now invoke method
number N on it." This is a convenient strategy because it allows us to define a
vtable whose vtable is itself; this covers the first fixed point.

Bytecode is naturally concatenative, which lends itself to a stack-focused
design -- but we have to be a little careful because we run into a few
constraints. First, phi doesn't steal bits from integers; if your system gives
you 64-bit ints, then phi does too. Second, phi's GC is accurate, not
conservative (as it has to be if objects are driving it). Third, phi doesn't
implement stack-wrapping machinery to tag the type of each entry; that is, the
stack is properly untyped.

We can satisfy all three constraints by allocating objects on the stack and
maintaining an active-frame pointer. The only thing we need to do is make sure
we've committed any object pointers into the active frame (or any other
GC-traceable location) before allocating memory; I refer to this as "GC
atomicity." This is something bytecode authors need to be aware of; the bytecode
semantics by themselves don't guarantee that you'll get this right.

Given that bytecode and machine code are both concatenative in nature, we can
define a thin interop convention and implement bytecode natives as machine code
snippets. This and here-pointers (described later on) jointly anchor the second
fixed point by native translation.


=head2 Objects and portability
We totally aren't done yet. Sure, we can have a self-referential vtable thing,
but that works only when the hosting runtime doesn't have opinions about what we
do with memory -- and many environments like Perl, Python, or Java definitely do
have opinions about these things. phi can't afford to be imperialistic about
these differences, which means the object system takes up the slack of backend
awareness. This awareness is the idiom translation layer.

Idiom translation is relevant to phi booting because it's a step between
"logical classes" and compiled objects -- that is, it's a term in the fixed
point equation. Before I get into the details of how this impacts the bootup
process, let's talk about what idiom translation entails.

Let's suppose we have something simple like a key/value pair that maps a string
to an array of integers. Here are some ways we might express this:

  // in Java
  class kvpair {
    String key;
    int[] value;
  }

  // in C
  struct kvpair {
    int   nkey;
    char *key;
    int   nvalues;
    int  *value;
  };

  // in C++
  struct kvpair {
    std::string key;
    std::vector<int> value;
  };

  # in Python
  ("foo", [1, 2, 3])

  # in Perl
  ["foo", [1, 2, 3]]                    # managed representation
  ["foo", pack "V/V", 1, 2, 3]          # flat representation

  (* in OCaml *)
  type kvpair = string * int array

  // in Javascript
  function kvpair() {
    this.key = "";
    this.value = new Int32Array(...);
  }

This is a lot of variation for such a simple class, and that variation brings
some semantic differences as well. For example, Java's strings support 16-bit
characters while C C<char>s are eight bits each. OCaml truncates ints by one bit
for GC type-tagging. C strings will fail for null bytes unless we manually
prefix them with a length.

Some of these decisions are forced but some come down to preference; take Java
for example. Should we use C<String> or C<byte[]>? It depends on the purpose of
the object. Any conversion between C<byte[]> and C<String> incurs a UTF-8
transcoding delay, so we should go with C<String> when we care about
interoperating with existing Java code and C<byte[]> or C<ByteBuffer> when we
want to minimize the cost of migrating values between runtimes.

There's also a question of how we manage and optimize allocation. We're
theoretically at liberty to flatten the string and int array into the object as
value types if those fields aren't shared elsewhere. The tradeoff is much better
memory locality, but we introduce a sizing invariant: once the structure is
allocated we can't do things like extending the key.

The above definitions also gloss over another variation, which is the way types
are encoded. The C, C++, Perl, Python, and OCaml structs are all type-unaware;
if we used any of those objects within a polymorphic context we would need to
add some information to have them support virtual method calls. Each language
uses a different mechanism to implement this.

Basically, the short version of this story is that idiom translation is a pretty
open-ended problem that sometimes demands full automation and other times needs
to be customizable. Bootstrapping is hopeless if we try to model it fully, but
luckily we have a way out: because we're just bootstrapping for a single
backend, we can constant-fold the backend into our idiomatic translation layer
to reduce it to a constant term; then we have a function we can manually apply
to our boot classes to derive vtables.


=head2 Self-descriptive structures
This is far more awful than it sounds, and the reason has to do with machine
code backends. Here's what's up.

We know up front that machine code fragments need to be encased inside objects.
Those objects are then referenced by the interpreter in its bytecode-dispatch
table. So far so good.

When it comes to executing those objects, though, we need to be able to take
those pointers-to-objects and generate machine jump offsets. We could easily do
this by having a fixed offset into the structure, but then we'd lose code
fragment polymorphism. We can't ask the objects themselves for the offsets at
runtime because the code to calculate those offsets is itself implemented in
bytecode. This leaves us two options:

1. Make the rule that machine code must be stored in some fixed object type
2. Ask the objects to precompute their offsets and store those

In the spirit of not being fascist, phi takes option (2) -- and we can optimize
this a little by using here-pointers. Here's how that works.

=head3 Here-pointers
Let's suppose our machine code object looks like this:

  struct machine_code {
    void *vtable;
    int   size;
    char  code[size];
  };

Ideally speaking, we could have the interpreter point straight to
C<machine_code.code> for each instruction; then the dispatch table contains
direct jump addresses and our advancement primitive is simplified to something
simple like C<jmp *(%rdi + 8*%rax)>. That's a beautiful world.

The problem, though, is that while it's completely fine to refer to the middle
of a structure, the garbage collector is going to have to figure out how to
trace through those pointers and ask the individual machine code objects to mark
themselves into a new heap. Then the code pointers will need to be rewritten to
refer to the new allocations. This is what here-pointers are for.

Pointing to the middle of an object is no problem at all if (1) we realize that
this is what's going on, and (2) we have a way to find the object's base
pointer. We get (1) for free with structs, and (2) can be implemented by adding
a two-byte integer immediately before the destination of any mid-pointer:

  struct machine_code {
    void          *vtable;
    int            size;
    unsigned short here_offset = (machine_code.code - machine_code);
    char           code[size];
  };

Now we have a simple rule: to convert a here-pointer into a regular pointer, we
just subtract the two-byte unsigned short immediately before it. Everything
remains polymorphic and traceable.

=head3 Here-pointers, description, and idiom translation
This is where things start to get ugly. Here-pointers aren't remotely portable;
most languages have no way to cheaply construct a reference to the middle of
something (e.g. a specific element within a primitive array). The best general
approximation would be a compound structure like C<< pair<obj*, int> >>, which
would often end up being heap-allocated and therefore slow.

We do have one way out, though. I mentioned earlier that we need to know in
advance whether a value is a here-pointer or a base pointer; this means we're
superimposing a type onto the fundamentally untyped pointer value -- which in
turn means that we're not relying on the pointer to store any information about
its type. So if we know up front that the object we're pointing to has only one
here-pointer location, then we can reduce the here-pointer to a base pointer for
most languages; we'll then know to invoke a method to simulate here-pointer
functionality when we address the object from a here-pointer superimposed
reference (if that makes sense). In other words, we can always pretend that we
had a here-pointer to an object by asking the object to tell us what we would
have pointed to -- and we'll always know when we need to do this.

This means we can commit to here-pointers as builtin types, which entails a
couple of things:

1. Struct descriptions need to include here-pointer destinations
2. We need to differentiate between base-pointer and here-pointer types

=head3 Values and references
C structs can include each other inline:

  struct foo {
    double x;
    double y;
  };
  struct bar {
    foo f1;
    foo f2;
  };

Nothing stops you from doing this in phi, but you have to be a little bit
careful. For example, you'll lose GC atomicity if you replace a C<bar> pointer
with one of its contained C<foo> pointers (e.g. C<< x = &(x->f2) >>) unless
each C<foo> member is prefixed with a here-marker. Then you'd be converting from
a base pointer to a here pointer, which is unusual and would require changing
the type of the active frame object.

In C you can address parts of sub-structs:

  a_bar      = another_bar;             // address the whole struct
  a_bar.f1   = a_bar.f2;                // address sub-structs as values
  a_bar.f1.x = a_bar.f1.y;              // address individual members

There's more going on here than is immediately obvious. For performance reasons,
C _doesn't_ treat member lookups as function calls against value structs;
C<a_bar.f1> doesn't value-return a C<foo>, whose C<.x> method value-returns a
C<double>. That would be incredibly wasteful. Instead, C combines the C<.field>
dereferences into a single (offset, size) pair describing the memory region
being retrieved.

We care about this in phi because it means structs aren't truly concatenative;
addressing a member of a value-struct is very different from addressing the full
value-struct. The simplest way to generalize this is to have each struct define
an as-value disposition:

  int.as_value    = primitive-value     # ints aren't boxed
  double.as_value = primitive-value     # doubles aren't boxed
  foo.as_value    = base-pointer        # structs are
  bar.as_value    = base-pointer

Then the aggregated C<bar.f1 = bar.f2> type stuff is an C<assign> method call
against a C<base_pointer(foo)> whose arg is also a C<base_pointer(foo)>. This is
strictly less efficient than C because we're not resolving sub-struct offsets at
compile time, but it's concatenative and limits our copy range to pointer-sized
things.

...so from phi's point of view, C<a_bar.f1> always means C<&(a_bar.f1)>; struct
aggregates are always addressed by reference.

=head3 Arrays
C's arrays are shorthands for pointer math but might as well be structs in their
own right. That is, C<char[2]> isn't very different from a struct defined with
two C<char> fields.

phi's arrays really are functional structs: C<array(char, 2)> asks C<char> how
much memory it takes, then multiplies that by the length to allocate many of
them. It calculates the offset of each when you ask for a specific element, and
its value-disposition is C<base-pointer>.

phi supports two types of arrays. C<fixed_array(char, 2)> is exactly twice the
size of a C<char>, whereas C<array(char, 2)> contains a vtable and a length
prefix. Here's the difference:

  struct fixed_array_char_2 {
    char 0;
    char 1;
  };
  struct array_char_2 {
    here_pointer<vtable> vtable;
    uint64_t             length;
    fixed_array_char_2   xs;            // not literally, but close enough
  };

=head3 Variable-offset fields
C struct layout is fixed: the location of C<foo.x> relative to C<foo> is known
at compile time, which means it has no data dependencies. phi supports structs
whose field locations are computed, which lets you do things like this:

  struct kv_pair {
    int  klen;
    int  vlen;
    char key[klen];                     // offset of this is 2*int
    char val[vlen];                     // offset of this is 2*int + klen
  };

This is a little tricky, though; here's a struct that won't work:

  struct broken_kv_pair {
    char key[klen];
    char val[vlen];
    int  klen;
    int  vlen;
  };

=head3 C<struct> protocols
Structs can opt into degrees of functionality; the base protocol, though,
specifies how structs interoperate with phi and each other:

  protocol struct
  {
    type            as_value_type();    # this specifies "val" below
    (val -> size)   sizeof_fn();
    ((?) -> val)    allocator_fn();     # incoming args depend on the struct
    ((val, ?) -> ?) method_fn(symbol);  # make a method call against the struct
  }

NB: GC isn't implemented at the struct level; instead, it's a protocol objects
can opt into. This lets you change or replace the GC algorithm at runtime.

The contract for C<allocator_fn> is that the returned struct has no dangling
pointers; it's important for it to initialize memory at allocation-time to
prevent segfaults if we kick off a GC and ask it to mark its dependencies.

Note something interesting here, which is that structs don't specify vtables
directly. This is what lets us implement C<base_pointer> and C<here_pointer> as
structs. For example:

  class here_pointer
  {
    type referent_type;

    implement protocol struct_base
    {
      type as_value_type() { [] };      # we are our own value type
      fn allocator_fn()    { [] };      # no allocation required
      fn sizeof_fn()       { [drop 64] };
      fn method_fn(symbol m)
      {
        # dereference ourselves, then delegate to whatever the base pointer
        # would have done
        [                               # self-ptr
          dup -2 +                      # self self-2
          mget16 neg +                  # base-ptr
        ] ++ base_pointer(referent_type).method_fn(m);
      }
    }
  }

Arrays and aggregate accessors are implemented as methods. So C<xs[3]> becomes a
method call C<xs.array_get(3)>; C<a_bar.f1> is C<a_bar.field_get('f1)>.

TODO: C<field_get()> with a runtime symbol is probably a lot slower than we
want; can/should we do something like C<a_bar.get_f1()>?
