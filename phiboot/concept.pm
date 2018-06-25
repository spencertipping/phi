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
    unsigned short here_marker = (&(machine_code.code) - &(machine_code));
    char           code[size];
  };

Now we have a simple rule: to convert a here-pointer into a regular pointer, we
just subtract the two-byte unsigned short immediately before it. Everything
remains polymorphic and traceable.

NB: we can create here-pointers in any unmanaged language, including in C even
with uncertain/inconsistent struct padding. We know that the padding is always
at least large enough for a C<here_marker>, so we can take the here-pointer,
back up two bytes, and write in the offset. We don't know how that position
relates to the address of C<here_marker>, but we don't have to -- here pointers
are positioned relative to the thing after them. (This means we'll have to be
careful with C because we can't expect to use the C<here_marker> field even
though it's present in the struct.)

=head3 C<gc> protocol
Objects need to be able to copy themselves into a new heap for GC purposes. The
protocol looks like this:

  protocol heap {
    base_pointer allocate(size);
  }

  protocol gc {
    cell mark_into(heap);
    void mark_in_place(heap);
  }

NB: C<mark_in_place> will fail horribly if you base-point to an inline object.
We should probably have disjoint protocol behaviors, one for inline-allocated
objects and one for reference objects. Some objects don't need to implement
C<gc> at all, particularly if they're unilaterally managed by a container.

A typical C<mark_into(heap)> implementation would look like this:

  copy self into new heap
  set *self+8 to the new address
  modify self.vtable to the "already marked" object type
  for each pointer field:
    new self.field = new self.field.mark_into(heap)
  return new self

This will copy the object exactly, which isn't a bad start. But we can often do
better. Since we have to copy the object either way, we can apply structural
optimizations in the process. This is a side-benefit of having objects own their
own GC. For example, a linked list could decide to inline a bunch of elements
into condensed blocks, thus incrementally converting itself into a chunked
sequence. Data structures are responsible for minimizing their amortized GC
overhead.

This design (and a lot about phi) creates the limitation that we can't support
any incremental or concurrent GC; garbage collection -- or more accurately heap
rewriting -- is an atomic operation that occurs separately from normal program
execution.

I'm ok with this limitation. We can't have objects rewrite themselves if the GC
is in any way concurrent; we don't know who will try to access them while
they're being rewritten. Concurrent/incremental GC is also strictly less
efficient than serial GC; the trade is between efficiency and low latency.
(Also, the train has long since sailed for concurrent GC given the way atomicity
works.)

Finalizers are easy to implement. The GC protocol includes a C<finalize> method,
which C<gc_marked> implements as a no-op. Objects interested in being finalized
add themselves to a finalization list maintained by the heap. Then, after all
objects are marked, the GC loops through the finalization list and invokes
everyone's C<finalize>.

Code run within a finalizer sees a slightly different version of the world.
C<gc_marked> behaves like a pointer to an object's new location -- so no worries
there -- but you can't do things like resurrect objects or persist modifications
to collected memory. You also can't resurrect the object being finalized.
Finalizers are strictly for things like freeing externally allocated resources.

...so: phi's GC is simple by design. When you need better throughput/latency/etc
you have options to manage memory to get those results (for instance, writing a
custom heap class, pointers, and GC algorithm).


=head3 Object size and allocation
The heap is compact; that is, each object is allocated immediately after the
previous one. phi isn't strictly required to do it this way, but it's also not
required to let you expand an existing allocation. This means objects need to
know how large they are at allocation-time. Dynamically-sized objects result in
garbage as they allocate new, larger buffers when required (e.g. variable-sized
array objects).

This creates a natural delegation: part of the C<class> protocol specifies an
C<allocate> method that accepts ctor arguments and returns a base pointer to the
new instance. If the class is a value type, C<allocate> returns the value
itself. The class owns its allocation contract and specifies it by returning
C<instance_type>, the thing that C<allocate> will return. Almost every class
provides an C<instance_type> that fits into a single stack slot, e.g. a pointer
or an C<int> or something.

Classes are allowed to provide a fixed allocation size, which is useful for
things like arrays. This is a separate protocol.


=head3 C<class> protocol
If an object wants to behave like a class, it needs to implement the C<class>
protocol at a minimum. Classes that support idiom translation will implement
more things to make them compilable, but this is what you need to implement to
be interpretable:

  type method = symbol;                 # not literally true, but close enough
  protocol class
  {
    type instance = cell | ...;
    type                 instance_type();
    instance             new(...);      # aka allocate()
    ((instance, ?) -> ?) method_fn(method);
  }

NB: methods are specified semantically because class/protocol objects are
responsible for allocating and resolving vtable slots (or doing anything else
that implements method calls; phi doesn't require that your classes use
vtables).

Note something interesting here, which is that structs don't specify vtables
directly. This is what lets us implement C<base_pointer> and C<here_pointer> as
value-type classes. For example:

  class here_pointer
  {
    type referent_type;

    implement protocol fixed_size
    {
      size size() { native_machine_word }
    }

    implement protocol class
    {
      type instance_type()                    { here_pointer }
      here_pointer<referent_type> allocate(x) { x }
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
method call C<xs.[](3)>; C<a_bar.f1> is a method call to C<.f1()> rather than a
direct field access. C<a_bar = ...> is also a method call: C<a_bar.=(...)>. The
expected signature of C<=> is C<< val -> val >>; it's an identity function (with
side effects) for reference types.

NB: field accessors typically return C<base_pointer(...)>; otherwise assignment
would be unable to update anything. That is, C<a_bar.f1.x = 1.0> works by having
C<a_bar.f1.x> resolve to a C<base_pointer(double)>. This means assignment is
type-overloaded: C<double* = double*> and C<double* = double> both work and must
be statically resolved.


=head3 Polymorphism
Polymorphic values are usually pointed to by C<base_pointer> or C<here_pointer>,
but they don't have to be. Polymorphic values also don't have to be reference
types. So two things aren't completely obvious:

1. Who's responsible for resolving RTTI and producing a vtable?
2. If we write C<< base_pointer<numeric> >>, who adds RTTI to C<int> and how?

(1) arguably falls to the class providing the method-calling interface, which in
this case is the pointer class. But pointers don't know how a given type will
store its RTTI (if any), nor should they. Maybe we have a compact multi-purpose
primitive type with three tag bits and 61 data bits or something. We should be
able to base-point to that without the pointer having to know what insane
strategy the referent is using.

(1) informs (2): the RTTI encoding strategy could be pretty much anything, so
there isn't a default way to add RTTI to things like primitives. In short, you
can't actually write C<< base_pointer<numeric> >>: you have to point to a class
rather than to a protocol. This means we need a way to convert one to another.
So we'd write C<< base_pointer<vtable_polymorphic<numeric> > >>.

C<vtable_polymorphic> is a general-purpose strategy for RTTI, but it may be
overkill; if C<numeric> has exactly two alternatives then we'd be using 64 bits
to encode one bit of information. That's obviously a tragedy; let's discuss how
to deal with it. We have a few options:

1. Use a tag bit
2. Use a one-byte prefix instead of the full vtable pointer (8 bits for one bit)
3. If we have an array, use a separate bit vector to store type bits
4. We rely on something domain-specific like NaN

Of these, (3) is interesting and deserves some discussion because it has broad
implications for collection types and small-polymorphism optimization.

=head4 Array element polymorphism and semi-boxing
There's no excuse to have arrays fully box small-polymorphic values of
fixed+equal size. For example, C<< array<int64|double> >> should absolutely not
result in an array of pointers to vtable-prefixed ints/doubles; that would be
egregious for two reasons:

1. 66% of the space usage of this structure is pointers
2. The cache-miss GC complexity of this structure is C<O(keep)>

The space-optimal solution is to have the array maintain a bit vector of type
bits that differentiate between C<int64> and C<double> for each element. Now we
have a new challenge, though: suppose we write something like C<xs[i] + 1>. What
does C<xs[i]> return in order to correctly resolve the C<+> method?

Here's why this isn't straightforward. The return value from C<xs[i]> is some
kind of indexed pointer. It can't be a C<base_pointer> to anything because the
referent won't have any way to encode its type; by that point we would have lost
the bit vector entry we'd use to resolve it. So we need C<xs[i]> to refer both
to the element and to its type.

A simple option is to have C<xs[i]> return a two-slot value consisting of a
vtable and the element (or a pointer to the element). Then we'd have a new value
type, C<externally_tagged_pointer>, whose size is 16 bytes. This may be a bit
clunky but is still far superior to having fully boxed values. A solution
exists; we're in good shape.
=cut


1;