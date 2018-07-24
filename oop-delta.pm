=head1 OOP delta
We have a bit of a problem right now, or at least it feels like a problem.
Basically, phi doesn't support duck typing; the virtual method set must exist
fully before we can compile any method calls against objects.

The reason this matters is a bit subtle. We already depend on type-signed
protocol methods' existence if we want to compile things; otherwise we have no
CTTI propagation for virtual methods. So if we're invoking a class or protocol
method (virtual or otherwise), it will already be defined in terms of CTTI.
Where's the problem?

The problem comes in when we start to modify the class/protocol structure as
we're writing code. If we bake any vtable indexes into compiled code, those need
to be proper invariants -- even after we then go and modify the class/protocol
relationship down the line. Compact vtables won't cut it, nor, most likely, will
indexed vtable calls in general.

Things get even worse when we consider class extensions. First, there's no
reason a class _needs_ to have a closed set of methods. Class are mutable enough
(and cloneable enough) that you might very well decide to dynamically extend
them, expecting new functionality to immediately become available on all
instances of those classes. Ideally speaking, you should be able to continuously
modify a class's method set and not have a space leak in the process, something
vtables wouldn't give us at all.

...so a quick recap:

1. Virtual method APIs aren't intrinsically stable; must be modifiable
2. ...which means compiled method calls must be symbolically lossless
3. ...therefore vtables are a nonstarter in the general case

Quick devil's advocate: what if we used a single protocol (like we do for the
boot layer), which removes all negotiation; then compact vtables and rewrite
method call indexes on GC? This leaves the question of vtable extension, which
we could do with a self-pointer, but it gives us native-speed method calls with
minimal machinery.

This isn't quite as easy as it sounds. Nobody knows the set of "all implemented
methods" when GC starts, so our live set will lag by one GC unless we introduce
a trace-the-classes stage up front. Introducing extra GC stages itself isn't
easy because marking an object destroys it, and GC requires marking in order to
deal with object reference cycles. This might be a problem.

NB: I may be overthinking this; the bar isn't terribly high for dealing with
string->int mapping in compilers:
L<https://groups.google.com/forum/#!topic/fa.caml/68oTntkRLyU>.


=head2 CTTI and methods, again
Broadly speaking, there are three different ways a method can work:

1. Virtual polymorphic dispatch
2. Monomorphic linked function call
3. Monomorphic inline asm transform

(2) and (3) are necessary because polymorphic dispatch is a managed process. (3)
is necessary only because our assembler maintains semantic state that needs to
be updated with things like arg/return types. Generalized inlining gives us a
simple way to propagate that information without doing something draconian like
type-hinting every function (even that wouldn't solve the arg-coercion problem
in any extensible way).

All of this raises a pretty big question: do we symbolically link monomorphic
interactions with classes? That is, if I go and change how a class's inline asm
transforms work, should those changes be immediately reflected in code I've
already written?

There are pros and cons to this, but I'd argue that those changes should be
reflected. The reason is that you can always create a static CTTI and use it for
your inline stuff if you don't want change propagation, but getting
recompilation in a world that doesn't provide it by default is quite a bit more
difficult.


=head2 Function objects and bytecode JIT
We want two things from functions:

1. Automatic recompilation/optimization/deoptimization
2. One-way GC pins against the classes they depend on

(2) is interesting. I thought about having classes store a list of
functions-to-invalidate, but that sets up GC the wrong way: the list would need
to contain weak references and compact itself automatically. Instead, functions
ask classes for their versions; if anything has changed, we re-JIT.

Q: should we actually replay the whole source compilation process per JIT given
that a class change might entail modifications to its interpolated grammar? This
does simplify stuff quite a bit.

JIT is when bytecode->bytecode optimizations are applied. It's not clear yet who
would own these; for example, who inlines constant functions in the general
case? No single class would take a holistic view of the code and do this,
although classes might conspire by contributing parsers to an overall
optimization process. This is a nice approach because it lets us change the
preferred optimizer for a given function.


=head2 Running the JIT machinery
The code implementing JIT makes method calls against objects, but it can't
depend on itself to run. This means we need a stable symbolic protocol we can
use as a failsafe to invoke methods. This protocol can't itself involve any
polymorphism; it needs to exist within the context of a single function.

=head3 GC interop and other non-JIT use cases
If the interpreter calls into the frame object requesting a GC, that frame
object shouldn't then go and JIT a bunch of stuff; GC is about copying existing
objects, not allocating a bunch of new ones.


=head2 Class/object layout
Any protocol-polymorphism we use needs to rely on just one class pointer. That
is, we get a single 64-bit vtable/class/whatever pointer for any RTTI we want to
store. So:

  struct my_object
  {
    base|hereptr class_thing;           # this is all we get
    ...;
  }

We almost certainly want a herepointer because that collapses some of the
polymorphism out of the object.

TODO: elaborate _if necessary_ -- but I'm really hoping we can do something with
vtables (see intro).
