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
to contain weak references and compact itself automatically.
