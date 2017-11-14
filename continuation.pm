=head1 Notes about the bootstrap compiler
Getting this thing designed and shipped.

=head2 Abstract evaluation
Abstract evaluation is real evaluation, just with quoted semantics. We can
resolve certain aspects of structs at abstract-time, erasing them from the
runtime.

The hard part of abstract evaluation comes in when we need stuff like flatmapped
parsers. This forces our evaluation model to be value-polymorphic because we're
using "real" values inside a world of quoted abstracts when we ask for things
like parse continuations. For example, do those real values get an IO?

=head2 Abstract recursion
For example:

  f = |i:int| i.gt(0).if(|| i.times(f(i.dec)), || 1)

Expansion never happens inline. Instead, optimization parsers match
polymorphically against various levels of flattening and force lazy values.

This means we have an op for function calls, and that edge can project downwards
into its constituent parts.

=head2 Working with IO
Abstract IO is a journal of updates encoded into a timeline. In list terms it
goes backwards; future events are consed onto the existing list. Abstract values
hold references to various points along the IO timeline to indicate dependency
ordering.

Compilation happens solely against this IO timeline: the very last value is
considered to be "returned" from the program, just like in Haskell. Backends are
parsers against the timeline.

Abstract IO isn't any different from abstract values in general; the IO quantity
being described behaves as though it's a pure, immutable value.

=head2 IO operations
IO contains every low-level operation the program might execute, including:

1. Memory allocation/deallocation/access/update
2. File/IO operations
3. Calls to runtime-hosted methods, like native code interop
4. Updates to the global scope?

Basically, IO encapsulates everything that shouldn't be optimized away: it's the
substance of what your program does.

=head2 Errors and the IO monad
The semicolon operator is a monadic transform (a flatmap, I think). So parts.
operations like checked downcasts or OOB tuple accesses will fail, however:
their values are undefined and erroneous.

Erroneous values aren't themselves problematic: you can store an error into a
data structure. The problem comes in when the IO monad itself holds an erroneous
state. This causes the program to fail unless you have a failover strategy
(try/catch). In more familiar terms, the IO continuation is parsed rather than
directly computed.

=head2 Primitives
We need four literal types to bootstrap the compiler:

1. Strings
2. Ints
3. Struct literals
4. Functions

This gives us enough to describe structs and parsers.

We need both consts and abstracts for each of these types.
=cut
