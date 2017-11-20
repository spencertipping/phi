=head1 Notes about the bootstrap compiler
Getting this thing designed and shipped.

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
=cut
