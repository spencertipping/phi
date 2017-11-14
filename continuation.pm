=head1 Rewrite planning
1. Greatly simplify the parser stuff, add IO+scope args to base library
2. Get rid of all abstraction in phi::syntax
3. Basic types: int, string, list, fn, unknown, + whitespace/line comments

=head2 Basic structure
We don't really need a parsing library for the base layer; we just need ad-hoc
parsers for strings, numbers, lists, unknowns, and functions. Those exist within
a parser-combinator type of world. There is no library of parser combinators
defined by the bootstrap compiler; that's defined within phi.

IO is a journal forwarded through various abstracts, which constant-fold
instantly in the base layer. Structs are described by lists. Functions are
written longhand.
=cut


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

=head2 Struct representation
We can flatmap structs just like we can flatmap parsers. So all we need is a set
of primitives and an encoding of structs-as-values (quoted structs).

A value having a type is simply a matter of it specifying its behavior when
asked for a type: C<value.class>. This should return a quoted struct. Any
polymorphic (or monomorphic) operations should use this to get a vtable (i.e.
parser continuation); monomorphic call sites are cases where we can fold
C<value.class> into a constant and resolve the method in question immediately.

This all seems a bit out of place, of course, because structs are the things
responsible for dictating parse continuations; if we have an abstract value
whose type is unknown then how do we do anything at all to it? The answer here
has two parts:

1. All values have bounded types; we don't have fully generic unknowns.
2. Scopes provide some minimal parse continuations for all values.

(1) doesn't help much since we can easily create a meaninglessly vague bound,
but is worth mentioning. (2) is more interesting and deserves some discussion.

There's a small handful of operations that are primitive: they are fully
monomorphic, untyped, and have no abstraction potential. For instance, objects
don't get to ponder the question "what type are you"; they store this value
rather than computing it. This is how the bootstrap layer grounds itself out.

Class objects also have a monomorphic protocol, but it provides a lot of
flexibility. They need to implement a few operations:

1. C<$p = parse_continuation($self, $scope)>
2. C<$s = scope_continuation($self, $scope)>
3. C<($io, $x) = method($self, $io, $name, @args)>

Note that IO is managed separately from scopes. It has to be this way; because
the scope has the potential to impact parsing, if we coupled IO and scopes
together then control flow would have to run strictly left-to-right -- but
that's not how applicative languages work.

method() is a universal protocol that makes it possible to address values whose
type is so vague that no parse continuation is useful -- it also handles
operators that are parsed by the scope.

Classes monomorphically link the above method implementations because -- duh --
they have to.

=head2 Abstract recursion
For example:

  f = |i:int| i.gt(0).if(|| i.times(f(i.dec)), || 1)

Expansion never happens inline. Instead, optimization parsers match
polymorphically against various levels of flattening and force lazy values.

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
The semicolon operator is a monadic transform (a flatmap, I think). Some
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
3. Heterogeneous lists (cons + nil)
4. Functions

This gives us enough to describe structs and parsers.

We need both consts and abstracts for each of these types.

=head2 Open-ended unions
There are a lot of situations where we need to do forward-referencing to a union
that won't be fully specified for a while. It's exactly like the C<mut> parser.

Given that all values are immutable, how do we do this? Does the compiler get
its own IO monad? (I don't see why not, actually; we might as well quantify any
actual IO dependencies, as well as having a way to deal with side effects.)
=cut
