=head1 Notes about the bootstrap compiler
Getting this thing designed and shipped.

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

1. parse_continuation($scope)
2. scope_continuation($scope)
3. method($self, $name, @args)

IO is maintained within dynamic scopes, so scope_continuation() provides a way
for values to interact with IO. method() is a universal protocol that makes it
possible to address values whose type is so vague that no parse continuation is
useful -- it also handles operators that are parsed by the scope.

=head2 Working with IO
Abstract IO is a journal of updates encoded into a timeline. In list terms it
goes backwards; future events are consed onto the existing list. Abstract values
hold references to various points along the IO timeline to indicate dependency
ordering.

Compilation happens solely against this IO timeline: the very last value is
considered to be "returned" from the program, just like in Haskell.

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

=head2 Open-ended unions
There are a lot of situations where we need to do forward-referencing to a union
that won't be fully specified for a while. It's exactly like the C<mut> parser.

Given that all values are immutable, how do we do this? Does the compiler get
its own IO monad? (I don't see why not, actually; we might as well quantify any
actual IO dependencies, as well as having a way to deal with side effects.)
=cut


=head1 Notes about strategy
Stuff for later.

=head2 Operator precedence and continuations
The scope can use operator-precedence parsing and still delegate all operators
to type-specific continuations. Precedence happens leftwards, and the operator
continuation consumes a wrapped or atomic value. I think this still produces the
right result.
=cut
