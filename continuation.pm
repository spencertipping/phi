=head1 Struct representation
We can flatmap structs just like we can flatmap parsers. So all we need is a set
of primitives and an encoding of structs-as-values (quoted structs).

A value having a type is simply a matter of it specifying its behavior when
asked for a type: C<value class>. This should return a quoted struct. Any
polymorphic (or monomorphic) operations should use this to get a vtable (i.e.
parser continuation); monomorphic call sites are cases where we can fold
C<value class> into a constant and resolve the method in question immediately.
=cut


=head1 Primitives
We need four literal types to bootstrap the compiler:

1. Strings
2. Ints
3. Heterogeneous lists
4. Functions

This gives us enough to describe structs and parsers.
=cut


=head1 Open-ended unions
There are a lot of situations where we need to do forward-referencing to a union
that won't be fully specified for a while. It's exactly like the C<mut> parser.

Given that all values are immutable, how do we do this? Does the compiler get
its own IO monad? (I don't see why not, actually; we might as well quantify any
actual IO dependencies, as well as having a way to deal with side effects.)
=cut


=head1 Operator precedence and continuations
The scope can use operator-precedence parsing and still delegate all operators
to type-specific continuations. Precedence happens leftwards, and the operator
continuation consumes a wrapped or atomic value. I think this still produces the
right result.
=cut
