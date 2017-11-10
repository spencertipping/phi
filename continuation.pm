=head1 Struct representation
We can flatmap structs just like we can flatmap parsers. So all we need is a set
of primitives and an encoding of structs-as-values (quoted structs).
=cut


=head1 Primitives
We need four literal types to bootstrap the compiler:

1. Strings
2. Ints
3. Heterogeneous lists
4. Functions

This gives us enough to describe structs and parsers.
=cut


=head1 Operator precedence and continuations
The scope can use operator-precedence parsing and still delegate all operators
to type-specific continuations. Precedence happens leftwards, and the operator
continuation consumes a wrapped or atomic value. I think this still produces the
right result.
=cut
