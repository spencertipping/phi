=head1 Struct representation
We can flatmap structs just like we can flatmap parsers. So all we need is a set
of primitives and an encoding of structs-as-values (quoted structs).
=cut


=head1 Operator precedence and continuations
The scope can use operator-precedence parsing and still delegate all operators
to type-specific continuations. Precedence happens leftwards, and the operator
continuation consumes a wrapped or atomic value. I think this still produces the
right result.
=cut
