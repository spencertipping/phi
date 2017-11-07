=head1 Struct representation
Right now the stuff in phi::struct is a disaster because it's using ad-hoc
representations of abstracts. If we have an abstract encoding, it should
correspond to struct schemas somehow -- and we'll need bindings in the language
to create new structs and parsers.
=cut


=head1 Operator precedence and continuations
The scope can use operator-precedence parsing and still delegate all operators
to type-specific continuations. Precedence happens leftwards, and the operator
continuation consumes a wrapped or atomic value. I think this still produces the
right result.
=cut
