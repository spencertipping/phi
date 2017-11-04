=head1 Precedence and scopes
Operator precedence isn't parsed by values; values simply take over the parsing
context to do type-specific things. They can provide ways to access operators
that aren't precedence-aware. Precedence is implemented by operator-precedence
scopes.

Scope grammar:

1. C<"(" expr ")">
2. C(name >cc $scope)
3. C<alt(@structs)>

Values are always parsed with a type-specific continuation on a loop.
=cut
