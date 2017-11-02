=head1 Refactoring
The compiler should define a core syntax that just bootstraps structs/scopes.
I'm tempted to say that abstract values should themselves function as parse
outputs, but I'm not sure yet; it seems like the concept is overloaded. (From a
user interface point of view, the relationship seems like displayable things
refer to abstracts, not the other way around.)

Probably delete phi::node for the moment. We can rewrite it.
=cut
