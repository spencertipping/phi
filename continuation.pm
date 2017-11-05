=head1 Struct representation
Right now the stuff in phi::struct is a disaster because it's using ad-hoc
representations of abstracts. If we have an abstract encoding, it should
correspond to struct schemas somehow -- and we'll need bindings in the language
to create new structs and parsers.
=cut
