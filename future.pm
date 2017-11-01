=head1 State management
Parsers should be immutable objects that we can destructure against and
reconstruct. Then state modifications amount to building a derivative parser,
using it, and throwing it away.
=cut


=head1 Operators and inheritance
Suppose arrays support some syntax like C<[1, 2, 3] match [x, y, z] in ...>;
ideally we define this in terms of the representational containers for a type,
rather than for the surface type itself. This means we'll want to do some type
of metaprogramming to define C<match>.

The question here is, do we create metaclasses as an indirect byproduct of
alt-inclusion? i.e. if arrays alt-include operators that apply to all values,
is this sufficient to do OOP?

One issue here is that we don't really have a way to do polymorphism with this
strategy. It might not be a problem if we encode an abstract op against a value
though. Technically that's a more accurate representation: monomorphic ops can
be inlined, whereas polymorphic ops require a runtime decision.
=cut


=head1 Type propagation
If we have a list of numbers, say C<[1,2,3]>, we can probably say
C<[1,2,3] + 1> to distribute. This requires the list to do some type inference
and runtime-delegate to the items.
=cut
