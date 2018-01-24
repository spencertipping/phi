=head1 phi optimization layer
Before I get into the full picture of how optimization works, let's go through a
simple example: C<[[] swons unswons drop]>. This is technically a no-op
function, so it should end up optimizing out to the empty list. phi can figure
this out by simulating its interpreter, building up an abstract representation
of the function's return value and eliminating every other computation. In this
case we have:

  [x d...]    c r    simulated_[]      -> [[] x d...] c r
  [[] x d...] c r    simulated_swons   -> [[x] d...]  c r
  [[x] d...]  c r    simulated_unswons -> [[] x d...] c r
  [[] x d...] c r    simulated_drop    -> [x d...]    c r

The key behind this particular optimization is that every one of the operators
can be applied pretty much literally. C<cons> ends up pulling a single element
from the data stack, but we don't have much going on beyond that. So we can take
our list and replace it with a new quoted-interpreter identity:

  [x d...] [[[] swons unswons drop] c...] r -> [x d...] [c...] r

Now the entire list is effectively a new instruction.
=cut
