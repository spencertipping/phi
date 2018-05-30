=head1 OK, let's get this sorted out
1. The data stack should be untraced and primitive-only
2. The return stack should be structured using polymorphic value types

If so, this means we need to commit things to the rstack before allocation.
Should be fine if we have stack frame objects.

Q: how do we invoke methods against rstack-allocated values if the rstack is
required to be structured?
