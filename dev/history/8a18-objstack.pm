=head1 OK, let's keep the stack
The stack is probably the simplest way to manage arguments and return values. It
also lets use a consistent abstraction to keep track of one-extra-thing of
state, and doesn't completely break static analysis. Let's cap it at 64 entries
or something; then the rest of the "stack" is for frame allocation.

Let's also change C<get_frameptr> to a more useful instruction that returns a
signed offset address:

  F XXXX == get_frameptr signed_lit16(XXXX) iplus

Then we have some idioms like C<F(XXXX) m64get> etc. We can also use this as an
allocation delta since C<XXXX> is signed.
