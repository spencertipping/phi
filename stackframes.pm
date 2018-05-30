=head1 OK, let's get this sorted out
1. The data stack should be untraced and primitive-only
2. The return stack should be structured using polymorphic value types

If so, this means we need to commit things to the rstack before allocation.
Should be fine if we have stack frame objects.

Q: how do we invoke methods against rstack-allocated values if the rstack is
required to be structured? We need to define a calling convention.

=head2 rstack calling convention
By default we need three things per function call on the rstack:

1. The traceable base pointer of the calling code object
2. The original C<%rsi> value (the untraceable insn pointer)
3. A vtable that structures (1) and (2) for GC purposes

Functions that use stack frames will then reserve extra space in C<%rbp> and
write a new vtable below. So we'd end up with a deeper stack:

  traceable-caller
  original-rsi
  <frame data>    <- original %rbp
  ...
  <frame data>
  frame-vtable    <- %rbp after frame allocation

This frame allocation needs to happen inline since we're modifying the return
stack. So it and the corresponding deallocation become the function's
prolog/epilog. A function with one such prolog/epilog might look like this:

  lit(40) lit(frame-vtable) rframe      # prolog (custom insn to help)
  rmgoto(3)                             # epilog (vtable manages cleanup?)

I don't love having so many variants of C<mcall>, C<mgoto>, C<rmcall>, etc, but
it may not be a bad thing.
