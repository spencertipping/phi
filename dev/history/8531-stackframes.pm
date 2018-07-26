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
it may not be a bad thing. It does require objects to be somewhat aware of which
stack they're allocated onto, though. This is broken.

...actually, is that true? In the case of C<rmgoto(3)> (where 3 =
C<deallocate-frame> or some such), the receiver is never explicitly addressed.
So we're basically doing this, but more efficiently:

  rpeek mgoto(3)

I think it's fine to use two insns for this. No need to have C<rmcall>/C<rmgoto>
variants. We can just use C<mgoto> and C<mcall>, both of which are required.

=head2 Implementation notes
1. During the function call, the frame object is used untyped; no method calls
2. C<mgoto(3)> in the epilog must be virtual to accommodate TCO (this is a lie)

TCO is fine, but we can't just do it by reusing the "caller" frame unless those
frame objects are the same size. A tail call should rewind the frame allocation
so the callee's prolog reallocates it correctly.

Q: how does the calling code object context know the base address to push onto
the rstack?

Q: how portable is any of this, really? Managed runtimes are going to work in a
completely different way, so our level of abstraction just increased
substantially. At this rate should we ditch concatenative and emit SSA or
something?

=head2 Backend-independent representation
The big thing about rstack-allocated objects is that they don't create GC
overhead. So far so good: if each function specifies its frame allocation
struct, we can easily port this to other languages -- including statically-typed
ones. That much is a win for portability.

Details around C<mcall>/C<mgoto> may or may not work out. vtables can be
universally supported, but tail calls are less clear. We may have to implement
trampolining if we care. This probably isn't the end of the world.

Structs are portable. So is concatenative-primitive stuff. GC atomicity is up to
an API: we can say "allocate me some memory; here's the address to drop the
pointer into," which means our allocations are all atomic by default.

Object self-reblessing is not portable to most managed languages, but we may be
able to simulate it. I'm not sure whether this is a dealbreaker; I feel like we
mainly use this as a hack to get things done in machine code.

...so all things considered we're in good shape. Having local variables in a
stack frame simplifies concatenative code a lot without complicating the
interpreter mechanics at all.

=head2 SSA vs concatenative
Pros of concatenative:

1. Trivial to implement: bytecode FORTH with simple threading
2. Simple enough to write
3. Portable enough
4. No explicit arg-addressing strategy required
5. GC atomicity has a clear boundary
6. Absolutely zero JIT overhead
7. No compiler required: bytecode interpretation works just fine

Pros of SSA:

1. Simplifies optimized JIT and register allocation
2. Automatic GC atomicity?
3. Easier to emit from a frontend compiler
4. Abstract interpretation and analysis are easier

I think that's mostly it. Given these tradeoffs concatenative seems like a much
better option, if for no other reason than the fact that it's so simple. I
believe we can easily convert most concatenative programs to SSA in any case.
