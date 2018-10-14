=head1 Time to simplify phi2
It's kind of a mess. We have several concepts that are redundant in some way.
First, bytecode assembly abstractions:

=item C<phi1/asm>: low-level structured bytecode assembly
=item C<phi2/ir>: higher-level structured bytecode assembly
=item C<phi2/schedule>: yet higher-level IR ranges for expressions

Then we also have redundant concepts around type information:

=item C<phi2/ctti>: dialect-independent type structures
=item C<phi2/abstract>: dialect-independent type/const structures

Finally, dialects themselves have a lot of methods that may or may not be fully
justifiable. How much of this can we nuke without losing important
functionality?


=head2 End-to-end
What's the shortest parameterizable path from source to GC-safe bytecode? Have
the dialect own almost all of the IR-style logic and have abstracts present a
minimal handoff interface. So really two abstractions:

  dialect  = source    -> frame classes  + bytecode
  abstract = semantics -> GC disposition + bytecode + abstract

This carries some benefits:

1. Dialects can manage things like frame linkages for lexical closures
2. We can simulate nonstandard languages like FORTH
3. Dialects can fully dictate control flow and laziness/strictness
4. Dialects can opt into disclosure about locals/globals/etc
5. Dialects can use nonstandard calling conventions
6. Parse states can manage their own IR tracking
7. Dialects can dictate memory management

(4) is a mixed blessing, but I think it gives us better fidelity.

(5) is a disaster in the making if we aren't careful, but like (4) I don't think
it's a dealbreaker. I think we can expect to ask a dialect how we should call
its functions, or have it publish its functions as objects that manage any
calling-convention differences.

(7) solves a long-running problem I've had with dialects like C, which need to
provide pointers to things. The problem with pointers is that they break GC: if
you have an C<int*> that refers to the field of some struct, how do we know
which struct it belongs to and how to relocate it if a GC happens? We can't just
add GC to any language arbitrarily.

...basically, dialects own most semantics and unify on the surface rather than
expecting to conform at a structural level.


=head2 Bridging semantic differences between dialects
Obviously we should have a standard set of protocols dialects can opt into, but
beyond that, how do we deal with more fundamental differences like memory
management? Can we meaningfully have an unmanaged dialect that interoperates
with a managed one? (Sure, but we need to be careful about GC pinning.)

This seems to imply that GC is a thing data structures opt into in order to
comply with managed dialects. That seems reasonable enough. We'll get a
compile-time error if we try to use an unmanaged data type from a managed
language (we hope). Managed datatypes from unmanaged languages might also throw
an error depending on expectations around destructors.
