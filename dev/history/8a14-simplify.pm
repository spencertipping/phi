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

(4) is a mixed blessing, but I think it gives us better fidelity.

(5) is a disaster in the making if we aren't careful, but like (4) I don't think
it's a dealbreaker. I think we can expect to ask a dialect how we should call
its functions, or have it publish its functions as objects that manage any
calling-convention differences.

...basically, dialects own most semantics and unify on the surface rather than
expecting to conform at a structural level.
