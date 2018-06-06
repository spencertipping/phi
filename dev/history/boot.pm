=head1 Booting phi
phi is the most self-referential project I've ever worked on, so it's not
remotely obvious to me where it bottoms out into a bootstrap script. Let's
figure this out.

First, the main goal of phi is to provide metaclass-aware OOP for a wide variety
of different runtimes with maximal code/data portability. So the language has a
few constraints:

1. Almost everything is written in a semantically consistent bytecode
2. Data layout is done with pre-idiom "logical structs", which are then compiled
3. Structs and classes are portable object instances
4. The interpreter implementation is replaceable and can be GC'd

(GC isn't independently a big deal; objects trace and collect themselves on
backends that don't provide memory management.)

(3) and (4) conspire to make our lives interesting. Jointly, they imply that
every aspect of our bytecode implementations and base objects/structs be
described in terms of objects. Combined with the other constraints, then, we
have three fixed point elements:

1. Classes are and implement objects
2. Bytecode functions are and implement classes
3. Interpreters are instances of and implement classes

phi is designed to implement a minimal solution to these fixed points while
providing a runtime you'd want to use for real problems.
