=head1 phi and CPS
phi's bytecode naturally lends itself to CPS rather than SSA. Every bytecode
segment ends in C<goto>, which is a tail call, and the set of stack-resident
values is the set of arguments and captured values to the next function. C<goto>
addresses a continuation/function as a value rather than as a constant.
Concatenative functions can produce multiple return values: arguments to the
continuation. CPS is also ideal because it produces the correct pin set for
function objects.

GC-correct CPS is simple enough to generate. We can impose a layout for the
stack, then alias the frame over it:

  <incoming-args...>
  <locals...>
  <original-frameptr>
  <frameclass>            <- new frameptr
  <new-stack...>

C<call> uses the new stack, C<goto> involves setting the current argument set
and popping the frame. There doesn't need to be any distinction between locals
and incoming args once we're inside the function.


=head2 Conditional branches
Branching is simple enough; we just need a unified type signature for all of the
alternatives. This unification serves the same purpose as a φ node in SSA.
