=head1 Applicative grammar for phi
Writing concatenative code is miserable. I'd much rather use something with
ocaml or python syntax than have to keep track of the stack state all the time.
So I'm going to write a self-hosting grammar for phi that compiles applicative
to concatenative code.

The basic idea is to track the state of the data stack by having an extra list
in the parse state. So instead of C<[s i]>, we'd have C<[s i stacklayout...]>.
The stack layout looks like this:

  [next-id id|name  id|name  ...]
  #        stack[0] stack[1] ...

Let's talk about this in more detail.

=head2 Stack layout encoding
Suppose we're compiling an expression like C<y = f(x, x + 1)>. This looks like
it would be straightforward to compile: we'd end up with C<x x 1 + f> or
similar. But C<x> is already on the stack somewhere, so really we'd write out a
sequence of constant-pushes, C<restack> instructions, and operations to arrange
the stack correctly.

B<TODO>: is it correct for the parsers to track this? Maybe it's more sensible
to cons up a graph first, then have that graph convert itself to concatenative.
=cut
