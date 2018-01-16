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
Suppose we're compiling an expression like C<f(x, x + 1)>. This looks like it
would be straightforward to compile: we'd end up with C<x x 1 + f> or similar.
But C<x> is already on the stack somewhere, so really we'd write out a sequence
of constant-pushes, C<restack> instructions, and operations to arrange the stack
correctly.

So let's walk through the compilation of a function in terms of the stack layout
list:

  f(x) = g(x, x + 1)

  |g(x, x + 1)        [0 x]               # initially just x on the stack
  g|(x, x + 1)        [1 0 x]             # "g" is resolved and bound to expr 0
  g(x|, x + 1)        [2 1 0 x]           # "x" is cloned and pushed
  g(x, x| + 1)        [3 2 1 0 x]         # ditto
  g(x, x + 1|)        [4 3 2 1 0 x]       # "1" is pushed as expr 3...
                      [5 4 3 2 1 0 x]     # ...and x+1 is pushed as expr 4
  g(x, x + 1)|        [6 5 4 3 2 1 0 x]   # ...and we make the tuple (expr 5)
                      [7 6 5 4 3 2 1 0 x] # ...and call g (expr 6)

Then the function context cleans up locals by just returning the stack top; this
is a single C<restack> operation.

The stack layout gets more interesting when we have local variable bindings.
Then we have something like this:

  f(x) = let y = x + 1 in g(x, y)

  |let y = x + 1 in g(x, y)     [0 x]
  let y = x| + 1 in g(x, y)     [1 0 x]
  let y = x + 1| in g(x, y)     [2 1 0 x] -> [3 2 1 0 x]
  let y = x + 1 in| g(x, y)     [3 y 2 1 0 x]

TODO: I'm not sure I like this strategy. We end up with a lot of leftover stack
stuff that doesn't seem particularly necessary or productive; do we want
parse-local concatenative management and then to store a mapping from
named bindings to stack positions? (Ah, but what if we need to look up a
variable I<while> we're building up the stack-local stuff? Add a depth offset?)
=cut
