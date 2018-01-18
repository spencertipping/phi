=head1 Local variable support
We want to bind stack entries to names so we can easily refer to them and place
them without using C<restack>. To do that, we need a C<let> construct that
stacks a custom resolver that contains local variables. (Although in this case
it's called C<fn> to emphasize that we're capturing args.)

This is the first example of a concatenative macro: we transform the
continuation stack to restore the original resolver after the local scope ends.
In this case that happens after the current list. For example:

  [fn [x y] x y +]

Here, C<fn> resolves to a value that immediately fetches the continuation and
pops C<[x y]> from that list. It then saves the current resolver, pops C<x y +>,
and adds it back after pushing a "resolver reset" list.

Manipulating continuation lists is delicate: if you grab the current
continuation and then put it back, you'll get an infinite loop. So we need to
pop the rest of the currently-executing continuation before replacing the
cstack.

=head2 C<fn> implementation
This is a fairly involved definition compared to stuff like parsers.

  fn = let c = get-clist in             # self continuation = c.head
       let resolver = get-rlist in      # capture current resolver
       let caller = c.tail.head in      # this contains the bindings + stuff
       let callernext = c.tail.tail in  # this is where we prepend restore

       # Now we need to build the binding list from the input args.
       # This involves quoting each thing by putting it into a list, then adding
       # an unswons/drop to that list to drop it (unevaluated) into whichever
       # context it belongs.
       


=cut
