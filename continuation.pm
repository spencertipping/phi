=head1 Iteration notes
1. Decide on primitive type behavior and document it.
2. Derive an encoding given base parsers so it isn't dog-slow.

=head2 IO/T monad
Let's start with arrays; suppose we have a function that allocates and accesses
an array in memory:

  (xs:int.array).reverse = do
    result = int.array(xs.size);
    xs.each_index(i -> result.set(i, xs.get(xs.size.minus(i).dec)));
    result
  end;

What's the side effect structure here? In particular, let's talk about
C<each_index>:

  (xs:int.array).each_index f:(int -> any) = do
    xs.size.times f;
    xs
  end;

C<do..end> is a monadic expansion structure, just like in Haskell. Let's
translate it, with type signatures:

  do                          # (int.array, int -> any) -> T int.array
    xs.size.times(f);         # T int
    xs                        # int.array
  end

NB: this won't work. We have arbitrary expression bindings in the middle of this
construct, each of which has the potential to impact the scope. We need
automatic T chaining at the expression level rather than in some type of control
flow construct; i.e. it should be possible to do this:

  (print("hi"), print("there"))

and have the return type be C<T (int, int)>.

=head2 It doesn't matter yet
Let's design the pure end of the language first and then tackle the T type. It
should all just work I think; the notational stuff is just the evaluator finding
implicit functions to bridge type gaps in an otherwise pure world.
=cut
