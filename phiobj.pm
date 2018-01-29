=head1 Objects and polymorphism in phi
Let's model everyone's favorite object, a 2D point:

  [x y]

Boom. Pure genius right there.

OK, so how do we make this thing self-aware and polymorphic and stuff? We'll
need to affix a type: C<[point-type x y]>. Then methods will look at the type,
do some kind of dispatch, and that becomes a new calling convention. But we can
do even better:

  [[x y] point-type...]

Now we've got more than just a piece of data; we have a fully self-aware
closure. It has some convenient equations:

  obj head = state
  obj tail = type

=cut


1;
