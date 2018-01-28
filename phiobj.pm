=head1 Objects and polymorphism in phi
Let's model everyone's favorite object, a 2D point:

  [x y]

Boom. Pure genius right there.

OK, so how do we make this thing self-aware and polymorphic and stuff? We'll
need to affix a type: C<[point-type x y]>. Then methods will look at the type,
do some kind of dispatch, and that becomes a new calling convention. But we can
do even better:

  [point-type . x y]

Now we don't have a piece of data, we have a backwards closure. And for our
purposes that's perfectly sufficient, because all C<point-type> needs to do up
front is to pop the enclosing continuation from the stack and hold onto it as
data:

  [[pop-parent ...] . x y] == [[x y] [...] .] == [[x y] ...]

=cut


1;
