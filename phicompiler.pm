=head1 phi compiler
Before I get into how this works, I'll go through a simple example.

=head2 Example: the point struct
Let's write a function that takes two 2D vectors and returns the dot product.

  my $point = struct->new(x => double, y => double);
  $point->method(dot => [double, $point, $point])
        ->dot(phi => q{ |self, rhs| self.x*rhs.x + self.y*rhs.y });

  my $agent = struct->new->method(dot => [double, $point, $point])
                         ->dot(phi => q{ |self, u, v| u.dot(v) });

  my $process  = c99->new->struct(agent => $agent);
  my $compiled = &$process(phi => q{ agent.new });

  # this prints 11
  print $compiled->dot($point->new(1, 2),
                       $point->new(3, 4));

=head2 How this works

=cut
