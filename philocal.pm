=head1 Local variable support
We want to bind stack entries to names so we can easily refer to them and place
them without using C<restack>. To do that, we need a C<let> construct that
stacks a custom resolver that contains local variables.


=cut
