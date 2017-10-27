=head1 phi compiler
Before I get into how this works, I'll go through a simple example.

=head2 Example: the point struct
Let's write a function that takes two 2D vectors and returns the dot product.

  my $point = struct->new(x => double, y => double);
  $point->method(dot => [[$point, $point] => double])
        ->dot(phi => q{ |self, rhs| self x * rhs x + self y * rhs y });

  my $runtime = c99->new($point);
  my $cpoint  = $point->in($runtime);
  my $p1      = $cpoint->new(x => 1, y => 2);
  my $p2      = $cpoint->new(x => 3, y => 4);

  # this prints 11
  print $p1->dot($p2);

=head3 What's going on here
Starting with the structs: we basically have two datatypes used by this
program, C<double> and C<$point>. Each is an untagged struct, which means
compiled operations against them are fully type-specified when they happen
(there are no virtual method calls).

We define our own struct C<$point> as a composite of two doubles; at this point
we've declared the representation. We then define a method C<dot> with type
signature C<double dot($point, $point)>. The next call against the struct
specifies that dot() can be executed in a phi runtime with the provided code.
phi doesn't do anything at this point; it stores the struct and moves on.

Next we create $runtime, a C99 environment, and we tell it about $point. This
kicks off the following chain of events:

1. $runtime asks $point for its list of forward declarations in C99, entailing:
2. $point goes through its methods and compiles them to C99, each entailing:
3. $point creates a phi compiler, parses the code, and invokes it on abstracts
4. $point then takes that compiler and asks it to generate c99 for the function
5. $runtime writes a C file, compiles it, and forks/execs

At this point we have a separate process in which we can create objects. This
object creation is mediated by a proxy struct, which we get by calling in() on
$point. This proxy struct manages remote instances as though they were local,
more or less.

Next we create two points in the C99 runtime. The runtime delegates ownership of
these points to the calling process, meaning that they'll be deallocated in the
remote as soon as they're deallocated in Perl. The Perl-side of these objects
doesn't store any field values; it just stores the runtime, $point, and an
object ID (which for C is a pointer).

Finally, we call dot() on one of the remote values. $p1 knows that this is a
monomorphic call, so it creates a runtime request for a monomorphic call to
$point::dot and specifies the pointers as arguments. It then synchronously
listens for a reply, decodes that, and returns it.
=cut
