=head1 Compiling concatenative code
...should be straightforward; our instruction set doesn't support any open-ended
stack accessors; that is, no runtime data informs the index of any given stack
operation. This means we should be able to erase the stack entirely and get a
register-style representation of any given function.


=head2 Targeting machine code
Our bytecode interpreter is probably fast enough that we can just interpret
stuff; but when we need more performance we should be able to compile hot
methods to save cycles. I think the idea is to go two steps:

1. Concatenative -> SSA or RTL
2. SSA/RTL -> register-allocated machine code native

Easy enough, and low-priority given how fast our bytecode interpreter is.


=head2 Targeting perl
I suspect a lot of this also applies to python/ruby, but let's start with just
perl.

SSA or some register format is a good starting point here. No sense in emulating
the stack; array accesses are a _lot_ more expensive than processor
C<push>/C<pop> instructions.

Q: how do struct accessors work? Do we generate custom natives? (Because if
that's the plan, we'll need to be able to inline those natives if we want any
kind of performance.)


=head2 Example: the C<point2d> class
It's trivial, so let's start here. Our struct layout is exactly what you'd
expect:

  [point2d vtable]            # implied, autogenerated when required
  double x
  double y

There are no here-references or anything and all fields can be allocated inline.
Methods:

  new(x, y) = let p = point2d.allocate() in
              p.x = x;
              p.y = y;
              p
  plus(self, rhs) = point2d.new(self.x + rhs.x, self.y + rhs.y)

Concatenatively:

  new = [                               # x y
    point2d .allocate                   # x y p
    set-field"y"                        # x p
    set-field"x" ]                      # p

  plus = [                              # rhs self
    dup get-field"x" rot3<              # self self.x rhs
    dup get-field"x" rot3<              # self rhs rhs.x self.x
    float+                              # self rhs x'
    rot3< get-field"y"                  # rhs x' self.y
    rot3< get-field"y" float+           # x' y'
    point2d .new ]                      # point2d.new(x', y')

Here's the kind of code we want to get:

  package point2d
  {
    <possibly more methods having to do with the struct as an object>

    sub allocate
    {
      bless [0.0, 0.0], shift;
    }

    sub a                     # base62 encoding of "new"
    {
      my $self = point2d->allocate;
      $$self[0] = $_[0];
      $$self[1] = $_[1];
      return $self;
    }

    sub b                     # base62 encoding of "plus"
    {
      return point2d->a($_[0]->[0] + $_[1]->[0],
                        $_[0]->[1] + $_[1]->[1]);
    }
  }

I'm not 100% sure on the details of method name assignments, but if we're doing
the numeric protocol thing then we'd end up with very compact numeric names like
the ones above. (I'm assuming a protocol with C<new> and C<plus>.)
