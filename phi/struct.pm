=head1 phi structs
Base types used to bootstrap the phi language in terms of perl. This is also
where we define base classes for structs and abstracts, so let's talk a bit
about how that works.

=head2 Self-hosting structs
There are two levels of structs going on here in practice, but in theory they're
the same level since the language is self-hosting. From the point of view of phi
code that's running, the levels are "hosted" (structs) and "quoted"
(meta-structs). Meta-structs should be able to describe the state of hosted
structs in terms of a minimal number of hosted structs.

=head2 Abstract values
Abstracts are hosted structs that describe the state of a runtime value. They
know two things:

1. Where they came from (or at least how to generate them)
2. Their type, for parsing purposes

Meta-structs derive (2) from (1); that's how the type system ends up working.
Although phi uses strong static typing, that type system isn't constrained by
anything in particular; abstracts are allowed to use arbitrarily complex logic
to dictate their continuations.

=head2 Compiling abstracts
There are a few things to keep in mind here:

1. Meta-structs define "operations", which abstracts store
2. Side effects are meta-struct operations against an opaque "state" global
3. Compilers are parsers that apply to a linearized operation list

It isn't necessarily obvious how this would work, so let's go through an
example in base syntax:

  n = stdin.readint             # or whatever
  xs = n.iota.map(fn |x:int|
    stdout.print(x)
    x.times(x)
  end)

This contains a loop and interleaved values/side effects. We need the side
effects even if C<xs> is thrown away, which means that blocks produce abstract
tuples: (side effect journal, value).

phi recognizes two kinds of loops. One is a bounded loop, which has an iteration
count that is ultimately constant. For example:

  s = "foo"
  bytes = s.length.iota.map(fn |i:int|
    s.byte(i)
  end)

This gets unrolled at compile-time and ends up being inlined into something very
efficient. This makes sense because it isn't really a loop at all; it's just the
way programmers specify to apply the same operation to each of an (in this case
finite) set of things.

However, suppose we have this:

  l = stdin.readline
  bytes = l.length.iota.map(fn |i:int|
    l.byte(i)
  end)

Now we can't avoid compiling a runtime loop because the line size is
theoretically unbounded. So we end up with a type equation:

  result = pre-loop = pre-loop iterate{1..}

That is, the parser's future needs to be indifferent to the number of iterations
we have; this is exactly the equation you'd get if you encoded loops
recursively.

It's worth noting that it's fine for loops to operate on heterogeneous values;
phi can work with union types. So if you had a map from string to int and wanted
to loop over both keys and values for some reason, the input arg would be of
type C<string|int> and the output would be C<f(string)|f(int)>. phi would
intersect the parse continuations from these types, and if you wanted to do a
type branch you'd have to introduce polymorphic selection.

This union behavior is handled by hosted implementations of C<int.if()>.

=head2 Side effects and compilation
Let's talk about what happens when we compile stuff like C<stdout.print("foo")>,
often within a context like this:

  100.iota.each(fn |x:int|
    stdout.print(x.to_s)
  end)

Unlike its appearance would suggest, phi uses monadic IO and side effects. This
makes it possible to optimize IO, memory allocation, and other actions exactly
the same way you'd optimize expression computation.

IO is threaded through scopes, follows a single unified timeline, and is
evaluated eagerly; in other words, phi functions as an imperative language. The
main difference is that any expression without an IO dependency can be evaluated
independently of that timeline; so if there's a loop with both side effects and
pure computations, for instance:

  ys = 100.iota.map(fn |x:int|
    stdout.print(x.to_s)
    stderr.print("another number\n")
    x + 1
  end)

Here, C<x + 1> can happen at any point in time because it doesn't interact with
the IO state.
=cut

package phi::struct;

use strict;
use warnings;

use phi::syntax ':all';
use phi::compiler;


=head1 Struct base
Common functionality you'll probably want.
=cut

package phi::struct::struct_base
{
  use parent -norequire => 'phi::parser::parser_base';

  sub parser;
  sub parse
  {
    my ($self, $input, $start, $scope) = @_;
    $self->parser($scope)->parse($input, $start, $scope);
  }
}


package phi::struct::abstract_base
{
  # Most abstract values don't modify the scope.
  sub scope_continuation { $_[1] }

  sub parse_continuation
  {
    my ($self, $scope) = @_;
    (phi::syntax::op(".")
      + phi::syntax::ident
      + (phi::syntax::de("(")->spaced
           + ($scope + phi::syntax::de(",")->maybe->spaced >>phi::syntax::nth(0))
               ->repeat(0)
         + phi::syntax::de(")")->spaced)->maybe
    >>sub {
      my ($input, $start, $length, $dot, $method, $op) = @_;
      my $cp   = pop @_;
      my @args = @_[6..$#_];
      $self->method($$method[0], @args);
    })->spaced;
  }

  sub method
  {
    my ($self, $methodname, @args) = @_;
    my $hosted_method = "phi_$methodname";
    return $self->$hosted_method(@args);
  }
}


sub deftype
{
  no strict 'refs';
  my ($name, $literal_parser, %methods) = @_;
  push @{"phi::struct::${name}::ISA"}, qw/ phi::struct::struct_base
                                           phi::struct::abstract_base /;

  if (defined $literal_parser)
  {
    my $ctor_parser = $literal_parser
      >>sub { bless { op   => 'const',
                      args => [$_[3]->[0]] }, "phi::struct::$name" };

    *{"phi::struct::${name}::parse"} = sub { shift; $ctor_parser->parse(@_) };
  }
  else
  {
    *{"phi::struct::${name}::parse"} = sub { shift->fail };
  }

  *{"phi::struct::${name}::explain"} = sub { "abstract $name" };

  while (my ($k, $v) = each %methods)
  {
    *{"phi::struct::${name}::phi_$k"} = ref $v
      ? $v
      : sub {
          my ($self, @args) = @_;
          bless { op   => $k,
                  args => [$self, @args] }, "phi::struct::$v";
        };
  }
}


deftype int => literal_int64,
  plus  => 'int',
  minus => 'int',
  times => 'int',
  inc   => 'int',
  dec   => 'int',
  to_f  => 'double',
  chr   => 'string',
  eq    => 'int',
  lt    => 'int',
  gt    => 'int',
  if    => 'int',
  or    => 'int',
  and   => 'int';

deftype double => literal_float,
  plus  => 'double',
  minus => 'double',
  times => 'double',
  to_i  => 'int',
  eq    => 'int',
  lt    => 'int';

deftype string => literal_softstring | literal_hardstring,
  length => 'int',
  substr => 'string',
  ord    => 'int',
  eq     => 'int',
  lt     => 'int',
  gt     => 'int';


=head1 Unknown values
This is how we modify scopes.
=cut

deftype unknown    => ident;
deftype assignment => undef;

sub phi::struct::unknown::parse_continuation
{
  my ($self, $scope) = @_;
  phi::syntax::op("=")->spaced + $scope
    >>sub { bless { name  => $$self{args}->[0],
                    value => $_[4] }, 'phi::struct::assignment' };
}

sub phi::struct::assignment::scope_continuation
{
  my ($self, $scope) = @_;
  $scope->with_bindings($$self{name}, $$self{value});
}


=head1 Lists
Lists of same-typed elements.
=cut

package phi::struct::list
{
  use parent -norequire => 'phi::struct::struct_base';
  use parent -norequire => 'phi::struct::abstract_base';
}


=head1 Functions
Functions are written with typed arguments using block/pipe syntax, e.g.
C<{|x:int| x.inc()}>.
=cut

package phi::struct::fn
{
  use parent -norequire => 'phi::struct::struct_base';
  use parent -norequire => 'phi::struct::abstract_base';

  sub args_parser
  {
    my ($self, $scope) = @_;
    (phi::syntax::de("|")->spaced
      + (phi::syntax::ident + phi::syntax::op(":")->spaced + $scope +
                              phi::syntax::de(",")->spaced->maybe
        >>sub { ($_[3]->[0], $_[5]) })->repeat(0)
      + phi::syntax::de("|")->spaced
     >>sub { @_[4..$#_-1] })->maybe;
  }

  sub child_scope
  {
    my ($self, $scope, @args) = @_;
    my %bindings;
    for (my $i = 0; ($i << 1 | 1) < @args; ++$i)
    {
      my ($name, $type) = @args[$i << 1, $i << 1 | 1];
      $bindings{$name} = bless { op => 'arg', args => [$i] }, $type;
    }
    $scope->child_with_bindings(%bindings);
  }

  sub parser
  {
    my ($self, $scope) = @_;
    (phi::syntax::de("{") | phi::syntax::de("fn"))->spaced
      + ($self->args_parser($scope)
         > sub {
             my ($input, $start, undef, @arg_bindings) = @_;
             my $cscope = $self->child_scope($scope, @arg_bindings);
             phi::compiler::block->new($cscope);
           })
      + (phi::syntax::de("}") | phi::syntax::de("end"))->spaced
    >>sub {
      bless { op   => 'const',
              args => [$_[4]] }, 'phi::struct::fn';
    };
  }

  sub phi_call
  {
    my ($self, @args) = @_;
    bless { op   => 'call',
            args => [$self, @args] }, 'phi::struct::fncall';
  }
}


1;
