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
anything in particular; abstracts are allowed to use arbitrarily complex (pure!)
logic to dictate their continuations.
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
    my ($self, $input, $start, $scope, $io) = @_;
    $self->parser($scope)->parse($input, $start, $scope, $io);
  }
}


sub arglist_parser
{
  my ($scope) = @_;
  $scope->flatmap(sub {
    my ($input, $offset, $l, $scope_and_io, $io, @vs) = @_;
    phi::syntax::de(",")->spaced
      + arglist_parser($vs[-1]->class->scope_continuation($vs[-1], $scope))
    >>sub {
      my ($input, $start, $length, $scope_and_io, $comma, $io, @parsed) = @_;
      ($io, @vs, @parsed);
    };
  });
}


package phi::struct::abstract_base
{
  # Most abstract values don't modify the scope.
  sub scope_continuation { $_[1] }

  # Default syntax: support type-specific method calls.
  sub parse_continuation
  {
    my ($self, $scope) = @_;
    (phi::syntax::op(".")
      + phi::syntax::ident
      + (phi::syntax::de("(")->spaced
         + phi::struct::arglist_parser($scope)
         + phi::syntax::de(")")->spaced)->maybe
    >>sub {
      my ($input, $start, $length, $scope_and_io, $dot, $method, $op) = @_;
      my $cp   = pop @_;
      my @args = @_[7..$#_];
      $self->method($$scope_and_io[1], $$method[0], @args);
    })->spaced;
  }

  sub method
  {
    my ($self, $io, $methodname, @args) = @_;
    my $hosted_method = "phi_$methodname";
    $self->$hosted_method($io, @args);
  }
}


# TODO: rewrite almost everything below
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
