=head1 Primitive structs
Base types used to bootstrap the phi language in terms of perl.
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
      + phi::syntax::de("(")->spaced
        + ($scope + phi::syntax::de(",")->maybe->spaced >>phi::syntax::nth(0))
            ->repeat(0)
      + phi::syntax::de(")")->spaced
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
  dec   => 'int';

deftype double => literal_float;

deftype string => literal_softstring | literal_hardstring;


=head1 Unknown values
This is how we modify scopes.
=cut

deftype unknown    => ident;
deftype assignment => undef;

sub phi::struct::unknown::parse_continuation
{
  my ($self, $scope) = @_;
  phi::syntax::op("=")->spaced + $scope + phi::syntax::op(";")->spaced
    >>sub { bless { name  => $$self{args}->[0],
                    value => $_[4] }, 'phi::struct::assignment' };
}

sub phi::struct::assignment::scope_continuation
{
  my ($self, $scope) = @_;
  $scope->with_bindings($$self{name}, $$self{value});
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
    phi::syntax::de("|")->spaced
      + (phi::syntax::ident + phi::syntax::op(":")->spaced + $scope +
                              phi::syntax::de(",")->spaced->maybe
        >>sub { ($_[3]->[0], $_[5]) })->repeat(0)
      + phi::syntax::de("|")->spaced
    >>sub { @_[4..$#_-1] };
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
    phi::syntax::de("{")->spaced
      + ($self->args_parser($scope)
         > sub {
             my ($input, $start, undef, @arg_bindings) = @_;
             my $cscope = $self->child_scope($scope, @arg_bindings);
             phi::compiler::block->new($cscope)->parse($input, $start);
           })
      + phi::syntax::de("}")->spaced
    >>sub {
      bless { op   => 'const',
              args => [$_[4]] }, 'phi::struct::fn';
    };
  }
}


1;
