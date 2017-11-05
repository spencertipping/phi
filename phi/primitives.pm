=head1 Primitive structs
Base types used to bootstrap the phi language in terms of perl.
=cut

use strict;
use warnings;

use phi::syntax;
use phi::compiler;


=head1 Struct base
Common functionality you'll probably want.
=cut

package phi::struct::struct_base
{
  use parent -norequire => 'phi::parser::parser_base';

  sub parser;
  sub parse { shift->parser->parse(@_) }

  sub abstract_const { (shift . "::const")->new(shift) }
  sub abstract_var   { (shift . "::var"  )->new(shift) }
}


package phi::struct::abstract_base
{
  sub parse_continuation
  {
    my ($self, $scope) = @_;
    phi::syntax::op(".")
      + phi::syntax::ident
      + phi::syntax::de("(")
        + ($scope + phi::syntax::de(",")->maybe >>phi::syntax::nth(0))
            ->repeat(0)
      + phi::syntax::de(")")
    >>sub {
      my ($input, $start, $length, $dot, $method, $op) = @_;
      my $cp   = pop @_;
      my @args = grep $_ ne ',', @_[6..$#_];
      $self->method($method, @args);
    };
  }

  sub method
  {
    my ($self, $methodname, @args) = @_;
    my $hosted_method = "phi_$methodname";
    $self->$hosted_method(@args);
  }
}


=head1 Numeric types
The usual suspects.
=cut

package phi::struct::int
{
  use parent -norequire => 'phi::struct::struct_base';
  use constant parser =>
    phi::syntax::literal_int64
      >>sub { phi::struct::int->abstract_const($_[3]->[0]) };
}

package phi::struct::int::const
{
  use parent -norequire => 'phi::struct::abstract_base';
  use parent -norequire => 'phi::struct::int';

  sub new
  {
    my ($class, $x) = @_;
    bless \$x, $class;
  }
}

package phi::struct::int::var
{
  use parent -norequire => 'phi::struct::abstract_base';
  use parent -norequire => 'phi::struct::int';
}


1;
