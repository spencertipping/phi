use warnings;

our $AUTOLOAD;
sub AUTOLOAD
{
  print "$AUTOLOAD: @_\n";
}

rhs << 10;
rhs() << 10;
