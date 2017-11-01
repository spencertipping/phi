=head1 phi base structs
These structs define parsing entry points for the language. The object structure
here is subtle and possibly confusing, so I'll go through what's going on.

TODO: explain object relationships

Because phi::struct::double is a struct, it provides a few methods:

1. parse(): parse literals, or none if there is no literal syntax
2. abstract(): generate an unconstrained abstract instance

TODO: formalize/fix up parse continuations, which aren't done correctly right
now (we need a way to forward scope, but this isn't it)
=cut

package phi::struct;

use strict;
use warnings;


=head1 Struct protocol
An abstract base class, just so we have one.
=cut

package phi::struct::struct_base
{
  use parent -norequire => 'phi::parser::parser_base';
  sub parse { $self->fail('no literal support') }
  sub abstract;
}


=head1 Primitive types
These are all implemented on a backend-specific basis.
=cut

use constant int_hex_literal => str('0x') + phi::parser::mo(0..9) >>as"int_hex";
use constant int_oct_literal => str('0')  + phi::parser::mo(0..7) >>as"int_oct";
use constant int_dec_literal =>             phi::parser::mo(0..9) >>as"int_dec";
use constant int_literal => int_hex_literal | int_oct_literal | int_dec_literal;

package phi::node::int
{
  use parent -norequire => 'phi::node::node_base';
  # TODO: use parent -norequire => 'phi::node::number';

  # TODO: parse_continuation() to parse numeric ops
  # Q: should nodes delegate straight to abstract vals for parse continuations?
}

package phi::node::int_hex
{
  use parent -norequire => 'phi::node::int';
  sub val
  {
    my $hex    = shift->x(1);
    my $bits   = 4 * length $hex;
    my ($size) = grep $_ >= $bits, 8, 16, 32, 64;

    # FIXME: should never die in val() (the parser should reject the value)
    die "hex literal 0x$hex is larger than 64 bits" unless defined $size;
    phi::struct::abstract_int->new("phi::struct::uint$size", hex $hex);
  }
}

package phi::node::int_oct
{
  use parent -norequire => 'phi::node::int';
  sub val
  {
    my $oct    = shift->x(1);
    my $bits   = 3 * length $oct;
    my ($size) = grep $_ >= $bits, 8, 16, 32, 64;

    # FIXME: should never die in val() (the parser should reject the value)
    die "octal literal 0$oct is larger than 64 bits" unless defined $size;
    phi::struct::abstract_int->new("phi::struct::uint$size", oct $oct);
  }
}

package phi::node::int_dec
{
  use parent -norequire => 'phi::node::int';
  sub val
  {
    my $dec    = 0 + shift->x(1);
    my ($size) = grep +($dec & !(-1 << $_)) == $dec, 8, 16, 32, 64;
    phi::struct::abstract_int->new("phi::struct::int$size", $dec);
  }
}

package phi::struct::int
{
  use parent -norequire => 'phi::struct::struct_base';
  sub bits;
  sub signed;

  sub parse    { shift; int_literal->parse(@_) }
  sub abstract { phi::struct::abstract_int->new(shift) }
}

BEGIN
{
  for my $bits (qw/8 16 32 64/)
  {
    for my $sign ('', 'u')
    {
      no strict 'refs';
      push @{"phi::struct::${sign}int${bits}::ISA"}, 'phi::struct::int';
      *{"phi::struct::${sign}int${bits}::bits"} = sub { $bits };
      *{"phi::struct::${sign}int${bits}::signed"}
        = length $sign ? sub { 0 } : sub { 1 };
    }
  }
}
