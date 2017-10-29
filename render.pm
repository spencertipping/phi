package phi::compiler;

use strict;
use warnings;


package phi::compiler::node
{
  use Scalar::Util qw/refaddr/;
  use overload qw/ @{} val_array ${} val_scalar "" explain ne ne eq eq /;

  sub print_at
  {
    my $self = shift;
    my $pos  = shift;
    my ($r, $c) = map $_+1, phi::e()->pos_rowcol($pos);
    print "\033[$r;${c}H", @_;
  }

  sub print
  {
    my $self = shift;
    $self->print_at($self->result->start, @_);
  }

  sub colored
  {
    my ($self, $c) = @_;
    $self->print("\033[${c}m" . $self->result->{input}->substr($self->result->start, $self->result->length));
  }

  sub explain    { shift->val }
  sub result     { shift->{result} }
  sub val_array  { shift->{value} }
  sub val_scalar { \shift->{value} }

  sub eq { refaddr $_[0] eq refaddr $_[1] }
  sub ne { refaddr $_[0] ne refaddr $_[1] }
}


sub phi::syntax::rf::render     { ${+shift}->render }
sub phi::syntax::rall::render   { $_->render for @{+shift} }
sub phi::syntax::rcolor::render { $_[0]->colored($_[0]->color) }
sub phi::syntax::rc0::color     { '0;30' }
sub phi::syntax::rc1::color     { '0;31' }
sub phi::syntax::rc2::color     { '0;32' }
sub phi::syntax::rc3::color     { '0;33' }
sub phi::syntax::rc4::color     { '0;34' }
sub phi::syntax::rc5::color     { '0;35' }
sub phi::syntax::rc6::color     { '0;36' }
sub phi::syntax::rc7::color     { '0;37' }


sub phi::syntax::vf::val    { ${+shift}->val }
sub phi::syntax::v::val     { ${+shift} }
sub phi::syntax::v0::val    { shift->[0]->val }
sub phi::syntax::v1::val    { shift->[1]->val }
sub phi::syntax::vjoin::val { join '', @{+shift} }


1;
