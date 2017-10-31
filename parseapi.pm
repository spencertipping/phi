=head1 Parser API
phi's parsers don't provide much of an interface by themselves, so this module
defines functions that make them easier to work with.
=cut

use strict;
use warnings;


=head2 Explain outputs
This is purely for debugging, and provides human-readable-(ish) string coercion
for results and outputs.
=cut

package phi::parser::parser_base
{
  use overload qw/ "" explain /;

  sub explain;
  sub TO_JSON { shift->explain }
}

sub phi::parser::seq_fixed::explain
{
  my ($self) = @_;
  '(' . join(' ', @$self) . ')';
}

sub phi::parser::seq_repeat::explain
{
  my ($self) = @_;
  my $limits = $$self{max} < (-1 & 0x7fffffff)
             ? "{$$self{min}, $$self{max}}"
             : "{$$self{min},}";
  "($$self{parser} $limits)";
}

sub phi::parser::alt_fixed::explain
{
  my ($self) = @_;
  '(' . join(' | ', @$self) . ')';
}

sub phi::parser::map::explain
{
  my ($self) = @_;
  "($$self{parser} >>f)";
}

sub phi::parser::flatmap::explain
{
  my ($self) = @_;
  "($$self{parser} >f)";
}

sub phi::parser::mutable::explain
{
  my ($self) = @_;
  defined $$self ? "mut(defined)" : "mut(undefined)";
}

sub phi::parser::lookahead::explain
{
  my ($self) = @_;
  "~$$self";
}

sub phi::parser::filter::explain
{
  my ($self) = @_;
  "($$self{parser} >?f)";
}

sub phi::parser::not::explain
{
  my ($self) = @_;
  "!$$self";
}


sub phi::parser::strconst::explain
{
  my ($self) = @_;
  my $escaped = $$self =~ s/"/\\"/gr;
  "\"$escaped\"";
}

sub phi::parser::strclass::explain
{
  my ($self) = @_;
  my @ascii  = grep vec($$self{charvec}, $_, 1), 0..65535;
  my @chars;
  for (my $i = 0; $i < $#ascii; ++$i)
  {
    my $start = my $end = $ascii[$i];
    $end = $ascii[++$i] while $i+1 < @ascii and $ascii[$i + 1] == $end + 1;
    push @chars, $end != $start ? chr($start) . '-' . chr($end)
                                : chr($start);
  }
  ($$self{include} ? '[' : '[^')
    . join('', @chars)
    . ($$self{many} ? ']+' : ']');
}


package phi::parser::result_base
{
  use overload qw/ "" explain /;

  sub explain
  {
    my ($self)      = @_;
    my $type        = ref($self) =~ s/.*:://r;
    my $val_explain = '' . ($self->val // '');
    $val_explain = "\n$val_explain\n" if CORE::length $val_explain > 40;
    "$type\@$$self{start}<$val_explain>";
  }

  sub TO_JSON { shift->explain }
}


=head2 Parser construction functions
Building parsers by calling their constructors is a lot of work. We can do
better by having parsers inherit from a shared base with more convenient
notation.
=cut

package phi::parser::parser_base
{
  use Scalar::Util;
  use overload qw/ |  alt
                   +  seq
                   *  repeat
                   >> map
                   >  flatmap
                   %  filter
                   !  not
                   eq eq
                   ne ne /;

  sub alt     { phi::parser::alt_fixed->new(@_[0, 1]) }
  sub seq     { phi::parser::seq_fixed->new(@_[0, 1]) }
  sub repeat  { phi::parser::seq_repeat->new(@_[0, 1]) }
  sub map     { phi::parser::map->new(@_[0, 1]) }
  sub flatmap { phi::parser::flatmap->new(@_[0, 1]) }
  sub filter  { phi::parser::filter->new(@_[0, 1]) }
  sub not     { phi::parser::not->new(shift) }

  # Some interfacing helpers: parsers are comparable (they need to be for
  # flatmap to work correctly), and you can put them into JSON values for
  # debugging.
  sub eq      { Scalar::Util::refaddr($_[0]) eq Scalar::Util::refaddr($_[1]) }
  sub ne      { Scalar::Util::refaddr($_[0]) ne Scalar::Util::refaddr($_[1]) }
  sub bool    { 1 }

  sub explain { die "no implementation for " . ref(shift) . "::explain" }
}


=head2 Parser-specific overrides
If you write something like C<$a | $b | $c>, we want to create a single
three-way alt rather than two nested two-way alts. The simplest way to make
this happen is to use type-specific overrides.

Note that this only works for left-associative operators, but that's fine
because that's exactly what these are.
=cut

sub phi::parser::alt_fixed::alt
{
  my ($self, @ps) = grep ref, @_;
  phi::parser::alt_fixed->new(@$self, @ps);
}

sub phi::parser::seq_fixed::seq
{
  my ($self, @ps) = grep ref, @_;
  phi::parser::seq_fixed->new(@$self, @ps);
}


1;
