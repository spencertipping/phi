=head1 Parser API
phi's parsers don't provide much of an interface by themselves, so this module
defines functions that make them easier to work with.
=cut

package phi::parseapi;

use strict;
use warnings;

use Exporter 'import';
our @EXPORT_OK;
our %EXPORT_TAGS = (all => \@EXPORT_OK);

require phi::parser;
require phi::parsestr;


=head2 Explain outputs
This is purely for debugging, and provides human-readable-(ish) string coercion
for results and outputs.
=cut

package phi::parser::parser_base
{
  use overload qw/ "" explain_misuse /;

  sub explain;
  sub explain_misuse { die "can't coerce " . shift->explain . " to a string" }
  sub TO_JSON        { shift->explain }
}

sub phi::parser::parse_none::explain { '(fail)' }

sub phi::parser::seq_fixed::explain
{
  my ($self) = @_;
  '(' . join(' ', map $_->explain, @$self) . ')';
}

sub phi::parser::seq_repeat::explain
{
  my ($self) = @_;
  my $limits = $$self{max} < (-1 & 0x7fffffff)
             ? "{$$self{min}, $$self{max}}"
             : "{$$self{min},}";
  my $explain = $$self{parser}->explain;
  "($explain $limits)";
}

sub phi::parser::alt_fixed::explain
{
  my ($self) = @_;
  '(' . join(' | ', map $_->explain, @$self) . ')';
}

sub phi::parser::map::explain
{
  my ($self) = @_;
  my $explain = $$self{parser}->explain;
  "($explain >>f)";
}

sub phi::parser::flatmap::explain
{
  my ($self) = @_;
  my $explain = $$self{parser}->explain;
  "($explain >f)";
}

sub phi::parser::mutable::explain
{
  my ($self) = @_;
  defined $$self ? "mut(defined)" : "mut(undefined)";
}

sub phi::parser::lookahead::explain
{
  my ($self) = @_;
  my $explain = $$self->explain;
  "~$explain";
}

sub phi::parser::filter::explain
{
  my ($self) = @_;
  my $explain = $$self{parser}->explain;
  "($explain >?f)";
}

sub phi::parser::not::explain
{
  my ($self) = @_;
  my $explain = $$self->explain;
  "!$explain";
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


=head2 Parser construction functions
Building parsers by calling their constructors is a lot of work. We can do
better by having parsers inherit from a shared base with more convenient
notation.
=cut

package phi::parser::parser_base
{
  use Scalar::Util;
  use overload qw/ |  alt_op
                   +  seq_op
                   *  repeat_op
                   >> map
                   >  flatmap
                   %  filter
                   ~  lookahead
                   !  not
                   "" explain /;

  sub alt_op    { shift->alt(shift) }
  sub seq_op    { shift->seq(shift) }
  sub repeat_op { shift->repeat(shift) }
  sub map       { phi::parser::map->new(@_[0, 1]) }
  sub flatmap   { phi::parser::flatmap->new(@_[0, 1]) }
  sub filter    { phi::parser::filter->new(@_[0, 1]) }
  sub lookahead { phi::parser::lookahead->new(shift) }
  sub not       { phi::parser::not->new(shift) }

  sub maybe     { phi::parser::seq_repeat->new(shift, 0, 1) }
  sub alt       { phi::parser::alt_fixed->new(@_) }
  sub seq       { phi::parser::seq_fixed->new(@_) }
  sub repeat    { phi::parser::seq_repeat->new(@_) }

  # Output transformations
  sub ignore    { shift >> sub {           () } }
  sub boxed     { shift >> sub { [@_[1..$#_]] } }

  sub explain   { die "no implementation for " . ref(shift) . "::explain" }
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
  ref $_ or die "tried to construct alt_fixed(@_) with non-ref $_" for @_;
  $_->can('parse') or die "tried to construct alt_fixed(@_) with non-parser $_" for @_;
  phi::parser::alt_fixed->new(@{+shift}, @_);
}

sub phi::parser::seq_fixed::seq
{
  ref $_ or die "tried to construct seq_fixed(@_) with non-ref $_" for @_;
  $_->can('parse') or die "tried to construct seq_fixed(@_) with non-parser $_" for @_;
  phi::parser::seq_fixed->new(@{+shift}, @_);
}


=head2 Parser shorthands
Accessors for common parsing idioms.
=cut

sub oc { phi::parser::strclass->one_of(@_) }
sub mc { phi::parser::strclass->many_of(@_) }
sub Mc { phi::parser::strclass->more_of(@_) }
sub oe { phi::parser::strclass->one_except(@_) }
sub me { phi::parser::strclass->many_except(@_) }
sub Me { phi::parser::strclass->more_except(@_) }

sub str($) { phi::parser::strconst->new(shift) }

# NB: single-arg, but multi-prototype to modify precedence
sub mut(@) { phi::parser::mutable->new(shift) }
sub alt(@) { phi::parser::alt_fixed->new(shift) }


push @EXPORT_OK, qw/ oc mc Mc oe me Me str mut alt /;


1;
