#!/usr/bin/env perl

# Value types
use constant pnil => bless \my $nil_var, 'phi::nil';
sub pcons { bless [$_[0], $_[1]], 'phi::cons' }
sub pint  { bless \my $x = $_[0], 'phi::int' }
sub pstr  { bless \my $x = $_[0], 'phi::str' }
sub psym  { bless \my $x = $_[0], 'phi::sym' }
sub pmut  { bless \my $x,         'phi::mut' }

sub phi::nil::explain  { '[]' }
sub phi::int::explain  { ${+shift} }
sub phi::str::explain  { "\"${+shift}\"" }
sub phi::sym::explain  { ${+shift} }
sub phi::mut::explain  { defined ${$_[0]} ? 'M[...]' : 'M[]' }

sub phi::cons::explain
{
  my $cell = my $self = shift;
  my @elements;
  for (; ref $cell eq 'phi::cons'; $cell = $cell->tail)
  {
    push @elements, $cell->head->explain;
  }
  ref $cell eq 'phi::nil'
    ? "[" . join(" ", @elements) . "]"
    : join(" :: ", @elements, $cell->explain);
}

BEGIN
{
  for my $op (qw/ head tail /)
  {
    *"phi::mut::$op" = sub { ${+shift}->$op(@_) };
  }
}

sub phi::cons::head { shift->[0] }
sub phi::cons::tail { shift->[1] }
