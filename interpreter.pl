#!/usr/bin/env perl
use strict;
use warnings;

package phi;

# Value types
use constant pnil => bless \my $nil_var, 'phi::nil';
sub pcons { bless [$_[0], $_[1]],   'phi::cons' }
sub pint  { bless \(my $x = $_[0]), 'phi::int' }
sub pstr  { bless \(my $x = $_[0]), 'phi::str' }
sub psym  { bless \(my $x = $_[0]), 'phi::sym' }
sub pmut  { bless \(my $x),         'phi::mut' }

sub phi::nil::explain  { '[]' }
sub phi::int::explain  { ${+shift} }
sub phi::str::explain  { "\"${+shift}\"" }
sub phi::sym::explain  { ${+shift} }
sub phi::mut::explain  { defined ${$_[0]} ? 'M[...]' : 'M[]' }

sub phi::cons::explain
{
  my $cell = my $self = shift;
  my @elements;
  for (; CORE::ref $cell eq 'phi::cons'; $cell = $cell->tail)
  {
    push @elements, $cell->head->explain;
  }
  CORE::ref $cell eq 'phi::nil'
    ? "[" . join(" ", @elements) . "]"
    : join(" :: ", @elements, $cell->explain);
}

BEGIN
{
  for my $op (qw/ head tail unlist /)
  {
    no strict 'refs';
    *{"phi::mut::$op"} = sub { ${+shift}->$op(@_) };
  }
}

sub phi::cons::head { shift->[0] }
sub phi::cons::tail { shift->[1] }

sub phi::cons::unlist { ($_[0]->[0], $_[0]->[1]->unlist) }
sub phi::nil::unlist  { () }

sub list { @_ ? pcons(shift, list(@_)) : pnil }

# Interpreter cases
# The interpreter is represented as a Perl array [d, c, r], and modified
# in-place.

sub phi::i::push  { $_[0]->[0] = phi::pcons $_[1], $_[0]->[0]; shift }
sub phi::i::pop   { my $d = $_[0]->[0]; $_[0]->[0] = $d->tail; $d->head }
sub phi::i::peek  { $_[0]->[0]->head }
sub phi::i::cpush { $_[0]->[1] = phi::pcons $_[1], $_[0]->[1]; shift }
sub phi::i::cpop  { my $c = $_[0]->[1]; $_[0]->[1] = $c->tail; $c->head }
sub phi::i::quote { phi::list @{+shift} }

sub phi::i::i0 { $_[0]->push($_[0]->quote) }
sub phi::i::i1 { @{$_[0]} = $_[0]->pop->unlist }
sub phi::i::i2 { $_[0]->cpush($_[0]->pop) }
sub phi::i::i3 { $_[0]->push(phi::psym(ref $_[0]->pop =~ s/^phi:://r)) }
sub phi::i::i4 { $_[0]->push(phi::pint($_[0]->pop eq $_[0]->pop ? 1 : 0)) }
sub phi::i::i5 { $_[0]->push(phi::pcons($_[0]->pop, $_[0]->pop)) }
sub phi::i::i6 { my $c = $_[0]->pop; $_[0]->push($c->tail)->push($c->head) }
sub phi::i::i7 { ... }
sub phi::i::i8 { shift->push(phi::pmut) }
sub phi::i::i9 { my $v = $_[0]->pop; $_[0]->peek->set($v); shift }

# TODO
