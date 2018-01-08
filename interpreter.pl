#!/usr/bin/env perl
use strict;
use warnings;

package phi;

use List::Util;

# Value types
use constant pnil => bless \my $nil_var, 'phi::nil';
sub pcons { bless [$_[0], $_[1]],       'phi::cons' }
sub pint  { bless \(my $x = 0 + $_[0]), 'phi::int' }
sub pstr  { bless \(my $x = $_[0]),     'phi::str' }
sub psym  { bless \(my $x = $_[0]),     'phi::sym' }
sub pmut  { bless \(my $x),             'phi::mut' }

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
  for my $op (qw/ head tail unlist val /)
  {
    no strict 'refs';
    *{"phi::mut::$op"} = sub { ${+shift}->$op(@_) };
  }
}

sub phi::int::val { ${+shift} }
sub phi::str::val { ${+shift} }
sub phi::sym::val { ${+shift} }

sub phi::cons::head { shift->[0] }
sub phi::cons::tail { shift->[1] }

sub phi::cons::nthcell { $_[1] ? shift->tail->nthcell(shift - 1) : shift }

sub phi::nil::unlist { () }
sub phi::cons::unlist
{ @_ == 1 || $_[1] ? ($_[0]->[0], $_[0]->[1]->unlist(($_[1] || -1) - 1))
                   : ($_[0]) }

sub list_onto_ { @_ > 1 ? pcons(pop, list_onto_(@_)) : shift }
sub list_onto  { list_onto_ shift, reverse @_ }
sub list       { list_onto pnil, @_ }

sub phi::nil::restack { pnil }
sub phi::cons::restack
{
  my ($self, $n, @is) = @_;
  phi::list_onto $self->nthcell($n), map $self->nthcell($_)->head, @is;
}

# Interpreter cases
# The interpreter is represented as a Perl array [d, c, r], and modified
# in-place.

sub phi::i::push  { $_[0]->[0] = phi::pcons $_[1], $_[0]->[0]; shift }
sub phi::i::pop   { my $d = $_[0]->[0]; $_[0]->[0] = $d->tail; $d->head }
sub phi::i::peek  { $_[0]->[0]->head }
sub phi::i::cpush { $_[0]->[1] = phi::pcons $_[1], $_[0]->[1]; shift }
sub phi::i::cpop  { my $c = $_[0]->[1]; $_[0]->[1] = $c->tail; $c->head }
sub phi::i::quote { phi::list @{+shift} }

sub phi::i::i0  { $_[0]->push($_[0]->quote) }
sub phi::i::i1  { @{$_[0]} = $_[0]->pop->unlist }
sub phi::i::i2  { $_[0]->cpush($_[0]->pop) }
sub phi::i::i3  { $_[0]->push(phi::psym(ref $_[0]->pop =~ s/^phi:://r)) }
sub phi::i::i4  { $_[0]->push(phi::pint($_[0]->pop eq $_[0]->pop ? 1 : 0)) }
sub phi::i::i5  { $_[0]->push(phi::pcons($_[0]->pop, $_[0]->pop)) }
sub phi::i::i6  { my $c = $_[0]->pop; $_[0]->push($c->tail)->push($c->head) }
sub phi::i::i7  { my ($n, $l, $d) = $_[0]->[0]->unlist(2);
                  $_[0]->[0] = $d->restack($n->val, $l->unlist); shift }
sub phi::i::i8  { shift->push(phi::pmut) }
sub phi::i::i9  { my $v = $_[0]->pop; $_[0]->peek->set($v); shift }

sub phi::i::i16 { $_[0]->push(phi::pint $_[0]->pop->val + $_[0]->pop->val) }
sub phi::i::i17 { $_[0]->push(phi::pint -$_[0]->pop->val) }
sub phi::i::i18 { $_[0]->push(phi::pint $_[0]->pop->val * $_[0]->pop->val) }
sub phi::i::i19 { ... }
sub phi::i::i20 { $_[0]->push(phi::pint $_[0]->pop->val << $_[0]->pop->val) }
sub phi::i::i21 { $_[0]->push(phi::pint $_[0]->pop->val >> $_[0]->pop->val) }
sub phi::i::i22 { $_[0]->push(phi::pint($_[0]->pop->val &  $_[0]->pop->val)) }
sub phi::i::i23 { $_[0]->push(phi::pint($_[0]->pop->val ^  $_[0]->pop->val)) }
sub phi::i::i24 { $_[0]->push(phi::pint ~$_[0]->pop->val) }
sub phi::i::i25 { $_[0]->push(phi::pint($_[0]->pop->val < $_[0]->pop->val)) }
sub phi::i::i26 { $_[0]->push(phi::pint(0 + !$_[0]->pop->val)) }

sub phi::i::i32 { $_[0]->push(phi::pstr("\0" x $_[0]->pop->val)) }
sub phi::i::i33 { $_[0]->push(phi::pint length $_[0]->pop->val) }
sub phi::i::i34 { $_[0]->push(phi::pint ord substr $_[0]->pop->val,
                                                   $_[0]->pop->val, 1) }
sub phi::i::i35 { my $c = chr $_[0]->pop->val;
                  my $i = $_[0]->pop->val;
                  substr(${$_[0]->peek}, $i, 1) = $c; shift }
sub phi::i::i36 { $_[0]->push(phi::pint($_[0]->pop->val cmp $_[0]->pop->val)) }

sub phi::i::i37 { $_[0]->push(phi::psym $_[0]->pop->val) }
sub phi::i::i38 { $_[0]->push(phi::pstr $_[0]->pop->val) }
sub phi::i::i39 { $_[0]->push(phi::pint($_[0]->pop->val eq $_[0]->pop->val)) }

sub phi::i::i64 { $_[0]->push(phi::pint 0) }
