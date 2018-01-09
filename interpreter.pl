#!/usr/bin/env perl
use strict;
use warnings;

package phi;

use List::Util;
use Time::HiRes qw/time/;

BEGIN
{
  eval qq{package phi::$_ { use overload qw/ "" explain / }}
    for qw/ i nil cons int str sym mut /;
}

# Value types
use constant pnil => bless \my $nil_var, 'phi::nil';
sub pcons   { bless [$_[0], $_[1]],       'phi::cons' }
sub pint($) { bless \(my $x = 0 + $_[0]), 'phi::int' }
sub pstr($) { bless \(my $x = $_[0]),     'phi::str' }
sub psym($) { bless \(my $x = $_[0]),     'phi::sym' }
sub pmut    { bless \(my $x),             'phi::mut' }

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
sub phi::cons::uncons { @{+shift} }

sub phi::nil::nthcell { shift }
sub phi::cons::nthcell { $_[1] ? $_[0]->tail->nthcell($_[1] - 1) : shift }

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

sub phi::i::new { bless [pnil, pnil, pnil], shift }
sub phi::i::explain { shift->quote->explain }

# Interpreter cases
# The interpreter is represented as a Perl array [d, c, r], and modified
# in-place.

package phi::i
{
  our @inames;
  @inames[0x00 .. 0x09] = qw/ i> i< . type == cons uncons restack mut mset /;
  @inames[0x10 .. 0x1a] = qw| + neg * /% << >> and xor ~ < not |;
  @inames[0x20 .. 0x27] = qw| str slen sget sset scmp strsym symstr sym= |;
  $inames[0x40] = 'version';

  sub push  { $_[0]->[0] = phi::pcons $_[1], $_[0]->[0]; shift }
  sub pop   { my $d = $_[0]->[0]; $_[0]->[0] = $d->tail; $d->head }
  sub peek  { $_[0]->[0]->head }
  sub cpush { $_[0]->[1] = phi::pcons $_[1], $_[0]->[1]; shift }
  sub cpop  { my $c = $_[0]->[1]; $_[0]->[1] = $c->tail; $c->head }
  sub quote { phi::list @{+shift} }

  sub i0  { $_[0]->push($_[0]->quote) }
  sub i1  { @{$_[0]} = $_[0]->pop->unlist }
  sub i2  { $_[0]->cpush($_[0]->pop) }
  sub i3  { $_[0]->push(phi::psym(ref($_[0]->pop) =~ s/^phi:://r)) }
  sub i4  { $_[0]->push(phi::pint($_[0]->pop eq $_[0]->pop ? 1 : 0)) }
  sub i5  { $_[0]->push(phi::pcons($_[0]->pop, $_[0]->pop)) }
  sub i6  { my $c = $_[0]->pop; $_[0]->push($c->tail)->push($c->head) }
  sub i7  { my ($n, $l, $d) = $_[0]->[0]->unlist(2);
            $_[0]->[0] = $d->restack($n->val, map $_->val, $l->unlist);
            shift }
  sub i8  { shift->push(phi::pmut) }
  sub i9  { my $v = $_[0]->pop; $_[0]->peek->set($v); shift }

  sub i16 { $_[0]->push(phi::pint $_[0]->pop->val + $_[0]->pop->val) }
  sub i17 { $_[0]->push(phi::pint -$_[0]->pop->val) }
  sub i18 { $_[0]->push(phi::pint $_[0]->pop->val * $_[0]->pop->val) }
  sub i19 { ... }
  sub i20 { $_[0]->push(phi::pint $_[0]->pop->val << $_[0]->pop->val) }
  sub i21 { $_[0]->push(phi::pint $_[0]->pop->val >> $_[0]->pop->val) }
  sub i22 { $_[0]->push(phi::pint($_[0]->pop->val &  $_[0]->pop->val)) }
  sub i23 { $_[0]->push(phi::pint($_[0]->pop->val ^  $_[0]->pop->val)) }
  sub i24 { $_[0]->push(phi::pint ~$_[0]->pop->val) }
  sub i25 { $_[0]->push(phi::pint($_[0]->pop->val < $_[0]->pop->val)) }
  sub i26 { $_[0]->push(phi::pint(0 + !$_[0]->pop->val)) }

  sub i32 { $_[0]->push(phi::pstr("\0" x $_[0]->pop->val)) }
  sub i33 { $_[0]->push(phi::pint length $_[0]->pop->val) }
  sub i34 { $_[0]->push(phi::pint ord substr $_[0]->pop->val,
                                             $_[0]->pop->val, 1) }
  sub i35 { my $c = chr $_[0]->pop->val;
            my $i = $_[0]->pop->val;
            substr(${$_[0]->peek}, $i, 1) = $c; shift }
  sub i36 { $_[0]->push(phi::pint($_[0]->pop->val cmp $_[0]->pop->val)) }

  sub i37 { $_[0]->push(phi::psym $_[0]->pop->val) }
  sub i38 { $_[0]->push(phi::pstr $_[0]->pop->val) }
  sub i39 { $_[0]->push(phi::pint($_[0]->pop->val eq $_[0]->pop->val)) }

  sub i64 { $_[0]->push(phi::pint 0) }
}

# Execution mechanics

sub phi::nil::eval  { $_[1]->push(shift) }
sub phi::cons::eval { $_[1]->push(shift) }
sub phi::int::eval  { my $mname = "i" . $_[0]->val; $_[1]->$mname }
sub phi::str::eval  { $_[1]->push(shift) }
sub phi::sym::eval  { $_[1]->push($_[0]); $_[1]->cpush(phi::pint(2))
                                               ->cpush($_[1]->[2]) }

package phi::i
{
  our @inames;

  sub nexti
  {
    my ($self) = @_;
    my ($h, $t) = $$self[1]->uncons;
    if (CORE::ref $h eq 'phi::cons')
    {
      my ($hh, $ht) = $h->uncons;
      $t = phi::pcons($ht, $t);
      $h = $hh;
    }
    $t = $t->tail while CORE::ref $t       eq 'phi::cons'
                     && CORE::ref $t->head eq 'phi::nil';
    ($h, $t);
  }

  sub step
  {
    my ($self, $insn, $c) = @_;
    ($insn, $c) = $self->nexti unless @_ > 1;
    $$self[1] = $c;
    eval { $insn->eval($self) };
    if ($@)
    {
      my $error = $@;
      my $iname = $inames[$insn->val];
      die "$@ evaluating $insn [$iname] on $self";
    }
    $self;
  }

  sub has_next { CORE::ref shift->[1] eq 'phi::cons' }
  sub run      { my ($self) = @_; $self->step while $self->has_next; $self }
  sub trace
  {
    my ($self) = @_;
    while ($self->has_next)
    {
      my ($insn, $c) = $self->nexti;
      my $iname = $insn->can('val') ? $inames[$insn->val] : '?';
      print "$insn [$iname] : ";
      print $self->step->explain, "\n";
    }
    $self;
  }
}

sub l      { list map ref ? $_ : pint $_, @_ }
sub lit($) { (l(shift), 0x06, l(2, 0), 0x06, 0x07) }

# Test code
phi::i->new->push(l lit(1), lit(2), 0x10, 0x00, 0x03, 0x26, 0x21, 0x12)
           ->i2
           ->trace;

phi::i->new->push(pint 1)
           ->push(pint 2)
           ->push(pint 3)
           ->push(l lit(4))
           ->i2
           ->trace;

phi::i->new->push(l lit(4), 0x20,
                    lit(0), lit(65), 0x23,
                    lit(1), lit(66), 0x23,
                    lit(2), lit(67), 0x23,
                    lit(3), lit(33), 0x23)
           ->i2
           ->trace;
