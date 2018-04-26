=head1 License
    phi programming language
    Copyright (C) 2018  Spencer Tipping

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
=cut

package phiboot;
use strict;
use warnings;

no warnings 'recursion';

use Exporter qw/import/;
our @EXPORT = qw/pnil pcons pint preal pstr psym pmut list/;

use constant pnil => bless \my $nil_var,   'phiboot::nil';
sub pcons    { bless [$_[0], $_[1]],       'phiboot::cons' }
sub pint($)  { bless \(my $x = 0 + $_[0]), 'phiboot::int' }
sub preal($) { bless \(my $x = 0 + $_[0]), 'phiboot::real' }
sub pstr($)  { bless \(my $x = $_[0]),     'phiboot::str' }
sub psym($)  { bless \(my $x = $_[0]),     'phiboot::sym' }
sub pmut     { bless \(my $x),             'phiboot::mut' }

sub phiboot::nil::type  { phiboot::psym 'nil' }
sub phiboot::cons::type { phiboot::psym 'cons' }
sub phiboot::int::type  { phiboot::psym 'int' }
sub phiboot::real::type { phiboot::psym 'real' }
sub phiboot::str::type  { phiboot::psym 'str' }
sub phiboot::sym::type  { phiboot::psym 'sym' }
sub phiboot::mut::type  { defined ${$_[0]} ? ${$_[0]}->type : phiboot::psym 'mut' }

sub phiboot::nil::is_cons  { 0 }
sub phiboot::cons::is_cons { 1 }
sub phiboot::int::is_cons  { 0 }
sub phiboot::real::is_cons { 0 }
sub phiboot::str::is_cons  { 0 }
sub phiboot::sym::is_cons  { 0 }
sub phiboot::mut::is_cons  { defined ${$_[0]} ? ${$_[0]}->is_cons : 0 }

sub phiboot::nil::is_nil  { 1 }
sub phiboot::cons::is_nil { 0 }
sub phiboot::int::is_nil  { 0 }
sub phiboot::real::is_nil { 0 }
sub phiboot::str::is_nil  { 0 }
sub phiboot::sym::is_nil  { 0 }
sub phiboot::mut::is_nil  { defined ${$_[0]} ? ${$_[0]}->is_nil : 0 }

BEGIN
{
  for my $op (qw/ eval head tail unlist val uncons ival sval yval /)
  {
    no strict 'refs';
    *{"phiboot::mut::$op"} = eval "sub { \${+shift}->$op(\@_) }";
  }
}

sub phiboot::int::ival        { ${+shift} }
sub phiboot::real::rval       { ${+shift} }
sub phiboot::str::sval:lvalue { ${+shift} }
sub phiboot::sym::yval        { ${+shift} }

sub phiboot::int::val        { ${+shift} }
sub phiboot::real::val       { ${+shift} }
sub phiboot::str::val:lvalue { ${+shift} }
sub phiboot::sym::val        { ${+shift} }

sub phiboot::cons::head { shift->[0] }
sub phiboot::cons::tail { shift->[1] }
sub phiboot::cons::uncons { @{+shift} }

sub phiboot::mut::set
{ my ($m, $v) = @_; die "$m already set to $$m" if defined $$m; $$m = $v }

sub phiboot::nthcell
{ my ($v, $i) = @_; $v = $v->tail, --$i while $i; $v }

sub phiboot::nil::unlist { () }
sub phiboot::cons::unlist
{ @_ == 1 || $_[1] ? ($_[0]->[0], $_[0]->[1]->unlist(($_[1] || -1) - 1))
                   : ($_[0]) }

sub list_onto_ { @_ > 1 ? pcons(pop, list_onto_(@_)) : shift }
sub list_onto  { list_onto_ shift, reverse @_ }
sub list       { list_onto pnil, @_ }

sub phiboot::nil::restack { pnil }
sub phiboot::cons::restack
{
  my ($self, $n, @is) = @_;
  phiboot::list_onto phiboot::nthcell($self, $n),
                     map phiboot::nthcell($self, $_)->head, @is;
}

sub phiboot::i::new { bless [pnil, pnil, pnil], shift }

package phiboot::i
{
  use bytes;
  use Scalar::Util qw/refaddr/;

  sub push  { $_[0]->[0] = phiboot::pcons $_[1], $_[0]->[0]; shift }
  sub pop   { my $d = $_[0]->[0]; $_[0]->[0] = $d->tail; $d->head }
  sub peek  { $_[0]->[0]->head }
  sub cpush { $_[0]->[1] = phiboot::pcons $_[1], $_[0]->[1]; shift }
  sub cpop  { my $c = $_[0]->[1]; $_[0]->[1] = $c->tail; $c->head }
  sub quote { phiboot::list @{+shift} }

  our @insns;
  our %extensions;

  $insns[0] = sub  { $_[0]->push($_[0]->quote) };
  $insns[1] = sub  { $_[0]->[1] = $_[0]->pop; shift };
  $insns[2] = sub  { $_[0]->cpush($_[0]->pop) };
  $insns[3] = sub  { $_[0]->push($_[0]->pop->type) };
  $insns[4] = sub  { ... };
  $insns[5] = sub  { $_[0]->push(phiboot::pcons($_[0]->pop, $_[0]->pop)) };
  $insns[6] = sub  { my $c = $_[0]->pop; $_[0]->push($c->tail)->push($c->head) };
  $insns[7] = sub  { my ($n, $l) = $_[0]->pop->uncons;
                     $_[0]->[0] = $_[0]->[0]->restack($n->ival, map $_->ival, $l->unlist);
                     shift };
  $insns[8] = sub  { shift->push(phiboot::pmut) };
  $insns[9] = sub  { my $m = $_[0]->pop; $m->set($_[0]->pop); shift->push($m) };
  $insns[10] = sub { $_[0]->[0] = $_[0]->pop; shift };
  $insns[11] = sub { $_[0]->[2] = $_[0]->pop; shift };
  $insns[12] = sub { my ($else, $then, $cond) =
                        ($_[0]->pop, $_[0]->pop, $_[0]->pop);
                     $_[0]->cpush($cond->ival ? $then : $else) };

  $insns[16] = sub { $_[0]->push(phiboot::pint $_[0]->pop->ival + $_[0]->pop->ival) };
  $insns[17] = sub { $_[0]->push(phiboot::pint -$_[0]->pop->ival) };
  $insns[18] = sub { $_[0]->push(phiboot::pint $_[0]->pop->ival * $_[0]->pop->ival) };
  $insns[19] = sub { ... };
  $insns[20] = sub { $_[0]->push(phiboot::pint $_[0]->pop->ival << $_[0]->pop->ival) };
  $insns[21] = sub { $_[0]->push(phiboot::pint $_[0]->pop->ival >> $_[0]->pop->ival) };
  $insns[22] = sub { $_[0]->push(phiboot::pint($_[0]->pop->ival &  $_[0]->pop->ival)) };
  $insns[23] = sub { $_[0]->push(phiboot::pint($_[0]->pop->ival ^  $_[0]->pop->ival)) };
  $insns[24] = sub { $_[0]->push(phiboot::pint ~$_[0]->pop->ival) };
  $insns[25] = sub { $_[0]->push(phiboot::pint($_[0]->pop->ival < $_[0]->pop->ival)) };
  $insns[26] = sub { $_[0]->push(phiboot::pint(0 + !$_[0]->pop->ival)) };

  $insns[32] = sub { $_[0]->push(phiboot::pstr("\0" x $_[0]->pop->ival)) };
  $insns[33] = sub { $_[0]->push(phiboot::pint length $_[0]->pop->sval) };
  $insns[34] = sub { my $i = $_[0]->pop->ival;
                     $_[0]->push(phiboot::pint ord substr $_[0]->pop->sval, $i, 1) };
  $insns[35] = sub { my $c = chr $_[0]->pop->ival;
                     my $i = $_[0]->pop->ival;
                     substr($_[0]->peek->sval, $i, 1) = $c; shift };
  $insns[36] = sub { $_[0]->push(phiboot::pint($_[0]->pop->sval cmp $_[0]->pop->sval)) };

  $insns[37] = sub { $_[0]->push(phiboot::psym $_[0]->pop->sval) };
  $insns[38] = sub { $_[0]->push(phiboot::pstr $_[0]->pop->yval) };
  $insns[39] = sub { $_[0]->push(phiboot::pint($_[0]->pop->yval eq $_[0]->pop->yval)) };
  $insns[40] = sub { my $i           = shift;
                     my $len         = $i->pop->ival;
                     my $to_offset   = $i->pop->ival;
                     my $to_str      = $i->pop;
                     my $from_offset = $i->pop->ival;
                     my $from_str    = $i->pop;
                     substr($to_str->sval, $to_offset, $len)
                       = substr($from_str->sval, $from_offset, $len);
                     $i->push($to_str) };

  $insns[48] = sub { $_[0]->push(phiboot::preal $_[0]->pop->rval + $_[0]->pop->rval) };
  $insns[49] = sub { $_[0]->push(phiboot::preal(-$_[0]->pop->rval)) };
  $insns[50] = sub { $_[0]->push(phiboot::preal $_[0]->pop->rval * $_[0]->pop->rval) };
  $insns[51] = sub { $_[0]->push(phiboot::preal $_[0]->pop->rval / $_[0]->pop->rval) };
  $insns[52] = sub { $_[0]->push(phiboot::preal $_[0]->pop->rval) };
  $insns[53] = sub { $_[0]->push(phiboot::pint  int $_[0]->pop->rval) };
  $insns[54] = sub { $_[0]->push(phiboot::preal unpack d => $_[0]->pop->sval) };
  $insns[55] = sub { $_[0]->push(phiboot::pstr  pack   d => $_[0]->pop->rval) };
  $insns[56] = sub { $_[0]->push(phiboot::preal log $_[0]->pop->rval) };
  $insns[57] = sub { $_[0]->push(phiboot::pint $_[0]->pop->rval < $_[0]->pop->rval) };
  $insns[58] = sub { $_[0]->push(phiboot::preal sqrt $_[0]->pop->rval) };
  $insns[59] = sub { $_[0]->push(phiboot::preal exp $_[0]->pop->rval) };

  $insns[64] = sub { $_[0]->push(phiboot::pint 0) };
  $insns[65] = sub { die "$_[0] crashed" };
  $insns[66] = sub { $_[0]->push(phiboot::pint($extensions{$_[0]->pop->yval} // 0)) };

  sub i2 { $insns[2]->(shift) }
}

sub phiboot::nil::eval  { $_[1]->push(shift) }
sub phiboot::cons::eval { $_[1]->push(shift) }
sub phiboot::int::eval  { $phiboot::i::insns[$_[0]->ival]->($_[1]) }
sub phiboot::real::eval { $_[1]->push(shift) }
sub phiboot::str::eval  { $_[1]->push(shift) }
sub phiboot::sym::eval  { $_[1]->push($_[0]); $_[1]->cpush(phiboot::pint(2))
                                                   ->cpush($_[1]->[2]) }

package phiboot::i
{
  sub cpack { $_[0] = $_[0]->tail while $_[0]->is_cons && $_[0]->head->is_nil;
              shift }

  sub nexti
  {
    my ($self) = @_;
    my ($h, $t) = cpack($$self[1])->uncons;
    if ($h->is_cons)
    {
      my ($hh, $ht) = $h->uncons;
      $t = phiboot::pcons($ht, $t);
      $h = $hh;
    }
    ($h, cpack $t);
  }

  sub step
  {
    my ($self, $insn, $c) = @_;
    my (@orig) = @$self;
    ($insn, $c) = $self->nexti unless @_ > 1;
    $$self[1] = $c;
    eval { $insn->eval($self) };
    @$self = @orig, die "$@ evaluating $insn on $self" if $@;
    $self;
  }

  sub has_next { cpack(shift->[1])->is_cons }
  sub run      { my ($self) = @_; $self->step while $self->has_next; $self }
}

# Debugging helper code below

use Carp;
use Scalar::Util qw/refaddr/;
our %explanations = (refaddr(pnil) => '[]');
our $explain_indent = 0;

sub phiboot::explain($) { $explanations{refaddr $_[0]} // shift->explain }

BEGIN
{
  $SIG{__WARN__} = $SIG{__DIE__} = sub { Carp::confess @_ };
  eval qq{package phiboot::$_
          {
            use overload qw/ "" explain_self fallback 1 /;
            sub explain_self { phiboot::explain shift }
          }}
    for qw/ i nil cons int str sym mut /;
}

sub phiboot::nil::explain  { '[]' }
sub phiboot::int::explain  { ${+shift} }
sub phiboot::real::explain { ${+shift} }
sub phiboot::str::explain  { (my $s = ${+shift}) =~ s/\n/\\n/g;
                                              $s =~ s/\"/\\"/g; "\"$s\"" }
sub phiboot::sym::explain  { "'${+shift}" }
sub phiboot::mut::explain
{
  defined ${$_[0]} ? refaddr(${$_[0]}) eq refaddr($_[0]) ? 'MUTBOMB' : 'M[...]'
                   : 'M[!!!]';
}

sub phiboot::cons::explain
{
  my $cell = my $self = shift;
  my $use_cons_notation = 0;
  my @elements;
  local $explain_indent = $explain_indent + 2;

  # NB: this is the rare case where we actually want to test perl's ref() rather
  # than asking the value whether it's a cons cell. If we asked the value, we'd
  # lose the visual distinction of mutable values and might loop indefinitely.
  for (; ref($cell) eq 'phiboot::cons'; $cell = $cell->tail)
  {
    push @elements, phiboot::explain $cell->head;

    $use_cons_notation = 1,
    push(@elements, phiboot::explain $cell->tail),
    last
      if $phiboot::explanations{refaddr $cell->tail}
      && !$cell->tail->is_nil;
  }

  if (!$use_cons_notation && !$cell->is_nil)
  {
    $use_cons_notation = 1;
    push @elements, phiboot::explain $cell;
  }

  my $total_length; $total_length += length for @elements;
  if ($total_length > 70)
  {
    my $outer_spaces = " " x ($explain_indent - 2);
    my $inner_spaces = " " x $explain_indent;

    my $opener = $use_cons_notation ? "(" : "[";
    my $join   = $use_cons_notation ? "\n$inner_spaces\:: "
                                    : "\n$inner_spaces";
    my $closer = $use_cons_notation ? ")" : "]";

    "$opener\n$inner_spaces"
      . join($join, @elements)
      . "\n$outer_spaces$closer";
  }
  else
  {
    $use_cons_notation
      ? "(" . join(" :: ", @elements) . ")"
      : "[" . join(" ", @elements) . "]";
  }
}

sub phiboot::i::explain
{
  my ($self) = @_;
  "i[\n"
  . "  d = $$self[0]\n"
  . "  c = $$self[1]\n"
  . "  r = $$self[2]\n"
  . "]";
}

package phiboot::i
{
  our @inames;
  @inames[0x00 .. 0x11] = qw/ i> c< . type == cons uncons restack mut mset d< r< /;
  @inames[0x10 .. 0x1a] = qw| + neg * /% << >> and xor ~ < not |;
  @inames[0x20 .. 0x27] = qw| str slen sget sset scmp strsym symstr sym= |;
  $inames[0x40] = 'version';

  sub trace
  {
    my ($self) = @_;
    while ($self->has_next)
    {
      my ($insn, $c) = $self->nexti;
      print "\n\n\n\n";
      print phiboot::explain $self->[1], "\n\n";
      print phiboot::explain $self->[0], "\n";
      $self->step;
    }
    $self;
  }
}


1;
