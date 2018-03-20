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
  for my $op (qw/ eval head tail unlist val uncons nthcell /)
  {
    no strict 'refs';
    *{"phiboot::mut::$op"} = sub { ${+shift}->$op(@_) };
  }
}

sub phiboot::int::val { ${+shift} }
sub phiboot::str::val { ${+shift} }
sub phiboot::sym::val { ${+shift} }

sub phiboot::cons::head { shift->[0] }
sub phiboot::cons::tail { shift->[1] }
sub phiboot::cons::uncons { @{+shift} }

sub phiboot::mut::set
{ my ($m, $v) = @_; die "$m already set to $$m" if defined $$m; $$m = $v }

sub phiboot::nil::nthcell { shift }
sub phiboot::cons::nthcell { $_[1] ? $_[0]->tail->nthcell($_[1] - 1) : shift }

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
  phiboot::list_onto $self->nthcell($n), map $self->nthcell($_)->head, @is;
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

  sub i0  { $_[0]->push($_[0]->quote) }
  sub i1  { $_[0]->[1] = $_[0]->pop; shift }
  sub i2  { $_[0]->cpush($_[0]->pop) }
  sub i3  { $_[0]->push($_[0]->pop->type) }
  sub i4  { ... }
  sub i5  { $_[0]->push(phiboot::pcons($_[0]->pop, $_[0]->pop)) }
  sub i6  { my $c = $_[0]->pop; $_[0]->push($c->tail)->push($c->head) }
  sub i7  { my ($n, $l, $d) = $_[0]->[0]->unlist(2);
            $_[0]->[0] = $d->restack($n->val, map $_->val, $l->unlist);
            shift }
  sub i8  { shift->push(phiboot::pmut) }
  sub i9  { my $v = $_[0]->pop; $_[0]->peek->set($v); shift }
  sub i10 { $_[0]->[0] = $_[0]->pop; shift }
  sub i11 { $_[0]->[2] = $_[0]->pop; shift }

  sub i16 { $_[0]->push(phiboot::pint $_[0]->pop->val + $_[0]->pop->val) }
  sub i17 { $_[0]->push(phiboot::pint -$_[0]->pop->val) }
  sub i18 { $_[0]->push(phiboot::pint $_[0]->pop->val * $_[0]->pop->val) }
  sub i19 { ... }
  sub i20 { $_[0]->push(phiboot::pint $_[0]->pop->val << $_[0]->pop->val) }
  sub i21 { $_[0]->push(phiboot::pint $_[0]->pop->val >> $_[0]->pop->val) }
  sub i22 { $_[0]->push(phiboot::pint($_[0]->pop->val &  $_[0]->pop->val)) }
  sub i23 { $_[0]->push(phiboot::pint($_[0]->pop->val ^  $_[0]->pop->val)) }
  sub i24 { $_[0]->push(phiboot::pint ~$_[0]->pop->val) }
  sub i25 { $_[0]->push(phiboot::pint($_[0]->pop->val < $_[0]->pop->val)) }
  sub i26 { $_[0]->push(phiboot::pint(0 + !$_[0]->pop->val)) }

  sub i32 { $_[0]->push(phiboot::pstr("\0" x $_[0]->pop->val)) }
  sub i33 { $_[0]->push(phiboot::pint length $_[0]->pop->val) }
  sub i34 { $_[0]->push(phiboot::pint ord substr $_[0]->pop->val,
                                                 $_[0]->pop->val, 1) }
  sub i35 { my $c = chr $_[0]->pop->val;
            my $i = $_[0]->pop->val;
            substr(${$_[0]->peek}, $i, 1) = $c; shift }
  sub i36 { $_[0]->push(phiboot::pint($_[0]->pop->val cmp $_[0]->pop->val)) }

  sub i37 { $_[0]->push(phiboot::psym $_[0]->pop->val) }
  sub i38 { $_[0]->push(phiboot::pstr $_[0]->pop->val) }
  sub i39 { $_[0]->push(phiboot::pint($_[0]->pop->val eq $_[0]->pop->val)) }
  sub i40 { $_[0]->push(phiboot::pstr($_[0]->pop->val . $_[0]->pop->val)) }

  sub i48 { $_[0]->push(phiboot::preal $_[0]->pop->val + $_[0]->pop->val) }
  sub i49 { $_[0]->push(phiboot::preal(-$_[0]->pop->val)) }
  sub i50 { $_[0]->push(phiboot::preal $_[0]->pop->val * $_[0]->pop->val) }
  sub i51 { $_[0]->push(phiboot::preal $_[0]->pop->val / $_[0]->pop->val) }
  sub i52 { $_[0]->push(phiboot::preal $_[0]->pop->val) }
  sub i53 { $_[0]->push(phiboot::pint int $_[0]->pop->val) }
  sub i54 { $_[0]->push(phiboot::preal unpack d => $_[0]->pop->val) }
  sub i55 { $_[0]->push(phiboot::pstr  pack   d => $_[0]->pop->val) }
  sub i56 { $_[0]->push(phiboot::preal log $_[0]->pop->val) }
  sub i57 { $_[0]->push(phiboot::pint $_[0]->pop->val < $_[0]->pop->val) }
  sub i58 { $_[0]->push(phiboot::preal sqrt $_[0]->pop->val) }
  sub i59 { $_[0]->push(phiboot::preal exp $_[0]->pop->val) }

  sub i64 { $_[0]->push(phiboot::pint 0) }
  sub i65 { die "$_[0] crashed" }
}

sub phiboot::nil::eval  { $_[1]->push(shift) }
sub phiboot::cons::eval { $_[1]->push(shift) }
sub phiboot::int::eval  { my $mname = "i" . $_[0]->val; $_[1]->$mname }
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
sub phiboot::mut::explain  { defined ${$_[0]} ? 'M[...]' : 'M[]' }

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

  $use_cons_notation ||= !$cell->is_nil;

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
