package phibootmacros;
use strict;
use warnings;

use phiboot;
use Scalar::Util qw/looks_like_number refaddr/;
use Exporter qw/import/;
our @EXPORT = (qw/ l le lit dup drop swap rot3l rot3r
                   swons unswons head tail nilp stack dget cget rget if_
                   quote /,
               grep /^i_/ || /^resolver/, keys %{phibootmacros::});

# Allow "use phi" to define something, and set up the explanation for it
BEGIN { ++$INC{'phi.pm'} }

our $phival_id = 0;
sub phi::import
{
  no strict 'refs';
  my (undef, $name, $val) = @_;
  die "use phi: too many args (you need to use l)" if @_ > 3;
  my ($package, $file, $line) = caller;
  my $val2 = $val;
  *{"$package\::$name"} = sub() { $val2 };
  my $explain = "$name:$file:$line:" . ++$phival_id;
  if (ref $val)
  {
    for (my ($i, $v) = (0, $val); ref $v eq 'phiboot::cons'; $i++, $v = $v->tail)
    {
      $phiboot::explanations{refaddr $v} //= "\033[1;34m$explain\[$i\]\033[0;0m";
    }
    $phiboot::explanations{refaddr $val} //= $explain;
  }
}

# Instruction aliases
use phi i_quote   => pint 0x00;
use phi i_cset    => pint 0x01;
use phi i_eval    => pint 0x02;
use phi i_type    => pint 0x03;
use phi i_id      => pint 0x04;
use phi i_cons    => pint 0x05;
use phi i_uncons  => pint 0x06;
use phi i_restack => pint 0x07;
use phi i_mut     => pint 0x08;
use phi i_mset    => pint 0x09;
use phi i_dset    => pint 0x0a;
use phi i_rset    => pint 0x0b;

use phi i_plus    => pint 0x10;
use phi i_neg     => pint 0x11;
use phi i_times   => pint 0x12;
use phi i_divmod  => pint 0x13;
use phi i_lsh     => pint 0x14;
use phi i_rsh     => pint 0x15;
use phi i_and     => pint 0x16;
use phi i_xor     => pint 0x17;
use phi i_inv     => pint 0x18;
use phi i_lt      => pint 0x19;
use phi i_not     => pint 0x1a;

use phi i_str     => pint 0x20;
use phi i_slen    => pint 0x21;
use phi i_sget    => pint 0x22;
use phi i_sset    => pint 0x23;
use phi i_scmp    => pint 0x24;
use phi i_strsym  => pint 0x25;
use phi i_symstr  => pint 0x26;
use phi i_symeq   => pint 0x27;
use phi i_strcat  => pint 0x28;

use phi i_version => pint 0x40;
use phi i_crash   => pint 0x41;

# Debug-print instructions
sub phiboot::nil::len  { 0 }
sub phiboot::cons::len { 1 + shift->tail->len }

use phi i_write    => pint 0x100;
use phi i_print    => pint 0x101;
use phi i_readline => pint 0x102;
use phi i_printall => pint 0x103;
sub phiboot::i::i256 { print $_[0]->pop->val; $_[0] }
sub phiboot::i::i257 { print phiboot::explain($_[0]->pop), "\n"; $_[0] }
sub phiboot::i::i258 { my $line = <STDIN>; $_[0]->push(defined $line ? pstr$line : pnil) }
sub phiboot::i::i259 { print $_[0]->[0]->len . ": " . phiboot::explain($_[0]->[0]), "\n"; $_[0] }

sub l { list map ref ? $_ : looks_like_number $_ ? pint $_ : psym $_, @_ }

# Compile-time macros
sub lit($)  { (l(shift), i_uncons, l(2, 0), i_uncons, i_restack) }
sub dup()   { (l(0, 0),       i_uncons, i_restack) }
sub drop()  { (l(1),          i_uncons, i_restack) }
sub swap()  { (l(2, 1, 0),    i_uncons, i_restack) }
sub rot3l() { (l(3, 2, 0, 1), i_uncons, i_restack) }
sub rot3r() { (l(3, 1, 2, 0), i_uncons, i_restack) }

sub swons()   { (swap, i_cons) }
sub unswons() { (i_uncons, swap) }
sub head()    { (i_uncons, stack(2, 0)) }
sub tail()    { (i_uncons, drop) }

sub nilp()    { (i_type, lit psym 'nil', i_symeq) }

sub stack     { (l(@_), i_uncons, i_restack) }

sub dget()  { (i_quote, head) }
sub cget()  { (i_quote, tail, head) }
sub rget()  { (i_quote, tail, tail, head) }

sub if_()   { (rot3l, i_not, i_not, pnil, swap, i_cons, lit 2, i_restack, i_eval) }

sub le { phiboot::i->new->push(l(l(lit no_resolver_configured => i_crash),
                                   i_rset, @_))->i2->run->pop }

# Resolver boot
use phi resolvercode_mut => pmut;
use phi resolvercode => l               # sym [[s1 ...] [s2 ...] ...]
  dup, nilp,                            # sym bs ?
  l(drop,                               # sym
    lit failed_to_resolve => i_crash),
  l(i_uncons, i_uncons,                 # sym bs' bt bsym
    stack(3, 3, 0, 1, 2),               # sym bs' bt bsym sym
    i_symeq,                            # sym bs' bt ?
    l(stack(3, 0)),                     # bt
    pcons(l(drop),                      # [drop . resolver...] .
          pcons(i_eval,
                resolvercode_mut)),
    if_),
  if_;

resolvercode_mut->set(resolvercode);

sub resolver
{
  my $l = pnil;
  while (@_)
  {
    my ($k, $v) = (shift, shift);
    $l = pcons pcons(psym $k, $v), $l;
  }
  pcons $l, resolvercode;
}


=head3 C<quote>
We need a way to force phi to quote stuff that might not be self-quoting. For
example, if we want to bind C<x> to C<5>, we can't just put C<[x 5]> into the
resolver because phi would run C<5> as an instruction (cons).

So instead, we do what C<lit> does and put the value into a list. Here's the
equation:

  quote(v) = [[v] head]

Concatenative:

  v     [] swons [head] swons     = [[v] head]

=cut

use phi quote => l pnil, swons, l(head), swons;


1;
