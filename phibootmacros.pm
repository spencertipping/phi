package phibootmacros;
use strict;
use warnings;

use phiboot;
use Exporter qw/import/;
our @EXPORT = (qw/l lit dup drop swap rot3l rot3r if_/,
               grep /^i_/ || /^resolver/, keys %{phibootmacros::});

# Instruction aliases
use constant {
  i_quote   => 0x00,
  i_cset    => 0x01,
  i_eval    => 0x02,
  i_type    => 0x03,
  i_eq      => 0x04,
  i_cons    => 0x05,
  i_uncons  => 0x06,
  i_restack => 0x07,
  i_mut     => 0x08,
  i_mset    => 0x09,
  i_dset    => 0x0a,
  i_rset    => 0x0b,

  i_plus    => 0x10,
  i_neg     => 0x11,
  i_times   => 0x12,
  i_divmod  => 0x13,
  i_lsh     => 0x14,
  i_rsh     => 0x15,
  i_and     => 0x16,
  i_xor     => 0x17,
  i_inv     => 0x18,
  i_lt      => 0x19,
  i_not     => 0x1a,

  i_str     => 0x20,
  i_slen    => 0x21,
  i_sget    => 0x22,
  i_sset    => 0x23,
  i_scmp    => 0x24,
  i_strsym  => 0x25,
  i_symstr  => 0x26,
  i_symeq   => 0x27,

  i_version => 0x40,
};

sub l { list map ref ? $_ : pint $_, @_ }

# Compile-time macros
sub lit($)  { (l(shift), i_uncons, l(2, 0), i_uncons, i_restack) }
sub dup()   { (l(0, 0),       i_uncons, i_restack) }
sub drop()  { (l(1),          i_uncons, i_restack) }
sub swap()  { (l(2, 1, 0),    i_uncons, i_restack) }
sub rot3l() { (l(3, 2, 0, 1), i_uncons, i_restack) }
sub rot3r() { (l(3, 1, 2, 0), i_uncons, i_restack) }

sub if_()   { (rot3l, i_not, i_not, pnil, swap, i_cons, lit 2, i_restack, i_eval) }

# Resolver boot
use constant resolvercode_mut => pmut;
use constant resolvercode => l
  dup, i_type, lit psym 'nil', i_symeq,
    l(drop),
    l(i_uncons, i_uncons, l(3, 3, 0, 1, 2), i_uncons, i_restack, i_symeq,
      l(l(3, 0), i_uncons, i_restack),
      pcons(l(drop), pcons(pint i_eval, resolvercode_mut)),
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

1;
