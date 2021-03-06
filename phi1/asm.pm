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

package phi;

use strict;
use warnings;
use bytes;


=head1 Assembler
Code to generate phi function objects and handle local linkages. Also manages
C<call> return continuation here-markers, and here markers in general.
=cut

package phi::asm
{
  our $id = 0;

  sub new
  {
    my ($class, $name) = @_;
    bless { data    => '',
            addr    => undef,
            deps    => [],
            heres   => [],
            name    => $name // "anon" . ++$phi::asm::id,
            patches => {} }, $class;
  }

  sub data { shift->{data} }
  sub len  { length shift->data }

  sub data_shifted_by
  {
    my ($self, $delta) = @_;
    my $d = $self->data;
    substr($d, $_, 4) = pack L => $delta + unpack L => substr($d, $_, 4)
      for @{$$self{heres}};
    $d;
  }

  BEGIN
  {
    no strict 'refs';
    for my $b (grep !/^call$/ && !/^code$/, keys %phi::bytecodes)
    {
      *{"phi::asm::$b"} = sub { shift->C($phi::bytecodes{$b}) };
    }
  }

  sub dup { shift->sget->C(0) }

  sub call { shift->C($phi::bytecodes{call})->here }
  sub code
  {
    my ($self, $code) = @_;
    $self->C($phi::bytecodes{code})
         ->Lb($code->len)
         ->here;
    $self->str($code->data_shifted_by($self->len));

    # Inherit dependencies, here markers, and patches from the code being
    # inlined.
    push @{$$self{deps}},  @{$$code{deps}};
    push @{$$self{heres}}, map $_ + $self->len, @{$$code{heres}};
    $$self{patches}{$_ + $self->len} = $$code{patches}{$_}
      for keys %{$$code{patches}};

    $self;
  }

  sub str { $_[0]->{data} .= $_[1]; shift }
  sub Qb  { shift->str(pack "Q>" => shift) }
  sub Lb  { shift->str(pack "L>" => shift) }
  sub Sb  { shift->str(pack "S>" => shift) }
  sub Ql  { shift->str(pack "Q<" => shift) }
  sub Ll  { shift->str(pack "L<" => shift) }
  sub Sl  { shift->str(pack "S<" => shift) }
  sub C   { shift->str(pack C    => shift) }

  sub here
  {
    my ($self) = @_;
    push @{$$self{heres}}, $self->len;
    $self->Ll($self->len + 4);
  }

  sub l
  {
    my ($self, $x) = @_;
    if (ref $x)
    {
      $self->l64->patch("$$x{name}:be", 8);
      push @{$$self{deps}}, $x;
    }
    else
    {
        $x & ~0xffffffff ? $self->l64->Qb($x)
      : $x & ~0xffff     ? $self->l32->Lb($x)
      : $x & ~0xff       ? $self->l16->Sb($x)
                         : $self->l8->C($x);
    }
    $self;
  }

  sub patch
  {
    my ($self, $label, $bytes) = @_;
    $$self{patches}{length $$self{data}} = $label;
    $$self{data} .= "\0" x $bytes;
    $self;
  }

  sub addr
  {
    my $self = shift;
    return $$self{addr} if defined $$self{addr};
    my $addr = $$self{addr} = phi::heap_write $$self{data};
    phi::heap_patch $addr + $_, $$self{patches}{$_} for keys %{$$self{patches}};
    phi::heap_label "$$self{name}:be" => pack "Q>" => $addr;
    phi::heap_label "$$self{name}:le" => pack "Q<" => $addr;
    $_->addr for @{$$self{deps}};
    $addr;
  }

  sub debug_print
  {
    my ($self, $message) = @_;
    $message .= "\n";
    $self->l(0)->l(0)->l(0)
         ->l(length $message)
         ->l(phi::asm->new->str($message)->addr)
         ->l(2)
         ->l(1)->l(phi::asm->new->str($phi::syscall_native)->addr)
         ->back->drop;
  }

  sub debug_crash { shift->l(0xb00f)->g64 }
}


1;
