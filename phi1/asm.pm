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
  sub new
  {
    my ($class, $name) = @_;
    bless { data    => '',
            name    => $name,
            patches => {} }, $class;
  }

  BEGIN
  {
    no strict 'refs';
    for my $b (keys %phi::bytecodes)
    {
      *{"phi::asm::$b"}
        = sub { $_[0]->{data} .= pack C => $phi::bytecodes{$b}; shift };
    }
  }

  sub here_marker { $_[0]->{data} .= pack L => length $_[0]->{data}; shift }
  sub Q           { $_[0]->{data} .= pack "Q>" => $_[1];             shift }
  sub L           { $_[0]->{data} .= pack "L>" => $_[1];             shift }
  sub S           { $_[0]->{data} .= pack "S>" => $_[1];             shift }
  sub C           { $_[0]->{data} .= pack C    => $_[1];             shift }

  sub l
  {
    my ($self, $x) = @_;
      $x & ~0xffffffff ? $self->l64->Q($x)
    : $x & ~0xffff     ? $self->l32->L($x)
    : $x & ~0xff       ? $self->l16->S($x)
                       : $self->l8->C($x);
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
    my $addr = phi::heap_write $$self{data};
    phi::heap_patch $addr + $_, $$self{patches}{$_} for keys %{$$self{patches}};
    if (defined $$self{name})
    {
      phi::heap_label "$$self{name}<" => pack  Q   => $addr;
      phi::heap_label "$$self{name}>" => pack "Q>" => $addr;
    }
    $addr;
  }
}


1;
