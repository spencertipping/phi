=head1 phi syntax nodes
Syntax-level tree structures used to store parsed code and run abstract
evaluation. These data structures are open-ended: you can write new protocols
and extend them to provide new compile-time functionality (support for new
editor integration, for example).
=cut

package phi::node;

use strict;
use warnings;


=head1 Node base
Every syntax node should inherit from this class, which provides low-level
accessors and is an insertion point for API extensions like operator overloads.
=cut

package phi::node::node_base
{
  use overload;

  sub new
  {
    # NB: @xs is the list of parser outputs that went into the mapper function
    # that generated this node. It's a good idea to store it verbatim because
    # we might want to do non-semantic things with this node, like rendering it.
    my ($class, $input, $start, $length, @xs) = @_;
    bless { input  => $input,
            start  => $start,
            length => $length,
            xs     => \@xs }, $class;
  }

  sub x      { ${shift->{xs}}[shift] }
  sub xs     { @{shift->{xs}} }
  sub input  { shift->{input} }
  sub start  { shift->{start} }
  sub length { shift->{length} }
  sub end    { $_[0]->{start} + $_[0]->{length} }
}


=head1 ANSI terminal rendering
A simple renderer using ANSI escape codes to STDOUT.
=cut

package phi::node::render::ansiterminal_base
{
  sub print_at
  {
    my $self = shift;
    my $pos  = shift;
    my ($r, $c) = map $_+1, $self->input->pos_rowcol($pos);
    print "\033[$r;${c}H", @_;
  }

  sub print
  {
    my $self = shift;
    $self->print_at($self->start, @_);
  }

  sub colored
  {
    my ($self, $c) = @_;
    $self->print("\033[${c}m" .
      $self->input->substr($self->start, $self->length) =~ s/\n/\r\n/gr);
  }
}

package phi::node::render::ansiterminal_color
{
  use parent -norequire => 'phi::node::render::ansiterminal_base';

  sub render
  {
    my $self = shift;
    $self->colored($self->color);
  }
}

BEGIN
{
  for my $c (0..7)
  {
    no strict 'refs';
    @{"phi::node::render::ansiterminal_color${c}::ISA"}
      = 'phi::node::render::ansiterminal_color';
    *{"phi::node::render::ansiterminal_color${c}::color"} = sub { "0;3$c" };

    @{"phi::node::render::ansiterminal_colorb${c}::ISA"}
      = 'phi::node::render::ansiterminal_color';
    *{"phi::node::render::ansiterminal_colorb${c}::color"} = sub { "1;3$c" };

    @{"phi::node::render::ansiterminal_colori${c}::ISA"}
      = 'phi::node::render::ansiterminal_color';
    *{"phi::node::render::ansiterminal_colori${c}::color"} = sub { "3;3$c" };
  }
}


1;
