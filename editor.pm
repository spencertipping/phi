=head1 Editable buffer
Not much going on here, just some coordinate mapping between (row, col) and
linear string. Buffers don't have cursors; those are defined separately. You
can have multiple cursors per buffer if you want to.

Buffers address lines and columns as zero-based values. Line
lengths/end-positions also include the trailing newline byte; this means the
sum of line lengths will be the size of the file you're editing.
=cut

use strict;
use warnings;

package phi::editor::buffer
{
  use Scalar::Util;

  sub new
  {
    my $class  = shift;
    my @lines  = map /.*\n?/g, @_;
    my $offset = 0;
    /\n$/ or $_ .= "\n" for @lines;
    (bless { lines     => \@lines,
             listeners => [],
             ends      => undef }, $class)
      ->build_offsets;
  }

  sub build_offsets
  {
    my ($self) = @_;
    my $offset = 0;
    $$self{ends} = [map $offset += CORE::length, @{$$self{lines}}];
    $self;
  }

  sub pos_rowcol
  {
    my ($self, $pos) = @_;
    my ($lower, $upper) = (0, scalar @{$$self{lines}});
    while ($lower + 1 < $upper)
    {
      my $mid = $lower + $upper >> 1;
      ($pos < $self->line_start($mid) ? $upper : $lower) = $mid;
    }
    ($lower, $pos - $self->line_start($lower));
  }

  sub rowcol_pos
  {
    my ($self, $row, $col) = @_;
    $self->line_start($row) + $col;
  }

  sub line_start { my ($self, $row) = @_; $row ? $$self{ends}->[$row - 1] : 0 }
  sub line_end   { shift->{ends}->[shift] }
  sub line       { shift->{lines}->[shift] }
  sub lines      { @{shift->{lines}} }

  sub line_inner_size { shift->line_outer_size(shift) - 1 }
  sub line_outer_size
  {
    my ($self, $row) = @_;
    $self->line_end($row) - $self->line_start($row);
  }


  sub length { shift->{ends}->[-1] + 1 }
  sub substr
  {
    my ($self, $start, $length) = @_;
    my ($r1, $c1) = $self->pos_rowcol($start);
    my ($r2, $c2) = $self->pos_rowcol($start + $length);

    # Simple case: everything's on a single line, so run a single substring
    # operation.
    return CORE::substr $$self{lines}->[$r1], $c1, $c2 - $c1 if $r1 == $r2;

    # We're spanning lines, so construct the requested string by joining.
    join '', CORE::substr($$self{lines}->[$r1], $c1),
             @{$$self{lines}}[$r1+1..$r2-1],
             CORE::substr($$self{lines}->[$r2], 0, $c2);
  }


  sub insert
  {
    my ($self, $pos, $text) = @_;
    my ($row, $col) = $self->pos_rowcol($pos);

    if (-1 < index $text, "\n")
    {
      my $lines = my ($first, @rest) = $text =~ /.*\n?/g;
      my $last  = pop @rest;

      $first = CORE::substr($$self{lines}->[$row], 0, $col) . $first;
      $last  = $last . CORE::substr($$self{lines}->[$row], $col);

      @{$$self{lines}} = (@{$$self{lines}}[0..$row-1],
                          $first, @rest, $last,
                          @{$$self{lines}}[$row+1..$#{$$self{lines}}]);
    }
    else
    {
      $$self{lines}->[$row] = CORE::substr($$self{lines}->[$row], 0, $col)
                            . $text
                            . CORE::substr($$self{lines}->[$row], $col);
    }

    $self->build_offsets;
    $self->cursor_delta($pos, CORE::length $text);
  }

  sub delete
  {
    my ($self, $start, $length) = @_;
    my ($row1, $col1) = $self->pos_rowcol($start);
    my ($row2, $col2) = $self->pos_rowcol($start + $length);

    if ($row1 == $row2)
    {
      $$self{lines}->[$row1]
        = CORE::substr($$self{lines}->[$row1], 0, $col1)
          . CORE::substr($$self{lines}->[$row1], $col2);
    }
    else
    {
      $$self{lines}->[$row1]
        = CORE::substr($$self{lines}->[$row1], 0, $col1)
          . CORE::substr($$self{lines}->[$row2], $col2);
      @{$$self{lines}} = @{$$self{lines}}[0..$row1, $row2+1..$#{$$self{lines}}];
    }

    $self->build_offsets;
    $self->cursor_delta($start, -$length);
  }


=head2 Cursor interfacing
Editor buffers propagate editing events to all cursors acting on the buffer.
This makes it possible for you to edit with multiple cursors, and for cursors
to behave in sane ways if you're making programmatic edits.
=cut

  sub add_listener
  {
    my $self = shift;
    push @{$$self{listeners}}, @_;
    Scalar::Util::weaken $_ for @{$$self{listeners}};
    $self;
  }

  sub remove_listener
  {
    my $self      = shift;
    my %to_remove = map +(Scalar::Util::refaddr($_) => 1), @_;
    @{$$self{listeners}}
      = grep !$to_remove{Scalar::Util::refaddr $_}, @{$$self{listeners}};
    Scalar::Util::weaken $_ for @{$$self{listeners}};
    $self;
  }

  sub cursor_delta
  {
    my ($self, $pos, $delta) = @_;
    defined and $_->delta($pos, $delta) for @{$$self{listeners}};
    $self;
  }
}


=head2 Editor cursor
Tracks document position, selected range, etc, and maintains this across
external edits made to the document.
=cut

package phi::editor::cursor
{
  use List::Util;

  sub new
  {
    my ($class, $buffer) = @_;
    my $self = bless { buffer    => $buffer,
                       pos       => 0,
                       selection => undef }, $class;
    $buffer->add_listener($self);
    $self;
  }

  sub DESTROY
  {
    my ($self) = @_;
    $self->buffer->remove_listener($self);
  }

  sub buffer { shift->{buffer} }
  sub pos
  {
    my ($self, $p) = @_;
    if (@_ > 1)
    {
      $$self{pos} = List::Util::max 0,
                    List::Util::min $self->buffer->length, $p;
      $self;
    }
    else
    {
      $$self{pos};
    }
  }

  sub dpos
  {
    my ($self, $delta) = @_;
    $self->pos($self->pos + $delta);
  }


  sub delta
  {
    my ($self, $pos, $delta) = @_;
    my $relative = $$self{pos} - $pos;
    if ($relative >= 0)
    {
      $self->dpos($delta)                              if $delta > 0;
      $self->dpos(-List::Util::min $relative, -$delta) if $delta < 0;
    }
    $$self{selection}->delta($pos, $delta) if defined $$self{selection};
    $self;
  }

  sub insert
  {
    my ($self, $text) = @_;
    $self->buffer->insert($self->pos, $text);
    $self;
  }

  sub backspace
  {
    my ($self, $chars) = (@_, 1);
    $self->buffer->delete($self->pos - $chars, $chars) if $self->pos;
    $self;
  }

  sub delete
  {
    my ($self, $chars) = (@_, 1);
    $self->buffer->delete($self->pos, $chars);
    $self;
  }

  sub col
  {
    my ($self, $ncol) = @_;
    my ($row, $col) = $self->rowcol;
    return $col if @_ == 1;
    $self->rowcol($row, $ncol < 0
      ? $self->buffer->line_outer_size($row) + $ncol
      : $ncol);
    $self;
  }

  sub rowcol
  {
    my $self = shift;
    @_
      ? $self->pos($self->buffer->rowcol_pos(@_))
      : $self->buffer->pos_rowcol($$self{pos});
  }

  sub move
  {
    my ($self, $drow, $dcol, $extend_line) = @_;
    my ($row, $col) = $self->buffer->pos_rowcol($$self{pos});
    my $nrow        = List::Util::min $self->buffer->lines - 1,
                      List::Util::max 0, $row + $drow;
    my $ncol        = $col + $dcol;
    my $linesize    = $self->buffer->line_inner_size($nrow);

    $extend_line // 0
      ? $self->buffer->insert($self->buffer->rowcol_pos($nrow, $linesize),
                              ' ' x ($ncol - $linesize))
      : $ncol = $linesize
    if $ncol > $linesize;

    $self->pos($self->buffer->rowcol_pos($nrow, $ncol));
  }
}
