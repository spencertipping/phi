=head1 Parser architecture
These parsers are optimized for reparsing inputs with small changes. This turns
out to be easier than it sounds: each parser stores the amount of text it has
consumed, and parsed outputs can quickly seek to the edit point and begin
reparsing.

There are a couple of things we do differently from most parsing libraries that
make reparsing easy:

1. Parse results are lossless; map outputs are derivative values.
2. Sequences of parsers can early-exit without failing for soft-EOF.

(2) means that we can stop parsing at the end of editor-visible content, or any
other arbitrary boundary we specify, and then efficiently resume parsing when
we want to. This minimizes the reparsing overhead associated with each
keystroke, even for very long input strings.

=head2 Basic class structure
There are two types of objects, each with its own base class:

1. C<phi::parser::parser_base>: parser objects, which are applied to inputs
like C<phi::parser::strinput> and return...

2. C<phi::parser::result_base>: co-mutable result objects, which support
incremental reparsing and produce...

Everything supports operator overloading, although I don't use / rely on it in
the main source.

=head2 Parse inputs
Parse inputs are usually strings, but they don't have to be. Only
C<parsestr.pm> assumes that you're parsing strings; parsers in this file and
C<parseapi.pm> will work on any linear datatype. The only requirement for
inputs is that they provide a length() method, although it doesn't need to
return a number -- just a value that supports subtraction and numeric
comparison.
=cut

use strict;
use warnings;


=head2 Parse results
This object manages co-mutability of parser outputs. That is, if you reparse a
region of a document, this class manages seeking and partial replay to keep
things efficient.

Each type of parser has a separate subclass of C<phi::parser::result>, and
should define the following functionality:

1. reparse($start, $end): updates the result's stored output and returns it
2. downto($position): returns a list of parsers descending to the position

NB: parsers store two different values that describe the amount of input
they've consumed. length() refers to the amount of text actually consumed by a
successful parse, whereas extent() is the contextual lookahead responsible for
setting the parser's state. This works around things like alternating into a
truncated sequence. extent() >= length() always.
=cut

package phi::parser::result_base
{
  use List::Util;
  use overload;

  sub new
  {
    my ($class, $parser, $input, $start) = @_;
    bless { parser  => $parser,
            input   => $input,
            start   => $start // 0,
            length  => -1,
            extent  => 0,
            error   => undef,
            val     => undef }, $class;
  }

  sub reparse;

  sub parse
  {
    my ($self, $start, $end) = @_;
    $start //= 0;
    $end   //= $$self{input}->length;
    $self->reparse($start, $end, $$self{val})
      if $$self{length} == -1
         or $start <= $self->extent_end && $self->start <= $end;
    $self;
  }

  sub ok
  {
    my ($self, $value, $length, $extent) = @_;
    $$self{length} = $length;
    $$self{extent} = $extent // $length;
    $$self{error}  = undef;
    $$self{val}    = $value;
  }

  sub fail
  {
    my ($self, $error, $extent) = @_;
    $$self{length} = 0;
    $$self{extent} = $extent // 0;
    $$self{error}  = $error // $$self{parser};
    $$self{val}    = undef;
  }

  sub extend_extent
  {
    my ($self, $new_extent) = @_;
    $$self{extent} = List::Util::max $$self{extent}, $new_extent;
    $self;
  }

  sub is_fail    { defined shift->{error} }
  sub is_ok      { not defined shift->{error} }
  sub start      { shift->{start} }
  sub length     { shift->{length} }
  sub extent     { shift->{extent} }
  sub end        { $_[0]->{start} + $_[0]->{length} }
  sub extent_end { $_[0]->{start} + $_[0]->{extent} }
  sub error      { shift->{error} }
  sub val        { shift->{val} }
}

package phi::parser::derived_result_base
{
  use parent -norequire => 'phi::parser::result_base';

  sub new
  {
    my $self = phi::parser::result_base::new(@_);
    $$self{parent} = $$self{parser}->parent_on($$self{input}, $$self{start});
    $self;
  }

  sub parent { shift->{parent} }
}


=head2 Parser base class
Every parser inherits from this; we can add methods to it later on. The
important thing for now is that we provide operator overloading, which perl
needs to know about early on.
=cut

package phi::parser::parser_base
{
  use overload;
}


=head2 Sequences
Any sequence of parsers that is applied sequentially/compositionally. This
class handles both repetition and fixed sequencing.
=cut

package phi::parser::seq_base
{
  use parent -norequire => 'phi::parser::parser_base';

  sub nth_exit;
  sub nth;

  sub on { phi::parser::seq_result->new(@_) }
}

package phi::parser::seq_fixed
{
  use parent -norequire => 'phi::parser::seq_base';

  sub new
  {
    my ($class, @ps) = @_;
    bless \@ps, $class;
  }

  sub nth_exit { 0 }
  sub nth
  {
    my ($self, $n) = @_;
    return undef if $n > $#$self;
    $$self[$n];
  }
}

package phi::parser::seq_repeat
{
  use parent -norequire => 'phi::parser::seq_base';

  sub new
  {
    my ($class, $p, $min, $max) = @_;
    bless { parser => $p,
            min    => $min //  0,
            max    => $max // -1 & 0x7fffffff }, $class;
  }

  sub nth
  {
    my ($self, $n) = @_;
    return undef if $n > $$self{max};
    $self->{parser};
  }

  sub nth_exit
  {
    my ($self, $n) = @_;
    $n >= $$self{min};
  }
}


=head2 Sequence results
This is where we handle partial recomputation and seeking. Sequences provide
their own subclass of C<ok_output> to simplify pulling values.
=cut

package phi::parser::seq_result
{
  use parent -norequire => 'phi::parser::result_base';

  sub reparse
  {
    my ($self, $start, $end, $val) = @_;

    # Loop through any existing parse results and reuse them until their
    # lookahead extent collides with the edit region. We can't accumulate
    # extents, though; we accumulate lengths.
    my @rs;
    my $parser     = $$self{parser};
    my $input      = $$self{input};
    my $extent_end = my $next = my $self_start = $$self{start};

    for my $r (defined $val ? @$val : ())
    {
      last if $next >= $start;
      my $l0 = $r->length;
      my $l1 = $r->parse($start, $end)->length;
      $extent_end = List::Util::max $extent_end, $r->extent_end;
      last if $r->is_fail;

      push @rs, $r;
      $next += $l1;
      last if $l1 != $l0;
    }

    # Resume the parse by consuming elements until:
    # 1. we're out of parsers,
    # 2. we encounter a failure and are able to early-exit, or
    # 3. we're out of input.
    my ($p, $r);
    while (defined($p = $parser->nth(scalar @rs)) && $next < $end)
    {
      ($r = $p->on($input, $next))->parse($start, $end);
      $extent_end = List::Util::max $extent_end, $r->extent_end;
      last if $r->is_fail;

      $next += $r->length;
      push @rs, $r;
    }

    my $length = $next       - $self_start;
    my $extent = $extent_end - $self_start;

    # If we're out of parsers or input, then $p is undefined: we can return
    # immediately.
    return $self->ok(\@rs, $length, $extent)
      if not defined $p
         or $next >= $end
         or $parser->nth_exit(scalar @rs);

    # Last case: we have a failed parse in $r.
    $self->fail($r->error, $extent);
  }
}


=head2 Alternatives
Alternatives use passthrough results so we don't have a result using itself as
an lvalue when we slip to a different branch. Like sequences, alternatives are
open-ended.
=cut

package phi::parser::alt_base
{
  use parent -norequire => 'phi::parser::parser_base';

  sub nth;
  sub on { phi::parser::alt_result->new(@_) }
}

package phi::parser::alt_fixed
{
  use parent -norequire => 'phi::parser::alt_base';

  sub new
  {
    my ($class, @ps) = @_;
    bless \@ps, $class;
  }

  sub nth
  {
    my ($self, $n) = @_;
    return undef if $n > $#$self;
    $$self[$n];
  }
}


package phi::parser::alt_result
{
  use parent -norequire => 'phi::parser::result_base';

  sub new
  {
    my $self = phi::parser::result_base::new(@_);
    $$self{alt_results} = [];
    $self;
  }

  sub reparse
  {
    my ($self, $start, $end) = @_;

    my $input      = $$self{input};
    my $self_start = $$self{start};
    my $parser     = $$self{parser};
    my $rs         = $$self{alt_results};
    my $extent     = 0;

    # Reparse each alternative until one works or we run out of options.
    for (my ($p, $i) = (undef, 0); defined($p = $parser->nth($i)); ++$i)
    {
      my $r = $i < @$rs ? $$rs[$i] : $p->on($input, $self_start);
      push @$rs, $r unless $i < @$rs;
      $extent = List::Util::max $extent, $r->parse($start, $end)->extent;

      if ($r->is_ok)
      {
        # Clear results after this one to prevent space leaks.
        @$rs = @$rs[0..$i];
        return $self->ok($r->val, $r->length, $extent);
      }
    }

    # No options worked: keep them cached and return failure.
    $self->fail($self, $extent);
  }
}


=head2 Value mapping
This is a passthrough that transforms non-error values.
=cut

package phi::parser::map
{
  use parent -norequire => 'phi::parser::parser_base';

  sub new
  {
    my ($class, $p, $f) = @_;
    bless { parser => $p,
            fn     => $f }, $class;
  }

  sub on        { phi::parser::map_result->new(@_) }
  sub parent_on { shift->{parser}->on(@_) }
}

package phi::parser::map_result
{
  use parent -norequire => 'phi::parser::derived_result_base';

  sub reparse
  {
    my ($self, $start, $end) = @_;
    my $parent = $self->parent;
    $parent->parse($start, $end)->is_ok
      ? $self->ok($$self{parser}->{fn}->($parent->val, $parent, $$self{start}, $parent->length),
                  $parent->length,
                  $parent->extent)
      : $self->fail($parent->error, $parent->extent);
  }
}


=head2 Parser mapping
This is much more involved than map() because we're constructing or returning
parsers during a parse. This begins with an initial parser that itself returns
a new parser; then we apply that parser to the input.
=cut

package phi::parser::flatmap
{
  use parent -norequire => 'phi::parser::parser_base';

  sub new
  {
    my ($class, $p, $f) = @_;
    bless \$p, $class;
  }

  sub on        { phi::parser::flatmap_result->new(@_) }
  sub parent_on { ${+shift}->on(@_) }
}

package phi::parser::flatmap_result
{
  use List::Util;
  use parent -norequire => 'phi::parser::result_base';

  sub new
  {
    my $self = phi::parser::result_base::new(@_);
    $$self{parent_result} = ${$$self{parser}}->on($$self{input}, $$self{start});
    $$self{parent_parser} = 0;
    $$self{result}        = undef;
    $self;
  }

  sub reparse
  {
    my ($self, $start, $end) = @_;
    my $pr = $$self{parent_result}->parse($start, $end);
    return $self->fail($pr->error, $pr->extent) if $pr->is_fail;

    my $r = $$self{result};
    if ($pr->val ne $$self{parent_parser})
    {
      $$self{parent_parser} = $pr;
      $r = $$self{result}   = $pr->val->on($$self{input}, $$self{start});
    }

    my $extent = List::Util::max $pr->extent, $r->parse($start, $end)->extent;
    $r->is_ok
      ? $self->ok($r->val, $r->length, $extent)
      : $self->fail($r->error, $extent);
  }
}


=head2 Mutability
Grammars are often recursive, which requires an indirectly circular reference.
=cut

package phi::parser::forward
{
  use parent -norequire => 'phi::parser::parser_base';

  sub new { bless \my $p, shift }
  sub on  { ${+shift}->on(@_) }

  sub set
  {
    my $self = shift;
    $$self = shift;
    $self;
  }
}


=head2 Assertions
Lookahead and computed acceptance (filter).
=cut

package phi::parser::lookahead
{
  use parent -norequire => 'phi::parser::parser_base';

  sub new
  {
    my ($class, $p) = @_;
    bless \$p, $class;
  }

  sub on        { phi::parser::lookahead_result->new(@_) }
  sub parent_on { ${+shift}->on(@_) }
}

package phi::parser::lookahead_result
{
  use parent -norequire => 'phi::parser::derived_result_base';

  sub reparse
  {
    my ($self, $start, $end) = @_;
    my $r = $self->parent->parse($start, $end);
    return $self->fail($r->error, $r->extent) unless $r->is_ok;
    $self->ok($r->val, 0, $r->extent);
  }
}


package phi::parser::filter
{
  use parent -norequire => 'phi::parser::parser_base';

  sub new
  {
    my ($class, $p, $f) = @_;
    bless { parser => $p,
            fn     => $f }, $class;
  }

  sub on        { phi::parser::filter_result->new(@_) }
  sub parent_on { ${+shift}->(@_) }
}

package phi::parser::filter_result
{
  use parent -norequire => 'phi::parser::derived_result_base';

  sub reparse
  {
    my ($self, $start, $end) = @_;
    my $r = $self->parent->parse($start, $end);
    $r->is_fail || $$self{fn}->($r->val, $r, $$self{start}, $r->length)
      ? $self->ok($r->val, $r->length, $r->extent)
      : $self->fail($r->error, $r->extent);
  }
}


package phi::parser::not
{
  use parent -norequire => 'phi::parser::parser_base';

  sub new
  {
    my ($class, $p) = @_;
    bless \$p, $class;
  }

  sub on        { phi::parser::not_result->new(@_) }
  sub parent_on { ${+shift}->on(@_) }
}

package phi::parser::not_result
{
  use parent -norequire => 'phi::parser::derived_result_base';

  sub reparse
  {
    my ($self, $start, $end) = @_;
    my $r = $self->parent->parse($start, $end);
    $r->is_ok
      ? $self->fail($self, $r->extent)
      : $self->ok($r->error, 0, $r->extent);
  }
}


1;
