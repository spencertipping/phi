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
There are three types of objects, each with its own base class:

1. C<phi::parser::parser_base>: parser objects, which are applied to inputs
like C<phi::parser::strinput> and return...

2. C<phi::parser::result_base>: co-mutable result objects, which support
incremental reparsing and produce...

3. C<phi::parser::output_base>: parse output wrappers, which are immutable and
represent a completed parse that either succeeded or failed.

Everything in this library is an instance of one of these three things.
Everything supports operator overloading, although I don't use / rely on it in
the main source.
=cut

use strict;
use warnings;


=head2 Parse outputs
These are encased inside parser-specific results; the output itself is a
polymorphic structure that encodes success+consumption vs error+backtrack.

TODO: rethink the organization here. We want locally-polymorphic parser
outputs, which is _sort of_ doable by subclassing C<output> ... but then we're
re-blessing the same object. Ideally we'd scalar-ref to the underlying output
and provide wrapping accessors on that ref; but then we're still dealing with
the ->val access overhead...?
=cut

package phi::parser::output_base
{
  use List::Util;
  use overload qw/ ${} val /;

  sub start  { shift->{start} }
  sub end    { $_[0]->{start} + $_[0]->length }
  sub extent { shift->{extent} }
  sub extend_extent
  {
    my ($self, $e) = @_;
    $$self{extent} = List::Util::max $$self{extent}, $e;
    $self;
  }

  sub lookahead
  {
    my ($self) = @_;
    $self->extent - $self->length;
  }
}

package phi::parser::ok_output
{
  use parent -norequire => 'phi::parser::output_base';

  sub new
  {
    my ($class, $val, $start, $length, $extent) = @_;
    bless { val    => $val,
            start  => $start,
            length => $length,
            extent => $extent // $length }, $class;
  }

  sub change
  {
    my ($self, $v) = @_;
    bless { val    => $v,
            start  => $$self{start},
            length => $$self{length},
            extent => $$self{extent} }, ref $self;
  }

  sub is_ok   { 1 }
  sub is_fail { 0 }
  sub val     { shift->{val} }
  sub length  { shift->{length} }
  sub error   { undef }
}

package phi::parser::fail_output
{
  use parent -norequire => 'phi::parser::output_base';

  sub new
  {
    my ($class, $error, $start, $extent) = @_;
    bless { error  => $error,
            start  => $start,
            extent => $extent // 0 }, $class;
  }

  sub is_ok   { 0 }
  sub is_fail { 1 }
  sub val     { undef }
  sub length  { 0 }
  sub error   { shift->{error} }
}


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
            output  => undef }, $class;
  }

  sub output
  {
    my ($self) = @_;
    $$self{output} //= $self->reparse($$self{start},
                                      $$self{input}->length - $$self{start});
  }

  sub parse
  {
    my ($self, $start, $end) = @_;
    $start //= 0;
    $end   //= $$self{input}->length + 1;
    return $$self{output} if $end <= $start
                          or defined $$self{output}
                         and $self->is_ok
                         and $start > $self->context_end || $self->start > $end;
    $$self{output} = $self->reparse($start, $end);
  }

  sub reparse;
  sub downto;

  sub spanning
  {
    my ($self, $start, $end) = @_;
    my @ps1 = $self->downto($start);
    my @ps2 = $self->downto($end);
    grep $ps1[$_] eq $ps2[$_], 0..List::Util::min($#ps1, $#ps2);
  }

  sub ok
  {
    my ($self, $value, $length, $extent) = @_;
    phi::parser::ok_output->new($value, $$self{start}, $length, $extent);
  }

  sub fail
  {
    my ($self, $error, $extent) = @_;
    phi::parser::fail_output->new($error, $$self{start}, $extent);
  }

  sub is_fail     { shift->output->is_fail }
  sub is_ok       { shift->output->is_ok }
  sub start       { shift->{start} }
  sub end         { $_[0]->{start} + $_[0]->output->length }
  sub context_end { $_[0]->{start} + $_[0]->output->extent }
  sub length      { shift->output->length }
  sub extent      { shift->output->extent }
  sub val         { shift->output->val }
  sub error       { shift->output->error }
}

package phi::parser::derived_result_base
{
  use parent -norequire => 'phi::parser::result_base';

  sub new
  {
    my $self = phi::parser::result_base::new(@_);
    $$self{parent} = $$self{parser}->on($$self{input}, $$self{start});
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

package phi::parser::seq_output
{
  use parent -norequire => 'phi::parser::ok_output';
  use overload qw/ @{} vals /;

  sub vals { [map $$_{output}, @{shift->{val}}] }
}

package phi::parser::seq_result
{
  use parent -norequire => 'phi::parser::result_base';

  sub ok
  {
    my ($self, $value, $length, $extent) = @_;
    phi::parser::seq_output->new($value, $$self{start}, $length, $extent);
  }

  sub reparse
  {
    my ($self, $start, $end) = @_;

    # Loop through any existing parse results and reuse them until their
    # lookahead context collides with the edit region. We can't accumulate
    # extents, though; we accumulate lengths.
    my @rs;
    my $context_end = my $next = $$self{start};

    for my $r (defined $$self{output} && defined $$self{output}->val
                 ? @{$$self{output}->val}
                 : ())
    {
      last if $next >= $start;
      my $l0     = $r->length;
      my $output = $r->parse($start, $end);
      $context_end = List::Util::max $context_end, $r->context_end;

      last if $output->is_fail;

      push @rs, $r;
      $next += $r->length;
      last if $r->length != $l0;
    }

    # Resume the parse by consuming elements until:
    # 1. we're out of parsers,
    # 2. we encounter a failure and are able to early-exit, or
    # 3. we're out of input.
    my ($p, $r);
    while (defined($p = $$self{parser}->nth(scalar @rs)) && $next < $end)
    {
      ($r = $p->on($$self{input}, $next))->parse($start, $end);
      $context_end = List::Util::max $context_end, $r->context_end;
      last if $r->is_fail;

      $next += $r->length;
      push @rs, $r;
    }

    my $length = $next        - $$self{start};
    my $extent = $context_end - $$self{start};

    # If we're out of parsers or input, then $p is undefined: we can return
    # immediately.
    return $self->ok(\@rs, $length, $extent)
      if !defined($p)
      or $next >= $end
      or $$self{parser}->nth_exit(scalar @rs);

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
    my $rs = $$self{alt_results};
    my $extent = 0;

    # Reparse each alternative until one works or we run out of options.
    for (my ($p, $i) = (undef, 0);
         defined($p = $$self{parser}->nth($i));
         ++$i)
    {
      my $r = $i < @$rs ? $$rs[$i] : $p->on($$self{input}, $$self{start});
      push @$rs, $r if $i > $#$rs;
      $extent = List::Util::max
                  $extent,
                  (my $output = $r->parse($start, $end))->extent;

      if ($output->is_ok)
      {
        # Clear results after this one to prevent space leaks.
        pop @$rs while $#$rs > $i;
        return $output->extend_extent($extent);
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

  sub on { phi::parser::map_result->new(@_) }
}

package phi::parser::map_result
{
  use parent -norequire => 'phi::parser::derived_result_base';

  sub reparse
  {
    my ($self, $start, $end) = @_;
    my $output = $self->parent->parse($start, $end);
    $output->is_ok
      ? $output->change($$self{parser}->{fn}->($output))
      : $output;
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

  sub on { phi::parser::flatmap_result->new(@_) }
}

package phi::parser::flatmap_result
{
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
    my $p = $$self{parent_result}->parse($start, $end);
    return $p unless $p->is_ok;

    if ($p->val ne $$self{parent_parser})
    {
      $$self{parent_parser} = $p;
      $$self{result}        = $p->val->on($$self{input}, $$self{start});
    }

    $$self{result}->parse($start, $end)->extend_extent($p->extent);
  }
}


=head2 Mutability
Grammars are often recursive, which requires an indirectly circular reference.
=cut

package phi::parser::forward
{
  use parent -norequire => 'phi::parser::parser_base';

  sub new { bless \my $p, shift }
  sub on
  {
    my $self = shift;
    $$self->on(@_);
  }

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

  sub on { phi::parser::lookahead_result->new(@_) }
}

package phi::parser::lookahead_result
{
  use parent -norequire => 'phi::parser::derived_result_base';

  sub reparse
  {
    my ($self, $start, $end) = @_;
    my $o = $self->parent->parse($start, $end);
    return $o unless $o->is_ok;
    $o->{length} = 0;
    $o;
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

  sub on { phi::parser::filter_result->new(@_) }
}

package phi::parser::filter_result
{
  use parent -norequire => 'phi::parser::derived_result_base';

  sub reparse
  {
    my ($self, $start, $end) = @_;
    my $o = $self->parent->parse($start, $end);
    !$o->is_ok || $$self{fn}->($o)
      ? $o
      : $self->fail($self, $o->extent);
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

  sub on { phi::parser::not_result->new(@_) }
}

package phi::parser::not_result
{
  use parent -norequire => 'phi::parser::derived_result_base';

  sub reparse
  {
    my ($self, $start, $end) = @_;
    my $o = $self->parent->parse($start, $end);
    $o->is_ok
      ? $self->fail($self, $o->extent)
      : $self->ok($o->error, 0, $o->extent);
  }
}


1;
