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
=cut

use strict;
use warnings;


=head2 Parse outputs
These are encased inside parser-specific results; the output itself is a
polymorphic structure that encodes success+consumption vs error+backtrack.
=cut

package phi::parser::ok_output
{
  sub complete
  {
    my ($class, $val, $length) = @_;
    bless { val    => $val,
            length => $length,
            cut    => 0 }, $class;
  }

  sub cut
  {
    my ($class, $val, $length) = @_;
    bless { val    => $val,
            length => $length,
            cut    => 1 }, $class;
  }

  sub change
  {
    my ($self, $v) = @_;
    bless { val    => $v,
            length => $$self{length},
            cut    => $$self{cut} }, ref $self;
  }

  sub is_ok   { 1 }
  sub is_fail { 0 }
  sub is_cut  { shift->{cut} }
  sub val     { shift->{val} }
  sub length  { shift->{length} }
  sub error   { undef }
}

package phi::parser::fail_output
{
  sub new
  {
    my ($class, $error) = @_;
    bless \$error, $class;
  }

  sub is_ok   { 0 }
  sub is_fail { 1 }
  sub is_cut  { 0 }
  sub val     { undef }
  sub length  { 0 }
  sub error   { ${$_[0]} }
}


=head2 Parse results
This object manages co-mutability of parser outputs. That is, if you reparse a
region of a document, this class manages seeking and partial replay to keep
things efficient.

Each type of parser has a separate subclass of C<phi::parser::result>, and
should define the following functionality:

1. reparse($start, $end): updates the result's stored output and returns it
2. downto($position): returns a list of parsers descending to the position
=cut

package phi::parser::result
{
  use List::Util;

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
    $end //= $$self{input}->length - $start;
    return $self->output if defined $$self{output}
                        and $self->end <= $start || $self->start >= $end;
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

  sub is_fail { shift->output->is_fail }
  sub is_ok   { shift->output->is_ok }
  sub is_cut  { shift->output->is_cut }
  sub start   { shift->{start} }
  sub end     { $_[0]->{start} + $_[0]->output->length }
  sub length  { shift->output->length }
  sub val     { shift->output->val }
  sub error   { shift->output->error }
}


=head2 Sequences
Any sequence of parsers that is applied sequentially/compositionally. This
class handles both repetition and fixed sequencing.
=cut

package phi::parser::seq_base
{
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

  sub nth { shift->{parser} }
  sub nth_exit
  {
    my ($self, $n) = @_;
    $n >= $$self{min} && $n <= $$self{max};
  }
}


=head2 Sequence results
This is where we handle partial recomputation and seeking.
=cut

package phi::parser::seq_result
{
  use parent -norequire => 'phi::parser::result';

  sub reparse
  {
    my ($self, $start, $end) = @_;

    # Recover existing parse outputs, if any are usable.
    my @existing;
    my $end_of_complete = $$self{start};
    if ($start >= $$self{start})
    {
      @existing = defined $$self{output} ? @{$$self{output}->val} : ();
      my @ends = ($$self{start});
      push @ends, $ends[-1] + $_->length for @existing;
      shift @ends;

      # Trim the list until we're outside the edit.
      pop @existing while @existing && $ends[$#existing] >= $start;
      pop @existing while @existing && $existing[-1]->is_cut;

      $end_of_complete = @existing ? $ends[$#existing] : $$self{start};
    }

    # Resume the parse by consuming elements until:
    # 1. we're out of parsers,
    # 2. we encounter a failure and are able to early-exit, or
    # 3. we're out of input.
    my ($p, $r);
    while (defined($p = $$self{parser}->nth(scalar @existing))
             && $end_of_complete < $end
             && ($r = $p->on($$self{input}, $end_of_complete)
                        ->parse($start, $end))->is_ok)
    {
      $end_of_complete += $r->length;
      push @existing, $r;
    }

    # If we're out of parsers, $p is undefined: we can return immediately.
    return phi::parser::ok_output->complete(\@existing, $end_of_complete)
      unless defined $p;

    # If we've run out of input, $r is out of date and
    # $end_of_complete >= $end: return a cut.
    return phi::parser::ok_output->cut(\@existing, $end_of_complete - $$self{start})
      if $end_of_complete >= $end;

    # Last case: we have a failed parse in $r.
    $$self{parser}->nth_exit(scalar @existing)
      ? phi::parser::ok_output->complete(\@existing, $end_of_complete - $$self{start})
      : phi::parser::fail_output->new($r->error);
  }
}


=head2 Alternatives
Alternatives use passthrough results so we don't have a result using itself as
an lvalue when we slip to a different branch. Like sequences, alternatives are
open-ended.
=cut

package phi::parser::alt_base
{
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
  use parent -norequire => 'phi::parser::result';

  sub new
  {
    my $self = phi::parser::result::new(@_);
    $$self{alt_results} = [];
    $self;
  }

  sub reparse
  {
    my ($self, $start, $end) = @_;
    my $rs = $$self{alt_results};
    my $r;

    # Reparse each alternative until one works or we run out of options.
    for (my ($p, $i) = (undef, 0);
         defined($p = $$self{parser}->nth($i));
         ++$i)
    {
      $r = $i < @$rs ? $$rs[$i] : $p->on($$self{input}, $$self{start});
      push @$rs, $r if $i >= @$rs;

      my $output;
      if (($output = $r->parse($start, $end))->is_ok)
      {
        # Clear results after this one to prevent space leaks.
        pop @$rs while $#$rs > $i;
        return $output;
      }
    }

    # No options worked: keep them cached and return failure.
    phi::parser::fail_output->new([map $_->error, @$rs]);
  }
}


=head2 Value mapping
This is a passthrough that transforms non-error values.
=cut

package phi::parser::map
{
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
  use parent -norequire => 'phi::parser::result';

  sub new
  {
    my $self = phi::parser::result::new(@_);
    $$self{parent_result}
      = $$self{parser}->{parser}->on($$self{input}, $$self{start});
    $self;
  }

  sub reparse
  {
    local $_;
    my ($self, $start, $end) = @_;
    my $output = $$self{parent_result}->parse($start, $end);
    $output->is_ok
      ? $output->change($$self{parser}->{fn}->($_ = $output->val))
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
  sub new
  {
    my ($class, $p, $f) = @_;
    bless \$p, $class;
  }

  sub on { phi::parser::flatmap_result->new(@_) }
}

package phi::parser::flatmap_result
{
  use parent -norequire => 'phi::parser::result';

  sub new
  {
    my $self = phi::parser::result::new($_);
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

    $$self{result}->parse($start, $end);
  }
}


=head2 Mutability
Grammars are often recursive, which requires an indirectly circular reference.
=cut

package phi::parser::forward
{
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
