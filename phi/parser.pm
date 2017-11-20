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

=head2 Parse inputs
Parse inputs are usually strings, but they don't have to be. Only
C<parsestr.pm> assumes that you're parsing strings; parsers in this file and
C<parseapi.pm> will work on any linear datatype.
=cut

use strict;
use warnings;


=head2 Parser base class
Every parser inherits from this; we can add methods to it later on. The
important thing for now is that we provide operator overloading, which perl
needs to know about early on.

We also provide two methods, return($length, @values) and fail(@error), that
return values consistent with the parsing protocol.
=cut

package phi::parser::parser_base
{
  use overload;

  sub return { shift; (1, @_) }
  sub fail   { shift; (0, 0, @_) }
}


package phi::parser::delegated
{
  use parent -norequire => 'phi::parser::parser_base';

  sub parse
  {
    my ($self, $input, $start, @xs) = @_;
    $self->parser->parse($input, $start, @xs);
  }
}


package phi::parser::parse_none
{
  use parent -norequire => 'phi::parser::parser_base';
  sub new   { bless {}, shift }
  sub parse { shift->fail }
}


=head2 Sequences
Any sequence of parsers that is applied sequentially/compositionally. This
class handles both repetition and fixed sequencing.

It's ok to repeat a zero-length parser; if you do, the repeat will early-exit
with an empty result instead of looping forever.
=cut

package phi::parser::seq_base
{
  use parent -norequire => 'phi::parser::parser_base';

  sub nth_exit;
  sub nth;

  sub parse
  {
    my ($self, $input, $start, @xs) = @_;
    my @r;
    my $end = $start;
    for (my ($n, $p) = (0, undef);
         defined($p = $self->nth($n));
         ++$n)
    {
      my ($ok, $l, @rs) = $p->parse($input, $end, @xs);
      return $self->nth_exit($n)
           ? $self->return($end - $start, @r)
           : $self->fail(\@rs) if !$ok or $self->must_consume && !$l;
      push @r, @rs;
      $end += $l;
    }
    $self->return($end - $start, @r);
  }
}

package phi::parser::seq_fixed
{
  use parent -norequire => 'phi::parser::seq_base';

  sub new
  {
    my ($class, @ps) = @_;
    bless \@ps, $class;
  }

  sub must_consume { 0 }
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

  sub must_consume { 1 }

  sub nth
  {
    my ($self, $n) = @_;
    $n > $$self{max} ? undef : $self->{parser};
  }

  sub nth_exit
  {
    my ($self, $n) = @_;
    $n >= $$self{min};
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
  sub parse
  {
    my ($self, $input, $start, @xs) = @_;
    my @fail;
    for (my ($n, $p) = (0, undef);
         defined($p = $self->nth($n));
         ++$n)
    {
      my ($ok, $l, @r) = $p->parse($input, $start, @xs);
      return $self->return($l, @r) if $ok;
      push @fail, @r;
    }
    $self->fail(\@fail);
  }
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


=head2 Intersection
Requires multiple parsers to consume the same input and length; if any fail,
then this parser fails too.
=cut

package phi::parser::intersection
{
  use parent -norequire => 'phi::parser::parser_base';

  sub new
  {
    my ($class, @ps) = @_;
    bless \@ps, $class;
  }

  sub parse
  {
    my ($self, $input, $start, @xs) = @_;
    my @result;
    my ($ok, $l, @r) = $$self[0]->parse($input, $start, @xs);
    return $self->fail(@r) unless $ok;
    push @result, \@r;

    for my $p (@$self[1..$#$self])
    {
      my ($pok, $pl, @pr) = $p->parse($input, $start, @xs);
      return $self->fail(@pr) unless $pok && $pl == $l;
      push @result, \@pr;
    }

    $self->return($l, @result);
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

  sub parse
  {
    my ($self, $input, $start, @xs) = @_;
    my ($ok, $l, @r) = $$self{parser}->parse($input, $start, @xs);
    return $self->fail(@r) unless $ok;
    $self->return($l, $$self{fn}->($input, $start, $l, \@xs, @r));
  }
}


=head2 Parser mapping
A parser that lets you write a function that returns the parser's result. You
should delegate to another parser if you want to do this; constructing results
manually may cause problems down the line.
=cut

package phi::parser::flatmap
{
  use parent -norequire => 'phi::parser::parser_base';

  sub new
  {
    my ($class, $p, $f) = @_;
    bless { parser => $p,
            fn     => $f }, $class;
  }

  sub parse
  {
    my ($self, $input, $start, @xs) = @_;
    my ($ok, $l, @r) = $$self{parser}->parse($input, $start, @xs);
    return $self->fail(@r) unless $ok;
    my ($fok, $fl, @fr) = $$self{fn}->($input, $start + $l, $l, \@xs, @r);
    ($fok, $fl, @fr) = $fok->parse($input, $start + $l, @xs) if ref $fok;
    $fok ? $self->return($l + $fl, @fr)
         : $self->fail(@fr);
  }
}


package phi::parser::fixedpoint
{
  use parent -norequire => 'phi::parser::parser_base';

  sub new
  {
    my ($class, $p, $f) = @_;
    bless { parser => $p,
            fn     => $f }, $class;
  }

  sub parse
  {
    my ($self, $input, $start, @xs) = @_;
    my ($ok, $l, @r) = $$self{parser}->parse($input, $start, @xs);
    return $self->fail(@r) unless $ok;

    # Consume continuations until we fail.
    my $offset = $start + $l;
    for (my @nr;
         ($ok, $l, @nr) = $$self{fn}->($input, $offset, $l, \@xs, @r) and $ok;
         $offset += $l, @r = @nr)
    {
      ($ok, $l, @nr) = $ok->parse($input, $offset, @xs) if ref $ok;
      last unless $ok;
    }

    $self->return($offset - $start, @r);
  }
}


=head2 Mutability
Grammars are often recursive, which requires an indirectly circular reference.
Note that if you have a grammar like this, you'll want to weaken links I<into>
the mutable element so perl can eventually GC the structure.
=cut

package phi::parser::mutable
{
  use parent -norequire => 'phi::parser::parser_base';

  sub new          { my ($class, $p) = @_; bless \$p, shift }
  sub parse        { ${+shift}->parse(@_) }
  sub val : lvalue { ${+shift} }
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

  sub parse
  {
    my ($self, $input, $start, @xs) = @_;
    my ($ok, $l, @r) = $$self->parse($input, $start, @xs);
    $ok ? $self->return(0, @r)
        : $self->fail(@r);
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

  sub parse
  {
    my ($self, $input, $start, @xs) = @_;
    my ($ok, $l, @r) = $$self{parser}->parse($input, $start, @xs);
    $ok && $$self{fn}->($input, $start, $l, \@xs, @r)
      ? $self->return($l, @r)
      : $self->fail($$self{fn});
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

  sub parse
  {
    my ($self, $input, $start, @xs) = @_;
    my ($ok, $l, @r) = $$self->parse($input, $start, @xs);
    $ok ? $self->fail(@r)
        : $self->return(0, @r);
  }
}


1;
