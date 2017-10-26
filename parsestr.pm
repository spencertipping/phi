=head1 String parsers
String-specific parsers and inputs. Editable documents use this interface
despite not being internally represented as strings.
=cut

use strict;
use warnings;


=head2 Input classes
Parser inputs need to provide two methods (TODO: not really; clarify this):

1. substr($start, $length)
2. length()
=cut

package phi::parser::strinput
{
  use parent -norequire => 'phi::parser::parser_base';

  sub new
  {
    my $class = shift;
    bless \$_[0], $class;
  }

  sub substr
  {
    my ($self, $start, $length) = @_;
    CORE::substr $$self, $start, $length;
  }

  sub length
  {
    my ($self) = @_;
    CORE::length $$self;
  }
}


=head2 Parsers
It's tempting to have a parser that consumes a regular expression here, but I'm
not sure how to do it with the abstractions we have. We'd need to hand Perl an
unlimited lookahead buffer to make it work, thus committing ourselves to
arbitrarily much work to copy stuff into memory.

...so unfortunately, although it would be convenient to have Perl do it for us,
we need to write our own regex conversion.
=cut

package phi::parser::strconst
{
  use parent -norequire => 'phi::parser::parser_base';

  sub new
  {
    my ($class, $str) = @_;
    bless \$str, $class;
  }

  sub on { phi::parser::strconst_result->new(@_) }
}

package phi::parser::strconst_result
{
  use parent -norequire => 'phi::parser::result_base';

  sub reparse
  {
    # This is a rare case for which we're willing to read beyond the end. We do
    # this because strconst parsers are typically very short, and it's likely
    # to be more expensive to alt() over to the next thing than it is to do a
    # minimal amount of lookahead here.
    my ($self, $start, $end) = @_;
    my $p = $$self{parser};
    my $l = length $$p;
    $$self{input}->substr($$self{start}, $l) eq $$p
      ? $self->ok($$p, $l)
      : $self->fail($self, $l);
  }
}


package phi::parser::strclass
{
  use parent -norequire => 'phi::parser::parser_base';

  sub new
  {
    my ($class, $chars, $include, $many, $min) = @_;
    my $charvec = '';
    vec($charvec, $_, 1) = 1 for unpack 'U*', $chars;
    bless { charvec => $charvec,
            include => $include,
            many    => $many,
            min     => $min // 0 }, $class;
  }

  sub one_of      { shift->new(join('', @_), 1, 0, 0) }
  sub many_of     { shift->new(join('', @_), 1, 1, 0) }
  sub more_of     { shift->new(join('', @_), 1, 1, 1) }
  sub one_except  { shift->new(join('', @_), 0, 0, 0) }
  sub many_except { shift->new(join('', @_), 0, 1, 0) }
  sub more_except { shift->new(join('', @_), 0, 1, 1) }

  sub min { shift->{min} }

  sub on
  {
    my ($self) = @_;
    $$self{many}
      ? phi::parser::strclass_many_result->new(@_)
      : phi::parser::strclass_one_result->new(@_);
  }

  sub match_length
  {
    my ($self, $str) = @_;
    my $matched = 0;
    vec($$self{charvec}, $_, 1) == $$self{include}
      ? ++$matched
      : return $matched
    for unpack "U*", $str;
    $matched;
  }
}

package phi::parser::strclass_one_result
{
  use parent -norequire => 'phi::parser::result_base';

  sub reparse
  {
    my ($self, $start, $end) = @_;
    my $next = $$self{input}->substr($$self{start}, 1);
    $$self{parser}->match_length($next) ? $self->ok($next, 1)
                                        : $self->fail($self);
  }
}

package phi::parser::strclass_many_result
{
  use List::Util;
  use parent -norequire => 'phi::parser::result_base';

  sub reparse
  {
    my ($self, $start, $end) = @_;
    my $input      = $$self{input};
    my $self_start = $$self{start};
    my $parser     = $$self{parser};
    my $n          = 0;

    # Fill in chunks of 64 chars until we get a partial one back or we hit the
    # end.
    pull_more:
      $n += my $next = $parser->match_length(
                         $input->substr($self_start + $n, 64));
      goto pull_more if $next == 64 && $self_start + $n < $end;

    return $self->fail($self) if $n < $$self{parser}->min;
    $self->ok($input->substr($self_start, $n), $n);
  }
}


1;
