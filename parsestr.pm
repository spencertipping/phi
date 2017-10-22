=head1 String parsers
String-specific parsers and inputs. Editable documents use this interface
despite not being internally represented as strings.
=cut

use strict;
use warnings;


=head2 Input classes
Parser inputs need to provide two methods:

1. substr($start, $length)
2. length()
=cut

package phi::parser::strinput
{
  sub new
  {
    my $class = shift;
    bless \$_[0], $class;
  }

  sub substr
  {
    my ($self, $start, $length) = @_;
    substr $$self, $start, $length;
  }

  sub length
  {
    my ($self) = @_;
    length $$self;
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
  sub new
  {
    my ($class, $str) = @_;
    bless \$str, $class;
  }

  sub on { phi::parser::strconst_result->new(@_) }
}

package phi::parser::strconst_result
{
  use parent -norequire => 'phi::parser::result';

  sub reparse
  {
    # This is a rare case for which we're willing to read beyond the end. We do
    # this because strconst parsers are typically very short, and it's likely
    # to be more expensive to alt() over to the next thing than it is to do a
    # minimal amount of lookahead here.
    my ($self, $start, $end) = @_;
    my $l    = length ${$$self{parser}};
    my $next = $$self{input}->substr($$self{start}, $l);
    $next eq ${$$self{parser}}
      ? phi::parser::ok_output->complete(${$$self{parser}}, $l)
      : phi::parser::fail_output->new($self);
  }
}


package phi::parser::strclass
{
  sub new
  {
    my ($class, $chars, $include, $many) = @_;
    my $charvec = '';
    vec($charvec, ord, 1) = 1 for split //, $chars;
    bless { charvec => $charvec,
            include => $include,
            many    => $many }, $class;
  }

  sub one_of      { shift->new(join('', @_), 1, 0) }
  sub many_of     { shift->new(join('', @_), 1, 1) }
  sub one_except  { shift->new(join('', @_), 0, 0) }
  sub many_except { shift->new(join('', @_), 0, 1) }

  sub on
  {
    my ($self) = @_;
    $$self{many}
      ? phi::parser::strclass_many_result->new($self, @_)
      : phi::parser::strclass_one_result->new($self, @_);
  }

  sub match_length
  {
    my ($self, $str) = @_;
    my $matched = 0;
    $matched += vec($$self{charvec}, $_, 1) == $$self{include}
      for unpack "U*", $str;
    return $matched;
  }
}

package phi::parser::strclass_one_result
{
  use parent -norequire => 'phi::parser::result';

  sub reparse
  {
    my ($self, $start, $end) = @_;
    my $next = $$self{input}->substr($$self{start}, 1);
    $$self{parser}->match_length($next)
      ? phi::parser::ok_output->complete($next, 1)
      : phi::parser::fail_output->new($self);
  }
}

package phi::parser::strclass_many_result
{
  use List::Util;
  use parent -norequire => 'phi::parser::result';

  sub reparse
  {
    my ($self, $start, $end) = @_;
    my $input = $$self{input};
    my $n =
      List::Util::max(0,
        List::Util::min(defined $$self{output} ? length $$self{output}->val : 0,
                        $start - $$self{start}));

    # Fill in chunks of 64 chars until we get a partial one back or we hit the
    # end.
    my $next;
    $n += $next while $$self{start} + $n < $end
                   && ($next = $$self{parser}->match_length(
                         $input->substr($$self{start} + $n, 64))) == 64;

    return phi::parser::fail_output($self) unless $n;
    $next == 64
      ? phi::parser::ok_output->cut($input->substr($$self{start}, $n), $n)
      : phi::parser::ok_output->complete($input->substr($$self{start}, $n), $n);
  }
}
