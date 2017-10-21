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

  sub at { phi::parser::strconst_result->new(@_) }
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
      ? phi::parser::ok_result->complete(${$$self{parser}}, $l)
      : phi::parser::fail_result->new($self);
  }
}


package phi::parser::strclass
{
  sub oneof
  {
    my ($class, @strings) = @_;
    bless { include => 1,
            chars   => join"", @strings }, $class;
  }

  sub noneof
  {
    my ($class, @strings) = @_;
    bless { include => 0,
            chars   => join"", @strings }, $class;
  }

  sub at { phi::parser::strclass_result->new(@_) }
}

package phi::parser::strclass_result
{
  
}
