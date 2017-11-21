=head1 String parsers
String-specific parsers and inputs. Editable documents use this interface
despite not being internally represented as strings.
=cut

use strict;
use warnings;


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

  sub parse
  {
    my ($self, $input, $start) = @_;
    return $self->fail("not a string input")
      unless ref($input) && $input->can('substr');
    $input->substr($start, length $$self) eq $$self
      ? $self->return(length $$self, $$self)
      : $self->fail([expected => $self]);
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

  sub parse
  {
    my $self = shift;
    return $self->fail("not a string input")
      unless ref($input) && $input->can('substr');

    $$self{many} ? $self->parse_many(@_)
                 : $self->parse_one(@_);
  }

  sub parse_one
  {
    my ($self, $input, $start) = @_;
    my $next = $input->substr($start, 1);
    return $self->fail(['eof', expected => $self]) unless length $next;
    vec($$self{charvec}, unpack(U => $next), 1) == $$self{include}
      ? $self->return(1, $next)
      : $self->fail([expected => $self]);
  }

  sub parse_many
  {
    my ($self, $input, $start) = @_;
    my @ps;
    my $length = 0;

    # First parse up to the minimum.
    if ($$self{min})
    {
      push @ps, my $piece = $input->substr($start, $$self{min});
      $length += $self->match_length($piece);
      return $self->fail([expected => $self]) if $length < $$self{min};
    }

    # Now grab as many more matching characters as we have.
    while (1)
    {
      my $next     = $input->substr($start + $length, 64);
      my $accepted = $self->match_length($next);
      return $self->return($length + $accepted,
                           join '', @ps, substr $next, 0, $accepted)
        unless $accepted == 64;

      push @ps, $next;
      $length += $accepted;
    }
  }
}


1;
