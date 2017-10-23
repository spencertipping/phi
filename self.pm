=head1 Self-referential parsers
phi provides a way for you to write language expressions that turn into
language parsers. You can then use these to modify the grammar during
compilation/interpretation.
=cut

use strict;
use warnings;


=head2 Parse result navigation
Parse results are difficult to work with for a couple of reasons. One is that
"result" objects are interspersed with the values they wrap (which is a
feature), and the other is that partial parses often result in undefined values
-- results that didn't fail, but that also don't provide a value.

Before I get into the implementation, though, I should describe the situations
we tend to run into when consuming results.

=head3 Parse result format
Let's start with a simple grammar:

  my $p_match = strconst->new('foo');
  my $p_out   = map->new($p_match, sub {
                  my ($output) = @_;
                  +{ value     => $output->val,
                     type      => 'foo',
                     start_pos => $output->start,
                     end_pos   => $output->end };
                });

map() passes the C<ok_output> into the function, and then takes whatever we
return and constructs a new C<ok_output> from that. So in the above grammar, we
have this:

  my $mutable_result = $p_out->on(strinput->new('foobar'));
  my $out            = $mutable_result->parse;

  $out->is_ok     # -> 1 (the parse was successful)
  $out->val       # -> {
                  #      value     => 'foo',
                  #      type      => 'foo',
                  #      start_pos => 0,
                  #      end_pos   => 3
                  #    }

So far so straightforward, but things get more complicated once we start using
compound parsers like sequences.

  my $multipart = repeat->new(seq_fixed->new('f', 'oo'));
  my $out       = $multipart->on(strinput->new('foofoo'))->parse(0, 6);

  $out->is_ok     # -> 1
  $out->val       # -> [ ok_output<[ok_output<'f'>, ok_output<'oo'>]>,
                  #      ok_output<[ok_output<'f'>, ok_output<'oo'>]> ]

This is a little clunky but still manageable. Here's where things go downhill:

  # NB: parse(0, 4) instead of parse(0, 6)
  my $partial_out = $multipart->on(strinput->new('foofoo'))->parse(0, 4);

  $partial_out->is_ok   # -> 1
  $partial_out->val     # -> [ ok_output<[ok_output<'f'>, ok_output<'oo'>]>,
                        #      ok_output<[ok_output<'f'>, undef]> ]

The parse succeeded, but some values don't exist because they haven't yet been
determined. phi represents them as undef, which will break your code and
requires a lot of casing to work around.
=cut


=head2 Parser construction functions
Building parsers by calling their constructors is a lot of work. We can do
better by having parsers inherit from a shared base with more convenient
notation.
=cut

package phi::parser::parser_base
{
  use Scalar::Util;
  use overload qw/ |  alt
                   +  seq
                   x  repeat
                   ~  maybe
                   >> map
                   >  flatmap
                   eq eq
                   "" explain /;

  sub alt     { phi::parser::alt_fixed->new(@_) }
  sub seq     { phi::parser::seq_fixed->new(@_) }
  sub repeat  { phi::parser::seq_repeat->new(@_) }
  sub map     { phi::parser::map->new(@_) }
  sub flatmap { phi::parser::flatmap->new(@_) }
  sub maybe   { phi::parser::seq_repeat->new(shift, 0, 1) }

  # Some interfacing helpers: parsers are comparable (they need to be for
  # flatmap to work correctly), and you can put them into JSON values for
  # debugging.
  sub eq      { Scalar::Util::refaddr($_[0]) eq Scalar::Util::refaddr($_[1]) }
  sub bool    { 1 }

  sub explain;
  sub TO_JSON { shift->explain }
}


=head2 Parser-specific overrides
If you write something like C<$a | $b | $c>, we want to create a single
three-way alt rather than two nested two-way alts. The simplest way to make
this happen is to use type-specific overrides.

Note that this only works for left-associative operators, but that's fine
because that's exactly what these are.
=cut

sub phi::parser::alt_fixed::alt
{
  my ($self, @ps) = @_;
  phi::parser::alt_fixed->new(@$self, @ps);
}

sub phi::parser::seq_fixed::seq
{
  my ($self, @ps) = @_;
  phi::parser::seq_fixed->new(@$self, @ps);
}


=head2 Parser explanations
Mostly for debugging: this provides a way to see the structure of a parser.
=cut

sub phi::parser::seq_fixed::explain
{
  my ($self) = @_;
  '(' . join(' ', @$self) . ')';
}

sub phi::parser::seq_repeat::explain
{
  my ($self) = @_;
  my $limits = $$self{max} < (-1 & 0x7fffffff)
             ? "{$$self{min}, $self{max}}"
             : "{$$self{min},}";
  "($$self{parser} $limits)";
}

sub phi::parser::alt_fixed::explain
{
  my ($self) = @_;
  '(' . join(' | ', @$self) . ')';
}

sub phi::parser::map::explain
{
  my ($self) = @_;
  "($$self{parser} >>f)";
}

sub phi::parser::flatmap::explain
{
  my ($self) = @_;
  "($$self{parser} >f)";
}


sub phi::parser::strconst::explain
{
  my ($self) = @_;
  my $escaped = $$self =~ s/"/\\"/gr;
  "\"$escaped\"";
}

sub phi::parser::strclass::explain
{
  my ($self) = @_;
  my @ascii  = grep vec($$self{charvec}, $_, 1), 0..65535;
  my @chars;
  for (my $i = 0; $i < $#ascii; ++$i)
  {
    my $start = my $end = $ascii[$i];
    $end = $ascii[++$i] while $i < @ascii and $ascii[$i + 1] == $end + 1;
    push @chars, $end != $start ? chr($start) . '-' . chr($end)
                                : chr($start);
  }
  ($$self{include} ? '[' : '[^')
    . join('', @chars)
    . ($$self{many} ? ']+' : ']');
}
