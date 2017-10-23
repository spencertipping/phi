=head1 Parser API
phi's parsers don't provide much of an interface by themselves, so this module
defines functions that make them easier to work with.
=cut

use strict;
use warnings;


=head2 Parse result navigation
Parse results are difficult to work with for a couple of reasons. One is that
"result" objects are interspersed with the values they wrap (which is a
feature), and the other is that partial parses often result in undefined values
-- results that didn't fail, but that also don't provide a value.

A lot of parser combinator libraries are built around the idea that you'd map
the outputs into a form you want, which phi supports. But there are some
situations where you don't want to do this: for example, if you're writing an
editor that both syntax-highlights and evaluates a language. A more appropriate
abstraction is to return a series of objects that provide different accessors
for different purposes.

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

  my $multipart = repeat->new(seq_fixed->new(strconst->new('f'),
                                             strconst->new('oo')));
  my $out       = $multipart->on(strinput->new('foofoo'))->parse(0, 6);

  $out->is_ok     # -> 1
  $out->val       # -> [ ok_output<[result<ok_output<'f'>>,
                  #                 result<ok_output<'oo'>>]>,
                  #      ok_output<[result<ok_output<'f'>>,
                  #                 result<ok_output<'oo'>>]> ]

This is a little clunky but still manageable; results provide a ->val method so
you can generally treat them as the outputs they contain. Here's where things
go downhill:

  # NB: parse(0, 4) instead of parse(0, 6)
  my $partial_out = $multipart->on(strinput->new('foofoo'))->parse(0, 4);

  $partial_out->is_ok   # -> 1
  $partial_out->val     # -> [ ok_output<[result<ok_output<'f'>>,
                        #                 result<ok_output<'oo'>>]>,
                        #      ok_output<[result<ok_output<'f'>>,
                        #                 result<ok_output<undef>>]> ]
=cut

package phi::parser::bless
{
  use parent -norequire => 'phi::parser::parser_base';

  sub new
  {
    my ($class, $p, $destination_class) = @_;
    bless { parser => $p,
            class  => $destination_class }, $class;
  }

  sub on { phi::parser::bless_result->new(@_) }
}

package phi::parser::bless_result
{
  use parent -norequire => 'phi::parser::result_base';

  sub new
  {
    my $self = phi::parser::result_base::new(@_);
    $$self{parent_result}
      = $$self{parser}->{parser}->on($$self{input}, $$self{start});
    $self;
  }

  sub reparse
  {
    my ($self, $start, $end) = @_;
    my $output = $$self{parent_result}->parse($start, $end);
    return $output unless $output->is_ok;
    $output->change(bless {val => $output->val, output => $output},
                          $$self{parser}->{class});
  }
}


=head2 Explain outputs
This is purely for debugging, and provides human-readable-(ish) string coercion
for results and outputs.
=cut

package phi::parser::parser_base
{
  use overload qw/ "" explain /;

  sub explain;
  sub TO_JSON { shift->explain }
}

sub phi::parser::seq_fixed::explain
{
  my ($self) = @_;
  '(' . join(' ', @$self) . ')';
}

sub phi::parser::seq_repeat::explain
{
  my ($self) = @_;
  my $limits = $$self{max} < (-1 & 0x7fffffff)
             ? "{$$self{min}, $$self{max}}"
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

sub phi::parser::forward::explain
{
  my ($self) = @_;
  defined $$self ? "fwd(defined)" : "fwd(undefined)";
}

sub phi::parser::lookahead::explain
{
  my ($self) = @_;
  "~$$self";
}

sub phi::parser::filter::explain
{
  my ($self) = @_;
  "($$self >?f)";
}

sub phi::parser::not::explain
{
  my ($self) = @_;
  "!$$self";
}

sub phi::parser::bless::explain
{
  my ($self) = @_;
  "($$self{parser} : $$self{class})";
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
    $end = $ascii[++$i] while $i+1 < @ascii and $ascii[$i + 1] == $end + 1;
    push @chars, $end != $start ? chr($start) . '-' . chr($end)
                                : chr($start);
  }
  ($$self{include} ? '[' : '[^')
    . join('', @chars)
    . ($$self{many} ? ']+' : ']');
}


package phi::parser::result_base
{
  use overload qw/ "" explain /;

  sub explain
  {
    my ($self)         = @_;
    my $type           = ref($self) =~ s/.*:://r;
    my $output_explain = defined $$self{output} ? $$self{output}->explain
                                                : 'undef';
    "$type\@$$self{start}<$output_explain>";
  }

  sub TO_JSON { shift->explain }
}


package phi::parser::output_base
{
  use overload qw/ "" explain /;

  sub explain_prefix
  {
    my ($self) = @_;
    my $type   = $self->is_ok ? 'ok' : 'fail';
    "$type\@$$self{start}+$$self{length}";
  }

  sub explain_arrayval
  {
    my ($self, $prefix) = @_;
    my $elements = join ', ', map "$_", @{$self->val};
    defined $prefix ? "$prefix<[$elements]>" : "[$elements]";
  }

  sub explain_hashval
  {
    my ($self, $prefix) = @_;
    my $v        = $self->val;
    my $elements = join ', ', map "$_ => $$v{$_}", sort keys %$v;
    defined $prefix ? "$prefix<{$elements}>" : "{$elements}";
  }

  sub explain_scalarval
  {
    my ($self, $prefix) = @_;
    my $v = $self->val;
    defined $prefix ? "$prefix<$v>" : "$v";
  }

  sub explain
  {
    my ($self) = @_;
    my $prefix = $self->explain_prefix;
    my $v      = $self->val;
    return "$prefix<undef>" unless defined $self->val;
    return $self->explain_arrayval($prefix) if ref $self->val eq 'ARRAY';
    return $self->explain_hashval($prefix)  if ref $self->val eq 'HASH';
    $self->explain_scalarval($prefix);
  }

  sub TO_JSON { shift->explain }
}


=head2 Parser construction functions
Building parsers by calling their constructors is a lot of work. We can do
better by having parsers inherit from a shared base with more convenient
notation.
=cut

package phi::parser::parser_base
{
  use Scalar::Util;
  use overload qw/ |  alt
                   <  bless
                   +  seq
                   *  repeat
                   >> map
                   >  flatmap
                   !  not
                   eq eq /;

  sub alt     { phi::parser::alt_fixed->new(@_[0, 1]) }
  sub seq     { phi::parser::seq_fixed->new(@_[0, 1]) }
  sub repeat  { phi::parser::seq_repeat->new(@_[0, 1]) }
  sub map     { phi::parser::map->new(@_[0, 1]) }
  sub flatmap { phi::parser::flatmap->new(@_[0, 1]) }
  sub bless   { phi::parser::bless->new(@_[0, 1]) }
  sub not     { phi::parser::not->new(shift) }

  # Some interfacing helpers: parsers are comparable (they need to be for
  # flatmap to work correctly), and you can put them into JSON values for
  # debugging.
  sub eq      { Scalar::Util::refaddr($_[0]) eq Scalar::Util::refaddr($_[1]) }
  sub bool    { 1 }
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
  my ($self, @ps) = grep ref, @_;
  phi::parser::alt_fixed->new(@$self, @ps);
}

sub phi::parser::seq_fixed::seq
{
  my ($self, @ps) = grep ref, @_;
  phi::parser::seq_fixed->new(@$self, @ps);
}


1;
