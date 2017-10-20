#!/usr/bin/env perl
# phi bootstrap interpreter
# We aren't up to much at this point. We just need to define enough parser
# combinators to define the language that converts nice-grammar into more
# parser combinators.

use strict;
use warnings;

use Carp;
$SIG{__DIE__} = sub {Carp::confess(@_)};


package phi::parser::state::str
{
  sub new
  {
    my ($class, $string_ref, $offset) = @_;
    bless {string_ref => $string_ref,
           offset     => $offset || 0}, $class;
  }

  sub return
  {
    my ($self, $v, $consumed) = @_;
    return $self->fail("EOF")
      if $$self{offset} + $consumed > length ${$$self{string_ref}};
    bless {value      => $v,
           string_ref => $$self{string_ref},
           offset     => $$self{offset} + $consumed}, ref $self;
  }

  sub fail
  {
    my ($self, $error) = @_;
    bless {error      => $error,
           string_ref => $$self{string_ref},
           offset     => $$self{offset}}, ref $self;
  }

  sub next
  {
    my ($self, $n) = @_;
    substr ${$$self{string_ref}}, $$self{offset}, $n;
  }

  sub value { shift->{value} }
  sub error { shift->{error} }
}


package phi::parser
{
  BEGIN {++$INC{'phi/parser.pm'}}

  use overload qw/ |    p_alt
                   >>   p_map
                   +    p_seq
                   x    p_repeat
                   !    p_not

                   bool op_bool
                   ""   explain /;

  sub p_alt    { phi::parser::alt   ->new(@_[0, 1]) }
  sub p_map    { phi::parser::map   ->new(@_[0, 1]) }
  sub p_seq    { phi::parser::seq   ->new(@_[0, 1]) }
  sub p_repeat { phi::parser::repeat->new(@_[0, 1]) }
  sub p_not    { phi::parser::not   ->new(@_[0, 1]) }

  sub op_bool  { 1 }

  sub TO_JSON  { shift->explain }
}


package phi::parser::seq
{
  use parent 'phi::parser';

  sub new
  {
    my ($class, @ps) = @_;
    bless \@ps, $class;
  }

  sub parse
  {
    local $_;
    my ($self, $s0) = @_;
    my $s = $s0;
    my @vs;
    ($s = $_->parse($s))->error ? return $s0->fail($_) : push @vs, $s->value
      for @$self;
    $s->error
      ? $s
      : $s->return(\@vs, 0);
  }

  sub explain
  {
    my ($self) = @_;
    "[" . join(", ", @$self) . "]";
  }
}


package phi::parser::alt
{
  use parent 'phi::parser';

  sub new
  {
    my ($class, @ps) = @_;
    bless \@ps, $class;
  }

  sub parse
  {
    local $_;
    my ($self, $s) = @_;
    my $parsed;
    my @failed;
    ($parsed = $_->parse($s))->error or return $parsed for @$self;
    $s->fail($self);
  }

  sub explain
  {
    my ($self) = @_;
    "(" . join(" | ", @$self) . ")";
  }
}


package phi::parser::str
{
  use parent 'phi::parser';

  sub new
  {
    my ($class, $s) = @_;
    bless \$s, $class;
  }

  sub parse
  {
    my ($self, $s) = @_;
    $s->next(length $$self) eq $$self
      ? $s->return($$self, length $$self)
      : $s->fail($self);
  }

  sub explain
  {
    my ($self) = @_;
    "\"$$self\"";
  }
}


package phi::parser::oneof
{
  use parent 'phi::parser';

  sub new
  {
    my ($class, $chars) = @_;
    bless \$chars, $class;
  }

  sub parse
  {
    my ($self, $s) = @_;
    index($$self, $s->next(1)) == -1
      ? $s->fail($self)
      : $s->return($s->next(1), 1);
  }

  sub explain
  {
    my ($self) = @_;
    "/[$$self]/";
  }
}


package phi::parser::one
{
  use parent 'phi::parser';
  sub new     { my $x = 0; bless \$x, shift }
  sub explain { "/./" }

  sub parse
  {
    my ($self, $s) = @_;
    $s->return($s->next(1), 1);
  }
}


package phi::parser::lookahead
{
  use parent 'phi::parser';

  sub new
  {
    my ($class, $p) = @_;
    bless \$p, $class;
  }

  sub parse
  {
    my ($self, $s) = @_;
    my $next = $$self->parse($s);
    $s->error
      ? $s
      : $s->return($next->value, 0);
  }

  sub explain
  {
    my ($self) = @_;
    ">[$$self]";
  }
}


package phi::parser::not
{
  use parent 'phi::parser';

  sub new
  {
    my ($class, $p) = @_;
    bless \$p, $class;
  }

  sub parse
  {
    my ($self, $s) = @_;
    my $next = $$self->parse($s);
    $next->error
      ? $s->return(1, 0)
      : $s->fail($self);
  }

  sub explain
  {
    my ($self) = @_;
    "!($$self)";
  }
}


package phi::parser::repeat
{
  use parent 'phi::parser';

  sub new
  {
    my ($class, $p, $min, $max) = @_;
    $min =  0 unless defined $min;
    $max = -1 unless defined $max;
    bless {parser => $p,
           min    => $min,
           max    => $max}, $class;
  }

  sub parse
  {
    my ($self, $s0) = @_;
    my $s = $s0;
    my @states;

    # First consume up to the minimum. If we can't get there, then fail
    # immediately.
    push @states, $s = $$self{parser}->parse($s)
      until $s->error or @states == $$self{min};
    return $s->fail($self) if $s->error;

    # Now consume additional states until (1) they fail, or (2) we hit the
    # upper limit.
    push @states, $s = $$self{parser}->parse($s)
      until $s->error or $$self{max} > -1 and @states >= $$self{max};

    pop @states if $s->error;
    $s->return([map $_->value, @states], 0);
  }

  sub explain
  {
    my ($self) = @_;
    "($$self{parser}){$$self{min},$$self{max}}";
  }
}


package phi::parser::map
{
  use parent 'phi::parser';

  sub new
  {
    my ($class, $p, $f) = @_;
    unless (ref $f)
    {
      my $i = $f;
      $f = sub {shift->[$i]};
    }
    bless {parser => $p,
           f      => $f}, $class;
  }

  sub parse
  {
    my ($self, $s) = @_;
    my $r = $$self{parser}->parse($s);
    $r->error
      ? $r
      : $r->return($$self{f}->($r->value), 0);
  }

  sub explain
  {
    my ($self) = @_;
    "$$self{parser} -> function";
  }
}


package phi::parser::forward
{
  use parent 'phi::parser';

  sub new
  {
    my $p = undef;
    bless \$p, shift;
  }

  sub get
  {
    my ($self) = @_;
    $$self;
  }

  sub set
  {
    my ($self, $p) = @_;
    $$self = $p;
    $self;
  }

  sub parse
  {
    my ($self, $s) = @_;
    $$self->parse($s);
  }

  sub explain { "<recursive>" }
}


# phi baseline grammar
# This defines enough to drop into a phi execution context and do everything
# else from there.
package phi;
use JSON;

use constant je => JSON->new->allow_nonref(1)->convert_blessed(1);

sub str($)   { phi::parser::str->new(shift) }
sub one()    { phi::parser::one->new }
sub oneof($) { phi::parser::oneof->new(shift) }
sub maybe($) { phi::parser::repeat->new(shift, 0, 1) >> 0 }


use constant
{
  # Toplevel expressions: for now just a placeholder
  expr       => phi::parser::forward->new,
  parse_expr => phi::parser::forward->new,

  # Helpful components
  maybe_negative => maybe(str '-') >> sub {defined shift ? -1 : 1},

  qq_escape      => str("\\") + ( str("n")  >> sub {"\n"}
                                | str("r")  >> sub {"\r"}
                                | str("t")  >> sub {"\t"}
                                | str("\\") >> sub {"\\"}
                                | str("\"") >> sub {"\""} ) >> 1,

  whitespace => oneof("\n\t\r ") x 1,
  op_string  => oneof('+-*/&|<>=^%!~'),
};

sub wsi($) { maybe(whitespace) + $_[0] + maybe(whitespace) >> 0 >> 1 }

use constant
{
  unop_token  => maybe(whitespace) + op_string x 1
              >> 1 >> sub {join"", @{$_[0]}},
  binop_token => maybe(whitespace) + op_string x 1 + maybe(whitespace)
              >> 0 >> 1 >> sub {join"", @{$_[0]}},
};

use constant
{
  # Basic value literals
  int_hex_matcher => maybe_negative + str("0x") + oneof('0123456789abcdefABCDEF') x 1,
  int_oct_matcher => maybe_negative + str("0")  + oneof('01234567') x 1,
  int_dec_matcher => maybe_negative             + oneof('0123456789') x 1,

  qq_matcher      => str('"') + ( qq_escape
                                | !oneof("\\\"") + one >> 1 ) x 0 + str('"')
                     >> 0 >> 1,

  # Value constructors
  list_matcher =>
    (str("[") + (expr + maybe(whitespace) + str(",") >> 0 >> 0) x 0 >> 1)
    +    (maybe(expr) + maybe(whitespace) + str("]") >> 0 >> 0),
};

use constant
{
  # Parsed basic values
  int_hex => int_hex_matcher >> sub {$_[0]->[0][0] * hex join"", @{$_[0]->[1]}},
  int_oct => int_oct_matcher >> sub {$_[0]->[0][0] * oct join"", @{$_[0]->[1]}},
  int_dec => int_dec_matcher >> sub {$_[0]->[0]    *     join"", @{$_[0]->[1]}},

  qq_str  => qq_matcher >> sub {join "", map ref ? $$_[1] : $_, @{$_[0]}},

  # Parsed constructors
  list => list_matcher
    >> sub {['list', @{$_[0]->[0]}, defined $_[0]->[1] ? ($_[0]->[1]) : ()]},
};

use constant
{
  # Parser-specific derivatives
  parse_qq_str => qq_str >> \&str,
};

use constant
{
  atom => maybe(whitespace)
        + ( int_hex
          | int_oct
          | int_dec
          | qq_str
          | list
          | str('(') + expr + str(')') >> 0 >> 1) >> 1,

  parse_atom => maybe(whitespace)
              + ( parse_qq_str
                | str('(') + parse_expr + str(')') >> 0 >> 1) >> 1,
};

use constant
{
  # Operations
  unop_matcher  => unop_token + expr,
  binop_matcher => atom + binop_token + expr,
  call_matcher  => atom + maybe(whitespace) + expr,

  # Parser operations
  parse_maybe_matcher => parse_atom + str('?') >> 0,
  parse_rep1_matcher  => parse_atom + str('+') >> 0,
  parse_rep0_matcher  => parse_atom + str('*') >> 0,
  parse_seq_matcher   => parse_atom + maybe(whitespace) + parse_expr,
  parse_alt_matcher   => parse_atom + str('|') + parse_expr,

  # Parser construction
  parse_matcher => str('parse(') + parse_expr + str(')') >> 0 >> 1,
};

use constant
{
  unop  => unop_matcher,
  binop => binop_matcher >> sub {[$_[0]->[0][1], $_[0]->[0][0], $_[0]->[1]]},
  call  => call_matcher  >> sub {['()', $_[0]->[0][0], $_[0]->[1]]},

  parse_maybe => parse_maybe_matcher >> \&maybe,
  parse_rep1  => parse_rep1_matcher  >> sub {phi::parser::repeat->new($_[0], 1)},
  parse_rep0  => parse_rep0_matcher  >> sub {phi::parser::repeat->new($_[0], 0)},

  parse_alt   => parse_alt_matcher >> sub {$_[0]->[0][0] | $_[0]->[1]},
  parse_seq   => parse_seq_matcher >> sub {$_[0]->[0][0] + $_[0]->[1]},
};

parse_expr->set(
  maybe(whitespace) + ( parse_maybe
                      | parse_rep1
                      | parse_rep0
                      | parse_alt
                      | parse_seq
                      | parse_atom ) >> 1);

expr->set(
  maybe(whitespace) + ( binop
                      | unop
                      | call
                      | parse_matcher
                      | atom ) >> 1);
