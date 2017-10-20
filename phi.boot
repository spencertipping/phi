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
                   >    p_flatmap
                   +    p_seq
                   x    p_repeat
                   !    p_not

                   bool op_bool
                   ""   explain /;

  sub p_alt     { phi::parser::alt    ->new(@_[0, 1]) }
  sub p_map     { phi::parser::map    ->new(@_[0, 1]) }
  sub p_flatmap { phi::parser::flatmap->new(@_[0, 1]) }
  sub p_seq     { phi::parser::seq    ->new(@_[0, 1]) }
  sub p_repeat  { phi::parser::repeat ->new(@_[0, 1]) }
  sub p_not     { phi::parser::not    ->new(@_[0, 1]) }

  sub op_bool   { 1 }

  sub TO_JSON   { shift->explain }
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
      until $s->error or @states >= $$self{min};
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
    "$$self{parser} >> function";
  }
}


package phi::parser::flatmap
{
  use parent 'phi::parser';

  sub new
  {
    my ($class, $p, $f) = @_;
    bless {parser => $p,
           f      => $f}, $class;
  }

  sub parse
  {
    my ($self, $s) = @_;
    my $r = $$self{parser}->parse($s);
    $r->error
      ? $r
      : $r->value->parse($r);
  }

  sub explain
  {
    my ($self) = @_;
    "$$self{parser} > function";
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
  unop_token  => wsi(op_string x 1) >> sub {join"", @{$_[0]}},
  binop_token => wsi(op_string x 1) >> sub {join"", @{$_[0]}},
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
  binop_matcher => atom + (binop_token + atom) x 1,

  # Parser operations
  parse_maybe_matcher => parse_atom + wsi(str '?') >> 0,
  parse_rep1_matcher  => parse_atom + wsi(str '+') >> 0,
  parse_rep0_matcher  => parse_atom + wsi(str '*') >> 0,
  parse_seq_matcher   => parse_atom + maybe(whitespace) + parse_expr,
  parse_alt_matcher   => parse_atom + wsi(str '|') + parse_expr,

  # Parser construction
  parse_matcher => wsi(str 'parse(') + parse_expr + wsi(str ')') >> 0 >> 1,
};

use constant
{
  # Parsed ops/calls
  unop  => unop_matcher,
  binop => binop_matcher >> sub {[$_[0]->[0][1], $_[0]->[0][0], $_[0]->[1]]},

  # Parsed parser-construction constructs
  parse_maybe => parse_maybe_matcher >> \&maybe,
  parse_rep1  => parse_rep1_matcher  >> sub {phi::parser::repeat->new($_[0], 1)},
  parse_rep0  => parse_rep0_matcher  >> sub {phi::parser::repeat->new($_[0], 0)},
  parse_alt   => parse_alt_matcher   >> sub {$_[0]->[0][0] | $_[0]->[1]},
  parse_seq   => parse_seq_matcher   >> sub {$_[0]->[0][0] + $_[0]->[1]},

  parse       => parse_matcher,
};

parse_expr->set(wsi( parse_seq
                   | parse_alt
                   | parse_maybe
                   | parse_rep1
                   | parse_rep0
                   | parse_atom ));

expr->set(wsi( binop_matcher
             | unop_matcher
             | parse
             | atom ));


=head1 State management
I think we need a few things:

1. Modifiable alts (can they just be lists?)
2. Localized edits to parsers


=head1 Operators and inheritance
Suppose arrays support some syntax like C<[1, 2, 3] match [x, y, z] in ...>;
ideally we define this in terms of the representational containers for a type,
rather than for the surface type itself. This means we'll want to do some type
of metaprogramming to define C<match>.

The question here is, do we create metaclasses as an indirect byproduct of
alt-inclusion? i.e. if arrays alt-include operators that apply to all values,
is this sufficient to do OOP?

One issue here is that we don't really have a way to do polymorphism with this
strategy. It might not be a problem if we encode an abstract op against a value
though. Technically that's a more accurate representation: monomorphic ops can
be inlined, whereas polymorphic ops require a runtime decision.


=head1 Type propagation
If we have a list of numbers, say C<[1,2,3]>, we can probably say
C<[1,2,3] + 1> to distribute. This requires the list to do some type inference
and runtime-delegate to the items.


=head1 Interactive parse states
Right now, we have a parser that converts strings to values directly (and
mostly works; see above note about operator precedence). But it would be great
if we had parsers that operated against live editor states so we could get
feedback per keystroke. I think this involves two things:

1. Parsers need to return results that track their original positions and parse
   states. In other words, we need to defer value extraction and have the parse
   step be itself lossless.

2. Parse states need to support early-exit so we can provide documentation and
   completions. This is mainly an issue for typing as it's happening, for
   instance for incomplete constructs.

This forces some things about how we treat continuations. For example, suppose
we're typing C<[1, 2, |>, where C<|> is the edit point. The obvious
continuation is to assume we'll get another C<]> to complete the list. In
parsing terms, we want to both leave an opening at the edit point and consume
future input using a reasonable continuation.

Maybe we just solve this using an early-exit-until-accepting arrangement: the
cursor lets you exit sequences without failing. Maybe the cursor suppresses all
errors; then we'd end up with a true list of alternatives. This is particularly
nice because all states that don't make it up to the cursor are implicitly
rejected.

Q: if we're returning multiple parses, how is this ambiguity represented? Maybe
we need a new amb() type.

Q: at what granularity do we memoize? If we have enough alternatives, the memo
table could become huge -- particularly for the continuation.

Q: can we optimize by treating offscreen content as a soft EOF? (Not really if
we need full data structure representation.)


=head1 Canard execution model
NB: not doing this; see below for reasons.

If we have parsers encoded as objects in canard, then we're at liberty to
serialize everything and use nested list structures. For example (in terms of
low-level lists):

  'alt defclass
  [$ 'parse *] 'parse alt defmethod
  [parse-state] [p1 p2 p3 alt-vtable] parse

The motivation for using canard is that it provides first-class continuation
support and a very flexible execution model. ...do we really want to go through
using it though? Do we need this type of low-level continuation management?

Ok, let's create a design principle: the execution model is not up for grabs.
If we need to do things like play with continuations, we do it by interpreting
a data structure. This means data needs to be code, which arguably is the whole
point.
