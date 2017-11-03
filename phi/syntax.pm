=head1 phi syntax elements
These are lex-level syntactic elements that are referred to by phi's builtin
types to construct a grammar.
=cut

package phi::syntax;

use strict;
use warnings;

use phi::parseapi ':all';

use Exporter 'import';
our @EXPORT_OK;
our %EXPORT_TAGS = (all => \@EXPORT_OK);


=head1 Syntactic element classes
Each syntactic element inherits from C<phi::syntax::syntax_base>. You can cast
into a subclass using C(>>one"subclass") or C(>>many"subclass"), which will
create the subclass and set up inheritance. (C<one> is used for single lex
tokens, C<many> is used for compound constructs that contain some amount of
parsing reduction.)
=cut

sub as($)
{
  no strict 'refs';
  my $class = "phi::syntax::$_[0]";
  push @{"${class}::ISA"}, 'phi::syntax::syntax_base';
  sub { $class->new(@_) };
}

package phi::syntax::syntax_base
{
  use overload;

  sub new
  {
    my ($class, $input, $start, $length, @xs) = @_;
    bless { input  => $input,
            start  => $start,
            length => $length,
            xs     => \@xs }, $class;
  }
}

package phi::syntaxconst
{
  BEGIN { ++$INC{'phi/syntaxconst.pm'} }

  sub import
  {
    my ($self, %kvs) = @_;
    push @phi::syntax::EXPORT_OK, keys %kvs;
    for (keys %kvs)
    {
      no strict 'refs';
      my $parser = $kvs{$_}->map(phi::syntax::as $_);
      *{"phi::syntax::$_"} = sub() { $parser };
    }
  }
}


=head1 Non-value elements
Whitespace, comments, and other things that are lexed and ignored.
=cut

push @EXPORT_OK, qw/ beginning_of_line /;
use constant beginning_of_line => sub
{
  my ($input, $start) = @_;
  $start == 0 or $input->substr($start - 1, 1) eq "\n";
};

use phi::syntaxconst
  whitespace          => Mc(" \n\r\t"),
  comment_pod_heading => me("\n") + str("\n");

use phi::syntaxconst
  comment_shebang     => str("#!") + me("\n"),
  comment_line        => str("#")  + me("\n"),

  comment_pod_start   => str("=") % beginning_of_line
                       + Me(" \n\r\t")
                       + (whitespace + comment_pod_heading)->maybe,

  comment_pod_line    => oe("=") % beginning_of_line
                       + me("\n")
                       + str("\n"),

  comment_pod_end     => (str("=cut") | str("=end")) % beginning_of_line;

use phi::syntaxconst
  comment_pod => comment_pod_start
               + (~!comment_pod_end
                  + (comment_pod_start | comment_pod_line)) * 0
               + comment_pod_end;

use phi::syntaxconst
  comment => comment_shebang
           | comment_line
           | comment_pod;

use phi::syntaxconst
  ignore => (whitespace | comment) * 0;


=head1 Primitive literals
Strings and numeric literals of various sorts.
=cut

use phi::syntaxconst
  digit_hex       => oc(0..9, "a".."f", "A".."F"),
  digit_oct       => oc(0..7),
  digit_dec       => oc(0..9),
  maybe_negative  => str("-")->maybe,
  unsigned_marker => str("u");

use phi::syntaxconst
  literal_int64_hex  => maybe_negative + str("0x") + digit_hex->repeat(1, 16),
  literal_int64_oct  => maybe_negative + str("0")  + digit_oct->repeat(1, 22),
  literal_int64_dec  => maybe_negative +             digit_dec->repeat(1, 19),

  literal_uint64_hex => str("0x") + digit_hex->repeat(1, 16) + unsigned_marker,
  literal_uint64_oct => str("0")  + digit_oct->repeat(1, 22) + unsigned_marker,
  literal_uint64_dec =>             digit_dec->repeat(1, 19) + unsigned_marker;

use phi::syntaxconst
  literal_uint64 => literal_uint64_hex | literal_uint64_oct | literal_uint64_dec,
  literal_sint64 => literal_int64_hex  | literal_int64_oct  | literal_int64_dec;

use phi::syntaxconst
  literal_int64 => literal_uint64 | literal_sint64;

use phi::syntaxconst
  float_integer_part    => Mc(0..9),
  float_fractional_part => Mc(0..9),
  float_exponent        => oc('eE') + oc('-+')->maybe + digit_dec->repeat(1, 3),
  float_decimal         => str('.');

use phi::syntaxconst
  literal_float =>
      maybe_negative
    + (  float_integer_part + float_decimal + float_fractional_part->maybe
       |                      float_decimal + float_fractional_part)
    + float_exponent->maybe;

# TODO: strings
