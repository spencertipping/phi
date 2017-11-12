=head1 phi syntax elements
These are lex-level syntactic elements that are referred to by phi's builtin
types to construct a grammar.
=cut

package phi::syntax;

use strict;
use warnings;

use Exporter 'import';
our @EXPORT_OK;
our %EXPORT_TAGS = (all => \@EXPORT_OK);

use phi::parseapi ':all';


=head1 Syntactic element classes
Each syntactic element inherits from C<phi::syntax::syntax_base>. You can cast
into a subclass using C(>>one"subclass") or C(>>many"subclass"), which will
create the subclass and set up inheritance. (C<one> is used for single lex
tokens, C<many> is used for compound constructs that contain some amount of
parsing reduction.)
=cut

push @EXPORT_OK, qw/ as nth /;

sub as($)
{
  no strict 'refs';
  my $class = "phi::syntax::$_[0]";
  push @{"${class}::ISA"}, 'phi::syntax::syntax_base';
  sub { $class->new(@_) };
}

sub nth($)
{
  my $n = 3 + shift;
  sub { $_[$n] };
}

sub k($)
{
  my $v = shift;
  sub { $v };
}

package phi::syntax::syntax_base
{
  use overload qw/ @{} as_array /;

  sub new
  {
    my ($class, $input, $start, $length, @xs) = @_;
    bless { input  => $input,
            start  => $start,
            length => $length,
            xs     => \@xs }, $class;
  }

  sub as_array { shift->{xs} }
  sub flatten  { map ref ? $_->flatten : $_, @{shift->{xs}} }
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
  ignore => (whitespace | comment) * 0 >>sub {[@_[3..$#_]]};


=head1 Numeric literals
C-style integer and float literals. Unlike C, phi doesn't use size indicators;
but it does support the "u" suffix to indicate unsigned.

Hex and oct are provided by libraries.
=cut

use phi::syntaxconst
  digit_dec       => oc(0..9),
  maybe_negative  => str("-")->maybe,
  unsigned_marker => str("u");

use phi::syntaxconst
  literal_int64_dec  => maybe_negative + digit_dec->repeat(1, 19),
  literal_uint64_dec => digit_dec->repeat(1, 19) + unsigned_marker;

use phi::syntaxconst
  literal_uint64 => literal_uint64_dec,
  literal_sint64 => literal_int64_dec;

use phi::syntaxconst
  literal_int64 => (literal_uint64 | literal_sint64)
                   >> sub { 0 + join"", $_[3]->flatten };

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
    + float_exponent->maybe
    >> sub { 0 + join"", map $_->flatten, @_[3..$#_] };


=head1 String literals
A conservative set of literals used to encode strings. More types of string
literals are provided by libraries.
=cut

use phi::syntaxconst
  string_escape_lf     => str("n")  >>k"\n",
  string_escape_tab    => str("t")  >>k"\t",
  string_escape_cr     => str("r")  >>k"\r",
  string_escape_dquote => str("\"") >>k"\"",
  string_escape_bs     => str("\\") >>k"\\",
  string_escape_hex    =>
    str("x") + digit_hex + digit_hex >> sub { chr hex $_[4].$_[5] };

use phi::syntaxconst
  string_hardescape => str("\\") + string_escape_bs >>nth(1),
  string_softescape => str("\\") + (  string_escape_lf
                                    | string_escape_tab
                                    | string_escape_cr
                                    | string_escape_bs
                                    | string_escape_hex) >>nth(1);

use phi::syntaxconst
  string_softbody => (Me("\\\"") | str("\\\"") >>k"\"" | string_softescape) * 0,
  string_hardbody => (Me("\\'")  | str("\\'")  >>k"'"  | string_hardescape) * 0;

use phi::syntaxconst
  literal_softstring => str("\"") + string_softbody + str("\"") >>nth(1),
  literal_hardstring => str("'")  + string_hardbody + str("'")  >>nth(1);


=head1 Parser-related literals
In this case, just character classes.
=cut

use phi::syntaxconst
  charclass_one => string_softescape | str("\\]") >>k"]" | oe("]");

use phi::syntaxconst
  charclass_range => charclass_one + str("-") + charclass_one
                     >>sub { map chr, ord($_[3]->[0])..ord($_[5]->[0]) };

use phi::syntaxconst
  literal_charclass => str("[") + (charclass_one | charclass_range) * 0
                                + str("]")
                       >>sub { join"", map $_->flatten, @_[4..$#_-1] };


=head1 Symbols
Bound symbols are parsed as scope alternatives, which makes it possible for you
to both define and refer to symbols whose names aren't identifiers.

Unbound symbols, however, use a more constrained syntax.
=cut

use phi::syntaxconst
  ident => Mc(grep /\w/, map chr, 0..65535) + mc("'") >>sub { join"", @_[3, 4] };


=head1 General operators and delimiters
If you use these functions, syntax highlighting and some editor semantics will
be taken care of for you.
=cut

push @EXPORT_OK, qw/ de op var /;

sub de($)  { str(shift) >>as"delimiter" }
sub op($)  { str(shift) >>as"operator" }
sub var($) { str(shift) >>as"variable" }


=head1 Whitespace/comment wrapping
This is such a common thing to do that we add a ->spaced method to parsers.
=cut

package phi::parser::parser_base
{
  sub spaced
  {
    phi::syntax::ignore + shift() + phi::syntax::ignore >>phi::syntax::nth(1);
  }
}


1;
