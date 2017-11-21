package phi::compiler;

use strict;
use warnings;

use phi::parseapi qw/:all/;


=head1 Parse elements
Basic syntax for phi values. This is enough to bootstrap the language.
=cut

use constant comment    => str('#') + oc(" \t") + me("\n");
use constant whitespace => ((comment + str("\n") | Mc("\n\t\r ")) * 1)->ignore;
use constant ident      => Mc 'a'..'z', 'A'..'Z', '_';
use constant method     => str('.')->ignore + ident;
use constant method_    => str('.#')->ignore + ident;
use constant string     => str('"')->ignore + me('"') + str('"')->ignore;
use constant integer    => str('-')->maybe + Mc(0..9)
                           >>sub { 0 + join"", @_[1..$#_] };


sub phi::parser::parser_base::spaced
{ phi::compiler::whitespace->maybe + shift() + phi::compiler::whitespace->maybe }

sub phi::parser::parser_base::parens
{ phi::compiler::str('(')->syntax + shift() + phi::compiler::str(')')->syntax }

sub phi::parser::parser_base::syntax { shift->spaced->ignore }


=head1 Quoted values
phi's compiler manipulates quoted values, which are structural representations
of things that happen at runtime. Here's an example:

  (x:int).inc = x.plus(1)

Breaking this down, C<x> is a map that looks like
C<< {val => "x", op => "constant", type => "unknown", syntax => 0} >>. Like all
values at this point, C<x> describes a runtime quantity but is materially just a
map. phi invokes C<x>'s C<.#parser> method, which returns a parser that consumes
C<:> followed by an expression.

C<int> is a map representing a parser. C<x:int> returns a new map describing a
parser map operation that binds C<x>. The compiler invokes the C<.#parser>
method on this map to see whether it specifies a parse continuation, which it
does but C<.inc> fails to match it so we proceed normally. (I'll explain the
C<#> syntax below.)

C<.inc> is a literal method call, which produces a map of the form
C<< {method => "inc", op => "method", val => x} >>. The parse continuation of
this form includes C<=>, so we parse that and the following value next. C<=>
also generates a unique parser ID used to track closure state.

C<x> is now bound; the parse continuation that consumed C<=> uses a derivative
scope that binds C<x> to a positional reference:

  { op        => "arg",
    type      => "int",
    position  => 0,
    parser_id => 4 }

Now C<=> uses C<.#parser_map> on the matching parser and the expression on the
right of C<=> to produce a new scope binding, which it then specifies in its
C<.#scope_continuation>.

=head2 C<#> method prefix
When you write C<.inc>, this is a stand-in for a regular runtime method: phi
isn't allowed to assume that C<.inc> has any magic; it needs to treat C<.inc>
strictly as an arbitrary symbol. But you can't specify an interpreter that way,
so we need methods with predefined behavior. That's what C<#> does:

  x.#parser       -> { op     => "method",
                       method => "#parser",
                       val    => {...} }

This is a method that phi will invoke and use at parse time. In other words,
you're now operating in a namespace that's shared with the phi interpreter.

You can (and need to) use interpreter methods to get low-level access to values.
For example, you can access value-maps as the maps that they are:

  x.#keys.head    -> { op   => "constant",
                       type => "string",
                       val  => "method" }

  x.#get "method" -> { op   => "constant",
                       type => "string",
                       val  => "#parser" }

The phi compiler is written as a series of parsers that rewrite C<#>-prefixed
methods. You can, of course, go more levels down, e.g. C<x.#keys.#keys>.

=head2 Parsing quoted values
These quoted maps are themselves combinatory parser inputs; positions are
specified by the number of C<val> dereferences we take. This means the parsers
technically work backwards: position 0 is the full expression, position 1 drops
the last one, and so forth.
=cut


package phi::compiler::value
{
  # Specific types of values
  sub constant { my ($c, $v, $t) = @_; $c->new(constant => $v, type   => $t) }
  sub method   { my ($c, $v, $m) = @_; $c->new(method   => $v, method => $m) }
  sub call     { my ($c, $v, $r) = @_; $c->new(call     => $v, rhs    => $r) }

  # Low-level constructor; you should usually use the shorthands above.
  sub new
  {
    my ($class, $op, $val, %keys) = @_;
    bless { op => $op, val => $val, %keys }, $class;
  }

  sub at
  {
    my ($self, $i) = @_;
    my $v = $self;
    return $self unless $i;
    return $$self{"#at_cache_$i"} if exists $$self{"#at_cache_$i"};
    $v = ref($v) ? $$v{val} : undef for 1..$i;
    $$self{"#at_cache_$i"} = $v;
  }
}


=head1 Structural parsers
These parsers create a sort of interpreted language that traverses structures.
We have the following instructions:

1. C<emit>: capture a value that's bound to a variable.
2. C<match_method(lhs, method)>: match a specific method call.
3. C<match_call(lhs, rhs)>: match a function call, emitting left values first.
4. C<match_constant(type, val)>
5. C<match_rewritten(lhs, method, r_parser)>

TODO: we probably need more than this

C<match_rewritten> is a way to call the evaluator from inside a parse. phi does
this in order to ask questions about value types using the C<#type> method.

For example, C<x:int.inc> is a structural parser that works like this (remember
that parsers go from right to left):

  match_method(
    match_rewritten(
      emit,
      "#type",
      match_constant("string", "int")),
    "inc")
=cut


package phi::compiler::value_parser_base
{
  use parent -norequire => 'phi::parser::parser_base';

  sub parse
  {
    my ($self, $input, $start, $scope) = @_;
    return $self->fail("not a phi value")
      unless ref($input) eq 'phi::compiler::value';
    $self->parse_val($input, $start, $scope);
  }
}


package phi::compiler::emit
{
  use parent -norequire => 'phi::compiler::value_parser_base';

  sub new { shift }
  sub parse_val
  {
    my ($self, $input, $start, $scope) = @_;
    my $v = $input->at($start);
    defined $v ? $self->return(0, $v)
               : $self->fail;
  }
}


package phi::compiler::match_method
{
  use parent -norequire => 'phi::parser::parser_base';

  sub new
  {
    my ($class, $p, $method) = @_;
    bless { parser => $p,
            method => $method }, $class;
  }

  sub parse_val
  {
    my ($self, $input, $start, $scope) = @_;
    my $v = $input->at($start);
    return $self->fail unless ref $v
                           && $$v{op}     eq 'method'
                           && $$v{method} eq $$self{method};

    my ($ok, $l, @xs) = $$self{parser}->parse($v->at(1), 0, $scope);
    $ok ? $self->return($l + 1, @xs)
        : $self->fail;
  }
}


package phi::compiler::match_call
{
  use parent -norequire => 'phi::parser::parser_base';

  sub new
  {
    my ($class, $lhs, $rhs) = @_;
    bless { lhs => $lhs,
            rhs => $rhs }, $class;
  }

  sub parse_val
  {
    my ($self, $input, $start, $scope) = @_;
    my $v = $input->at($start);
    return $self->fail unless ref $v && $$v{op} eq 'call';
    my ($rok, $rl, @ys) = $$self{rhs}->parse($$v{rhs}, 0, $scope);
    return $self->fail unless $rok;
    my ($lok, $ll, @xs) = $$self{lhs}->parse($$v{lhs}, 0, $scope);
    $lok ? $self->return($ll, @xs, @ys)
         : $self->fail;
  }
}


package phi::compiler::match_constant
{
  use parent -norequire => 'phi::parser::parser_base';

  sub new
  {
    my ($class, $type, $val) = @_;
    bless { type => $type,
            val  => $val }, $class;
  }

  sub parse_val
  {
    my ($self, $input, $start, $scope) = @_;
    my $v = $input->at($start);
    ref($v) && $$v{op}   eq 'constant'
            && $$v{type} eq $$self{type}
            && $$v{val}  eq $$self{val}
      ? $self->return(0)
      : $self->fail;
  }
}


package phi::compiler::match_rewritten
{
  use parent -norequire => 'phi::parser::parser_base';

  sub new
  {
    my ($class, $lhs, $method, $parser) = @_;
    bless { lhs    => $lhs,
            method => $method,
            parser => $parser }, $class;
  }

  sub parse_val
  {
    my ($self, $input, $start, $scope) = @_;
    my $v = $input->at($start);
    my ($rok, $rl, $r)
      = $scope->parse(phi::compiler::value->method($v, $$self{method}),
                      0,
                      $scope);

    return $self->fail unless $rok;
    $$self{lhs}->parse($v);
  }
}


=head1 Scopes
A scope is just an alt parser with a parent; however it's important to know that
a single scope can parse both text and data structures. Parsers are individually
typed and will reject inputs they weren't built to handle -- but the composite
stuff in the parser library (seq, alt, not, filter, flatmap, etc) is all
type-agnostic.
=cut


package phi::compiler::scope
{
  use parent -norequire => 'phi::parser::parser_base';

  sub new
  {
    my ($class, $parent, $parser) = @_;
    bless { parent => $parent,
            parser => $parser }, $class;
  }

  sub parse
  {
    my ($self, $input, $start, $scope) = @_;
    $scope //= $self;

    my ($ok, $l, @xs) = $$self{parser}->parse($input, $start, $scope);
    return $self->return($l, @xs) if $ok;

    defined $$self{parent}
      ? $$self{parent}->parse($input, $start, $scope)
      : $self->fail;
  }
}


1;
