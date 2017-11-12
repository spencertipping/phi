=head1 phi compiler
This is where we connect individual expressions together with scopes, blocks,
and struct compilers. The result is a very low-level syntax with lists,
functions, and working lexical scope.

Operator precedence parsing (and operator handling in general) is implemented as
a library within phi.

=head2 How IO is propagated
Every expression is allowed to use the IO monad and potentially be impure; we
detect this by passing in an abstract IO value and comparing the output to what
we sent in. There are two ways for IO to be updated:

1. In a statement context, we capture the "return IO" and forward it.
2. In an expression context, we thread the IO in evaluation order.

(2) means that you can have stuff like C<sum(print("foo"), print("bar"))> and
the print() statements are guaranteed to be ordered correctly. Internally we
propagate the IO from left to right:

  ($io1, $r1) = print($io0, "foo");
  ($io2, $r2) = print($io1, "bar");
  ($io3, $s)  = sum($io2, $r1, $r2);

Because a lot of functions are pure, the IOs they return are unchanged; this
means phi is at liberty to reorder their evaluation. You generally want this.

=head3 Propagation and parse/scope continuations
First, parse/scope continuations don't have access to an IO; you can't involve
the runtime IO at parse time. The (forced) assumption here is that compilation
is a pure function with no side effects, which is pretty much the only sane way
to do it.

What _does_ happen, though, is that scopes and parsers thread the IO as per the
above rules, and it's worth being aware of that. The protocol for interacting
with values is dictated by structs and involves three monomorphic calls against
the struct type:

  $p = $struct->parse_continuation($value, $scope);
  $s = $struct->scope_continuation($value, $scope);
  ($io, $x) = $struct->op($value, $io, $name, @args);

NB: op() isn't used for OOP methods! In particular, we're talking to a single
struct when we call it -- so the phi runtime has no obligation (nor ability) to
resolve the call polymorphically against an unknown type. This is all hammered
out for us by asking abstract values for their types to figure out which structs
we want to address. Usually this gets constant-folded and the call ends up being
monomorphic, but not always. It's up to backends to make this efficient.
=cut

package phi::compiler;

use strict;
use warnings;

use phi::syntax;


=head1 Core language elements
Management structures to parse basic things like sequences of statements with
local variable scoping. This scope class doesn't implement operator precedence,
hence the name. Operator precedence is implemented within phi as a new type of
scope.

Parsers are invoked with these arguments:

  parse(input, start, scope, io)

Parse results are always of the form C<(io, x)>. This is different from what we
get from primitives in C<phi::syntax>, hence the wrapping we do here.
=cut


sub pure(&)
{
  my ($f) = @_;
  sub {
    my ($input, $start, $l, $scope_and_io, @r) = @_;
    ($$scope_and_io[1], $f->($scope_and_io, @r));
  };
}


package phi::compiler::binding
{
  use parent -norequire => 'phi::parser::parser_base';

  sub new
  {
    my ($class, $name, $value) = @_;
    bless { name   => $name,
            value  => $value,
            parser => phi::syntax::var($name)
                        >>phi::compiler::pure { $value } }, $class;
  }

  sub parse   { shift->{parser}->parse(@_) }
  sub explain { "binding for " . shift->{name} }
}


package phi::compiler::nop_scope
{
  use Scalar::Util;
  use parent -norequire => 'phi::parser::parser_base';

  sub new
  {
    my ($class, $parent, $previous, @defs) = @_;
    my $self = bless { parent   => $parent,
                       previous => $previous,
                       defs     => \@defs,
                       atom     => undef,
                       expr     => undef }, $class;

    my $atom = phi::parser::alt_fixed->new(
                 @defs,
                 defined $previous ? $previous : (),
                 defined $parent   ? $parent   : ())
               ->fixedpoint(
                   sub {
                     my ($input, $offset, $l, $scope_and_io, $io, $value) = @_;
                     $value->class
                       ->parse_continuation($value)
                       ->parse($input, $offset, $$scope_and_io[0], $io);
                   });

    # Without op precedence, parens don't contribute much.
    Scalar::Util::weaken(my $weak = $self);
    my $expr = phi::syntax::de("(")->spaced
               + $weak
               + phi::syntax::de(")")->spaced >>phi::compiler::pure {
                   my ($scope_and_io, $p1, $v, $p2) = @_;
                   $v
                 }
             | $atom;

    $$self{atom} = $atom->spaced;
    $$self{expr} = $expr->spaced;
    $self;
  }

  sub parse
  {
    my ($self, $input, $start, undef, $io) = @_;
    $$self{expr}->parse($input, $start, $self, $io);
  }

  sub with_bindings
  {
    my ($self, %nvs) = @_;
    ref($self)->new($self->parent,
                    $self,
                    map phi::compiler::binding->new($_ => $nvs{$_}), keys %nvs);
  }

  sub child_with_bindings
  {
    my ($self, %nvs) = @_;
    ref($self)->new($self,
                    undef,
                    map phi::compiler::binding->new($_ => $nvs{$_}), keys %nvs);
  }

  sub previous { shift->{previous} }
  sub parent   { shift->{parent} }
  sub explain  { "nop scope" }
}


=head1 Blocks of statements
This is the parser used to parse series of statements, for instance the toplevel
or the body of a function. The important thing here is the way we propagate IO
and scopes.

A block doesn't maintain any state. That's all done by parse and scope
continuations inside the parsing function.
=cut

package phi::compiler::block
{
  use parent -norequire => 'phi::parser::parser_base';

  sub new
  {
    my ($class) = @_;
    bless \$class, $class;
  }

  sub parse
  {
    my ($self, $input, $start, $scope, $io) = @_;
    my $offset = $start;
    my @xs;
    for (my ($ok, $l, $rio, $r);
         ($ok, $l, $rio, $r) = $scope->parse($input, $offset, $scope, $io)
           and $ok;
         $offset += $l, $io = $rio)
    {
      push @xs, $r;
      $scope = $r->class->scope_continuation($r, $scope);
    }
    $self->return($offset - $start, $io, @xs);
  }

  sub explain { ref shift }
}


1;
