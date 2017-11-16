package phi::compiler;

use strict;
use warnings;

use phi::parseapi;
use phi::syntax;


=head1 Ops, ordering, and IO
The phi compiler is a parser that consumes code and produces an op graph. There
aren't very many core ops; scopes get erased by runtime, so ops apply directly
to values.

Ops are implemented as methods against an IO object, which is an opaque
representation of the state of some runtime. IOs are responsible for evaluating
as many ops as possible at compile time (e.g. compile-time file read or gensym),
then constructing a low-level op graph that gets consumed by a backend.

It's worth pointing out that separate ops exist for compile-time and runtime
side effects. This distinction is a semantic one, not just an
implementation/dependency detail; a good example is the difference between a
file open() for a runtime value vs for compile-time inclusion of code. We want
every runtime open() to be deferred, even if it's resolvable at compile time. In
other words, there are some ops we deliberately don't constant-fold because
they're only fictitiously constant: they close over a mutable environment.

=head2 Op sequencing
Ops refer to IO states. This is used to inform backends about sequence points:
ops sharing an IO can be reordered without changing any semantics. Ops of a
lesser IO must be evaluated first.
=cut


package phi::compiler::op_base
{
  sub new
  {
    my ($class, $io, $type, @args) = @_;
    $io = $io->merge($_->io_continuation) for @args;
    my $self = bless { args => \@args,
                       io   => $io,
                       type => $type,
                       val  => undef }, $class;
    $$self{val} = $self->eval;
    $self;
  }

  sub type { shift->{type} }
  sub val  { shift->{val} }

  sub parse_continuation { $_[0]->{type}->parse_continuation(@_) }
  sub scope_continuation { $_[0]->{type}->scope_continuation(@_) }
}


package phi::compiler::pure_op_base
{
  use parent -norequire => 'phi::compiler::op_base';

  sub io_continuation { shift->{io} }
}


package phi::compiler::impure_op_base
{
  use parent -norequire => 'phi::compiler::op_base';

  sub io_continuation { shift->{io}->child }
}


package phi::compiler::io
{
  # IOs are fully-ordered quantities. This is used to determine time steps and
  # evaluation scheduling.
  use overload qw/ +0       order
                   fallback 1 /;

  sub new
  {
    my ($class, $order) = @_;
    $order //= 0;
    bless \$order, $class;
  }

  use constant invariant => phi::compiler::io->new(0);

  sub order { ${+shift} }
  sub child
  {
    my ($self) = @_;
    ref($self)->new($$self + 1);
  }

  sub merge
  {
    my ($self, $other) = @_;
    $self > $other ? $self : $other;
  }
}


=head1 Structs
Structs are central to a lot about how phi works, so it's worth talking a bit
about they interact with scopes. At a high level:

1. Structs are values that exist within lexical scopes.
2. Structs (as values) define parsers that parse literal values.
3. Structs don't themselves define methods.
4. Structs do define unknowns that create method bindings.
5. Structs behave as nominal types, not structural types.

=head2 Parsing literal values
Normally, values simply existing within a scope don't impact the way the scope
parses atoms/literals, and structs are no different. To add literal syntax,
structs add "bindings" that are custom parsers which consume literal syntax
elements and emit the corresponding abstracts. This works because scopes are
simply alternatives of arbitrary parsers.

=head2 Nominal typing
Structs behave like Java classes: the identity of the struct matters a lot more
than its internal representation. This is a departure from functional
programming sensibilities, but it makes sense from the method-resolution point
of view.

Structs acquire their identity using gensym() against the IO.

=head2 Hosted vs composite structs
In C, C<int> is different from C<struct x>. Both can produce values and have
defined sizes, but C<int> is an indivisible quantity: if you asked "why do ints
work this way," C would reply "they just do." In phi terms, C<int> is a "hosted
struct" and C<struct x> is a "composite struct." phi isn't responsible for the
semantics of hosted quantities; it's just responsible for forwarding method
calls reliably.

I'm avoiding terms like "primitive" for hosted structs because you might want to
use a hosted struct for very non-primitive values. For example, suppose you're
working with a class instance hosted by a JVM. The class could easily be (and
most likely is) user-defined; the JVM doesn't see it as a primitive type. But
phi would see it as a primitive because operations against it are irreducible
and probably have few semantic guarantees.

=head2 Union structs
Unions are how polymorphism happens. A union represents a value whose type phi
doesn't know at compile-time, and any method calls against it will be
polymorphic.
=cut


package phi::compiler::struct_base
{
  use overload qw/ "" name
                   |  union /;

  sub method_name
  {
    my ($self, $method) = @_;
    join ".", $self->name, $method;
  }

  sub union
  {
    my ($self, $rhs) = @_;
    $self eq $rhs
      ? $self
      : phi::compiler::struct_union->new($self, $rhs);
  }
}

package phi::compiler::struct_hosted
{
  use parent -norequire => 'phi::compiler::struct_base';

  sub new
  {
    my ($class, $name) = @_;
    bless { name => $name }, $class;
  }
}

package phi::compiler::struct_composite
{
  use parent -norequire => 'phi::compiler::struct_base';

  sub new
  {
    my ($class, $name, @fields) = @_;
    bless { name   => $name,
            fields => \@fields }, $class;
  }
}


# TODO: are unions really structs?
package phi::compiler::struct_union
{
  use parent -norequire => 'phi::compiler::struct_base';

  sub new
  {
    my ($class, @options) = @_;
    bless \@options, $class;
  }

  sub method_name
  {
    # TODO: die here?
    ...;
  }
}


package phi::compiler::struct_unknown
{
  # This is the type used for forward-referenced values. When they're
  # encountered, we won't have a type calculated for them, and we aren't going
  # to require the user to specify in every case.
  #
  # Unlike a union, an unknown won't insist that you use safe operations against
  # it. It's a placeholder that doesn't specify a parse continuation but that
  # also defers method invocations until we know the type of the underlying
  # quantity.
  use parent -norequire => 'phi::compiler::struct_base';
  use parent -norequire => 'phi::parser::delegate';
  use Scalar::Util;

  sub new
  {
    my ($class, $forward_ref) = @_;
    Scalar::Util::weaken $forward_ref;
    bless \$forward_ref, $class;
  }

  # Parse assignments into values whose scope continuation contains a new
  # binding. Assignments don't introduce sequence points because there isn't any
  # IO dependency (unless the values themselves involve one).
  sub parser
  {
    my ($class, $scope) = @_;
    phi::syntax::ident + phi::syntax::op('=')->spaced
                       + $scope
      >>sub {
          my ($input, $start, $l, $xs, $ident, $val) = @_;
          # TODO: return $val, but have its scope continuation bind it.
        };
  }
}


=head1 Scopes and bindings
Scopes bind both named values and struct methods. The latter means that structs
aren't themselves structural; they're nominal for the purposes of method
resolution, and each has a unique identity. This makes it possible for you to
cast to a structural equivalent to get a different set of methods for a value.

Because scopes themselves do method resolution, they also handle runtime
polymorphism by generating the necessary type dispatching logic. Depending on
whether the function is monomorphic or polymorphic, the call will be one of two
ops:

  fn_call(f, receiver, args...)   # static resolution (monomorphic)

  method_call(                    # runtime resolution (polymorphic)
    { type1: fn1,                 # type -> fn dispatch at call point
      type2: fn2, ... },
    receiver.class,               # abstract value for receiver type
    receiver,
    args...)

Note that because method calls receive a static dispatch table, you can't
lexically extend a class and then expect to have those extensions available
within a dynamically-scoped context (which is what you might intuitively expect
to happen). That is, this won't work:

  f = fn |x:some-type| x.new_method() end;            # (3) ...and isn't here!
  g = fn |x:some-type|
    some-type.new_method = fn |x:some-type| 5 end;    # (1) lexical method...
    f(x)                                              # (2) ends here...
  end

If you want to do things like this, you need to shift C<f> to be inside C<g> to
inherit its lexical scope; then the call to new_method() will have alternatives
that are aware of the binding.
=cut


package phi::compiler::scope
{
  use parent -norequire => 'phi::parser::parser_base';

  sub new
  {
    my ($class, $parent_scope,
                $previous_scope,
                $var_bindings,
                $syntax_bindings,
                $method_bindings) = @_;

    bless { parent   => $parent_scope,
            previous => $previous_scope,
            vars     => $var_bindings,
            syntax   => $syntax_bindings,
            methods  => $method_bindings }, $class;
  }

  sub parse
  {
    my ($self, $input, $start) = @_;
    my (undef, $li1) = phi::syntax::ignore->parse($input, $start);

  }
}


1;
