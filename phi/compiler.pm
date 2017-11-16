package phi::compiler;

use strict;
use warnings;

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
ops sharing an IO can be reordered without changing any semantics.
=cut


package phi::compiler::op_base
{
  sub new
  {
    my ($class, $io, @args) = @_;
    bless { args => \@args,
            io   => $io,
            val  => undef }, $class;
  }

  sub val
  {
    my ($self) = @_;
    $$self{val} //= $self->eval;
  }
}


package phi::compiler::io
{
  # IOs are fully-ordered quantities. This is used to determine time steps and
  # evaluation scheduling.
  use overload qw/ +0       order
                   fallback 1 /;

  # defruntimeop() defines an operation that is always deferred until runtime;
  # i.e. that cannot be constant folded ever. Examples are file IO, network
  # functions, random numbers, memory allocation, and other
  # obviously-not-compile-time things.
  #
  # TODO: runtime ops specify how to linearize their arguments.
  # TODO: runtime ops should build abstract return values.

  sub defruntimeop($)
  {
    my ($op) = @_;
    push @$phi::compiler::io::{$op}::{ISA}, 'phi::compiler::op_base';
    $phi::compiler::io::{$op} = eval "sub { phi::compiler::${op}_op->new(\@_) }";

    # TODO: evaluate each argument, maybe
    $phi::compiler::io::{$op}::eval = sub { shift };
  }


  # defop() defines an operation that might be resolvable at compile time. These
  # operations will be constant-folded unless they hold runtime dependencies.

  sub defop($&)
  {
    my ($op, $fn) = @_;
    push @$phi::compiler::io::{$op}::{ISA}, 'phi::compiler::op_base';
    $phi::compiler::io::{$op} = eval "sub { phi::compiler::${op}_op->new(\@_) }";
    $phi::compiler::io::{$op}::eval = $fn;
  }


  # Baseline op implementations
  # TODO: is it correct to have one op per syscall? Or do we want a "syscall" op
  # where the call itself is an argument?
  defruntimeop 'write';

  defop gensym => sub {
    my ($io, $suffix) = @_;
    $io->return("gensym_$${io}_$suffix");
  };


  sub new
  {
    my ($class, $order) = @_;
    $order //= 0;
    bless \$order, $class;
  }

  sub order { ${+shift} }
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
=cut


package phi::compiler::struct_base
{
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


1;
