package phi::compiler;

use strict;
use warnings;

use phi::syntax;


=head1 Ops, ordering, and IO
The phi compiler is a parser that consumes code and produces an op graph. There
aren't very many core ops; scopes get erased by runtime (except for polymorphic
struct methods; see below), so ops apply directly to values.

Ops are implemented as methods against an IO object, which is an opaque
representation of the state of some runtime. There are normally two IOs you'll
work with: the compile-time IO, which is used to do impure stuff like including
other files or calling gensym(), and the runtime IO used for the usual set of
side effects. The IO isn't threaded through any parse states; evaluation
ordering is dictated by the ops themselves.

Q: are there two IOs really, or is it one IO that can do a subset of things at
compile-time and delegates the rest?
=cut


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

Structs acquire their identity using gensym() against the compile IO.
=cut


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
    scope,                        # the scope we should use to resolve methods
    receiver.class,               # abstract value for receiver type
    receiver,
    args...)

Note that because method calls receive a static scope reference, you can't
lexically extend a class and then expect to have those extensions available
within a dynamically-scoped context (which is what you might intuitively expect
to happen). That is, this won't work:

  f = fn |x:some-type| x.new_method() end;            # (3) ...and isn't here!
  g = fn |x:some-type|
    some-type.new_method = fn |x:some-type| 5 end;    # (1) lexical method...
    f(x)                                              # (2) ends here...
  end

If you want to do things like this, you need to shift C<f> to be inside C<g> to
inherit its lexical scope; then the call to new_method() will be a closure
reference.
=cut


1;
