package phi::compiler;

use strict;
use warnings;

use phi::parseapi;
use phi::syntax;


=head1 Abstract values
A phi value has three fundamental properties:

1. A type
2. An IO state
3. A construction path

(3) informs (1). If a construction path involves a runtime barrier (IO
dependency), then the value is indeterminate; otherwise the value can be
constant-folded because it will be fully determined at compile time. There are
times when it's useful to introduce fictitious runtime barriers; for instance,
if the purpose of a program is to calculate a constant expression but to defer
until runtime to do so.

=head2 Abstract value protocol
Abstract values are stand-ins for runtime expressions, which means they exist in
various states of defined-ness. The easiest way to explain how they work is to
through an example.

  x = 5;          # abstract_const(int => 5)
  y = random();   # abstract_call(random, [])
  x.plus(y);      # abstract_call(int.plus, x, y)
  if (x) {        # abstract_if(x, abstract_call(string.print, [y]))
    y.print();
  }

phi's C<abstract_if> tries to evaluate conditions that are constants, which C<x>
happens to be. So the if-block will be folded down to C<y.print()>. This is a
constant-space operation, which means the compiler won't leak memory while
evaluating arbitrarily involved nested conditionals -- however, it will probably
be fairly slow.

Abstracts need to provide the following methods:

1. type(): an abstract containing a type, usually IO-independent
2. io_r(): true if the value reads from the runtime IO (i.e. isn't constant)
3. io_w(): true if the value impacts the runtime IO (i.e. isn't pure)
4. children(): an array of nodes referred to by this one
5. with_children(@cs): a new node whose children have been modified

They also need to inherit from C<phi::compiler::abstract_base>.

Abstracts with C<io_w> create sequence points, and abstracts with C<io_r> won't
be constant-folded. C<io_w> almost implies C<io_r>; the only exceptions are
things like pure-passthrough assignments to mutables, for example:

  y = (x := <expr>);  # := is io_w and io_r, but y is whatever <expr> is

In this case, C<abstract_mutable_assign> can return C<io_r = 0> and C<io_w = 1>
to indicate that its return value is known but that it still creates a
side-effect.

Children are accessed during rewriting operations, for instance when inlining
function calls. The interface is generalized beyond rewriting because we might
want to apply nonlinear optimization parsers to the graph structure.

=head2 Types and abstract constants
C<abstract_const> is used for primitive types. Structs are always represented in
terms of their members; the struct as a whole isn't a constant, though all of
its members might be.

=head2 Functions and closures
First, an example without closure state:

  f = fn (x:int) x.inc() end;
  f(5).print()

Here, C<f> will be encoded like this:

  f = abstract_fn(n,                              # n = the function ID
        abstract_const(struct => []),             # no closure state
        abstract_monomorphic_call(
          int.inc,
          [abstract_nth(abstract_arg(n), 0)]))    # first arg of function N

Now with closure state:

  f = fn (x:int) fn (y:int) x.plus(y) end end;
  g = f(5);
  g(6)

  f = abstract_fn(n1,
    [],                                           # no closure state
    abstract_fn(n2,
      [abstract_arg(n1, 0)],                      # closure state
      abstract_monomorphic_call(
        int.plus,
        [abstract_nth(
           abstract_closure(n2,
             abstract_const(struct =>
               struct_tuple([int]))),
           0),                                    # refer to closed val 0
         abstract_nth(abstract_arg(n2,
                        abstract_const(struct =>
                          abstract_tuple([int])))
           0)])))                                 # refer to incoming arg 0

Normally closures would involve memory allocation, but they can be
constant-folded away in the case of monomorphic invocation. In this case C<g>
can be specialized by inlining its reference to C<5>:

  g = abstract_fn(n2,
    [],                               # no closure state now
    abstract_monomorphic_call(
      int.plus,
      [abstract_const(int, 5),        # specialized closure val here
       abstract_nth(abstract_arg(n2, abstract_const(struct => int)), 0)]))

...and similarly, since the invocation of C<g> is itself monomorphic, we can
inline that as well:

  g(6) = abstract_monomorphic_call(
    int.plus,
    [abstract_const(int, 5),
     abstract_const(int, 6)])

...finally, C<int.plus> can be applied at compile time to produce
C<abstract_const(int, 11)>.
=cut


package phi::compiler::abstract_base
{
  use overload qw/ "" explain /;

  sub is_mutable { 0 }

  sub explain
  {
    my ($self) = @_;
    my $ref  = ref($self) =~ s/^phi::compiler:://r;
    my $io_r = $self->io_r ? "R" : "";
    my $io_w = $self->io_w ? "W" : "";
    my $type = $self->type;
    my $args = join ", ", $self->children;
    "$ref($args) $io_r$io_w : $type";
  }

  sub rewrite_arg
  {
    my ($self, $fn_id, $arg_val) = @_;
    $self->with_children(map $_->rewrite_arg($fn_id, $arg_val),
                             $self->children);
  }

  sub rewrite_closure
  {
    my ($self, $fn_id, $arg_val) = @_;
    $self->with_children(map $_->rewrite_closure($fn_id, $arg_val),
                             $self->children);
  }

  sub val
  {
    my ($self) = @_;
    $self->io_r
      ? die "can't flatten IO-dependent value $self"
      : die "IO-independent value $self failed to implement val()";
  }
}


package phi::compiler::abstract_compile_error
{
  use parent -norequire => 'phi::compiler::abstract_base';

  sub new
  {
    my ($class, $message, $class, @args) = @_;
    bless { message => $message,
            class   => $class,
            args    => \@args }, $class;
  }

  sub type
  {
    my ($self) = @_;
    phi::compiler::abstract_error->new(
      "compile error value $self has no type",
      'phi::compiler::abstract_typeof',
      $self);
  }

  sub io_r     { 1 }
  sub io_w     { 1 }
  sub children { @{shift->{args}} }
  sub with_children
  {
    # NB: not fixed under type because rewriting is how compile errors are
    # resolved.
    my ($self, @cs) = @_;
    $$self{class}->new(@cs);
  }
}


package phi::compiler::abstract_const
{
  use parent -norequire => 'phi::compiler::abstract_base';

  sub new
  {
    my ($class, $type, $val) = @_;
    die "$type isn't a primitive type (this is a misuse of abstract_const)"
      unless $type->is_primitive;
    bless { type => $type,
            val  => $val }, $class;
  }

  sub io_r          { 0 }
  sub io_w          { 0 }
  sub type          { shift->{type} }
  sub val           { shift->{val}  }
  sub children      { () }
  sub with_children { shift }

  sub explain { "c:" . shift->{val} }
}


package phi::compiler::abstract_if
{
  use parent -norequire => 'phi::compiler::abstract_base';

  sub new
  {
    my ($class, $cond, $then, $else) = @_;

    return phi::compiler::abstract_compile_error->new(
      "if condition must be an integer (got $cond_type)",
      @_)
    if $cond_type->io_r || !$cond_type->val->is_int;

    return $cond->val ? $then : $else unless $cond->io_r;

    my $io_w = $cond->io_w || $then->io_w || $else->io_w;
    bless { cond => $cond,
            then => $then,
            else => $else,
            io_w => $io_w }, $class;
  }

  sub io_r          { 1 }
  sub io_w          { shift->{io_w} }
  sub children      { my ($self) = @_; @$self{'cond', 'then', 'else'} }
  sub with_children { ref(shift)->new(@_) }

  sub type
  {
    my ($self) = @_;
    phi::compiler::type_union->new($$self{then}->type, $$self{else}->type);
  }
}


package phi::compiler::abstract_fn
{
  use parent -norequire => 'phi::compiler::abstract_base';

  sub new
  {
    my ($class, $id, $arg_type, $closure, $expr) = @_;

    return phi::compiler::abstract_compile_error->new(
      "function argument type $arg_type is IO-dependent",
      @_)
    if $arg_type->io_r;

    my $expanded_expr = $expr->rewrite_closure($id, $closure);

    my $type = phi::compiler::struct_fn->new($arg_type->val,
                                             $expanded_expr->type);

    # NB: we can't rewrite closure state at compile time; if we did, we'd have
    # no way to efficiently compile generalized functions-with-closures. We also
    # can't store the expanded expression because it will require an exponential
    # amount of space.
    bless { id       => $id,
            arg_type => $arg_type,
            closure  => $closure,
            expr     => $expr,
            type     => $type }, $class;
  }

  sub type     { shift->{type} }
  sub io_r     { 0 }
  sub io_w     { 0 }
  sub children { @{+shift}{'arg_type', 'closure', 'expr'} }
  sub with_children
  {
    my ($self, $arg_type, $closure, $expr) = @_;
    ref($self)->new($$self{id},
                    $arg_type,
                    $closure,
                    $expr);
  }

  sub val          { shift }
  sub expr         { shift->{expr} }
  sub return_type  { shift->{expr}->type }
  sub closure_type { shift->{closure}->type }
  sub arg_type     { shift->{arg_type}->val }

  sub inline
  {
    my ($self, $arg) = @_;
    $$self{expr}->rewrite_closure($$self{id}, $$self{closure})
                ->rewrite_arg($$self{id}, $arg);
  }
}


package phi::compiler::abstract_arg
{
  use parent -norequire => 'phi::compiler::abstract_base';

  sub new
  {
    my ($class, $fn_id, $fn_arg_type) = @_;
    return phi::compiler::abstract_compile_error->new(
      "fn arg type $fn_arg_type has a runtime IO dependency",
      @_)
    if $fn_arg_type->io_r;

    bless { type_abstract => $fn_arg_type,
            fn_id         => $fn_id }, $class;
  }

  sub type     { shift->{type_abstract}->val }
  sub children { shift->{type_abstract} }
  sub with_children
  {
    my ($self, $type_abstract) = @_;
    ref($self)->new($$self{fn_id}, $type_abstract);
  }

  # NB: accessing an arg is never side-effectful, but we don't yet know whether
  # the arg's value was constructed with an IO dependency. At this point we have
  # to be general about it, so io_r is set to force the arg not to be constant
  # folded.
  sub io_r { 1 }
  sub io_w { 0 }

  sub rewrite_arg
  {
    my ($self, $fn_id, $arg_val) = @_;
    $fn_id == $$self{fn_id} ? $arg_val : $self;
  }
}


package phi::compiler::abstract_closure
{
  use parent -norequire => 'phi::compiler::abstract_base';

  sub new
  {
    my ($class, $fn_id, $fn_closure_type) = @_;
    return phi::compiler::abstract_compile_error(
      "cannot use runtime-variant type $fn_arg_type as a function closure",
      @_)
    if $fn_closure_type->io_r;

    bless { type_abstract => $fn_closure_type,
            fn_id         => $fn_id }, $class;
  }

  sub type          { shift->{type_abstract}->val }
  sub children      { () }
  sub with_children { shift }

  # NB: IO dependent for the same reasons that abstract_args are.
  sub io_r { 1 }
  sub io_w { 0 }

  sub rewrite_closure
  {
    my ($self, $fn_id, $closure_val) = @_;
    $fn_id == $$self{fn_id} ? $closure_val : $self;
  }
}


package phi::compiler::abstract_call
{
  use parent -norequire => 'phi::compiler::abstract_base';

  sub new
  {
    my ($class, $fn, $arg) = @_;
    my $io_w = $fn->io_w || $arg->io_w;
    $fn->io_r ? bless { fn   => $fn,
                        arg  => $arg,
                        io_w => $io_w }, $class
              : $fn->val->inline($arg);
  }

  sub type          { shift->{fn}->return_type }
  sub io_r          { 1 }
  sub io_w          { shift->{io_w} }
  sub children      { @{+shift}{'fn', 'arg'} }
  sub with_children { ref(shift)->new(@_) }
}


package phi::compiler::abstract_typeof
{
  use parent -norequire => 'phi::compiler::abstract_base';

  sub new
  {
    my ($class, $val) = @_;
    $val->type;
  }
}


package phi::compiler::abstract_instantiate
{
  use parent -norequire => 'phi::compiler::abstract_base';

  sub new
  {
    my ($class, $type, @vals) = @_;

    return phi::compiler::abstract_compile_error->new(
      "can't instantiate struct of IO-dependent type $type",
      @_)
    if $type->io_r;

    my $io_w = 0;
    my $union_type = $vals[0]->type;
    for (0..$#vals)
    {
      my $v = $vals[$_];
      $io_w      ||= $v->io_w;
      $union_type |= $v->type;
    }

    bless { vals       => \@vals,
            type       => $type->val,
            io_w       => $io_w,
            union_type => $union_type }, $class;
  }

  sub type     { shift->{type} }
  sub io_r     { 0 }
  sub io_w     { shift->{io_w} }
  sub children { @{shift->{vals}} }
  sub with_children
  {
    my ($self, @cs) = @_;
    ref($self)->new($$self{type}, @cs);
  }

  sub union_type { shift->{union_type} }
  sub type_at
  {
    my ($self, $n) = @_;
    $n->io_r ? $self->union_type
             : $self->type->at($n->val);
  }

  sub val { shift }
  sub at
  {
    my ($self, $n) = @_;
    $$self{vals}->[$n];
  }
}


package phi::compiler::abstract_index
{
  use parent -norequire => 'phi::compiler::abstract_base';

  sub new
  {
    my ($class, $aggregate, $index) = @_;

    return phi::compiler::abstract_compile_error(
      "cannot take indexed element of non-aggregate $aggregate",
      @_)
    unless $aggregate->type->is_indexed;

    return $aggregate->val->at($index->val)
      unless $aggregate->io_r || $index->io_r;

    my $type = $aggregate->type_at($index);
    my $io_w = $aggregate->io_w || $index->io_w;
    bless { aggregate => $aggregate,
            index     => $index,
            type      => $type,
            io_w      => $io_w }, $class;
  }

  sub type          { shift->{type} }
  sub io_r          { 1 }
  sub io_w          { shift->{io_w} }
  sub children      { @{+shift}{'aggregate', 'index'} }
  sub with_children { ref(shift)->new(@_) }
}


package phi::compiler::abstract_mutable
{
  use parent -norequire => 'phi::compiler::abstract_base';

  sub new
  {
    my ($class, $type) = @_;
    return phi::compiler::abstract_compile_error->new(
      "can't create mutable value of IO-dependent type $type",
      @_)
    if $type->io_r;

    bless { type => $type->val }, $class;
  }

  sub type          { shift->{type} }
  sub io_r          { 1 }
  sub io_w          { 0 }
  sub children      { shift->{type} }
  sub with_children { ref($self)->new(@_) }
  sub is_mutable    { 1 }
}


package phi::compiler::abstract_mutable_assign
{
  use parent -norequire => 'phi::compiler::abstract_base';

  sub new
  {
    my ($class, $mutable, $val) = @_;
    return phi::compiler::abstract_compile_error->new(
      "assignment of $val into non-mutable $mutable",
      @_)
    unless $mutable->is_mutable;

    bless { mutable => $mutable,
            val     => $val }, $class;
  }

  sub type          { shift->{val}->type }
  sub io_r          { shift->{val}->io_r }
  sub io_w          { 1 }
  sub children      { @{+shift}{'mutable', 'val'} }
  sub with_children { ref(shift)->new(@_) }

  sub val { shift->{val}->val }
}


package phi::compiler::abstract_io_r
{
  # NB: the sole purpose of this class is to introduce a fictitious IO reference
  # that prevents constant evaluation.

  use parent -norequire => 'phi::compiler::abstract_base';

  sub new
  {
    my ($class, $v) = @_;
    bless \$v, $class;
  }

  sub type          { ${+shift}->type }
  sub io_r          { 1 }
  sub io_w          { ${+shift}->io_w }
  sub children      { ${+shift} }
  sub with_children { ref(shift)->(@_) }
  sub is_mutable    { ${+shift}->is_mutable }
}


=head1 Types
Types are used by compiler backends to allocate values. Because types are
compile-time (and occasionally runtime) values, they behave as abstract
constants.
=cut


package phi::compiler::type_base
{
  use parent -norequire => 'phi::compiler::abstract_base';
  sub val           { phi::compiler::type_meta->new(shift) }
  sub type          { shift->val->type }
  sub io_r          { 0 }
  sub io_w          { 0 }
  sub children      { () }
  sub with_children { shift }

  sub is_primitive   { 0 }
  sub is_int         { 0 }
  sub is_indexed     { 0 }
  sub is_monomorphic { 1 }

  sub flatten_union  { shift }
  sub explain        { shift->id }
}


package phi::compiler::type_meta
{
  use parent -norequire => 'phi::compiler::type_base';
  sub is_primitive { 1 }

  sub new
  {
    my ($class, $type) = @_;
    my $package = ref $type;
    bless { package => $package,
            type    => $type }, $class;
  }

  sub id { shift->{package} }
}


package phi::compiler::type_int
{
  use parent -norequire => 'phi::compiler::type_base';
  sub is_primitive { 1 }
  sub is_int       { 1 }

  sub new
  {
    my ($class) = @_;
    bless {}, $class;
  }

  sub id { 0 }
}


package phi::compiler::type_nominal_composite
{
  use Scalar::Util;
  use parent -norequire => 'phi::compiler::type_base';
  sub is_indexed { 1 }

  sub new
  {
    my ($class, @field_types) = @_;
    bless \@field_types, $class;
  }

  sub id
  {
    my ($self)    = @_;
    my $name      = Scalar::Util::refaddr $self;
    my $sub_names = join ", ", map $_->id, @$self;
    "nominal $name\{$sub_names}";
  }
}


package phi::compiler::type_fn
{
  use parent -norequire => 'phi::compiler::type_base';

  sub new
  {
    my ($class, $arg_type, $return_type) = @_;
    bless { arg_type    => $arg_type,
            return_type => $return_type }, $class;
  }

  sub id
  {
    my ($self) = @_;
    "($$self{arg_type} -> $$self{return_type})";
  }
}


package phi::compiler::type_union
{
  use parent -norequire => 'phi::compiler::type_base';

  sub new
  {
    my ($class, @alternatives) = @_;
    my %distinct;
    $distinct{$_->id} = $_ for map $_->flatten_union, @alternatives;
    bless [values %distinct], $class;
  }

  sub flatten_union { @{+shift} }
  sub id            { "(" . join(" | ", shift->flatten_union) . ")" }
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
