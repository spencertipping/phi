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

  x = 5;          # abstract_hosted(int => 5)
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

=head2 Types and abstract hosted values
C<abstract_hosted> is used for values that belong to the hosting environment.
Structs are always represented in terms of their members; the struct as a whole
isn't a constant, though all of its members might be.
=cut


package phi::compiler::abstract_base
{
  use overload qw/ "" explain_outer /;

  sub is_specified { 0 }
  sub is_fn
  {
    my ($self) = @_;
    $self->type->is_specified && $self->type->val->is_fn;
  }

  sub is_constant
  {
    my ($self) = @_;
    $self->is_specified && $self->type->is_specified
                        && !$self->type->is_t;
  }

  sub explain
  {
    my ($self) = @_;
    my $ref  = ref($self) =~ s/^phi::compiler:://r;
    my $type = $self->type;
    my $args = join ", ", $self->children;
    "$ref($args) : $type";
  }

  sub explain_outer
  {
    my ($self) = @_;
    my $inner = $self->explain;
    my $specified = $self->is_specified ? $self->is_constant ? 'C' : 'S' : 'U';
    $inner; # "$inner//$specified";
  }

  sub clone
  {
    my ($self) = @_;
    $self->with_children(map $_->clone, $self->children);
  }

  sub rewrite_arg
  {
    my ($self, $fn_id, $arg_val) = @_;
    $self->with_children(map $_->rewrite_arg($fn_id, $arg_val),
                             $self->children);
  }

  sub val
  {
    my ($self) = @_;
    $self->is_specified
      ? die "specified value $self failed to implement val()"
      : die "can't flatten runtime-dependent value $self";
  }

  sub method_parser
  {
    my ($self, $scope) = @_;
    my $type = $self->type;
    phi::parser::strconst->new('.')->spaced->ignore
      + phi::parser::strclass->more_of('a'..'z', 'A'..'Z', '_')
    >> sub
       {
         $scope->resolve($type->method($_[4]));
       };
  }

  sub parse_continuation
  {
    my ($self, $scope) = @_;
    ($self->type->is_specified
       ? $self->type->val->parse_continuation($scope, $self)
       : phi::parser::parse_none->new)
    | $self->method_parser($scope);
  }

  sub scope_continuation
  {
    my ($self, $scope) = @_;
    $self->type->is_specified
      ? $self->type->val->scope_continuation($scope, $self)
      : $scope;
  }

  sub t_continuation
  {
    my ($self, $t) = @_;

    # Create a sequence point unless we can prove that the value is
    # time-independent.
    $self->type->is_constant
      ? $self->type->val->t_continuation($t, $self)
      : $t->point($self);
  }

  sub resolve
  {
    my ($self, $val) = @_;
    $self->type->resolve($self, $val);
  }
}


package phi::compiler::abstract_compile_error
{
  use parent -norequire => 'phi::compiler::abstract_base';

  # As a value, a compile error isn't specified. This will prevent any attempt
  # to evaluate it, which is what we want. It also has an unknown type -- so no
  # actions can be validated against it.
  sub is_specified { 0 }
  sub type         { phi::compiler::type_any->new }

  sub new
  {
    my ($class, $message, $klass, @args) = @_;
    bless { message => $message,
            class   => $klass,
            args    => \@args }, $class;
  }

  sub explain { "error(" . shift->{message} . ")" }

  sub children { @{shift->{args}} }
  sub with_children
  {
    # NB: not fixed under type because rewriting is how compile errors are
    # resolved (i.e. try to rewrite into the original type, which will return
    # another error if it fails).
    my ($self, @cs) = @_;
    $$self{class}->new(@cs);
  }
}


package phi::compiler::abstract_hosted
{
  use parent -norequire => 'phi::compiler::abstract_base';
  sub is_specified { 1 }

  sub new
  {
    my ($class, $type, $val) = @_;
    bless { type => $type, val => $val }, $class;
  }

  sub get      { shift->{val} }

  sub type     { shift->{type} }
  sub val      { shift }
  sub children { shift->{type} }
  sub with_children
  {
    my ($self, $type) = @_;
    ref($self)->new($type, $$self{val});
  }

  sub explain
  {
    my ($self) = @_;
    "($$self{val} : $$self{type})";
  }

  # NB: abstract_hosted values need to behave as types because containers like
  # tuples are encoded using perl data structures, which are themselves hosted
  # values. A "tuple type" is a tuple of type values, but it's a value like
  # anything else.
  sub is_t          { shift->get->is_t }
  sub id            { shift->get->id }
  sub flatten_union { shift->get->flatten_union }
}


package phi::compiler::abstract_if
{
  use parent -norequire => 'phi::compiler::abstract_base';

  sub new
  {
    my ($class, $cond, $then, $else) = @_;
    my $cond_type = $cond->type;

    return phi::compiler::abstract_compile_error->new(
      "if condition must be a primitive integer (got $cond_type)",
      @_)
    if !$cond_type->is_specified || !$cond_type->val->is_int;

    return $cond->val->get ? $then : $else if $cond->is_constant;

    my $type = phi::compiler::type_union->new($then->type, $else->type);
    bless { cond => $cond,
            then => $then,
            else => $else }, $class;
  }

  sub type          { shift->{type} }
  sub children      { my ($self) = @_; @$self{'cond', 'then', 'else'} }
  sub with_children { ref(shift)->new(@_) }

  sub explain
  {
    my ($self) = @_;
    "if ($$self{cond}) ($$self{then}, $$self{else})";
  }
}


package phi::compiler::abstract_fn
{
  # TODO OMG
  # don't use args + rewriting here; just use an abstract_forward of the arg
  # type. Then resolve it at call-time and reinvoke ->val.

  use parent -norequire => 'phi::compiler::abstract_base';

  sub is_specified { 1 }
  sub can_inline   { 1 }
  sub is_hosted    { 0 }

  sub new
  {
    my ($class, $id, $arg_type, $expr) = @_;
    my $return_type = $expr->type;

    return phi::compiler::abstract_compile_error->new(
      "function type $arg_type -> $return_type is not yet specified",
      @_)
    unless $arg_type->is_constant
        && $return_type->is_constant;

    my $type = phi::compiler::type_fn->new($arg_type->val, $return_type->val);

    bless { id          => $id,
            arg_type    => $arg_type->val,
            return_type => $return_type->val,
            expr        => $expr,
            type        => $type }, $class;
  }

  sub type     { shift->{type} }
  sub children { @{+shift}{'arg_type', 'expr'} }
  sub with_children
  {
    my ($self, $arg_type, $expr) = @_;
    ref($self)->new($$self{id},
                    $arg_type,
                    $expr);
  }

  sub val         { shift }
  sub expr        { shift->{expr} }
  sub arg_type    { shift->{arg_type} }
  sub return_type { shift->{return_type} }

  sub inline
  {
    my ($self, $arg) = @_;
    $$self{expr}->rewrite_arg($$self{id}, $arg);
  }

  sub explain
  {
    my ($self) = @_;
    "fn $$self{id} ($$self{arg_type} -> $$self{return_type}) {$$self{expr}}";
  }
}


package phi::compiler::abstract_hosted_fn
{
  use parent -norequire => 'phi::compiler::abstract_base';

  sub is_specified { 1 }
  sub is_hosted    { 1 }

  sub new
  {
    my ($class, $id, $inline_fn, $arg_type, $return_type) = @_;

    return phi::compiler::abstract_compile_error->new(
      "hosted function $id type $arg_type -> $return_type is not yet specified",
      @_)
    unless $arg_type->is_constant
        && $return_type->is_constant;

    my $type = phi::compiler::type_fn->new($arg_type->val, $return_type->val);
    bless { id          => $id,
            inline_fn   => $inline_fn,
            arg_type    => $arg_type->val,
            return_type => $return_type->val,
            type        => $type }, $class;
  }

  sub type          { shift->{type} }
  sub children      { () }
  sub with_children { shift }

  sub can_inline    { defined shift->{inline_fn} }
  sub val           { shift }
  sub return_type   { shift->{return_type} }
  sub arg_type      { shift->{arg_type} }

  sub inline
  {
    my ($self, $arg) = @_;
    $$self{inline_fn}->($arg);
  }

  sub explain
  {
    my ($self) = @_;
    my $inline = $self->can_inline ? "I" : "i";
    "(hosted $$self{id} $$self{arg_type} -> $$self{return_type} //$inline)";
  }
}


package phi::compiler::abstract_arg
{
  use parent -norequire => 'phi::compiler::abstract_base';

  sub is_specified { 0 }

  sub new
  {
    my ($class, $fn_id, $fn_arg_type) = @_;
    return phi::compiler::abstract_compile_error->new(
      "fn arg type $fn_arg_type is unspecified",
      @_)
    unless $fn_arg_type->is_specified;

    bless { type  => $fn_arg_type->val,
            fn_id => $fn_id }, $class;
  }

  sub type          { shift->{type} }
  sub children      { () }
  sub with_children { shift }

  sub rewrite_arg
  {
    my ($self, $fn_id, $val) = @_;
    $fn_id == $$self{fn_id} ? $val : $self;
  }

  sub explain
  {
    my ($self) = @_;
    "arg($$self{fn_id}) : $$self{type}";
  }
}


package phi::compiler::abstract_call
{
  use parent -norequire => 'phi::compiler::abstract_base';

  sub new
  {
    my ($class, $fn, $arg) = @_;

    return phi::compiler::abstract_compile_error->new(
      "can't call a non-function $fn",
      @_)
    unless $fn->type->is_specified
        && $fn->type->val->is_fn;

    bless { fn   => $fn,
            arg  => $arg,
            type => $fn->type->val->return_type }, $class;
  }

  sub type          { shift->{type} }
  sub children      { @{+shift}{'fn', 'arg'} }
  sub with_children { ref(shift)->new(@_) }

  sub is_specified
  {
    my ($self) = @_;
    $$self{fn}->is_specified && $$self{arg}->is_specified;
  }

  sub val
  {
    my ($self) = @_;
    die "can't take val of an unspecified function call $self"
      unless $self->is_specified;
    return $self unless $$self{fn}->is_constant;
    my $f = $$self{fn}->val;
    $f->can_inline && $$self{arg}->is_specified
      ? $f->inline($$self{arg}->val)->val
      : $self;
  }

  sub explain
  {
    my ($self) = @_;
    "($$self{fn} $$self{arg})";
  }
}


package phi::compiler::abstract_scope
{
  use parent -norequire => 'phi::compiler::abstract_base';

  sub new
  {
    my ($class, $value, $scope) = @_;
    bless { scope => $scope,
            val   => $value }, $class;
  }

  sub type          { shift->{val}->type }
  sub children      { @{+shift}{'scope', 'val'} }
  sub with_children { ref(shift)->new(@_) }

  sub val { shift->{val}->val }

  sub scope_continuation
  {
    my ($self, $scope) = @_;
    $$self{scope}->val;
  }
}


package phi::compiler::abstract_binding
{
  use parent -norequire => 'phi::compiler::abstract_base';
  sub is_specified { shift->{val}->is_specified }

  sub new
  {
    my ($class, $name, $value) = @_;
    bless { name => $name,
            val  => $value }, $class;
  }

  sub type     { shift->{val}->type }
  sub children { shift->{val} }
  sub with_children
  {
    my ($self, $val) = @_;
    ref($self)->new($$self{name}, $val);
  }

  sub val { shift->{val}->val }

  sub scope_continuation
  {
    my ($self, $scope) = @_;
    $scope->bind($$self{name}, $$self{val});
  }

  sub resolve
  {
    my ($self, $v) = @_;
    $$self{val}->resolve($v);
    $self;
  }
}


package phi::compiler::abstract_forward
{
  use parent -norequire => 'phi::compiler::abstract_base';

  sub new
  {
    # NB: typically $val is unspecified
    my ($class, $type, $val) = @_;
    bless { val  => $val,
            type => $type // phi::compiler::type_any->new }, $class;
  }

  sub type { shift->{type} }
  sub val  { shift->{val} }
  sub is_specified
  {
    my ($self) = @_;
    defined $$self{val};
  }

  # WARNING: it's crucial that we return a distinct value from with_children.
  # Forward values are mutable, so we need a way to isolate side-effects.
  sub children
  {
    my ($self) = @_;
    defined $$self{val}
      ? (@$self{'type', 'val'})
      : $$self{type};
  }

  sub with_children { ref(shift)->new(@_) }

  sub resolve
  {
    my ($self, $v) = @_;
    if (defined $$self{val})
    {
      $$self{val}->resolve($v);
    }
    else
    {
      $$self{val} = $v->is_specified ? $v->val : $v;
    }
    $self;
  }

  sub explain
  {
    my ($self) = @_;
    defined $$self{val} ? "forward : $$self{type}"
                        : "unresolved : $$self{type}";
  }
}


=head1 Types
Types are used by compiler backends to allocate values. Because types are
compile-time (and occasionally runtime) values, they behave as abstract
constants. phi also uses types to solve for degrees of monomorphism within
backends. Unlike most statically-typed functional languages, we don't apply
systems like Hindley-Milner to force convergence.

=head2 T (time-dependent)
phi's T is the equivalent of IO monads from Haskell, though as a user of the
language you don't interact with it directly. It's easier to think of T as a
graph of timelines; when you want control over event ordering, you take a
timeline and cons stuff onto it. Like Haskell programs, phi programs are
operations against a global timeline.

=head2 T and union types
phi values can exist in union types, which cause some type of polymorphic
encoding to happen in the backend. T commutes across unions automatically; for
example:

  f = fn (x:int)
    x.if(fn() x.print end,
         fn() stdin.readline end)
  end;

Let's assume C<print> has return type int and C<readline> string; then the type
of C<f> is technically C<< int -> T(int) | T(string) >>. However, phi can
normalize the type by factoring T to the outside, producing C< T(int|string) >.

What happens when only some branches are T?

  f = fn (x:int)
    x.if(fn() x.print end,
         fn() "foo" end)
  end;

Now the type of C<f> is C<< int -> T(int) | string >>. If the function gets
compiled into a backend with this type, the type will be normalized into
C<< int -> T(int|string) >>; but if the function is inlined into the string
case, then T is uninvolved because C<"foo"> is just a constant.

=head2 Type equations
There are some equations that apply to types:

  a | (b | c)   = (a | b) | c           # | is associative
  T T a         = T a                   # T is idempotent
  T (a | b)     = (T a) | T b           # T distributes across unions
  unknown | a   = unknown               # unknown is the limit (bottom) type
  unknown | T a = T (unknown | a)       # unknown doesn't contain T
  (a | b) -> c  = (a -> c) | (b -> c)   # functions can be specialized

Non-equations:

  T unknown    != unknown               # T can't be dropped
  a -> (b | c) != (a -> b) | (a -> c)   # can't constant-fold across unions
  a -> T b     != T (a -> b)            # can't constant-fold T
  (T a) -> T b != T (a -> b)            # ditto

Things we can do in one direction but not both:

  (a -> b) | (a -> c) => (a -> (b | c)) # duh
  a                   => T a            # duh
  a                   => a | b          # duh
  a                   => unknown        # duh
=cut


package phi::compiler::type_base
{
  use parent -norequire => 'phi::parser::parser_base';
  use parent -norequire => 'phi::compiler::abstract_base';

  sub is_specified   { 1 }

  sub is_fn          { 0 }      # are values of this type callable?
  sub is_primitive   { 0 }      # does this type wrap a perl scalar?
  sub is_int         { 0 }      # ...is that scalar an int?
  sub is_string      { 0 }      # ...is it a string?
  sub is_monomorphic { 1 }      # are methods constants?
  sub is_unknown     { 0 }      # are values of this type opaque?
  sub is_t           { 0 }      # does this type use the global timeline?

  sub val            { shift }
  sub type           { phi::compiler::type_meta->new(shift) }
  sub children       { () }
  sub with_children  { shift }

  sub flatten_union  { shift }
  sub explain        { shift->id }

  sub method
  {
    my ($self, $methodname) = @_;
    $self->id . "." . $methodname;
  }

  sub scope_continuation { my ($self, $scope, $val) = @_; $scope }
  sub parse_continuation { phi::parser::parse_none->new }

  sub t_continuation
  {
    my ($self, $t, $val) = @_;
    $self->is_t ? $t->point($val) : $t;
  }

  sub resolve
  {
    # By default, do nothing; only forwards can be resolved, and they specify
    # their own implementation of abstract_base::resolve.
    my ($self, $v, $rv) = @_;
    $v;
  }
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

  sub id { "metatype {" . shift->{type} . "}" }
}


package phi::compiler::type_int
{
  use parent -norequire => 'phi::compiler::type_base';
  sub is_primitive { 1 }
  sub is_int       { 1 }
  sub new          { bless {}, shift }
  sub id           { 'int' }

  sub literal_parser
  {
    my ($self) = @_;
    phi::parser::strclass->more_of("-", 0..9)
    >> sub { phi::compiler::abstract_hosted->new($self, $_[4]) };
  }
}


package phi::compiler::type_string
{
  use parent -norequire => 'phi::compiler::type_base';
  sub is_primitive { 1 }
  sub is_string    { 1 }
  sub new          { bless {}, shift }
  sub id           { 'string' }

  sub literal_parser
  {
    my ($self) = @_;
    phi::parser::strconst->new('"')
      + phi::parser::strclass->many_except('"')
      + phi::parser::strconst->new('"')
    >> sub { phi::compiler::abstract_hosted->new($self, $_[5]) };
  }
}


package phi::compiler::type_any
{
  use parent -norequire => 'phi::compiler::type_base';

  sub is_monomorphic { 0 }
  sub is_unknown     { 1 }
  sub is_primitive   { 1 }

  sub new            { bless {}, shift }
  sub id             { 'any' }

  sub method
  {
    my ($self, $method) = @_;
    phi::compiler::abstract_compile_error->new("virtual method call");
  }
}


package phi::compiler::type_unknown
{
  use parent -norequire => 'phi::compiler::type_base';

  sub is_monomorphic { 0 }
  sub is_unknown     { 1 }
  sub is_primitive   { 1 }

  sub new            { bless {}, shift }
  sub id             { 'unknown' }

  sub literal_parser
  {
    my ($self) = @_;
    phi::parser::strclass->more_of('a'..'z', 'A'..'Z', '_')
    >> sub { phi::compiler::abstract_hosted->new($self, $_[4]) };
  }

  sub bind_parser
  {
    my ($self, $scope, $ident) = @_;
    my $forward       = phi::compiler::abstract_forward->new;
    my $forward_scope = $scope->bind($ident => $forward);

    phi::parser::strconst->new('=')->spaced->ignore + $forward_scope
    >> sub
       {
         $forward->resolve($_[4]);
         phi::compiler::abstract_binding->new($ident => $_[4]);
       };
  }

  sub type_parser
  {
    my ($self, $scope, $ident) = @_;
    my $forward       = phi::compiler::abstract_forward->new;
    my $forward_scope = $scope->bind($ident => $forward);

    phi::parser::strconst->new(':')->spaced->ignore + $forward_scope
    >> sub
       {
         my $f = phi::compiler::abstract_forward->new($_[4]);
         $forward->resolve($f);
         phi::compiler::abstract_binding->new($ident => $f);
       };
  }

  sub parse_continuation
  {
    my ($self, $scope, $ident) = @_;
    return $self->method_parser($scope) unless defined $ident;
    $self->bind_parser($scope, $ident->val->get)
      | $self->type_parser($scope, $ident->val->get);
  }
}


package phi::compiler::type_nominal_struct
{
  use parent -norequire => 'phi::compiler::type_base';

  sub new
  {
    my ($class, $name, %fields) = @_;
    bless { name   => $name,
            union  => phi::compiler::type_union->new(values %fields),
            fields => \%fields }, $class;
  }

  sub type_at_any { shift->{union} }
  sub type_at
  {
    my ($self, $name) = @_;
    $$self{fields}->{$name};
  }

  sub id
  {
    my ($self) = @_;
    my $fields = join ", ", map "$_: $$self{fields}{$_}",
                            sort keys %{$$self{fields}};
    "struct $$self{name} \{$fields}";
  }

  sub t_continuation { die "TODO" }
}


package phi::compiler::type_tuple
{
  use parent -norequire => 'phi::compiler::type_base';

  sub new
  {
    my ($class, @types) = @_;
    return $types[0] if @types == 1;
    bless { types => \@types,
            union => phi::compiler::type_union->new(@types) }, $class;
  }

  sub type_at_any { shift->{union} }
  sub type_at     { my ($self, $i) = @_; $$self{types}->[$i] }
  sub id          { "(" . join(", ", @{shift->{types}}) . ")" }

  sub literal_parser
  {
    my ($self) = @_;
    phi::parser::strconst->new('(')->spaced
    >> sub
       {
         phi::compiler::syntactic_tuple->new;
       };
  }

  sub scope_continuation
  {
    my ($self, $scope, $val) = @_;
    return $scope unless $val->is_specified;
    $scope = $_->scope_continuation($scope) for @{$val->val->get};
    $scope;
  }

  sub t_continuation
  {
    my ($self, $t, $val) = @_;
    return $t->point($val) unless $val->is_specified;
    $t = $_->t_continuation($t) for @{$val->val->get};
    $t;
  }

  sub resolve
  {
    my ($self, $xs, $v) = @_;
    my @xs = @{$xs->val->get};
    for my $i (0..$#xs)
    {
      $xs[$i]->resolve(
        phi::compiler::abstract_call->new(
          phi::compiler::abstract_hosted_fn->new(
            "tuple get $i",
            sub { shift->val->get->[$i] },
            $self,
            $self->type_at($i)),
          $v));
    }
    $xs;
  }
}


package phi::compiler::type_fn
{
  use parent -norequire => 'phi::compiler::type_base';
  sub is_fn { 1 }

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

  sub arg_type    { shift->{arg_type} }
  sub return_type { shift->{return_type} }

  sub parse_continuation
  {
    my ($self, $scope, $val) = @_;
    $scope >> sub { phi::compiler::abstract_call->new($val, $_[4]) };
  }
}


package phi::compiler::type_union
{
  use parent -norequire => 'phi::compiler::type_base';
  sub is_specified   { shift->{is_specified} }
  sub is_monomorphic { 0 }
  sub is_t           { shift->{is_t} }
  sub is_fn          { shift->{is_fn} }

  sub new
  {
    my ($class, @alternatives) = @_;
    my %distinct;
    my $one;
    $one = $distinct{$_->id} = $_
      for map $_->is_specified ? $_->val->flatten_union : $_, @alternatives;

    return $one if keys(%distinct) == 1;

    my @compact      = values %distinct;
    my $is_t         =  grep  $_->is_t,         @compact;
    my $is_fn        = !grep !$_->is_fn,        @compact;
    my $is_specified = !grep !$_->is_specified, @compact;

    bless { alternatives => \@compact,
            is_specified => $is_specified,
            is_t         => $is_t,
            is_fn        => $is_fn }, $class;
  }

  sub flatten_union { @{shift->{alternatives}} }
  sub id            { "(" . join(" | ", shift->flatten_union) . ")" }

  sub parse_continuation
  {
    my ($self, $scope, $v) = @_;
    phi::parser::intersection->new(
      map $_->parse_continuation($scope, $v), @{$$self{alternatives}});
  }
}


package phi::compiler::type_t
{
  use parent -norequire => 'phi::compiler::type_base';
  sub is_specified   { ${+shift}->is_specified }
  sub is_t           { 1 }
  sub is_monomorphic { ${+shift}->is_monomorphic }
  sub is_unknown     { ${+shift}->is_unknown }
  sub is_fn          { ${+shift}->is_fn }

  sub new
  {
    my ($class, $type) = @_;
    ref($type) eq $class
      ? $type
      : bless \$type, $class;
  }

  sub id { "T(" . ${+shift} . ")" }

  sub flatten_union
  {
    my ($self) = @_;
    map ref($self)->new($_), $$self->flatten_union;
  }

  sub parse_continuation { ${+shift}->parse_continuation(@_) }
}


=head1 Syntactic values
Entry points for things like function and struct parsing. We don't have the
option of using literal parsers for these because the parsers incorporate the
surrounding scope.
=cut


package phi::compiler::syntactic_base
{
  use parent -norequire => 'phi::parser::parser_base';
  sub new          { bless {}, shift }
  sub type         { phi::compiler::type_any->new }
  sub is_specified { 1 }

  sub scope_continuation { my ($self, $scope) = @_; $scope }

  sub comma_separated
  {
    my ($self, $parser) = @_;
    $parser + (phi::parser::strconst->new(',')->spaced->ignore + $parser)
              ->repeat(0);
  }

  sub explain { ref shift }
}


package phi::compiler::val_tuple
{
  use overload qw/ "" explain /;
  sub explain { '(' . join(', ', @{+shift}) . ')' }
}


package phi::compiler::syntactic_tuple
{
  use parent -norequire => 'phi::compiler::syntactic_base';

  sub parse_continuation
  {
    my ($self, $scope) = @_;
    $self->comma_separated($scope)
      + phi::parser::strconst->new(')')->spaced->ignore
      >> sub
         {
           my @xs = @_[4..$#_];
           @xs > 1 ? phi::compiler::abstract_hosted->new(
                       phi::compiler::type_tuple->new(map $_->type, @xs),
                       bless \@xs, 'phi::compiler::val_tuple')
                   : $xs[0];
         };
  }
}


package phi::compiler::syntactic_fn
{
  use parent -norequire => 'phi::compiler::syntactic_base';

  sub parse_continuation
  {
    my ($self, $scope) = @_;
    $scope >> sub { phi::compiler::syntactic_fn_body->new($_[4]) };
  }
}


package phi::compiler::syntactic_fn_body
{
  use parent -norequire => 'phi::compiler::syntactic_base';
  our $fn_id_gensym = 0;

  sub new
  {
    my ($class, $arg) = @_;
    bless \$arg, $class;
  }

  sub parse_continuation
  {
    my ($self, $scope) = @_;
    my $cloned_args = $$self->clone;
    $cloned_args->scope_continuation($scope)
    >> sub
       {
         my $fn_body = $_[4];
         my $fn_id   = ++$fn_id_gensym;
         $cloned_args->resolve(
           phi::compiler::abstract_arg->new($fn_id, $cloned_args->type));

         phi::compiler::abstract_fn->new(
           $fn_id,
           $cloned_args->type,
           $fn_body);
       };
  }
}


=head1 Scopes, locals, and methods
Scopes are responsible for all bindings including literals, struct methods,
variables, and globals. They also drive expression parsing and are responsible
for delegating to parse continuations.

Internally, a scope is an alternation of parsers that resolve various syntactic
elements. I'll go through a quick example to describe the linkage structure:

  # root scope; parent = undef
  x = 10;                   # sibling: previous = root, parent = undef
  y = 20;                   # sibling: previous = (x = 10), parent = undef
  f = fn (x:int)            # sibling: previous = (y = 20), parent = undef
    x.inc                   # child: previous = undef, parent = (y = 20)
  end;
  z = 30;                   # sibling: previous = (f = ...), parent = undef

=head2 More detail about how the above works
Let's start right at the top. phi's root scope is prepopulated with a parser
that resolves any identifier into an unknown value, and the parse continuation
of an unknown includes a rule that matches C<= expression>. That, in turn,
produces an abstract assignment, which is just a wrapper for C<expression> whose
scope continuation contains the new binding.

The word C<fn> binds to a value whose continuation is a function body. This
means you can alias it:

  function = fn;
  f = function (x:int) x.inc end;

The same is true of structs, which are also lexically bound. C<int> refers to a
primitive type that internally has no name:

  integer = int;
  f = fn (x:integer) x.inc end;

In other words, phi's grammar is deceptively simple: everything is either a
value or a minimal operator over values, all values are parsed by a scope, and
all of this works because expressions are aggressively constant-folded at parse
time.

=head2 Method resolution
Scopes bind methods to types just like they bind values to variables. For
example, C<inc> was bound to C<int> like this:

  int.method("inc") = fn (self:int) self.plus(1) end;

The scope sees this no differently from a variable assignment: method() returns
an unbound value whose parse continuation consumes the assignment and creates a
scope continuation with a new variable named C<int.inc>. (Types have names they
use for themselves; if you defined a type, it would look something like
C<struct foo {int, int}.inc>.)

Scopes resolve methods in exactly the same way; C<5.inc> is resolved to
C<int.method("inc")>, which returns a name that in turn is resolved by the
scope (possibly into an unknown value).

=head2 Scopes as parsers
Scopes return singular values that define scope continuations. The scope is also
an argument to every parser, and both continuation methods:

  $scope = $val->scope_continuation($scope);
  ...    = $val->parse_continuation($input, $start, $scope);
=cut


package phi::compiler::scope_base
{
  use parent -norequire => 'phi::parser::parser_base';
  sub is_specified { 1 }
  sub val          { shift }

  sub child
  {
    my ($self) = @_;
    phi::compiler::scope_root->new($self);
  }

  sub bind
  {
    my ($self, $name, $value, %bindings) = @_;
    my $parser = phi::parser::strconst->new($name) >>sub {$value};
    for my $k (keys %bindings)
    {
      $parser |= phi::parser::strconst->new($k) >>sub {$bindings{$k}};
    }
    $self->link($parser);
  }

  sub link
  {
    my ($self, $parser) = @_;
    phi::compiler::scope_link->new($self->parent, $self, $parser);
  }

  sub parent   { shift->{parent} }
  sub previous { shift->{previous} }

  sub atom_parser { shift->{parser} }

  sub previous_parse
  {
    my ($self, $input, $start, $scope) = @_;
    return $self->previous->parse($input, $start, $scope) if defined $self->previous;
    return $self->parent->parse($input, $start, $scope)   if defined $self->parent;
    $self->fail("end of scope chain");
  }

  sub parse
  {
    my ($self, $input, $start, $scope) = @_;
    $scope //= $self;
    my ($iok, $il)      = phi::syntax::ignore->parse($input, $start, $scope);
    my $offset          = $start + $il;
    my ($eok, $el, $ev) = $self->atom_parser->parse($input, $offset, $scope);

    return $self->previous_parse($input, $start, $scope) unless $eok;

    $offset += $el;
    for (my ($cok, $cl, $cv) = ($eok, $el, $ev);
         $scope = $ev->scope_continuation($scope),
         ($cok, $cl, $cv) = $ev->parse_continuation($scope)
                               ->parse($input, $offset, $scope)
           and $cok;
         $offset += $cl, $ev = $cv)
    {}

    $self->return($offset - $start, $ev);
  }

  sub resolve
  {
    my ($self, $name) = @_;
    my ($ok, $l, $x)  = $self->parse(phi::parser::strinput->new($name), 0);
    $ok && $l == length $name
      ? $x
      : phi::compiler::abstract_hosted->new(
          phi::compiler::type_unknown->new,
          $name);
  }

  sub explain
  {
    my ($self)   = @_;
    my $parent   = $$self{parent}   // "no parent";
    my $previous = $$self{previous} // "no previous";
    "scope { $parent, $previous, $$self{parser} }";
  }
}


package phi::compiler::scope_root
{
  use parent -norequire => 'phi::compiler::scope_base';

  sub new
  {
    my ($class, $parent) = @_;
    bless { parent   => $parent,
            previous => undef,
            parser   => phi::parser::parse_none->new }, $class;
  }

  sub explain
  {
    my ($self) = @_;
    my $p = $$self{parent} // "";
    "root scope($p)";
  }
}


package phi::compiler::scope_link
{
  use parent -norequire => 'phi::compiler::scope_base';

  sub new
  {
    my ($class, $parent, $previous, $parser) = @_;
    bless { parent   => $parent,
            previous => $previous,
            parser   => $parser }, $class;
  }
}


1;
