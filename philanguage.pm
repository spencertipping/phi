=head1 phi language constructs
phi is mostly defined in terms of structs, which exist in two varieties. One is
the mono-struct, which corresponds to an opaque backend type with specific
operation dispatch. This gives you a low-level way to work with types you
didn't define in phi, including primitives.

The other variety is the poly-struct, which is defined as a union of
mono-structs (just like an ocaml union). Method calls against poly-structs are
virtual. Unlike ocaml, you can have a struct that belongs to multiple unions at
a time.

Structs are two-layered constructs. This file defines Perl classes like
C<phi::language::monostruct>, each instance of which will define a new Perl
class like C<phi::language::genstruct::s1>. When you compile that struct, for
instance in Perl, then you'll get a runtime class like C<myprogram::s1>. Here's
what instances within each layer are responsible for:

- C<phi::language::> describes name/fields/methods; methods are second-order
(metaprogramming)

- C<phi::language::genstruct::> describes compiler allocation, e.g. stack vs
heap; methods are first-order (will be compiled into method calls)

- C<myprogram::> a runtime value; method calls have been compiled by phi
=cut

package phi::language;

use strict;
use warnings;


=head1 Perl genpackages
This is something like C<phi::language::genstruct::foo>, and these packages are
used to resolve second-order metafunction calls against struct types themselves.

These packages don't store any data; they just contain methods. They are GC'd
when nobody refers to them.
=cut

package phi::language::genpackage
{
  use Symbol ();
  our $gensym_id = 0;

  sub new
  {
    my ($class, $name, %methods) = @_;
    my $self = bless { name      => $name,
                       gensym_id => ++$gensym_id,
                       methods   => {} }, $class;
    $self->method($_, $methods{$_}) for keys %methods;
    $self;
  }

  sub DESTROY { Symbol::delete_package shift->packagename }

  sub packagename
  {
    my $self = shift;
    "phi::language::gen::$$self{gensym_id}_$$self{name}";
  }

  sub entryname
  {
    my ($self, $e) = @_;
    $self->packagename . "::$e";
  }

  sub method
  {
    my ($self, $name, $f) = @_;
    die "phi::language::genpackage [$$self{name}]: method $name is not a function"
      unless ref $f eq 'CODE';
    *{$self->entryname($name)} = $f;
    $self;
  }
}


=head1 Monomorphic structs
Used for low-level datatypes like ints, strings, etc; these typically compile
into either small value types or types that weren't defined within phi and have
backend-specific interfacing.
=cut

package phi::language::monostruct
{
  sub new
  {
    # Is this being called against phi::language::monostruct itself
    # (constructor), or against a struct instance? If it's against an instance,
    # then we return a blessed Perl value inside the genpackage.
    my $class = shift;
    return $class->instantiate_gen(@_) if ref $class;

    my ($name, $fields, %methods) = @_;
    my $self = bless { name       => $name,
                       genpackage => phi::language::genpackage->new($name),
                       fields     => $fields,
                       methods    => {} }, $class;
    $self->method($_, $methods{$_}) for keys %methods;
    $self;
  }

  sub method
  {
    my ($self, $name, $f) = @_;
    $$self{genpackage}->method($name, $$self{methods}{$name} = $f);
  }

  sub instantiate_gen
  {
    my ($self, @ctor_args) = @_;
    bless { class     => $self,
            ctor_args => \@ctor_args }, $$self{genpackage}->packagename;
  }
}
