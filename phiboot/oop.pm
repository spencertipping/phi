=head1 License
    phi programming language
    Copyright (C) 2018  Spencer Tipping

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
=cut

package phi;

use strict;
use warnings;


=head2 Dual implementation and the self-reference critical path
If the boot image's behavior depends on an object, then we'll need two
implementations of it: one in perl and one in phi. Otherwise (if the object is
just data for bootstrapping purposes), we can get away with just implementing it
in phi. Any Perl code we write is effectively throwaway: Perl is a simple enough
language that we don't have tons of magic we could inherit from the boot process
if we dropped it into the image.

We can also lie to phi to save effort. For example, vtables link to the classes
that produced them but that linkage is strictly informational: there's no
requirement that those classes would reproduce _those_ vtables. This means we
can generate boot-time vtables that all use a single protocol, but tell phi that
we have some well-thought-out class/protocol system. phi can then recompile the
vtables accordingly if it needs to. (The reason all of this must work is that
classes and protocols are mutable objects, so at any given moment there's no
implication that they would be in a state consistent with the vtables they had
generated at a different moment in time.)

Anyway, all of this just lets us save ourselves a little bit of effort when
we're putting phi together.


=head2 Single-protocol vtables
I'm writing all of our methods into a single protocol to simplify the boot
image. This buys us a couple of things:

1. Much better type-error detection: all methods use distinct indexes
2. Method->index allocation is no longer a potential failure mode

All of the methods are declared up front, so it's easy to allocate vtables of
the correct size.
=cut

sub vtable_missing_method
{
  my ($vtable_name, $index) = @_;
  my ($method_name) = grep method_lookup->{$_} == $index,
                           keys %{+method_lookup};
  $method_name //= "undefined method";
  debug_die "$vtable_name doesn't implement method $index ($method_name)";
}

sub vtable
{
  my ($name, %bindings) = @_;
  my @bindings;
  $bindings[mi $_] = phi::allocation->constant($bindings{$_})
                                    ->named("$name\::$_")
                     >> heap
    for keys %bindings;

  # Fill in any missing bindings with functions that die noisily.
  $bindings[$_] //= vtable_missing_method($name, $_) >> heap
    for 0..vtable_size - 1;

  # TODO: add object headers to these vtables (we're returning here-pointers)
  phi::allocation->constant(pack "Q*" => @bindings)->named($name);
}


=head2 Classes and protocols
The idea here is to provide a little bit of safeguarding around the above vtable
stuff, and later on to use this code to generate actual classes/protocols used
by phi. In particular, I want to make sure that objects (1) define which
protocols they claim to implement, and (2) actually implement those protocols.
=cut


our $methods_are_finalized;

sub register_method($)
{
  my $method = shift;
  my $n      = keys %{+method_lookup};
  die "cannot register methods after the table is finalized"
    if $methods_are_finalized
    && !exists method_lookup->{$method};
  method_lookup->{$method} //= $n;
}


package phi::protocol
{
  sub new
  {
    my ($class, $name, @methods) = @_;
    phi::register_method $_ for @methods;
    bless { name    => $name,
            classes => [],
            methods => \@methods }, $class;
  }

  sub name    { shift->{name} }
  sub classes { @{shift->{classes}} }
  sub methods { @{shift->{methods}} }

  # TODO: generate an allocation to describe this object
  # (this will depend on some vtables, so it's not entirely straightforward)
}


package phi::class
{
  sub new
  {
    my ($class, $name, @protocols) = @_;
    bless { name      => $name,
            protocols => \@protocols,
            vtable    => undef,
            defs      => {} }, $class;
  }

  sub name      { shift->{name} }
  sub protocols { @{shift->{protocols}} }

  sub implement
  {
    my $self = shift;
    push @{$$self{protocols}}, @_;
    $self;
  }

  sub def
  {
    my $self = shift;
    die "already finalized class $$self{name}" if defined $$self{vtable};
    while (@_)
    {
      my $name = shift;
      my $def  = shift;
      $$self{defs}{$name} = $def;
    }
    $self;
  }

  sub unimplemented_methods
  {
    my $self = shift;
    map grep(!exists $$self{defs}{$_}, $_->methods), @{$$self{protocols}};
  }

  sub vtable
  {
    my $self = shift;
    my @unimplemented = $self->unimplemented_methods;
    die "class $$self{name} fails to implement @unimplemented"
      if @unimplemented;

    $phi::methods_are_finalized = 1;
    $$self{vtable} //= phi::vtable "$$self{name}_vtable", %{$$self{defs}};
  }

  # TODO: generate an allocation to describe this object (different from the
  # vtable, which is its compiled output)
}


1;
