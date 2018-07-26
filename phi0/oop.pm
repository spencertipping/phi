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

no warnings 'portable';


=head2 Reflection lists
We end up exporting all of our classes and protocols, so we need to store them
all as they're being defined.
=cut

use constant defined_classes   => [];
use constant defined_protocols => [];


=head2 Method hashing
We need a way to convert method names to stable 64-bit values. I'm using a
murmur2-64 hash here, murmurhash2A to be specific, ported from
L<https://github.com/abrandoned/murmur2/blob/master/MurmurHash2.c>.
=cut

use constant murmur2_m => 0xc6a4a7935bd1e995;
use constant murmur2_r => 47;

sub murmur2a($$)
{
  use integer;
  use bytes;

  my $seed = shift;
  my $h    = $seed ^ length $_[0];

  for my $k (unpack 'Q<*', $_[0] . "\0\0\0\0\0\0\0")
  {
    $k *= murmur2_m;
    $k ^= $k >> murmur2_r;
    $k *= murmur2_m;

    $h ^= $k;
    $h *= murmur2_m;
  }

  $h;
}

sub method_hash($) { murmur2a 0, shift }


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
  phi::allocation
    ->constant(
      bin qq{ [ >debug_die "$vtable_name doesn't implement $method_name"
                ] call_native })
    ->named("method_missing $vtable_name\::$method_name");
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
  $bindings[$_] //= DEBUG_MISSING_METHODS
    ? vtable_missing_method($name, $_) >> heap
    : runtime_fail
  for 0..vtable_size - 1;

  phi::allocation->constant(pack "Q*" => @bindings)->named($name);
}


=head2 Classes and protocols
The idea here is to provide a little bit of safeguarding around the above vtable
stuff, and later on to use this code to generate actual classes/protocols used
by phi. In particular, I want to make sure that objects (1) define which
protocols they claim to implement, and (2) actually implement those protocols.
=cut


our $methods_are_finalized;

sub finalize_methods { $methods_are_finalized = 1 }
sub register_method($)
{
  my $method = shift;
  my $n      = keys %{+method_lookup};
  die "cannot register new methods after the boot vtable is finalized"
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
    my $self = bless { name    => $name,
                       classes => [],
                       methods => \@methods }, $class;
    push @{+phi::defined_protocols}, $self;
    $self;
  }

  sub name    { shift->{name} }
  sub classes { @{shift->{classes}} }
  sub methods { @{shift->{methods}} }

  sub add_class
  {
    my ($self, $class) = @_;
    push @{$$self{classes}}, $class
      unless grep $_ eq $class, @{$$self{classes}};
    $self;
  }
}


package phi::class
{
  use Scalar::Util;
  use overload qw/ 0+ address eq refeq /;

  sub new
  {
    my ($class, $name, @protocols) = @_;
    my $self = bless { name      => $name,
                       protocols => [],
                       vtable    => undef,
                       methods   => {} }, $class;
    push @{+phi::defined_classes}, $self;
    $self->implement(@protocols);
  }

  sub name      { shift->{name} }
  sub methods   { %{shift->{methods}} }
  sub protocols { @{shift->{protocols}} }
  sub address   { (shift->vtable >> phi::heap)->address }
  sub refeq     { Scalar::Util::refaddr(shift) == Scalar::Util::refaddr(shift) }

  sub implement
  {
    my $self = shift;
    for my $p (@_)
    {
      $p->add_class($self);
      push @{$$self{protocols}}, $p
        unless grep $_ eq $p, @{$$self{protocols}};
    }
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
      die "redefining method $$self{name}.$name"
        if exists $$self{methods}{$name};
      $$self{methods}{$name} = $def;
    }
    $self;
  }

  sub unimplemented_methods
  {
    my $self = shift;
    map grep(!exists $$self{methods}{$_}, $_->methods), $self->protocols;
  }

  sub vtable
  {
    my $self = shift;

    # Make sure we implement all protocol contracts
    my @unimplemented = $self->unimplemented_methods;
    warn "class $$self{name} fails to implement:"
       . join("", map "\n- $_", @unimplemented)
       . "\n\n"
      if @unimplemented;

    # Make sure no two protocols we implement have colliding method definitions.
    my %method_mapping;
    for my $p ($self->protocols)
    {
      for my $m ($p->methods)
      {
        warn "$$self{name} implements colliding protocols (method = $m)"
          if exists $method_mapping{$m};
        $method_mapping{$m} = $p;
      }
    }

    # Make sure we don't have stray methods -- i.e. that every method we
    # implement is owned by some protocol
    for my $m (keys %{$$self{methods}})
    {
      warn "$$self{name}\::$m is not specified by any implemented protocol"
        unless grep $_ eq $m, map $_->methods, $self->protocols;
    }

    # NB: technically this finalization is redundant given that we finalize
    # methods automatically at the beginning of classes.pm, but it's worth
    # having it in both places in case I later change the design and forget.
    phi::finalize_methods;

    # Always cache the vtable -- I'm not sure whether phi relies on vtable
    # object identity for some semantic purpose, but we should cache it anyway
    # just to save space.
    $$self{vtable} //= phi::vtable "$$self{name}_vtable", $self->methods;
  }
}


1;
