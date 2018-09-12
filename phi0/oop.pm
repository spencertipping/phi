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
no warnings 'void';


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

# NB: always set LSB so we can differentiate between hashed values and string
# pointers (the latter will always be 8-byte aligned)
sub method_hash($) { 1 | murmur2a 0, shift }


=head2 Global timing functions
We use these for profiling, so we'll need to have them defined very early on.
=cut

use phi::fn micros => bin q{            # cc
  =0 =0                                 # cc micros seconds
  =0 =0 =0 =0 =0
  get_stackptr =40 iplus                # cc us s 0... &s
  =96 syscall drop                      # cc us s
  lit32 000f4240 itimes iplus           # cc us'
  _ goto                                # us' };

use constant rdtsc_native => phi::allocation
  ->constant(bin q{
    0f 31                               # rdtsc -> %edx:%eax
    # %edx are high 32 bits, %eax are low 32 bits. Left-shift and OR them
    # into a single value, push that, then clear %eax and use the regular
    # advancement macro.
    48c1o342 +32                        # shlq 32, %rdx
    4809o302                            # %rdx |= %rax
    31o300                              # xor %eax, %eax
    52 N                                # push %rdx })
  ->named('rdtsc_native') >> heap;

use phi::binmacro rdtsc => bin q{$rdtsc_native call_native};


=head2 Linear table method dispatch
This is pretty simple. The basic idea is that we have an inline list of hash ->
fn pairs, each of which is built more or less like this:

  pack QQ => method_hash("methodname"), $code_hereptr;

Then the lookup function seeks 16 bytes at a time, comparing the first entry and
jumping to the second if it matches. If we hit the end of the list (a key of 0)
and haven't found a match, then we segfault.

Here's the function that does the lookup.
=cut

BEGIN
{
  heap << [method_hash_lookup_table => 8];

  # Suppress warnings from mlookup_fn
  defined_methods->{'pnl_err'} = 0;
  defined_methods->{'{}'} = 0;
  defined_methods->{'die'} = 0;
}

use constant MLOOKUP_USE_NATIVE => 1;

use constant mlookup_native => phi::allocation
  ->constant(bin q{
    5e595a                              # cc->%rsi, kvs->%rcx, m->%rdx
    483bo021                            # cmp %rax, *%rcx   [offset=3]
    7406                                # je found_it       [offset=5]
    4883o301 10                         # %rcx += 16        [offset=9]
    ebf5                                # jmp loopstart     [offset=11]
  # found_it:
    ffo16108                            # push *(%rcx + 8)
    N                                   # ->phi })
  ->named('mlookup_native') >> heap;

use constant profiled_methods   => {};
use constant profiled_receivers => {};

use phi::constQ mlookup_cycles => 0;

use constant mlookup_profstart => PROFILE_MLOOKUP
  ? bin q{mlookup_cycles m64get rdtsc ineg iplus
          mlookup_cycles m64set}
  : '';

use constant mlookup_profend => PROFILE_MLOOKUP
  ? bin q{mlookup_cycles m64get rdtsc iplus
          mlookup_cycles m64set}
  : '';

use phi::fn mlookup => DEBUG_MISSING_METHODS
  ? bin q{                                # m &kvs cc
    >mlookup_profstart
    [                                     # m &kvs cc loop
      sget02 m64get dup                   # m &kvs cc loop k k?

      [ sget04 ieq                        # m &kvs cc loop k==m?
        [ drop sset01 =8 iplus            # cc &v
          m64get swap                     # v cc
          >mlookup_profend
          goto ]                          # v
        [ sget02 =16 iplus sset02         # m &kvs' cc loop
          dup goto ]                      # ->loop
        if goto ]                         # m &kvs cc loop

      [                                   # m &kvs cc loop k
        # We've hit the end of the method list. Print the hash of the method
        # that wasn't defined on this class.
        [ >debug_print "\n", 2
          N
        ] call_native
        drop drop debug_trace             # m &kvs cc [print(cc)]
        drop      debug_trace             # m &kvs [print(&kvs)]
        drop      debug_trace             # m [print(m)]

        # NB: the following will fail horribly (infinite loop) if these methods
        # don't exist on their respective objects.
        lit64 >pack "Q>", heap->addressof("method_hash_lookup_table")
        m64get .{} i.pnl_err
        "call to undefined method" i.die ]
      if goto ]

    dup goto                              # ->loop }

  : MLOOKUP_USE_NATIVE
  ? bin q{$mlookup_native call_native}
  : bin q{                                # m &kvs cc
    >mlookup_profstart
    [                                     # m &kvs cc loop
      sget02 m64get sget04 ieq            # m &kvs cc loop k==m?
      [ drop sset01 =8 iplus              # cc &v
        m64get swap                       # v cc
        >mlookup_profend
        goto ]                            # v
      [ sget02 =16 iplus sset02           # m &kvs' cc loop
        dup goto ]                        # ->loop
      if goto ]                           # m &kvs cc loop
    dup goto                              # ->loop };


sub method_trace_prefix($$)
{
  my ($classname, $method) = @_;
  DEBUG_TRACE_ALL_METHODS
    ? bin qq{ [ >debug_print "$classname\::$method\\n"
                31o300 N ] call_native }
    : '';
}

sub method_profile_prefix($$$)
{
  my ($classname, $method, $rcounter) = @_;
  my $mcounter = PROFILE_METHODS
    && (phi::allocation->constant(pack Q => 0)
                       ->named("$classname\::$method profcount") >>
                       heap)->address;

  profiled_methods->{"$classname\::$method"} = $mcounter;

  my $mprof = PROFILE_METHODS
    ? bin qq{ lit64 >pack"Q>", $mcounter
              dup m64get =1 iplus swap m64set }
    : '';

  my $rprof = PROFILE_RECEIVERS
    ? bin qq{ lit64 >pack"Q>", $rcounter
              dup m64get =1 iplus swap m64set }
    : '';

  $mprof . $rprof;
}

use constant method_profile => {};
BEGIN
{
  if (-r "phi1.mcounts")
  {
    open my $fh, "<phi1.mcounts" or die $!;
    %{+method_profile} = split /\n/, join"", <$fh>;
  }
}

sub method_dispatch_fn
{
  my ($classname, %methods) = @_;
  my $rcounter = PROFILE_RECEIVERS
    && (phi::allocation->constant(pack Q => 0)
                       ->named("$classname profcount") >> heap)->address;

  profiled_receivers->{$classname} = $rcounter;

  my $table_addr = (phi::allocation
    ->constant(pack 'Q*',
      map((method_hash $_,
           (ref $methods{$_}
             ? $methods{$_}
             : phi::allocation
                 ->constant(method_trace_prefix($classname, $_)
                          . method_profile_prefix($classname, $_, $rcounter)
                          . $methods{$_})
                 ->named("$classname\::$_")) >> heap),
          sort { (method_profile->{$b} // 0) <=> (method_profile->{$a} // 0)
                 || $a cmp $b }
               keys %methods),
      0)
    ->named("$classname method table") >> heap)->address;

  phi::allocation
    ->constant(bin qq{                  # m cc
      lit64 >pack "Q>", $table_addr     # m cc &kvs
      swap \$mlookup_fn goto            # ->mlookup_fn[cc] })
    ->named("$classname method dispatch");
}


=head2 Classes and protocols
The idea here is to provide a little bit of safeguarding around the above vtable
stuff, and later on to use this code to generate actual classes/protocols used
by phi. In particular, I want to make sure that objects (1) define which
protocols they claim to implement, and (2) actually implement those protocols.
=cut


package phi::protocol
{
  sub new
  {
    my ($class, $name, @methods) = @_;
    my $self = bless { name    => $name,
                       classes => [],
                       methods => \@methods }, $class;
    push @{+phi::defined_protocols}, $self;
    phi::defined_methods->{$_} = phi::method_hash $_ for @methods;
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
    my $self = bless { name          => $name,
                       name_str_addr => (phi::str($name) >> phi::heap)->address,
                       protocols     => [],
                       fn            => undef,
                       methods       => {} }, $class;
    push @{+phi::defined_classes}, $self;
    $self->implement(@protocols);
  }

  sub name      { shift->{name} }
  sub methods   { %{shift->{methods}} }
  sub protocols { @{shift->{protocols}} }
  sub address   { (shift->fn >> phi::heap)->address }
  sub refeq     { Scalar::Util::refaddr(shift) == Scalar::Util::refaddr(shift) }

  sub methods_only
  {
    my ($self, @ms) = @_;
    %{$$self{methods}}{@ms};
  }

  sub methods_except
  {
    my ($self, @nope) = @_;
    my %nope;
    ++$nope{$_} for @nope;
    my %result;
    $nope{$_} or $result{$_} = $$self{methods}{$_}
      for keys %{$$self{methods}};
    %result;
  }

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
    die "already finalized class $$self{name}" if defined $$self{fn};
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

  sub verify
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
        unless $m eq "to_s"
            or grep $_ eq $m, map $_->methods, $self->protocols;
    }

    $self;
  }

  sub fn
  {
    my $self = shift;
    $$self{fn} //= phi::method_dispatch_fn
      $$self{name},
      to_s => phi::bin qq{ lit64 >pack"Q>", $$self{name_str_addr}
                           sset01 goto },
      $self->verify->methods;
  }
}


=head2 Declarative notation
Just like we do for functions and constants (in L<phi0/image.pm>), let's define
shorthands to define classes and protocols. Here's what we can write:

  use phi::class foo => p1, p2, ...,
      method1 => bin q{...},
      method2 => bin q{...};

  use phi::protocol bar => qw/ bif baz ... /;

Those will turn into this:

  use constant foo_class => phi::class->new('foo',
    p1, p2, ...)
    ->def(method1 => bin q{...},
          method2 => bin q{...});

  use constant bar_protocol => phi::protocol->new('bar',
    qw/ bif baz ... /);

=cut

BEGIN
{
  ++$INC{"phi/class.pm"};
  ++$INC{"phi/protocol.pm"};
}

sub phi::class::import
{
  no strict 'refs';
  my ($self, $classname, @args) = @_;
  my @protos;
  push @protos, shift @args while @args && ref $args[0];
  my $class = phi::class->new($classname, @protos)->def(@args);
  *{"phi::$classname\_class"} = sub() { $class };
}

sub phi::protocol::import
{
  no strict 'refs';
  my ($self, $protoname, @methods) = @_;
  my $proto = phi::protocol->new($protoname, @methods);
  *{"phi::$protoname\_protocol"} = sub() { $proto };
}


1;
