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

no warnings 'void';


=head2 Heap allocator and pointer management
Our initial machine code file is basically a heap image, so we need to simulate
the behavior of a real heap during bootup. Fortunately that isn't difficult:
objects are just allocated one after another. All we need to do is keep track of
where each object goes so we can point to it.

Most of the code for allocations and the heap is either for convenience or for
safeguarding.
=cut


package phi::allocation
{
  use overload qw/ ${} data_ref
                   ""  name
                   0+  address /;

  sub sized
  {
    my ($class, $size) = @_;
    my $data;
    bless { size => $size,
            addr => undef,
            data => \$data }, $class;
  }

  sub constant
  {
    my $class = shift;
    my $data  = \shift;
    bless { size => length $$data,
            addr => undef,
            data => $data }, $class;
  }

  sub named
  {
    my ($self, $name) = @_;
    $$self{name} = $name;
    $self;
  }

  sub name
  {
    $_[0]->{name} // sprintf "anon 0x%x+0x%x",
                             $_[0]->{addr} // -1,
                             $_[0]->{size};
  }

  sub data_ref       { shift->{data} }
  sub address        { $_[0]->{addr} // die "$_[0] is not yet allocated" }
  sub size           { shift->{size} }
  sub is_allocated   { defined shift->{addr} }
  sub is_initialized { defined ${shift->{data}} }

  sub init_with
  {
    my ($self, $f) = @_;
    $$self = &$f();
    $self;
  }

  sub set_addr
  {
    my ($self, $addr) = @_;
    die "phi::allocation: modifying address of already-allocated object"
      if defined $$self{addr}
      && $$self{addr} != $addr;
    $$self{addr} = $addr;
    $self;
  }

  sub compile
  {
    my $self = shift;

    warn "phi::allocation: compiling an unallocated reference $$self{name}"
       . " (probably a mistake)"
      unless $self->is_allocated;

    die "phi::allocation: cannot compile uninitialized reference $$self{name}"
      unless $self->is_initialized;

    my $reserved = $self->size;
    my $actual   = length $$self;

    die sprintf "phi::allocation: data overflows allocation (%d > %d)",
                $actual,
                $reserved
      if $actual > $reserved;

    warn sprintf "phi::allocation: %s is undersized", $self->name
      if $actual < $reserved;

    # Make sure we pad the data out to the size of the allocation; otherwise
    # we'll accumulate offset errors when we join in the heap.
    pack "a$$self{size}", $$self;
  }
}


package phi::heap
{
  use overload qw/ << allocate
                   >> allocate_left_handed /;

  sub new
  {
    my ($class, $address) = @_;
    bless { location => $address // 0,
            address  => $address // 0,
            lookup   => {},
            objects  => [] }, $class;
  }

  sub address   { shift->{address} }
  sub addressof { shift->{lookup}{+shift}->address }
  sub size      { $_[0]->{address} - $_[0]->{location} }
  sub objects   { @{shift->{objects}} }

  sub align
  {
    my ($self, $to) = @_;
    my $gap = ($to - $self->address) % $to;
    $self->allocate("\0" x $gap) if $gap;
    $self;
  }

  sub mark
  {
    my ($self, $name) = @_;
    $self << phi::allocation->constant("")->named($name);
  }

  sub compile
  {
    my $self = shift;
    my @uninitialized = grep !$_->is_initialized, @{$$self{objects}};
    die "phi::heap: one or more uninitialized refs: "
      . join(", ", @uninitialized)
      if @uninitialized;

    join"", map $_->compile, @{$$self{objects}};
  }

  sub initialize
  {
    my $self = shift;
    while (@_)
    {
      my $name = shift;
      my $data = shift;
      die "phi::heap: no allocation named $name"
        unless exists $$self{lookup}{$name};
      ${$$self{lookup}{$name}} = $data;
    }
    $self;
  }

  sub allocate
  {
    my $self = shift;

    if (ref $_[0] eq 'ARRAY')
    {
      my @xs = @{+shift};
      for (my $i = 0; $i + 1 < @xs; $i += 2)
      {
        $self->allocate(phi::allocation->sized($xs[$i + 1])
                                       ->named($xs[$i]));
      }
      return $self;
    }

    my $thing = ref $_[0] ? shift : phi::allocation->constant(shift);
    return $self if $thing->is_allocated;

    $thing->set_addr($$self{address});
    $$self{address} += $thing->size;
    if (defined $thing->name)
    {
      die sprintf "phi::heap: redefining existing name %s",
                  $thing->name
        if exists $$self{lookup}{$thing->name};
      $$self{lookup}{$thing->name} = $thing;
    }

    push @{$$self{objects}}, $thing;
    $self;
  }

  sub allocate_left_handed
  {
    my $self       = shift;
    my $allocation = ref $_[0] ? shift : phi::allocation->constant(shift);
    $self << $allocation;
    $allocation;
  }
}


=head2 ELF image
Let's go ahead and generate the ELF headers. We have a dangling reference,
C<$elf_start_addr>, that we'll fill in later on.

There's no special reason I use C<0x400000> for the ELF base address. It could
be pretty much anything. I went with C<0x400000> instead of C<0x0> because it
makes it much easier to figure out whether something is a valid boot-image
address.
=cut

use constant elf_base_addr => 0x400000;

use constant heap =>
  phi::heap->new(elf_base_addr)
    << pack(C16SSL => 0x7f, ord"E", ord"L", ord"F", 2, 1, 1, 0,
                      0, 0, 0, 0, 0, 0, 0, 0,
                      2, 62, 1)
    << [elf_start_addr => 8]
    << pack(QQLSSSSSS => 64, 0, 0, 64, 56, 1, 0, 0, 0)
    << pack(LLQQQ => 1, 7, 0, elf_base_addr, 0)
    << [elf_memory_size => 8, elf_file_size => 8]
    << pack(Q => 0x1000);


use constant once_cache => {};

sub once($$)
{
  my ($name, $stuff) = @_;
  once_cache->{$stuff} //=
    phi::allocation->constant($stuff)->named($name) >> heap;
}


=head2 Hashed method calls
Pretty simple: hash the method up front, which produces a 64-bit constant that
we drop into the code to do the lookup.
=cut

use constant defined_methods => {};     # NB: just for debugging

sub bin($);
sub method_hash($);

sub mc($) { mg(shift) . bin"call" }
sub mg($)
{
  warn "generating a call to method $_[0], which doesn't exist in any protocol"
    unless exists defined_methods->{$_[0]};

  my $h = method_hash shift;
  bin qq{                                   # obj &fn
    lit64 >pack "Q>", $h                    # obj &fn mh
    swap call                               # obj &mfn };
}


=head2 Writing code in perl
We can hack this together with a mixture of C<pack> and C<bin>, the latter being
a mixed hex/octal format that's ideal for writing machine code.

NB: C<bin> defines a method-calling macro that generates the hashed value and
calls into the receiver's resolver. C<.foo> expands to this:

  dup m64get lit64 <foohash> swap call call

C<:foo> is a variant that leaves off the final C<call>. This means we return the
method as a function rather than invoking it.


=head3 String literals
Double-quoted stuff will push a heap-allocated string pointer onto the data
stack. C<"foo"> is equivalent to C<< lit64 >pack('Q>', str('foo')) >>.


=head3 Conditional jumps and bracket notation
phi bytecode provides the C<if> instruction, which returns one of two values
based on the truthiness (non-zeroness) of a conditional. This instruction
doesn't by itself manage control flow; that's done by using C<goto> in
conjunction with a code pointer. For example:

  before_stuff...
  get_insnptr                           # offset = 0
  lit8(9)                               # offset = 2
  iplus                                 # offset = 3
  goto                                  # offset = 4

  # these drop instructions get skipped
  drop drop                             # offset = 6
  drop drop                             # offset = 8
  drop                                  # offset = 9

  # ...and goto lands here
  after_stuff...

C<bin> needs a construct that jumps over some code and instead pushes that
code's address, so the above, plus an address push, would become:

  before_stuff...
  [ drop drop drop drop drop ]
  after_stuff...

Then brackets behave like function-quoting delimiters -- and that's exactly how
you would use them. (You wouldn't inline bracketed code because it wouldn't
return to the caller.)

NB: the code generated by this strategy isn't remotely portable, nor is it
ideally fast. It works only because phi0 implements direct memory access. phi2
does brackets correctly by emitting separate code objects that are stored as
dependent links.
=cut

use constant bin_macros => {};
use constant bin_memo_table => {};

sub safe_eval($)
{
  my @r = eval "use strict;" . shift;
  die "eval @_: $@" if $@;
  @r;
}

sub jump_over($)
{
  my $snippet = shift;
  my $length  = length $snippet;

  # NB: using a constant offset size to keep it simple, although this is
  # suboptimal for code size.
  bin("get_insnptr                      # &prefix             [offset = 0]
       lit8 +10 iplus                   # &code               [offset = 3]
       sget00 lit16 >pack('S>', $length)# &code &code cl      [offset = 8]
       iplus goto                       # &code               [offset = 10]")
  . $snippet;
}

sub str($);

sub bin_($);
sub bin_($)
{
  my $macro_regex = shift;
  my @r;

  while (1)
  {
    last if /\G(?:\]|$)/gc;
    next if /\G(?:\s+|#.*\n?)/gc;

    # Handle brackets recursively
    push(@r, jump_over bin_ $macro_regex), next if /\G\[/gc;

    push(@r, bin_macros->{$1}), next if defined $macro_regex
                                         && /\G($macro_regex)/gc;

    push(@r, pack "C", $1), next     if /\G\+(\d+)/gc;
    push(@r, pack "H*", $1), next    if /\Gx?((?:[0-9a-fA-F]{2})+)/gc;
    push(@r, pack "C", oct $1), next if /\Go([0-3][0-7]{2})/gc;
    push(@r, $1), next               if /\G'(\S+)/gc;
    push(@r, safe_eval $1), next     if /\G>(.*)\n?/gc;

    push(@r, pack'CQ>', 0x13, safe_eval $1),    next if /\G\$(\S+)/gc;
    push(@r, pack'CQ>', 0x13, str($1) >> heap), next if /\G"([^"]*)"/gc;
    push(@r, bin"\"$1\" i.global"), next             if /\G%(\S+)/gc;

    push(@r, bin qq{"$1" swap .symbolic_method}), next if /\G\.'(\S+)/gc;
    push(@r, bin"dup m64get >mc q{$1}"),          next if /\G\.(\S+)/gc;
    push(@r, mg $1), next                              if /\G:(\S+)/gc;

    die "phi::bin: failed to parse starting at " . substr $_, pos $_;
  }

  join"", @r;
}

sub bin($)
{
  my $macro_pattern = join"|", sort { length $b <=> length $a }
                                    keys %{+bin_macros};
  local $_ = shift;

  # NB: memoization strictly for performance (about 2x improvement). There
  # should be no semantic differences at all.
  bin_memo_table->{$_} //=
    bin_(length $macro_pattern ? qr/$macro_pattern/ : undef);
}


=head3 C<bin> macro definitions
A shorthand for defining one-off bin macros. Otherwise you'd need a C<BEGIN>
block to make sure these happened inline with C<use> definitions.
=cut

BEGIN
{
  ++$INC{"phi/binmacro.pm"};
}

use constant profiled_macros => {};

sub phi::binmacro::import
{
  my ($self, $name, $value) = @_;
  my $mcounter = PROFILE_MACROS
    && (phi::allocation->constant(pack Q => 0)
                       ->named("$name macro counter") >> heap)->address;

  profiled_macros->{$name} = $mcounter;

  bin_macros->{$name} =
    (PROFILE_MACROS ? bin qq{13 >pack"Q>", $mcounter
                             3200 46 1001 50 31 47}
                    : '')
    . $value;
}


=head2 Instruction macros
I define the machine code implementations in L<phi0/interpreter.pm>, but for now
we need a way to easily refer to bytecode instructions without using hex.
=cut

use constant insns =>
{ qw/ lit8  10 lit16 11
      lit32 12 lit64 13

      get_insnptr   20
      get_interpptr 21
      get_stackptr  22
      framerel      23

      goto          24
      set_interpptr 25
      set_stackptr  26
      set_frameptr  27

      call        28
      call_native 29
      if          2a

      drop 30
      swap 31
      sget 32
      sset 33

      m8get  40 m8set  41
      m16get 42 m16set 43
      m32get 44 m32set 45
      m64get 46 m64set 47
      memset 4c memcpy 4d

      iplus   50
      itimes  51
      idivmod 52
      ishl    53
      isar    54
      ishr    55
      iand    56
      ior     57
      ixor    58
      ilt     59
      ieq     5a
      iinv    5b
      ineg    5c
      bswap16 5d
      bswap32 5e
      bswap64 5f / };


sub insn_index($) { hex insns->{+shift} }


BEGIN
{
  bin_macros->{$_} = bin insns->{$_} for keys %{+insns};

  # Constant-pushing macros
  bin_macros->{"=$_"} = pack CC => insn_index("lit8"), $_
    for 0..127;
  bin_macros->{"=128"} = pack Cn => insn_index("lit16"), 128;
  bin_macros->{"=256"} = pack Cn => insn_index("lit16"), 256;
}

use phi::binmacro dup => bin q{sget00};

# Hereptr dereferencing
use phi::binmacro unhere => bin q{      # ptr
  dup =2 ineg iplus                     # ptr ptr-2
  m16get ineg iplus                     # ptr - *(ptr-2) };

use phi::binmacro F            => bin q{framerel};
use phi::binmacro get_frameptr => bin q{F 0000};
use phi::binmacro i            => bin q{get_interpptr unhere};
use phi::binmacro inot         => bin q{=0 ieq};


# "under" notation
# It's common to want to push values underneath an object you're working with.
# Normally you'd write this:
#
#   "foo" swap "bar" swap .method
#
# ...but that's clunky. Better is to have a shorthand:
#
#   "foo"_ "bar"_ .method
#
# We can get that by aliasing _ to swap.
use phi::binmacro _ => bin q{swap};


=head3 Bootup C<str> support
phi1 objects begin with a function pointer, which gives us a little bit of
flexibility about how we generate them. In particular, we don't need to have the
dispatch function defined when we create something; we can allocate a forwarding
function that we then rewrite.
=cut

BEGIN
{
  heap->mark("str_dispatch_fn_address")
    << bin("lit64")
    << [str_dispatch_fn => 8]
    << bin("goto");
}

# NB: will be redefined later
sub str($)
{
  once "boot string const \"" . ($_[0] =~ s/[[:cntrl:]]/./gr)
                              . "\""
                              . ++($phi::str_index //= 0),
       pack("QLLna*" => heap->addressof("str_dispatch_fn_address"),
                        1,
                        length $_[0],
                        18,
                        $_[0]);
}


=head2 Defining functions
Currently, writing a phi function looks like this:

  use constant my_fn => phi::allocation
    ->constant(bin q{ ... })
    ->named('my_fn') >> heap;

  BEGIN
  {
    # for convenience:
    bin_macros->{myfn} = bin q{$my_fn call};
  }

Almost all of this is fluff; it would be better to write something simpler:

  use phi::fn my_fn => bin q{...};

=cut

BEGIN
{
  ++$INC{"phi/fn.pm"};
}

use constant profiled_fns => {};

sub phi::fn::import
{
  # Always define into phi:: because phi0 is a single-package project.
  no strict 'refs';
  my ($self, $name, $code) = @_;
  my $fcounter = PROFILE_FNS
    && (phi::allocation->constant(pack Q => 0)
                       ->named("$name fn counter") >> heap)->address;

  profiled_fns->{$name} = $fcounter;

  my $allocation = phi::allocation->constant(
    (PROFILE_FNS ? bin qq{lit64 >pack"Q>", $fcounter
                          dup m64get =1 iplus swap m64set}
                 : '')
    . $code)
    ->named($name) >> heap;

  *{"phi::$name\_fn"} = sub() { $allocation };
  bin_macros->{$name} = bin qq{\$$name\_fn call};
}


=head3 Related: defining global state
Constants are also clunky:

  use constant some_value => phi::allocation
    ->constant(pack Q => ...)
    ->named('some_value') >> heap;

Better is this:

  use phi::constQ some_value => ...;

It's also useful to be able to generate quantities from a functional expression.
For example:

  use phi::fn generate_x => bin q{      # cc
    ... "foo" i.def                     # cc
    goto                                # };

Better is to write the generating expression and bind a compile-time constant
that will end up holding the address of the generated quantity:

  use phi::genconst x => bin q{         #
    ...                                 # x };

Then you can address it by using C<m64get> on the resulting constant:

  bin q{
    $x m64get                           # generated-x
    x                                   # same thing, via bin macro
  };

C<genconst> values are automatically generated by phi0.
=cut

use constant genconst_bindings => [];

BEGIN
{
  ++$INC{"phi/constQ.pm"};
  ++$INC{"phi/genconst.pm"};
}

sub phi::constQ::import
{
  no strict 'refs';
  my ($self, $name, $v) = @_;
  my $allocation = phi::allocation->constant(pack Q => $v)->named($name) >> heap;
  *{"phi::$name"} = sub() { $allocation };
  bin_macros->{$name} = bin qq{\$$name};
}

sub phi::genconst::import
{
  no strict 'refs';
  my ($self, $name, $fn) = @_;
  my $address = (phi::allocation->constant(pack Q => 0)
                                ->named("genconst_$name") >> heap)->address;
  my $fn_addr = (phi::allocation->constant($fn . bin q{swap goto})
                                ->named("genconstfn_$name") >> heap)->address;

  push @{+genconst_bindings}, { name    => $name,
                                fn_addr => $fn_addr,
                                address => $address };

  *{"phi::$name"} = sub() { $address };
  bin_macros->{$name} = bin qq{ lit64 >pack"Q>", $address
                                m64get };
}

sub genconst_generator_code
{
  join '', map bin qq{ [ >debug_print("\\0337\\033[J$$_{name}\\0338", 2)
                         N ] call_native
                       lit64 >pack"Q>", $$_{fn_addr}
                       call
                       lit64 >pack"Q>", $$_{address}
                       m64set },
               @{+genconst_bindings};
}


=head2 Debugging macros
We need a quick way to drop print statements into machine code (and in some
cases exit). These functions provide minimally destructive implementations of
that logic.

It's also useful to be able to nondestructively print hex values; I've written a
macro for that below.
=cut

sub pp { pack shift, pack shift, @_ }

sub debug_print
{
  my ($message, $fd) = @_;
  my $len = length $message;
  $fd //= 1;
  $message = unpack "H*", $message;
  bin qq{
    50 51 52 56 57                      # save registers
    31o300 b001                         # %rax = syscall 1 (write)
    e8 >pp("l/a", "H*", q{$message})    # call +sizeof(message)
    5e                                  # pop &message -> %rsi
    ba >pack(l => $len)                 # mov size -> %rdx
    48c7o307 >pack(l => $fd)            # mov fd -> %rdi
    0f05                                # syscall
    5f 5e 5a 59 58                      # pop register state};
}

use constant exit_by_segfaulting => bin q{ 31o300 ffo060 };

sub exit_with_status
{
  my $code = shift;
  bin qq{
    31o300 b03c                         # %rax = 60
    48c7o307 >pack l => $code           # %rdi = exit code
    0f05                                # syscall};
}

sub debug_die($)
{
  # Prints to stderr, then exits. Use this to indicate an assertion error within
  # the interpreter.
  #
  # We exit via segfault because gdb recognizes this as an unplanned error and
  # preserves program context.
  debug_print(shift, 2) . exit_by_segfaulting;
}


use constant runtime_fail => phi::allocation->constant(exit_with_status 1)
                                            ->named("fail") >> heap;


1;
